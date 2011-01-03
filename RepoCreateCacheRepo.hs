{-# LANGUAGE PatternGuards #-}

import Control.Monad (foldM, liftM, when)
import Data.Char (isSpace)
import qualified Data.Foldable as DF
import Data.List (isPrefixOf, stripPrefix, intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import HSH (run)
import System.Directory (getModificationTime, getDirectoryContents, getCurrentDirectory, copyFile)
import System.Environment (getProgName, getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (replaceDirectory, takeFileName)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Printf (printf)


-- * Usage and argument processing

showUsageAndExit :: ExitCode -> IO a
showUsageAndExit exitCode = do
  getProgName >>= \p -> putStrLn $ concat [" Usage:  ", p, " <repo-name> [<repo-path>]"]
  putStrLn "   Creates a pacman repo of installed pkgs found in your pacman cache\n\
           \   dir (assuming you only have one). And copies the packages to the repo.\n\
           \ Command line args:\n\
           \   repo-name    name your repo. You should put this name in your\n\
           \                pacman config\n\
           \   repo-path    defaults to working dir"
  exitWith exitCode

repoFromArgsOrShowUsage :: IO Repo
repoFromArgsOrShowUsage =
  getArgs >>= \argv ->
    case argv of
         []                    -> showUsageAndExit (ExitFailure 1)
         ["-h"]                -> showUsageAndExit ExitSuccess
         ["--help"]            -> showUsageAndExit ExitSuccess
         [repName']            -> return . Repo repName' =<< getCurrentDirectory
         (repName':repDir':[]) -> return $ Repo repName' repDir'
         _                     -> showUsageAndExit (ExitFailure 1)


-- * General helper functions

putMe, putMeLn :: String -> IO ()
putMe s = getProgName >>= \p -> putStr $ concat [p, ": ", s]
putMeLn s = putMe (s ++ "\n")

-- | Trims any leading and trailing spaces from a string
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

getRealDirectoryContentsFullPaths :: FilePath -> IO [FilePath]
getRealDirectoryContentsFullPaths dir = do
  fileNames <- filter (`notElem` [".",".."]) `liftM` getDirectoryContents dir
  -- This is a cunning way of adding on the dir
  return $ map (`replaceDirectory` dir) fileNames


-- * Types

data Repo = Repo {
    repoName :: String
  , repoDir  :: FilePath
  }

type PkgName = String
type PkgVersion = String

data PkgId = PkgId {
    pkgName    :: PkgName
  , pkgVersion :: PkgVersion
  }
  deriving (Eq, Ord)

instance Show PkgId where
  show pkgId = concat [pkgName pkgId, " ", pkgVersion pkgId]

type PkgIdSet = Set.Set PkgId

type PkgFilePath = FilePath
type CacheDir = FilePath
type PkgCache = Map.Map PkgId PkgFilePath


-- * Main program functions

-- | Discovers the pacman cache dir by looking at pacman's verbose output.
pacmanCacheDir :: IO CacheDir
pacmanCacheDir = do
  (out, finish) <- run "pacman -v" :: IO (String, IO (String, ExitCode))
  _ <- finish  -- Let pacman finish
  -- This is safe; "Cache Dirs: " is a hard coded string in the pacman source
  let cacheDirLinePrefix = "Cache Dirs: "
  cacheDirLine <- case filter (cacheDirLinePrefix `isPrefixOf`) (lines out) of
                       []  -> error $ concat ["Could not find '", cacheDirLinePrefix, "' in the following pacman output:\n", out]
                       [x] -> return x
                       _   -> error $ "Unexpected pacman output:\n" ++ out
  let Just cacheDirUntrimmed = cacheDirLinePrefix `stripPrefix` cacheDirLine
  let cacheDir = trim cacheDirUntrimmed
  if ' ' `elem` cacheDir
     then error $ "It seems you have more than one cache dir: " ++ cacheDir
     else return cacheDir

installedPkgIdSet :: IO PkgIdSet
installedPkgIdSet =
  (run "pacman -Q" :: IO [String]) >>=
    foldM f Set.empty
  where f set line =
          case break isSpace line of
               (name@(_:_), ver@(_:_))
                 | verTrimmed@(_:_) <- trim ver ->
                   return $ Set.insert (PkgId name verTrimmed) set
                 | otherwise                    -> ignorePkg
               _                      -> ignorePkg
          where ignorePkg = do
                  putMeLn $ "ignoring strange package: " ++ line
                  return set

-- | Given a 'PkgIdSet' and a file name, finds the pkg in the 'PkgIdSet' which
-- corresponds to the file name.
matchFileNameWithPkg :: PkgIdSet -> String -> Maybe PkgId
matchFileNameWithPkg idSet fn
  | '-' `elem` fn = findMatchingPkgId "" (removeFileNameSuffixUpToVersion fn)
  | otherwise     = Nothing
  where removeFileNameSuffixUpToVersion =
          reverse . tail . dropWhile (/= '-') . reverse
        -- Recursively splits the filename at '-' to construct PkgId
        findMatchingPkgId :: String -> String -> Maybe PkgId
        findMatchingPkgId nam ver =
          case span (/= '-') ver of
               ([]    , '-':xs  ) -> findMatchingPkgId (nam ++ "-") xs
               (namSuf, '-':ver') ->
                  let nam' = if nam == ""
                                then namSuf
                                else concat [nam, "-", namSuf]
                      pkgId = PkgId nam' ver'
                  in if pkgId `Set.member` idSet
                        then Just pkgId
                        else findMatchingPkgId nam' ver'
               (_     , []      ) -> Nothing
               -- This final case is unreachable given span's contract
               _                    -> Nothing

pkgCache :: PkgIdSet -> CacheDir -> IO PkgCache
pkgCache idSet cacheDir =
  getRealDirectoryContentsFullPaths cacheDir >>= foldM f Map.empty
  where f m fp =
          case matchFileNameWithPkg idSet (takeFileName fp) of
               Just pkgId
                 | pkgId `Map.member` m -> do
                   latestFp <- latest fp (m Map.! pkgId)
                   return $ Map.insert pkgId latestFp m
                 | otherwise            ->
                   return $ Map.insert pkgId fp m
               Nothing    -> return m
        latest fp1 fp2 = do
          t1 <- getModificationTime fp1
          t2 <- getModificationTime fp2
          if t1 > t2 then return fp1 else return fp2

installedButNotFoundInCache :: PkgIdSet -> PkgCache -> [PkgId]
installedButNotFoundInCache pkgIdSet cache =
  DF.foldr consIfNotInCache [] pkgIdSet
  where consIfNotInCache pkgId xs =
          if not (pkgId `Map.member` cache) then pkgId:xs else xs

repoAdd :: Repo -> PkgCache -> IO ()
repoAdd repo cache =
  run (  "repo-add"
       , [  "-q"
          , (repoName repo `replaceDirectory` repoDir repo) ++ ".db.tar.gz"
         ] ++ cacheFps
      )
  where cacheFps :: [PkgFilePath]
        cacheFps = DF.foldr (:) [] cache

copyCacheFilesToRepo :: Repo -> PkgCache -> IO ()
copyCacheFilesToRepo repo cache = do
  showPerc 0 False
  DF.foldrM copy 1 cache >> return ()
  where copy :: PkgFilePath -> Int -> IO Int
        copy fp i = do
          copyFile fp (fp `replaceDirectory` repoDir repo)
          showPerc ((i*100) `div` Map.size cache) True
          return (i+1)
        showPerc :: Int -> Bool -> IO ()
        showPerc perc ovr = do
          when ovr $ putStr "\ESC[3D"  -- move cursor back 3 chars
          printf "%2d%%" perc

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  repo      <- repoFromArgsOrShowUsage
  cacheDir  <- pacmanCacheDir
  pkgIdSet  <- installedPkgIdSet
  pkgCache' <- pkgCache pkgIdSet cacheDir
  putMeLn "Running repo-add:"
  repoAdd repo pkgCache'
  case installedButNotFoundInCache pkgIdSet pkgCache' of
       [] -> return ()
       xs ->
         putMeLn $ "These installed pkgs were not found in your cache (so were\n\
                   \ not added to the repo):\n\
           \ " ++ intercalate "\n " (map show xs)
  putMe "Copying pkg files to repo... "
  copyCacheFilesToRepo repo pkgCache'
  putMeLn "  I'm Done!"

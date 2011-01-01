{-# LANGUAGE PatternGuards #-}

-- alpm_option_get_cachedirs (pacman.c 1427)
-- clean up is on line 277 (init 1326)

import Control.Monad (foldM)
import Data.Char (isSpace)
import qualified Data.Foldable as DF
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Set as Set
import Data.String.HT (trim)
import HSH (run)
import System.Directory (getModificationTime)
import System.Environment (getProgName, getArgs)
import System.Exit (ExitCode(..), exitWith)
import qualified System.FilePath.Find as F
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Time (ClockTime(TOD))

showUsageAndExit :: ExitCode -> IO a
showUsageAndExit exitCode = do
  getProgName >>= \p -> putStrLn $ concat [" Usage:  ", p, " [<repo-path>] <repo-name>"]
  putStrLn "   Creates a pacman repo of installed pkgs found in your pacman cache\n\
           \   dir (assuming you only have one). And copies the packages to the repo.\n\
           \ Command line args:\n\
           \   repo-path    defaults to working dir\n\
           \   repo-name    name your repo. You should put this name in your\n\
           \                pacman config"
  exitWith exitCode

putMe, putMeLn :: String -> IO ()
putMe s = getProgName >>= \p -> putStr $ concat [p, ": ", s]
putMeLn s = putMe (s ++ "\n")

-- TODO update
repoNameFromArgsOrShowUsage :: IO String
repoNameFromArgsOrShowUsage =
  getArgs >>= \argv ->
    case argv of
         []         -> showUsageAndExit (ExitFailure 1)
         ["-h"]     -> showUsageAndExit ExitSuccess
         ["--help"] -> showUsageAndExit ExitSuccess
         [x]        -> return x
         (_:_:_)    -> showUsageAndExit (ExitFailure 1)

findYoungestFile :: [FilePath] -> IO FilePath
findYoungestFile xs =
  foldM f (TOD 0 0, "") xs >>= \(_, fp) -> return fp
  where f :: (ClockTime, FilePath) -> FilePath -> IO (ClockTime, FilePath)
        f youngest@(yct, _) fp = do
          nct <- getModificationTime fp
          if nct > yct
             then return (nct, fp)
             else return youngest

type PkgName = String
type PkgVersion = String

data PkgId = PkgId {
    pkgName :: PkgName
  , pkgVersion :: PkgVersion
  }
  deriving (Eq, Ord)

instance Show PkgId where
  show pkgId = concat [pkgName pkgId, " ", pkgVersion pkgId]

type PkgIdSet = Set.Set PkgId

type PkgFilePath = FilePath
type CacheDir = FilePath
type PkgCache = [PkgFilePath]

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

pkgCache :: CacheDir -> PkgIdSet -> IO PkgCache
pkgCache cacheDir pkgIdSet =
  DF.foldrM f [] pkgIdSet
  where f pkgId xs =
          F.find F.always (F.fileName F.~~? (filePrefix ++ "*")) cacheDir >>=
            \fs -> case fs of
                        []  -> do
                          putMeLn $ "package installed but not found in cache: " ++ show pkgId
                          return xs
                        [x] -> return (x:xs)
                        -- It might seem impossible that more than one file
                        -- could be found in the cache for the same PkgId, but
                        -- I've had it happen! So we chose the youngest.
                        _   -> findYoungestFile fs >>= \x -> return (x:xs)
          where filePrefix = concat [pkgName pkgId, "-", pkgVersion pkgId]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  repoName  <- repoNameFromArgsOrShowUsage
  cacheDir  <- pacmanCacheDir
  pkgCache' <- installedPkgIdSet >>= pkgCache cacheDir
  putMeLn $ show pkgCache'

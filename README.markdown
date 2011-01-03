     Usage:  repo-create-cache-repo <repo-name> [<repo-path>]
       Creates a pacman repo of installed pkgs found in your pacman cache
       dir (assuming you only have one). And copies the packages to the repo.
     Command line args:
       repo-name    name your repo. You should put this name in your
                    pacman config
       repo-path    defaults to working dir

I use this little Haskell program to help clone my Arch setup to another computer.

[Let me know](github.com/stepb/repo-create-cache-repo/issues) if there are any bugs, or if you want this in AUR.

Does anyone use more than one pacman cache dir? If so, it would be easy for me to add support for this.

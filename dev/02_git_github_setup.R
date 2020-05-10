## Did you miss the previous step? The one about creating your package
rstudioapi::navigateToFile(usethis::proj_path("dev", "01_create_pkg.R"))

## ********************
## Setup Git and GitHub
## ********************

## Note that Bioconductor doesn't allow *.Rproj files
## So we have to ignore it before anything else
usethis::use_git_ignore("*.Rproj")
usethis::use_git() ## Choose the option to make the commit, then to restart RStudio

## After the restart, continue by connecting your local git repository to
## GitHub. You might want to use the `organisation` and `private` arguments
args(usethis::use_github)
usethis::use_github()
## Follow any prompts, such as running: git push --set-upstream origin master

## Move to the next step: setting up your package core files
rstudioapi::navigateToFile(usethis::proj_path("dev", "03_core_files.R"))

## This template was made using https://lcolladotor.github.io/biocthis/

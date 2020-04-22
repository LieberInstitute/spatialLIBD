## Create the dev directory
dir.create("dev", showWarnings = FALSE)
file.create("dev/01_b_setup.R")
rstudioapi::navigateToFile("dev/01_b_setup.R")

## Ignore the dev directory
# system("echo dev >> .Rbuildignore")

## Remove travis
unlink(".travis.yml")

## Update package doc
dir.create("R", showWarnings = FALSE)
unlink(dir("R", "-package.R$", full.names = TRUE))
usethis::use_package_doc()

## GitHub tests
## Original one
# usethis::use_github_action('check-standard')
## Modified one:
usethis::use_github_action("check-bioc",
    url = "https://raw.githubusercontent.com/leekgroup/derfinderPlot/master/.github/workflows/check-bioc.yml"
)
## See also:
# * https://github.com/csoneson/dreval/blob/master/.github/workflows/R-CMD-check.yaml
# * https://github.com/seandavi/BiocActions/blob/master/.github/workflows/main.yml

## pkgdown setup
# usethis::use_pkgdown()

## Support files
# usethis::use_readme_rmd() ## Edit with original README.md contents
# usethis::use_news_md()
usethis::use_tidy_coc()
usethis::use_tidy_contributing()

## Customize the support message to mention the Bioconductor Support Website
usethis::use_tidy_support()
support <- readLines(".github/SUPPORT.md")
support <-
    gsub(
        "\\[community.rstudio.com\\]\\(https://community.rstudio.com/\\), and/or StackOverflow",
        "the [Bioconductor Support Website](https://support.bioconductor.org/) using the appropriate package tag",
        support
    )
writeLines(support, ".github/SUPPORT.md")

## Use my own ISSUE template, modified from https://github.com/lcolladotor/osca_LIIGH_UNAM_2020
contents <- xfun::read_utf8("https://raw.githubusercontent.com/leekgroup/derfinderPlot/master/.github/ISSUE_TEMPLATE/issue_template.md")
save_as <- file.path(".github", "ISSUE_TEMPLATE", "issue_template.md")
usethis:::create_directory(dirname(save_as))
usethis::write_over(proj_path(save_as), contents)

## Add some badges
usethis::use_lifecycle_badge("Stable")
usethis::use_bioc_badge()

## Tests
usethis::use_coverage()

## GitHub badges
usethis::use_github_actions_badge("R-CMD-check-bioc")

## Deploy with pkgdown at least once locally such that the automatic updates
## from GitHub actions will work
pkgdown::deploy_to_branch(new_process = FALSE)

## Update prior to committing
file.create("dev/02_b_update.R")
rstudioapi::navigateToFile("dev/02_b_update.R")

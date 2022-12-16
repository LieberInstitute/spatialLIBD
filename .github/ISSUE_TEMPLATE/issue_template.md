---
name: Bug Report
about: Describe the bug in detail
title: "[BUG] A short description of the bug"
labels: ''
assignees: ''
---

Please ask questions about how to use `spatialLIBD` on the
[Bioconductor Support Site](https://support.bioconductor.org) using the
appropriate tag(s) including the one for this package.

**Note**. Update the issue title to concisely describe the bug.

## Describe the bug

Please provide a clear and concise description of what the bug is.

### Provide a minimally reproducible example (reprex)

Provide a clear and concise description of the bug. It can be easily (but not
necessarily) illustrated with a minimally reproducible example using the
[`reprex` package](https://reprex.tidyverse.org/articles/learn-reprex.html).

For tips on creating a reprex, see this
[StackOverflow link](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example).

## Expected behavior

A clear and concise description of what you expected to happen.

## R Session Information

Please report the output of either `sessionInfo()` or
`sessioninfo::session_info()` here.

<details>

```R
options(width = 120)
## insert session info here
# sessioninfo::session_info() ## provides GitHub, pandoc, and other details
# sessionInfo() ## base R function in case you don't want to install sessioninfo
```

</details>

Indicate whether `BiocManager::valid()` returns `TRUE`. 

- [ ] `BiocManager::valid()` is `TRUE`

**Note**. To avoid potential issues with version mixing and reproducibility, do
not install packages from `GitHub`.

## Additional Context

Provide some additional context for the bug report. You may include web links
(e.g., from GitHub) to:

* raw code
* a commit
* code inside a commit
* code from an R package

## Is the package installed via bioconda? 

We find that [bioconda](https://bioconda.github.io/) installations can often be
problematic due to the nature of the setup environment and potential for version
mixing.

The preferred method for installing Bioconductor software through `BiocManager`
and we do not support issues related to `bioconda` installations at this time.


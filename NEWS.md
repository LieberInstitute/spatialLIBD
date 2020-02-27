# spatialLIBD 0.99.4

* Fix Travis badges
* Fix Kristen's name on the vignette
* Add the same welcome information to the top of the vignette, since this will
be what Bioconductor users see first. Basically, we have made sure that users
will see the same information first regardless if they find the package README,
open the shiny app, or find the package vignette.

# spatialLIBD 0.99.3

* Further refine the READMEs (pkg and shiny). They now include the list of
links to the raw 10x Genomics files as well as a short description of the
project at the top. This was in response to feedback by Andrew Jaffe.

# spatialLIBD 0.99.2

* Update main package READMEs to reflect the changes to the shiny web app README.md.

# spatialLIBD 0.99.1

* Added Kristen R Maynard to the DESCRIPTION file.
* Improved the shiny app page footer.
* Moved around the documentation and added a new main tab with an overview in
response to the feedback by Stephanie Hicks.

# spatialLIBD 0.99.0

* Added a `NEWS.md` file to track changes to the package.
* First full version of the package to be submitted to Bioconductor. Note that
the `ExperimentHub::ExperimentHub()` functionality won't work until they
approve the package. However, for now `fetch_data()` has a backup mechanism
in place.
* Submitted to Bioconductor [here](https://github.com/Bioconductor/Contributions/issues/1389).

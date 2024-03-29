% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_spe.R
\name{check_spe}
\alias{check_spe}
\title{Check input spe}
\usage{
check_spe(
  spe,
  variables = c("sum_umi", "sum_gene", "expr_chrM", "expr_chrM_ratio")
)
}
\arguments{
\item{spe}{Defaults to the output of
\code{fetch_data(type = 'spe')}. This is a
\link[SpatialExperiment:SpatialExperiment]{SpatialExperiment-class}
object with the spot-level Visium data and information required for
visualizing the histology. See \code{\link[=fetch_data]{fetch_data()}} for more details.}

\item{variables}{A \code{character()} vector of variable names expected to
be present in \code{colData(spe)}.}
}
\value{
The input object if all checks are passed.
}
\description{
This function checks that the \code{spe} object has the appropriate structure.
For more details please check the vignette documentation.
}
\examples{

if (enough_ram()) {
    ## Obtain the necessary data
    if (!exists("spe")) spe <- fetch_data("spe")

    ## Check the object
    check_spe(spe)
}
}
\seealso{
Other Check input functions: 
\code{\link{check_modeling_results}()},
\code{\link{check_sce}()},
\code{\link{check_sce_layer}()}
}
\author{
Brenda Pardo, Leonardo Collado-Torres
}
\concept{Check input functions}

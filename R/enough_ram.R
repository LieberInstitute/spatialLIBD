#' Determine if you have enough RAM memory
#'
#' This function determines if you have enough RAM memory on your system.
#'
#' @param how_much The number of bytes you want to compare against.
#'
#' @return A `logical(1)` indicating whether your system has enough RAM memory.
#' @export
#'
#' @details If `benchmarkme::get_ram()` fails, this function will return
#' `FALSE` as a save bet.
#' @importFrom benchmarkme get_ram
#'
#' @examples
#'
#' ## Do you have ~ 4 GB in your system?
#' enough_ram(4e9)
#'
#' ## Do you have ~ 100 GB in your system
#' enough_ram(100e9)
enough_ram <- function(how_much = 4e9) {
    ## Found at https://stackoverflow.com/a/56104211/9374370
    ram <- get_ram()

    if (is.na(ram)) {
        ## Link in https://github.com/drisso/mbkmeans/blob/497b65901bbcf7300cd31ec1654e888427529415/R/blocksize.R#L17
        return(FALSE)
    } else {
        return(ram > how_much)
    }
}

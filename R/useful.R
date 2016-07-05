# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# Notes to self:
# -- Use packages:  devtools; roxygen2 (for documentation);
#    formatR::tidy_dir   ?? - to make code neater
#    lintr               ?? - to identify code readability problems


##' Quick method to check if package is installed
##'
##' Sees if package is installed, then installs if not yet there.
##'
##' @param s Name of package to attach (string)
##' @return NULL
##' @export

## qlib <- function(s, ...){
##   if (!(s %in% installed.packages()[ ,1])) install.packages(s, ...)
##   library(s)
## }



#' Quicker rbind / cbind
#'
#' Combines list of xts objects into one large xts object.
#' from Dominik and GSee at
#' http://stackoverflow.com/questions/12028671/merging-a-large-list-of-xts-objects
#' @param lst List of xts objects
#' @return combined single xts object
#' @aliases do.call.cbind
#' @export
#' @examples
#' l <- lapply(Sys.Date()-6000:1, function(x) {N=60*8;list(rnorm(N),as.POSIXct(x)-seq(N*60,1,-60))})
#' system.time(l1 <- do.call.rbind(l))

do.call.rbind <- function(lst) {
  while (length(lst) > 1) {
    idxlst <- seq(from=1, to=length(lst), by=2)
    lst <- lapply(idxlst, function(i) {
      if(i==length(lst)) { return(lst[[i]]) }
      return(rbind(lst[[i]], lst[[i+1]]))
    })
  }
  lst[[1]]
} # do.call.rbind

do.call.cbind <- function(lst) {
  while(length(lst) > 1) {
    idxlst <- seq(from=1, to=length(lst), by=2)
    lst <- lapply(idxlst, function(i) {
      if(i==length(lst)) { return(lst[[i]]) }
      return(cbind(lst[[i]], lst[[i+1]]))
    })
  }
  lst[[1]]
} # do.call.cbind


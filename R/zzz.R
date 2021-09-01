#' @useDynLib rdaemon, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import futile.logger
#' @import base64enc
#' @import utils
NULL

.onLoad <- function(libname, pkgname){
    clientData$lastRegisteredDaemon <- paste0("DefaultDaemon_", Sys.getpid())
    clientData$lastSetTaskId <- paste0("DefaultTask_", Sys.getpid())
}
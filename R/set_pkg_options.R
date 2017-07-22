# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
PKG_OPTIONS <- settings::options_manager(wd = "C:/ptdash/",
                                         quandlapi = "17zLYKUTzzt6whHxFZEF",
                                         inception.date = zoo::as.Date("2017-06-16"))

# User function that gets exported:
set_pkg_options <- function(...) {
  #' Set or get options for my package
  #'
  #' @param ... Option names to retrieve option values or
  #'   \code{[key]=[value]} pairs to set options.
  #'
  #' @section Supported options:
  #' The following options are supported
  #' \itemize{
  #'  \item{\code{wd}}{(\code{string}) The working directory of all data. }
  #'  \item{\code{quandlapi}}{(\code{string}) Your API Key for Quandl. }
  #' }
  #' @export

  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  PKG_OPTIONS(...)
}

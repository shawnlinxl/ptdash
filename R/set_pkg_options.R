# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
PKG_OPTIONS <- settings::options_manager(wd = "C:/")

# User function that gets exported:
set_pkg_options <- function(...) {
  #' Set or get options for my package
  #'
  #' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
  #'
  #' @section Supported options:
  #' The following options are supported
  #' \itemize{
  #'  \item{\code{wd}}{(\code{string};1) The working directory of all data. }
  #' }
  #' @export

  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  PKG_OPTIONS(...)
}

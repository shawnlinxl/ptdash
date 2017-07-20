create_return_series <- function() {
  #' Create a daily return series.
  #'
  #' Create a daily return series based on the return on the historical date
  #' instead of the current NAV.
  #'
  #' @import dplyr
  #' @export

  ## Load log file
  log_file <-
    read.csv(paste0(PKG_OPTIONS()$wd, "trade_log.csv"),
             colClasses = c("Date", "character", "double", "integer", "character"))

  log_file <-
    log_file %>%
    arrange(Date)
    group_by(Ticker) %>%
    mutate(InitiateInvestment = (cumsum(Quantity) - Quantity == 0))

  initiation_list <-
    log_file %>%
    filter(InitiateInvestment) %>%
    select(Date, Ticker)


}

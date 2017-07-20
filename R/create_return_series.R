create_return_series <- function() {
  #' Create a daily return series.
  #'
  #' Create a daily return series based on the return on the historical date
  #' instead of the current NAV.
  #'
  #' @import dplyr
  #' @importFrom Quandl Quandl
  #' @importFrom Quandl Quandl.api_key
  #'
  #' @export

  ## Load log file
  log_file <-
    read.csv(paste0(PKG_OPTIONS()$wd, "trade_log.csv"),
             colClasses = c("Date", "character", "double", "integer", "character"))

  log_file <-
    log_file %>%
    arrange(Date) %>%
    group_by(Ticker) %>%
    mutate(InitiateInvestment = (cumsum(Quantity) - Quantity == 0))

  price_get <-
    log_file %>%
    filter(InitiateInvestment) %>%
    select(Date, Ticker)

  Quandl.api_key(PKG_OPTIONS()$quandlapi)
  dir_price_data <- paste0(PKG_OPTIONS()$wd, "price_data/")
  for (stock in 1:nrow(price_get)) {
    ## File location of the price data for the ticker
    ticker <- price_get[stock,]$Ticker
    dir_price_file <- paste0(dir_price_data, "price_",ticker, ".csv")

    if (!file.exists(dir_price_file)) {
      ## if the price file does not exists, create one that contains all the
      ## price information since position is initiated
      start_date <- price_get[stock,]$Date
      price_data <-
        Quandl(paste0("WIKI/",ticker),
               start_date = start_date,
               end_date = "2017-07-15")
      price_data <- arrange(price_data, Date)
      write.csv(price_data, dir_price_file, row.names = FALSE)
    } else {
      price_data <- read.csv(dir_price_file)
      start_date <- zoo::as.Date(tail(price_data$Date,1)) + lubridate::days(1)
      new_price_data <-
        Quandl(paste0("WIKI/",ticker),
               start_date = start_date,
               end_date = Sys.Date())
      new_price_data <- arrange(new_price_data, Date)
      names(price_data) <- names(new_price_data)
      price_data <- rbind(price_data, new_price_data)
    }

  }


}

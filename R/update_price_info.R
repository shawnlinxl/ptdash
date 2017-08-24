update_price_info <- function(dir_log_file = NULL) {
  #' Create a daily return series.
  #'
  #' Create a daily return series based on the return on the historical date
  #' instead of the current NAV.
  #'
  #' @param dir_log_file directory of the log file. If not provided, look for
  #'   log file at default location.
  #'
  #' @import dplyr
  #'
  #' @export

  ## Load log file
  if (is.null(dir_log_file)) {
    log_file <-
      read.csv(paste0(PKG_OPTIONS()$wd, "trade_log.csv"),
               colClasses = c("Date", "character", "double", "integer", "character"))
  } else {
    log_file <-
      read.csv(dir_log_file,
               colClasses = c("Date", "character", "double", "integer", "character"))
  }


  log_file <-
    log_file %>%
    arrange(Date) %>%
    group_by(Ticker) %>%
    mutate(InitiateInvestment = (Date == min(Date)))

  price_get <-
    log_file %>%
    filter(InitiateInvestment) %>%
    select(Date, Ticker) %>%
    unique()

  dir_price_data <- paste0(PKG_OPTIONS()$wd, "price_data/")
  for (stock in 1:nrow(price_get)) {
    ## File location of the price data for the ticker
    ticker <- price_get[stock,]$Ticker
    dir_price_file <- paste0(dir_price_data, "price_",ticker, ".csv")

    tryCatch({
      if (!file.exists(dir_price_file)) {
        ## if the price file does not exists, create one that contains all the
        ## price information since position is initiated
        start_date <- price_get[stock,]$Date
        price_data <-
          quantmod::getSymbols(ticker, from = start_date,
                               to = Sys.Date(), auto.assign = FALSE) %>%
          data.frame %>%
          mutate(., Date = rownames(.)) %>%
          select(Date, everything()) %>%
          setNames(c("Date", "Open", "High", "Low", "Close", "Volume", "Adj.Close"))
          # Quandl(paste0("WIKI/",ticker),
          #        start_date = start_date,
          #        end_date = Sys.Date())
        # names(price_data) <-
        #   gsub(".", "", names(price_data), fixed = TRUE) %>%
        #   gsub(" ", ".", .)
        # price_data <-
        #   arrange(price_data, Date) %>%
        #   select(Date, Open, High, Low, Close, Adj.Close, Volume)
        write.csv(price_data, dir_price_file, row.names = FALSE)

      } else {
        price_data <- read.csv(dir_price_file,
                               colClasses = c("Date", rep("double", 6)))
        start_date <- zoo::as.Date(tail(price_data$Date,1)) + lubridate::days(1)
        if (start_date <= Sys.Date()){
          # update price data if is not up-to-date
          new_price_data <-
            quantmod::getSymbols(ticker, from = start_date,
                                 to = Sys.Date(), auto.assign = FALSE) %>%
            data.frame %>%
            mutate(., Date = rownames(.)) %>%
            select(Date, everything()) %>%
            setNames(c("Date", "Open", "High", "Low", "Close", "Volume", "Adj.Close"))
          price_data <- rbind(price_data, new_price_data)
          write.csv(price_data, dir_price_file, row.names = FALSE)
        }
      }
    }, error = function(e) {cat("\n", ticker, "is not available from Quandmod!\n",
                                "Download for period ",
                                strftime(start_date, "%Y-%m-%d"),
                                "to",
                                strftime(Sys.Date(), "%Y-%m-%d"))})

  }

}

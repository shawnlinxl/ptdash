calc_return_series <-
  function(dir_log_file = NULL, return_level = c("ticker", "fund")) {
  #' Use stock holdings and price data to find daily return on fund using
  #' only invested asset. Result is produced for indivisual tickers.
  #'
  #' @param dir_log_file directory of the log file. If not provided, look for
  #'   log file at default location.
  #' @param return_level
  #'   * ticker: return info for each ticker in a long format data.frame
  #'   * fund: fund return info
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

  pos_change <- log_file
  ## remap Buy and Sell to integer values for calculation
  pos_change$Type <-
    as.numeric(plyr::revalue(pos_change$Type, c("Buy" = 1, "Sell" = -1)))

  ##
  pos_change <-
    pos_change %>%
    group_by(Ticker) %>%
    arrange(Ticker, Date) %>%
    mutate(Total = cumsum(Quantity*Type), Change = Quantity*Type) %>%
    select(Date, Ticker, Price, Total, Change) %>%
    ungroup() %>%
    as.data.frame()

  ticker.list <- unique(pos_change$Ticker)
  result <- list()

  for (ticker in ticker.list) {
    dir_price_data <- paste0(PKG_OPTIONS()$wd, "price_data/")
    dir_price_file <- paste0(dir_price_data, "price_",ticker, ".csv")

    ## Read the price data
    price_data <-
      read.csv(dir_price_file, colClasses = c("Date", rep("double", 6))) %>%
      select(Date, Adj.Close)

    ## Manipulate position information to merge the same day's position changes
    ## and output Cost/Profit info
    pos_ticker <-
      filter(pos_change, Ticker == ticker) %>%
      group_by(Date) %>%
      summarise(Cost = sum(Change*Price),
                Total = max(Total),
                Change = sum(Change)) %>%
      ungroup() %>%
      as.data.frame()

    ## output ticker information with Date, Return, Total Return, NAV
    result[[ticker]] <-
      merge(price_data, pos_ticker, by = "Date", all = TRUE) %>%
      tidyr::fill(Total) %>%
      mutate(Change = ifelse(is.na(Change), 0, Change)) %>%
      mutate(Cost = ifelse(is.na(Cost), 0, Cost)) %>%
      mutate(
        Return =
          ifelse(
            Total == Change,
            (Total*Adj.Close) / Cost - 1,
            (Total*Adj.Close) / ((Total - Change) * lag(Adj.Close) + Cost) - 1)
      ) %>%
      mutate(Total.Return = cumprod(Return + 1) - 1) %>%
      mutate(NAV = Total*Adj.Close) %>%
      select(Date, Return, Total.Return, NAV) %>%
      mutate(Ticker = ticker)
  }

  return_level <- return_level[1]
  if (! (return_level %in% c("ticker", "fund"))) {
    stop("ERROR: return_level must be `ticker` or `fund`!")
  }

  if (return_level == "ticker") {
    return(Reduce(rbind,result))

  } else if (return_level == "fund") {
    ## Calculate fund NAV, daily returns and VAMI
    result <-
      Reduce(rbind, result) %>%
      group_by(Date) %>%
      summarize(Fund.NAV = sum(NAV), Return = sum(Return*NAV)/Fund.NAV) %>%
      ungroup() %>%
      as.data.frame() %>%
      mutate(Total.Return = cumprod(Return + 1) - 1)

    return(result)

  }

}

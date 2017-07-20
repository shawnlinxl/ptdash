log_trade <-
function(ticker, price, trade_quantity, trade_type, trade_date = Sys.Date()) {
  #' Log the trade to trade log
  #'
  #' Trade log is used for all other analysis. Examples are like creating a
  #' track record series, analyzing returns etc.
  #'
  #' @param ticker Ticker of the stock.
  #' @param price The price at which the trade is executed at.
  #' @param trade_date The date that the trade is executed on.
  #' @param trade_quantity The quantity traded.
  #' @param trade_type Buy/Sell.
  #'
  #' @export

  ## Directory of the Log file
  dir_file <- paste0(PKG_OPTIONS()$wd, "trade_log.csv")

  ## Exceptions handling
  if (!is.double(price)) {
    stop("Price should be a floating number.")
  }

  if (!is.double(trade_quantity)) {
    stop("trade_quantity must be an integer.")
  }

  if (!(trade_type %in% c("Buy", "Sell"))) {
    stop("trade_type must be `Buy` or `Sell`")
  }

  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", trade_date)) {
    stop("trade_date must be in YYYY-MM-DD format.")
  }

  trade_date <- zoo::as.Date(trade_date)

  ## If log file does not exist in the directory, create new log file
  if (!file.exists(dir_file)) {
    log_file <-
      data.frame(Date = trade_date, Ticker = ticker, Price = price,
                 Quantity = trade_quantity, Type = trade_type)
    write.csv(log_file, dir_file, row.names = FALSE)

  } else {
    ## Otherwise append to the present log file
    log_file <-
      paste(trade_date, paste0('"', ticker, '"'), price,
            trade_quantity, paste0('"', trade_type, '"'), sep = ",")
    write(log_file, dir_file, append = TRUE)
  }

}

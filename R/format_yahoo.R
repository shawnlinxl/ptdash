format_yahoo <- function(ticker) {
  #' Process yahoo finance csv to conform to other price information's format.
  #'
  #' @param yahoo_finance_file file_name of the csv file
  #'
  #' @export

  ## load price data
  dir_price_data <- paste0(PKG_OPTIONS()$wd, "price_data/")
  dir_price_file <- paste0(dir_price_data, "price_",ticker, ".csv")
  price_data <- read.csv(dir_price_file)

  ## adjust date format
  price_data$Date <-
    zoo::as.Date(price_data$Date) %>%
    strptime("%Y-%m-%d")

  ## adjust column name, remove spaces and write to csv
  names(price_data) <- gsub(" ", ".", names(price_data), fixed = TRUE)
  write.csv(price_data, dir_price_file, row.names = FALSE, eol = "\r\n")
}

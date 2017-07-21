format_yahoo <- function(ticker) {
  #' Process yahoo finance csv to conform to other price information's format.
  #'
  #' @param yahoo_finance_file file_name of the csv file
  #'
  #' @export

  dir_price_data <- paste0(PKG_OPTIONS()$wd, "price_data/")
  dir_price_file <- paste0(dir_price_data, "price_",ticker, ".csv")

  price_data <- read.csv(dir_price_file)

  price_data$Date <-
    zoo::as.Date(price_data$Date, "%m/%d/%Y") %>%
    strptime("%Y-%m-%d")
  write.csv(price_data, dir_price_file, row.names = FALSE)
}

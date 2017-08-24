update_benchmark <- function(ticker = "^SP500TR", save_name = "SPTR") {
  #' Get benchmark prcie data
  #'
  #' @param ticker benchmark ticker. Only tested for SPTR
  #'
  #' @import Quandl
  #' @import dplyr
  #' @import quantmod
  #'
  #' @export

  dir_price_data <- paste0(PKG_OPTIONS()$wd, "price_data/")
  dir_price_file <- paste0(dir_price_data, "price_", save_name, ".csv")


  # price_data <- Quandl(paste0("BCIW/", ticker),
  #                      start_date = PKG_OPTIONS()$inception.date,
  #                      end_date = Sys.Date())

  price_data <-
    quantmod::getSymbols(ticker, from = PKG_OPTIONS()$inception.date,
                         to = Sys.Date(), auto.assign = FALSE) %>%
    data.frame %>%
    mutate(., Date = rownames(.)) %>%
    select(Date, everything()) %>%
    setNames(c("Date", "Open", "High", "Low", "Close", "Volume", "Adj.Close"))

  write.csv(price_data, dir_price_file, row.names = FALSE)
}

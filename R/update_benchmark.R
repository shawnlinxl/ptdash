update_benchmark <- function(ticker = "_SPXT") {
  #' Get benchmark prcie data
  #'
  #' @param ticker benchmark ticker. Only tested for SPXT (SPTR)
  #'
  #' @import Quandl
  #' @import dplyr
  #'
  #' @export

  Quandl.api_key(PKG_OPTIONS()$quandlapi)
  dir_price_data <- paste0(PKG_OPTIONS()$wd, "price_data/")
  dir_price_file <- paste0(dir_price_data, "price_",ticker, ".csv")

  price_data <- Quandl(paste0("BCIW/", ticker),
                      start_date = PKG_OPTIONS()$inception.date,
                      end_date = Sys.Date())
  price_data <-
    arrange(price_data, Date) %>%
    select(Date, Open, High, Low, Close, Volume)

  write.csv(price_data, dir_price_file, row.names = FALSE)
}

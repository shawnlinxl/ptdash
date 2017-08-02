plot_asset_return <- function(data.return, ticker.list = NULL) {
  #' Plot each individual's total return since inception
  #'
  #' @param ticker.list tickers to plot, if not ALL
  #'
  #' @import ggplot2
  #' @import dplyr
  #'
  #' @export

  fund.return <- calc_total_return(data.return, "ticker")

  if (is.null(ticker.list)) {
    ticker.list <- unique(fund.return$Ticker)
  }

  result <- list()
  for (ticker in ticker.list) {
    result[[ticker]] <-
      filter(fund.return, Ticker == ticker)%>%
      select(Date, Total.Return, Ticker) %>%
      rbind(data.frame(Date = as.Date(.$Date[1] - 1), Total.Return = 0, Ticker = .$Ticker[1]), .)
  }

  fund.return <- Reduce(rbind, result)

  g <-
    ggplot(fund.return,
           aes(x = Date, y = Total.Return, group = Ticker, color = Ticker)) +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::percent)

  return(g)

}

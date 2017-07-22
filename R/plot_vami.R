plot_vami <- function() {
  #' Plot cumulative return of the fund
  #'
  #' @import ggplot2
  #' @import dplyr
  #'
  #' @export

  fund.return <-
    calc_return_series(return_level = "fund") %>%
    select(Date, Total.Return)

  fund.return <- rbind(data.frame(Date = as.Date(fund.return$Date[1] - 1), Total.Return = 0), fund.return)

  g <-
    ggplot(fund.return, aes(x = Date, y = Total.Return)) +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::percent)

  return(g)
}

plot_vami <- function(data.return, benchmark = TRUE) {
  #' Plot cumulative return of the fund
  #'
  #' @param data.return Daily or monthly return series
  #' @param benchmark If TRUE, plot the benchmark return during the same period
  #'   on the same graph.
  #'
  #' @import ggplot2
  #' @import dplyr
  #'
  #' @export

  fund.return <-
    data.return %>%
    calc_total_return("fund")



  if (!benchmark) {filter(fund.return, Strategy = "Fund")}

  strategy.list <- unique(fund.return$Strategy)
  result <- list()
  for (strategy in strategy.list) {
    result[[strategy]] <-
      filter(fund.return, Strategy == strategy) %>%
      rbind(data.frame(Date = as.Date(.$Date[1] - 1), Total.Return = 0, Strategy = strategy), .)
  }

  fund.return <- Reduce(rbind, result)


  g <-
    ggplot(fund.return, aes(x = Date, y = Total.Return, group = Strategy, color = Strategy)) +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::percent)

  return(g)
}

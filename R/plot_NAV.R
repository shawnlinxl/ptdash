plot_NAV <- function() {
  #' Plot net asset value of the fund
  #'
  #' @import ggplot2
  #' @import dplyr
  #'
  #' @export

  fund.return <-
    calc_return_series(return_level = "fund") %>%
    select(Date, Fund.NAV)

  g <-
    ggplot(fund.return, aes(x = Date, y = Fund.NAV)) +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::dollar)

  return(g)
}

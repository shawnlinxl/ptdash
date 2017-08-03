plot_NAV <- function(fund.NAV) {
  #' Plot net asset value of the fund
  #'
  #' @import ggplot2
  #'
  #' @export

  fund.NAV <-
    fund.NAV %>%
    padr::pad() %>%
    tidyr::fill(Fund.NAV)

  g <-
    ggplot(fund.NAV, aes(x = Date, y = Fund.NAV)) +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::dollar)

  return(g)
}

plot_NAV <- function(fund.NAV) {
  #' Plot net asset value of the fund
  #'
  #' @import ggplot2
  #'
  #' @export

  fund.NAV <-
    fund.NAV %>%
    group_by(Ticker) %>%
    padr::pad %>%
    ungroup %>%
    as.data.frame %>%
    tidyr::fill(Return,NAV,Ticker)

  g <-
    ggplot(fund.NAV, aes(x = Date, y = Fund.NAV)) +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::dollar)

  return(g)
}

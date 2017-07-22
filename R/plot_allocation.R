plot_allocation <- function() {
  #' Plot Asset Allocation by Ticker
  #'
  #' @import ggplot2
  #' @import dplyr
  #'
  #' @export

  NAV.data <-
    calc_return_series() %>%
    filter(Date == max(Date)) %>%
    mutate(Allocation = NAV/sum(NAV)) %>%
    arrange(desc(Allocation))

  g <-
    ggplot(NAV.data, aes(x=factor(1), y = Allocation, fill = Ticker)) +
    geom_bar(stat = "identity") +
    ylab("") +
    xlab("") +
    labs(fill="") +
    coord_polar("y") +
    geom_text(
      aes(y = Allocation + c(0, cumsum(Allocation)[-length(Allocation)]),
          label = scales::percent(Allocation)))

  return(g)

}

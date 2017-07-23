plot_allocation <- function(Report.Date = NULL) {
  #' Plot Asset Allocation by Ticker
  #'
  #' @import ggplot2
  #' @import dplyr
  #'
  #' @export

  if (is.null(Report.Date)) {
    Report.Date <- Sys.Date()
  }

  NAV.data <-
    calc_return_series() %>%
    filter(Date <= Report.Date) %>%
    filter(Date == max(Date)) %>%
    mutate(Allocation = NAV/sum(NAV)) %>%
    arrange(desc(Allocation))

  g <- plot_ly(
    NAV.data,
    labels = ~Ticker, values = ~NAV, type="pie",
    textposition = 'inside',
    textinfo = 'label+percent',
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = 'text',
    text = ~paste('$', NAV))

  return(g)

}

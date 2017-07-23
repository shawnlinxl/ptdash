calc_NAV <- function(Ticker.NAV) {
  #' Calculate Fund NAV from Ticker NAV Info
  #'
  #' @param Ticker.NAV dataframe with NAV and Date informaion
  #'
  #' @import dplyr
  #'
  #' @export
  result <-
    Ticker.NAV %>%
    group_by(Date) %>%
    summarize(Fund.NAV = sum(NAV)) %>%
    ungroup() %>%
    as.data.frame()

  return(result)

}

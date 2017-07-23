calc_total_return <-
function(data.return, return_level = c("ticker", "fund")) {
  #' calculate the cumulative return from daily or monthly returns
  #'
  #' @param data.return a daily or monthly return series
  #'
  #' @import dplyr
  #'
  #' @export

  return_level <- return_level[1]
  if (return_level == "ticker") {
    result <-
      data.return %>%
      group_by(Ticker) %>%
      mutate(Total.Return = cumprod(Return + 1) - 1) %>%
      ungroup() %>%
      select(Date, Total.Return, Ticker) %>%
      as.data.frame()
  } else if (return_level == "fund") {
    result <-
      data.return %>%
      group_by(Strategy) %>%
      mutate(Total.Return = cumprod(Return + 1) - 1) %>%
      ungroup() %>%
      select(Date, Total.Return, Strategy) %>%
      as.data.frame()
  }


  return(result)

}

df_to_xts <- function(dataframe) {
  #' Convert Dataframe with `Date`` column to an xts
  #'
  #' @param dataframe data.frame object
  #'
  #' @export

  row.names(dataframe) <- dataframe$Date
  dataframe <-
    dataframe %>%
    select(-Date) %>%
    xts::as.xts()

  return(dataframe)
}

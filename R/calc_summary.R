calc_summary <-
function(summary_type = c("quick", "CAPM", "Drawdown")) {
  #' calculate performance summary
  #'
  #' @import dplyr
  #' @import PerformanceAnalytics
  #'
  #' @export

  returns.data <- calc_return_series("fund")
  returns.fund <-
    filter(returns.data, Strategy == "Fund") %>%
    select(Date, Return) %>%
    df_to_xts()
  returns.benchmark <-
    filter(returns.data, Strategy == "Benchmark") %>%
    select(Date, Return) %>%
    df_to_xts()

  fund.return <- returns.fund$Return
  bm.return <- returns.benchmark$Return

  summary_type <- summary_type[1]

  if (summary_type == "quick") {
    ## fund stats
    ret <- Return.annualized(fund.return)
    std <- StdDev.annualized(fund.return)
    max.dd <- maxDrawdown(fund.return)
    alpha <- CAPM.alpha(fund.return, bm.return)
    beta <- CAPM.beta(fund.return, bm.return)
    sharpe <- SharpeRatio.annualized(fund.return)
    correlation <- cor(fund.return, bm.return)
    stats <- data.frame(Fund = c(ret, std, max.dd, alpha, beta, sharpe, correlation))

    ## benchmark stats
    ret <- Return.annualized(bm.return)
    std <- StdDev.annualized(bm.return)
    max.dd <- maxDrawdown(bm.return)
    alpha <- NA
    beta <- NA
    sharpe <- SharpeRatio.annualized(bm.return)
    correlation <- cor(fund.return, bm.return)
    stats$Benchmark <- c(ret, std, max.dd, alpha, beta, sharpe, correlation)

    row.names(stats) = c("Ann.Return", "Ann.Std", "Max Drawdown", "Alpha", "Beta",
                         "Sharpe", "Correlation")

    return(stats)
  } else if (summary_type == "CAPM") {
    return(table.CAPM(fund.return, bm.return))
  } else if (summary_type == "Drawdown") {
    return(table.Drawdowns(fund.return))
  } else {
    stop("ERROR: summary_type is not standard.")
  }

}

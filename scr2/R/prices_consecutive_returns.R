#' @title Stock price in correspondence of consecutive returns with same sign
#'
#' @description This function computes the stock price when consecutive strictly positive or
#' negative returns occur.
#'
#' @import quantmod stats xts zoo
#'
#' @param n Number of consecutive returns with same sign.
#' @param ticker Stock symbol on Yahoo Finance.
#' @param start_date Start date of the data.
#' @param end_date End date of the data.
#' @param frequency Periodicity of the data (e.g., "daily", "weekly").
#' @param positive Boolean which gives consecutive positive returns if TRUE (by default) or
#' negative if FALSE.
#'
#' @return The stock price when this event occurs.
#'
#' @examples
#' prices_consecutive_returns(3, "AAPL", "2022-01-01", "2022-12-31", "daily", positive = TRUE)
#'
#' @export
prices_consecutive_returns <- function(n, ticker, start_date, end_date, frequency, positive = TRUE) {
  # Function for having the prices at which n consecutive positive/negative returns occur

  # Import the data
  stock.Prices <- coredata(na.omit(getSymbols(ticker, env = NULL, from = start_date, to = end_date, periodicity = frequency))[, grep("\\.Adjusted", names(na.omit(getSymbols(ticker, env = NULL, from = start_date, to = end_date, periodicity = frequency))))])
  stock.Returns <- coredata(dailyReturn(na.omit(getSymbols(ticker, env = NULL, from = start_date, to = end_date, periodicity = frequency))[, grep("\\.Adjusted", names(na.omit(getSymbols(ticker, env = NULL, from = start_date, to = end_date, periodicity = frequency))))]))

  # Check for n consecutive strictly positive (if positive = TRUE) or negative
  # (if positive = FALSE) returns
  if(positive==TRUE){
    prices <- rep(NA,length(stock.Returns) - n - 1)
    for (i in 1:(length(stock.Returns) - n - 1)) {
      if (all(stock.Returns[i:(i + n - 1)] > 0)) {
        prices[i]<-stock.Prices[i]
      }
    }
  } else {
    prices <- rep(NA,length(stock.Returns) - n + 1)
    for (i in 1:(length(stock.Returns) - n - 1)) {
      if (all(stock.Returns[i:(i + n - 1)] < 0)) {
        prices[i]<-stock.Prices[i]
      }
    }
  }
  return(sort(round(prices[!is.na(prices)])))
}

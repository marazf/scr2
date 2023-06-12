#' @title Probability of consecutive returns with the same sign according to historical data
#'
#' @description This function computes the probability of consecutive strictly positive or
#' negative returns for a given stock.
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
#' @return The percentage of times this event occurs.
#'
#' @examples
#' probability_consecutive_returns(3, "AAPL", "2022-01-01", "2022-12-31", "daily",
#' positive = FALSE)
#'
#' @export
probability_consecutive_returns <- function(n, ticker, start_date, end_date, frequency, positive = TRUE) {
  # Function for computing the probability of n consecutive strictly positive returns

  # Import the data
  stock.Returns <- coredata(dailyReturn(na.omit(getSymbols(ticker, env = NULL, from = start_date, to = end_date, periodicity = frequency))[, grep("\\.Adjusted", names(na.omit(getSymbols(ticker, env = NULL, from = start_date, to = end_date, periodicity = frequency))))]))

  # Initialize counter
  count <- 0

  # Check for s consecutive strictly positive (if positive = TRUE) or negative
  # (if positive = FALSE) returns
  if (positive==TRUE){
    for (i in 1:(length(stock.Returns) - n - 1)) {
      if (all(stock.Returns[i:(i + n - 1)] > 0)) {
        count <- count + 1
      }
    }
  } else {
    for (i in 1:(length(stock.Returns) - n - 1)) {
      if (all(stock.Returns[i:(i + n - 1)] < 0)) {
        count <- count + 1
      }
    }
  }
  return(count / length(stock.Returns))
}

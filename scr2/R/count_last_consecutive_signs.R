#' @title Number of last consecutive returns with same sign
#'
#' @description This function computes the number of last consecutive strictly positive or
#' negative returns for a given stock.
#'
#' @import quantmod xts zoo
#'
#' @param ticker Stock symbol on Yahoo Finance.
#' @param start_date Start date of the data.
#' @param end_date End date of the data.
#' @param frequency Periodicity of the data (e.g., "daily", "weekly").
#'
#' @return The number of times this event occurs.
#'
#' @examples
#' count_last_consecutive_signs("AAPL", "2022-01-01", "2022-12-31", "daily")
#'
#' @export
count_last_consecutive_signs <- function(ticker, start_date, end_date, frequency) {
  # Function for counting the number of n last consecutive strictly positive or negative returns

  # Import the data
  stock.Returns <- coredata(dailyReturn(na.omit(getSymbols(ticker, env = NULL, from = start_date, to = end_date, periodicity = frequency))[, grep("\\.Adjusted", names(na.omit(getSymbols(ticker, env = NULL, from = start_date, to = end_date, periodicity = frequency))))]))

  # Check if the returns vector is empty
  if (length(stock.Returns) == 0) {
    return(0)
  }

  # Get the sign of the last return
  last_sign <- sign(stock.Returns[length(stock.Returns)])

  # Initialize counter
  count <- 0

  # Iterate over the returns from the end
  for (i in rev(1:length(stock.Returns))) {
    # Check if the sign of the return matches the last_sign
    if (sign(stock.Returns[i]) == last_sign) {
      count <- count + 1
    } else {
      # If the sign changes, stop counting
      break
    }
  }

  return(count)
}

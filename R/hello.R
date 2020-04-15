#' Functions for crypto
#' @export
hello <- function() {
  print("Hello, world!")
}


#' get the most recent BTC price from Binance API. If error, run again
#'
#' This is a wrapper around the \code{binancer} package ...
#' @export
#' @param retried the number if retries previously done before the exponential backoff sleep
#' @importFrom binancer binance_coins_prices
get_bitcoin_price <- function() {
  tryCatch(
    binance_coins_prices()[symbol=="BTC", usd],
    error = function(e) get_bitcoin_price())
}

#' assign Forint sign
#' @export
#' @param x number
#' @importFrom scales
forint <- function(x) {
  dollar(x, prefix = "", suffix = "Ft")
}

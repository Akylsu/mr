#' Functions for crypto
#' @export
hello <- function() {
  print("Hello, world!")
}


#' get BTC price. If error, run again
#' @export

get_bitcoin_price <- function() {
  tryCatch(
    binance_coins_prices()[symbol=="BTC", usd],
    error = function(e) get_bitcoin_price())
}

#' assign Forint sign
#' export
forint <- function(x) {
  dollar(x, prefix = "", suffix = "Ft")
}

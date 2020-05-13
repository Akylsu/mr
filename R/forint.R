#' Access Binance API for BTC price in USD
#' @export
#' @param retied # the number of retries before sleep
#' @importFrom binancer binance_coins_prices
get_bitcoin_price <- function(retried = 0) {
  tryCatch(
    subset(binance_coins_prices(), symbol == "BTC")$usd,
    error = function(e){
      Sys.sleep(1 + retried^2)
      get_bitcoin_price(retried = 1 + retried)
    })
}
#' Formatter function for Hungarian Forint
#' @param x number
#' @export
#' @importFrom scales dollar
forint <- function(x) {
  dollar(x, prefix = '', suffix = 'Ft')}

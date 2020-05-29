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

#' Get exchange rates for a pair of currencies for a certain number of days
#' @param base: base currency, symbol: equivalent in a given currency, days: number of days
#' @export
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' @importFrom tidyr fill
convert_currency <- function(base, symbol, days){
  data <- content(GET(
    "https://api.exchangeratesapi.io/history",
    query = list(
      start_at = Sys.Date() - days + 1,
      end_at   = Sys.Date(),
      base = paste0(base),
      symbols = paste0(symbol)
    )))$rates
  ## days != actual number of days, skips non-working days
  data <- data.table(date = as.Date(names(data)),
                     value = unlist(data))[order(date)]
  return(data)
}

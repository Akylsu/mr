#' Access Binance API for BTC price in USD
#' @export
#' @param retried # the number of retries before sleep
#' @return Return the latest price of 1 BTC in USD
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
#' @param number: converts a given number to the forint format
#' @examples
#' forint(42)
#' @export
#' @importFrom scales dollar
forint <- function(number) {
  dollar(number, prefix = '', suffix = 'Ft')}

#' Get exchange rates for a pair of currencies for a certain number of days
#' @param base: base currency
#' @param quote: quote currency
#' @param days: number of days to return
#' @examples
#' convert_currency("USD", "JPY", 10)
#' convert_currency("EUR", "HUF", 15)
#' @return Table that indicates how much of the \code{quote} currency is needed to purchase one unit of
#' the \code{base} currency for a given number of \code{days}
#' @export
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' @importFrom tidyr fill
convert_currency <- function(base, quote, days){
  data <- content(GET(
    "https://api.exchangeratesapi.io/history",
    query = list(
      start_at = Sys.Date() - days + 1,
      end_at   = Sys.Date(),
      base = paste0(base),
      symbols = paste0(quote)
    )))$rates
  ## days != actual number of days, skips non-working days
  data <- data.table(date = as.Date(names(data)),
                     value = unlist(data))[order(date)]
  return(data)
}

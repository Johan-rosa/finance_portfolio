#' Configure HTTP/HTTPS proxy for R sessions
#'
#' Sets HTTP and HTTPS proxy environment variables using credentials stored in
#' environment variables. This enables internet access from R when working behind
#' a corporate proxy requiring authentication.
#'
#' The function reads the following environment variables:
#'
#' - `W_USER`  : Proxy username
#' - `W_PASS`  : Proxy password
#' - `PROXY`   : Proxy host and port (format: "host:port")
#'
#' It sets:
#'
#' - http_proxy / https_proxy
#' - HTTP_PROXY / HTTPS_PROXY
#'
#' These settings are used by:
#'
#' - quantmod
#' - httr
#' - curl
#' - download.file
#' - readr, data.table, and other packages using libcurl
#'
#' @return Invisibly returns the proxy URL used.
#'
#' @examples
#' # Set credentials in environment first:
#' Sys.setenv(
#'   W_USER = "myusername",
#'   W_PASS = "mypassword",
#'   PROXY  = "proxy.company.com:8080"
#' )
#'
#' # Configure proxy
#' set_proxy()
#'
#' # Example usage with quantmod
#' quantmod::getSymbols("VTI", src = "yahoo")
#'
#' @export
set_proxy <- function() {

  user  <- Sys.getenv("W_USER", unset = NA_character_)
  pass  <- Sys.getenv("W_PASS", unset = NA_character_)
  proxy <- Sys.getenv("PROXY",  unset = NA_character_)
  
  if (any(is.na(c(user, pass, proxy)))) {
    stop(
      "Missing proxy environment variables. Please set:\n",
      "  W_USER  : proxy username\n",
      "  W_PASS  : proxy password\n",
      "  PROXY   : proxy host:port\n",
      call. = FALSE
    )
  }
  
  proxy_url <- sprintf("http://%s:%s@%s", user, pass, proxy)
  
  Sys.setenv(
    http_proxy  = proxy_url,
    https_proxy = proxy_url,
    HTTP_PROXY  = proxy_url,
    HTTPS_PROXY = proxy_url
  )
  
  invisible(proxy_url)
}


#' Test proxy connectivity
#'
#' Attempts to connect to a known external host to verify proxy configuration.
#'
#' @param url Character. URL to test. Default is Yahoo Finance endpoint.
#'
#' @return TRUE if successful, otherwise FALSE.
#'
#' @examples
#' set_proxy()
#' test_proxy()
#'
#' @export
test_proxy <- function(
    url = "https://query2.finance.yahoo.com"
) {
  tryCatch(
    {
      readLines(url, n = 1)
      TRUE
    },
    error = function(e) FALSE
  )
}

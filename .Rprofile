source("renv/activate.R")

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

if (Sys.getenv("ENV") == "BCRD")   set_proxy()
rm(set_proxy)

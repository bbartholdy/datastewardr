#' Function to retrieve statistics for DMPonline usage
#'
#' @param type string with type of request. Can be `users_joined`, `using_template`, `plans_by_template`, or `plans`.
#' @param start_date start date as string from when to retrieve stats in form "YYYY-MM-DD".
#' @param end_date end date as string until when to retrieve stats in form "YYYY-MM-DD".
#' @inheritParams retrieve_dmp
#' @examples
#' # Retrieve statistics for plans created between 1st January 2023 and 31st January 2023 (inclusive)
#' retrieve_statistics(type = "plans", start_date = "2023-01-01", end_data = "2023-01-31")
#' @importFrom httr content
#' @export
retrieve_statistics <- function(type = "plans", start_date = NULL, end_date = NULL, instance = "tudelft", ...){
  endpoint <- dmp_api_endpoints("statistics", ver = "v0", instance)
  auth_config <- dmponline_auth(ver = "v0", ...)

  endpt_ext <- match.arg(type, c(
    "users_joined",
    "using_template",
    "plans_by_template",
    "plans"
  ))

  req_url <- paste0(endpoint, "/", endpt_ext)
  if(!is.null(start_date) & is.null(end_date)) {
    req_url <- paste0(req_url, "?start_date=", start_date)

  } else if(is.null(start_date) & !is.null(end_date)) {
    req_url <- paste0(req_url, "?end_date=", end_date)
  } else if(!is.null(start_date) & !is.null(end_date)){
    req_url <- paste0(req_url, "?start_date=", start_date, "&end_date=", end_date)
  }
  resp <- GET(
    req_url,
    add_headers(
      auth_config$headers
    )
  )
  resp$request[[3]][3] <- "Token redacted"
  resp_content <- content(resp)
  output <- list("response" = resp, "content" = resp_content)
  print(resp)
  return(invisible(output))

}

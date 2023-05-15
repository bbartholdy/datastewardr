#' Function to retrieve a DMPs based on the users in 3mE
#'
#' @param type string with type of request. Can be `users_joined`, `using_template`, `plans_by_template`, or `plans`.
#' @param start_date start date to retrieve stats in form YYYY-MM-DD.
#' @param end_date end date to retrieve stats in form YYYY-MM-DD.
#' @importFrom httr content
#' @export
retrieve_statistics <- function(type = "plans", start_date = NULL, end_date = NULL){
  endpoint <- dmp_api_endpoints("statistics", ver = "v0")
  auth_config <- dmponline_auth(ver = "v0")

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
  return(resp)
}

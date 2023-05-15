#' Function to ping the DMPonline API.
#'
#' Basic function to ping the DMPonline API. No authentication required.
#' If the call returns Status: 200, then the API was successfully reached.
#' @importFrom httr GET

retrieve_heartbeat <- function(){
  req_url <- dmp_api_endpoints("heartbeat")
  GET(req_url)
}

#' Function to retrieve a DMP based on the plan number
#'
#' @details Takes the DMP number as an argument and returns the DMP as a list.
#' Default arguments returns the DMP metadata, including contact details of owner,
#' dates of creation and last updated, and project description.
#' The full plan also contains all the questions (with answers).
#' @source https://github.com/DMPRoadmap/roadmap/wiki/API-documentation
#' @param dmp_number number of the plan (i.e. https://dmponline.dcc.ac.uk/plans/<plan_number>)
#' @param full_plan logical. Whether or not to download the full plan (only available in API v0).
#' @param ... pass through arguments to `dmponline_auth()`
#' @importFrom httr GET add_headers content
#' @export
retrieve_dmp <- function(dmp_number, full_plan = F, ...){

  if(full_plan == TRUE){
    ver <- "v0"
    auth_config <- dmponline_auth(ver = ver)
    resp <- GET(
      paste0(dmp_api_endpoints("plans", ver = ver), "?plan=", dmp_number),
      add_headers(
        'Content-Type' = auth_config$headers[[1]],
        'Authorization' = auth_config$headers[[2]]
      )
    )
    return(resp)

  } else {
    if(is.null(cache$auth_config)){
      dmponline_auth(...)
    }
    auth_token <- cache$auth_config

    plan_url <- paste0(dmp_api_endpoints("plans"), "/", dmp_number)
    #print(plan_url)
    request <- GET(
      plan_url,
      add_headers(
        Accept = "application/json",
        'Authorization' = auth_token
      )
    )
    #print(request)
    resp_content <- content(request)$items
    output <- list("response" = request, "content" = resp_content)
    request
    return(invisible(output))

    #content(request)
  }
}

#' Function to retrieve plans created on a certain date
#'
#' @param date retrieve plans created on this date. defaults to today.
#' @inheritParams retrieve_dmp
#' @importFrom httr GET add_headers content
#' @export
retrieve_date <- function(date = Sys.Date(), ...){
  if(is.null(cache$auth_config)){
    dmponline_auth(...)
  }
  auth_token <- cache$auth_config
  access_token <- cache$access_token
  token <- retrieve_token()
  #date <- Sys.Date()
  # if multiple dates: created_after=X&created_before=Y
  req_url <- paste0(dmp_api_endpoints("plans", ...), "?created_after=", date)
  print(req_url)
  request <- GET(
    req_url,
    add_headers(
      'Content-Type' = "application/json",
      'Authorization' = paste("Token", token)
    )
  )
  content(request)
}


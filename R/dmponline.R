dmponline <- function(instance, endpoint, ver, ...){
  req_url <- dmp_api_endpoints(endpoint = endpoint, instance = instance, ver = ver)


  if(ver == "v0"){
    auth <- dmponline_auth(ver = ver)
    resp <- GET(
      paste0(req_url, path = NULL),
      add_headers(
        'Content-Type' = auth$headers[[1]],
        'Authorization' = auth$headers[[2]]
      )
    )
  } else {
    auth <- dmponline_auth(ver = ver)
    resp <- GET(
      req_url,
      add_headers(
        Accept = "application/json",
        'Authorization' = auth
      )
    )
  }

  return(resp)
}

#' Function to ping the DMPonline API.
#'
#' Basic function to ping the DMPonline API. No authentication required.
#' If the call returns Status: 200, then the API was successfully reached.
#' @importFrom httr GET
#' @export

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
#' @param instance string. Whether you are using the default DMPonline (dmponline.dcc.ac.uk) or your own organisation has an instance.
#' @param ... pass through arguments to `dmponline_auth()`
#' @importFrom httr GET add_headers content
#' @export
retrieve_dmp <- function(dmp_number, full_plan = F, ..., instance = "tudelft"){

  if(full_plan == TRUE){
    ver <- "v0"
    auth_config <- dmponline_auth(ver = ver, ...)
    resp <- GET(
      paste0(dmp_api_endpoints("plans", ver, instance), "?plan=", dmp_number),
      add_headers(
        'Content-Type' = auth_config$headers[[1]],
        'Authorization' = auth_config$headers[[2]]
      )
    )
    resp$request[[3]][3] <- "Token redacted"
    return(resp)

  } else {
    if(is.null(cache$auth_config)){
      dmponline_auth(...)
    }
    auth_token <- cache$auth_config
    ver = "v1"
    plan_url <- paste0(dmp_api_endpoints("plans", ver, instance), "/", dmp_number)
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
    output$response$request[[3]][2] <- "Bearer token redacted"
    request
    return(invisible(output))

    #content(request)
  }
}

#' Function to retrieve plans created from a certain date
#'
#' @param date retrieve plans created from this date. defaults to today.
#' @param verbose logical. Whether or not to print the API request (default = `TRUE`).
#' @inheritParams retrieve_dmp
#' @importFrom httr GET add_headers content
#' @export
retrieve_date <- function(date = Sys.Date(), ..., instance = "tudelft", verbose = T){

  ver <- "v0"
  token <- retrieve_token()
  # if multiple dates: created_after=X&created_before=Y
  req_url <- paste0(dmp_api_endpoints("plans", ver, instance), "?created_after=", date)
  request <- GET(
    req_url,
    add_headers(
      'Content-Type' = "application/json",
      'Authorization' = paste("Token", token)
    )
  )
  if(verbose == T)  print(request)
  content(request)
}


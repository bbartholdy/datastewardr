#' Store and retrieve DMPonline email and API token from environment
#'
#' @details You can find your DMPonline access token under Edit Profile > API Access
#' Use the email address associated with your DMPonline account.
#'
#' For the first time using the package, store your DMPonline email and API access
#' token in .Renviron using `set_env_var()`. For more details see `vignette("authorise")`
#' @rdname auth
#' @export

retrieve_token <- function(){
  tok <- Sys.getenv('DMPONLINE_TOKEN')
  if (identical(tok, "")) {
    stop("Please set env var DMPONLINE_TOKEN to your dmponline personal access token",
         call. = FALSE)
  }

  tok # encrypt the token?
}

#' @rdname auth
#' @export
retrieve_email <- function(){
  email <- Sys.getenv('DMPONLINE_EMAIL')
  if (identical(email, "")) {
    stop("Please set env var DMPONLINE_EMAIL to your dmponline email address",
         call. = FALSE)
  }

  email
}

#' @seealso [usethis::edit_r_environ()]
#' @inheritParams usethis::edit_r_environ
#' @rdname auth
set_env_var <- function (scope = c("user", "project"))
{
  path <- usethis:::scoped_path_r(scope, ".Renviron", envvar = "R_ENVIRON_USER")
  line <- "DMPONLINE_EMAIL=example@email.com\nDMPONLINE_TOKEN=randomlettersandnumbers"
  write(
    line,
    file=path,
    append=TRUE
  )
  usethis::edit_file(path)
  usethis::ui_todo("Replace the placeholder email and token and restart R for changes to take effect")
  invisible(path)
}

#' Authorise API access to DMPonline
#'
#' @param email email associated with DMPonline
#' @param token API token for DMPonline
#' @param ver Which version of DMPonline API to use ("v1" or "v0")
#' @rdname auth
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
dmponline_auth <- function(email = retrieve_email(), token = retrieve_token(), ver = "v1", instance = "tudelft"){
  if(ver == "v1"){
    endpoint <- dmp_api_endpoints("authenticate", ver = "v1", instance = instance)
    v1_headers <- list(
      'Content-Type' = "application/x-www-form-urlencoded;charset=UTF-8",
      Accept = "application/json",
      #Content-Type = "application/json", # if not admin
      grant_type = "authorization_code",
      #grant_type = "client_credentials",
      #client_id = client_id, # if not org admin
      #client_secret = client_secret # if not org admin
      email = email,
      code = token
    )

    json_body <- toJSON( # body passed to POST function
      v1_headers,
      auto_unbox = TRUE
    )
    tok <- POST(
      endpoint,
      body = json_body
    )
    auth_config <- paste0("Bearer ", content(tok)$access_token)
    cache_token(auth_config)

  } else if (ver == "v0"){
    v0_headers <- add_headers(
      'Content-Type' = "application/json",
      'Authorization' = paste0("Token ", "token=", token)
    )
    return(v0_headers)
  }
}

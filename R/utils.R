
dmponline_api <- function(instance){
  switch(
    instance,
    tudelft = "https://dmponline.tudelft.nl/api/",
    dcc = "https://dmponline.dcc.ac.uk/api/"
  )
}

dmp_api_endpoints <- function(endpoint = NULL, ver = "v1", instance = "tudelft"){
  # v1
  if(is.null(endpoint)) stop("Need to define endpoint\n Available endpoints:\n v1: plans, hearbeat, authenticate, templates\n v0: plans, guidance, departments, templates, statistics")
  if(ver == "v1"){
    endpt <- switch(
      endpoint,
      plans = "plans",
      heartbeat = "heartbeat",
      authenticate = "authenticate",
      templates = "templates"
    )
  } else if (ver == "v0"){
    endpt <- switch(
      endpoint,
      plans = "plans",
      guidance = "guidance",
      departments = "departments",
      templates = "templates",
      statistics = "statistics"
    )
  }
  base_url <- dmponline_api(instance)
  req_url <- paste0(base_url, ver, "/", endpt)
  return(req_url)
}

cache <- new.env(parent = emptyenv())

cache_token <- function(auth_config){
  # cache the bearer token
  cache$auth_config <- auth_config
}

# extract response to ethics question
check_ethics <- function(dmp){
  #ethics_json <- jsonlite::read_json("inst/extdata/ethics_questions.json")
  ethics_json <- jsonlite::read_json(json_file_path("ethics_questions.json"))
  template_id <- as.character(dmp[[1]]$template$id)
  template <- ethics_json$template_id[[template_id]]
  section <- template$ethics$section
  number <- unlist(template$ethics$number)
  q <- dmp[[1]]$plan_content[[1]]$sections[[section]]$questions[number]
  return(q)
}

# check if template is recommended by HREC
is_recommended <- function(org_id){
  #templates_json <- jsonlite::read_json("inst/extdata/ethics_templates.json")
  templates_json <- jsonlite::read_json(json_file_path("ethics_templates.json"))
  out <- templates_json$org_id[org_id]
  return(unlist(out, use.names = F))
}

json_file_path <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "datastewardr"))
  } else {
    system.file("extdata", path, package = "datastewardr", mustWork = TRUE)
  }
}

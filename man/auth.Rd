% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dmponline_auth.R
\name{retrieve_token}
\alias{retrieve_token}
\alias{retrieve_email}
\alias{set_env_var}
\alias{dmponline_auth}
\title{Store and retrieve DMPonline email and API token from environment}
\usage{
retrieve_token()

retrieve_email()

set_env_var(scope = c("user", "project"))

dmponline_auth(email = retrieve_email(), token = retrieve_token(), ver = "v1")
}
\arguments{
\item{scope}{Edit globally for the current \strong{user}, or locally for the
current \strong{project}}

\item{email}{email associated with DMPonline}

\item{token}{API token for DMPonline}

\item{ver}{Which version of DMPonline API to use ("v1" or "v0")}
}
\description{
Store and retrieve DMPonline email and API token from environment

Authorise API access to DMPonline
}
\details{
You can find your DMPonline access token under Edit Profile > API Access
Use the email address associated with your DMPonline account.

For the first time using the package, store your DMPonline email and API access
token in .Renviron using \code{set_env_var()}. For more details see \code{vignette("authorise")}
}
\seealso{
\code{\link[usethis:edit]{usethis::edit_r_environ()}}
}

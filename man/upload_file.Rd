% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vrac2.R
\name{upload_file}
\alias{upload_file}
\title{Upload a file in a seafile repos}
\usage{
upload_file(path, repos_id_destination = get_repo_id(),
  output_directory = "/", seafile_url = get_seafile_url(),
  token = get_seafile_api_token(),
  upload_link = get_upload_link(repos_id = repos_id_destination,
  seafile_url = seafile_url, token = token))
}
\arguments{
\item{path}{to to file to upload}

\item{repos_id_destination}{repo id destination}

\item{output_directory}{path to directory in the repos}

\item{seafile_url}{seafile url}

\item{token}{seafile token}

\item{upload_link}{upload link see get_upload_link()}
}
\description{
Upload a file in a seafile repos
}
\examples{
\dontrun{
upload_file(path = "file.txt",repos_id_destination = get_repo_id())

}

}

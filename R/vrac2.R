#' Title
#'
#' @param name library name
#' @param list_libraries all availables libraries
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
get_repo_id <- function(name = "My Library",ll = list_libraries()){
    ll %>%
    filter(name == !!name) %>%
    pull(id)
}


template_repos <-
  function(
           repos_id,
           base = "",
           seafile_url = get_seafile_url()  ,
           token = get_seafile_api_token()) {
    # curl -H 'Authorization: Token 24fd3c026886e3121b2ca630805ed425c272cb96' -H 'Accept: application/json; indent=4' https://cloud.seafile.com/api2/repos/

    r <- httr::GET(
      glue::glue("{seafile_url}/api2/repos/{repos_id}/{base}/"),
      add_headers(Authorization = glue::glue("Token {token}"))
    )
    httr::content(r)

  }


#' Title
#'
#' @param repos_id redos id
#' @param seafile_url seafile url
#' @param token seafile token
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_upload_link(repos_id = get_repo_id(name = "Pauline"))
#' }
get_upload_link <- function(
  repos_id =get_repo_id(),
  seafile_url = get_seafile_url()  ,
                            token = get_seafile_api_token()){

  template_repos(repos_id = repos_id,base = "upload-link/?p=/&replace=0",seafile_url = seafile_url,token=token)
}


#' Upload a file in a seafile repos
#'
#' @param path to to file to upload
#' @param repos_id_destination repo id destination
#' @param seafile_url seafile url
#' @param token seafile token
#' @param upload_link upload link see get_upload_link()
#'
#' @export
#' @importFrom glue glue
#' @examples
#' \dontrun{
#' upload_file(path = "file.txt",repos_id_destination = get_repo_id())
#'
#' }
#'
upload_file <- function(
  path,
  repos_id_destination,
  arbo="/",
  seafile_url = get_seafile_url(),
  token = get_seafile_api_token(),
  upload_link = get_upload_link(repos_id = repos_id_destination,seafile_url = seafile_url,token = token)
){

  to_run <- glue::glue('curl -H "Authorization: Token {token}" -F file=@{basename(path)}   -F filename={basename(path)}   -F parent_dir={arbo} {upload_link}' )
  message(glue::glue("to run ={to_run}"))
  system(to_run,wait = TRUE)
}



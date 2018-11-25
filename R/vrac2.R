#' Get repo is
#'
#' @param name library name
#' @param ll all availables libraries see get_repo_id()
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull
get_repo_id <- function(name = "My Library",ll = list_libraries()){
    ll %>%
    filter(name == !!name) %>%
    pull(id)
}

#' @importFrom httr GET add_headers content
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


#' Get upload link
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

#' Get directory
#'
#' @param dir directory
#' @param repos_id redos id
#' @param seafile_url seafile url
#' @param token seafile token
#'
#' @export
#'
get_dir <- function(
  dir="/",
  repos_id =get_repo_id(),
  seafile_url = get_seafile_url()  ,
  token = get_seafile_api_token()){

  template_repos(repos_id = repos_id,base = glue::glue("dir/?p={dir}"),seafile_url = seafile_url,token=token) %>% bind_rows()
}

#' Upload a file in a seafile repos
#'
#' @param path to to file to upload
#' @param repos_id_destination repo id destination
#' @param seafile_url seafile url
#' @param token seafile token
#' @param upload_link upload link see get_upload_link()
#' @param output_directory path to directory in the repos
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
  repos_id_destination= get_repo_id(),
  output_directory="/",
  seafile_url = get_seafile_url(),
  token = get_seafile_api_token(),
  upload_link = get_upload_link(repos_id = repos_id_destination,seafile_url = seafile_url,token = token)
){

  create_dir_r(dir = output_directory,repos_id = repos_id_destination,seafile_url = seafile_url,token = token)

httr::POST(
    url = upload_link,
    add_headers(Authorization = glue::glue("Token {token}")),
    body = list(
      file = httr::upload_file(path),
      filename = basename(path),
      parent_dir = output_directory
    )
  )




  # to_run <- glue::glue('curl -H "Authorization: Token {token}" -F file=@{basename(path)}   -F filename={basename(path)}   -F parent_dir={output_directory} {upload_link}' )
  # message(glue::glue("to run ={to_run}"))
  # system(to_run,wait = TRUE)





}



#' Create directory in a seafile repos
#'
#' @param dir directory to create
#' @param repos_id redos id
#' @param seafile_url seafile url
#' @param token seafile token
#'
#'
create_dir <- function(
  dir="/",
  repos_id =get_repo_id(),
  seafile_url = get_seafile_url()  ,
  token = get_seafile_api_token()


){

  if ( ! does_path_exist(dir = dir,repos_id = repos_id,seafile_url = seafile_url,token = token)){


  base <- glue::glue("dir/?p=/{dir}")



  httr::POST(
    glue::glue("{seafile_url}/api2/repos/{repos_id}/{base}/"),
    add_headers(Authorization = glue::glue("Token {token}")),
    body = list(

      operation="mkdir"

    )
  )
  }
}


#' Create directory in a seafile repos
#'
#'
#' @param dir directory to create
#' @param repos_id redos id
#' @param seafile_url seafile url
#' @param token seafile token
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom purrr map map_chr
#' @importFrom stringr str_split
create_dir_r <- function(dir="/",
                         repos_id =get_repo_id(),
                         seafile_url = get_seafile_url()  ,
                         token = get_seafile_api_token()
){
  b <- dir %>% str_split("/") %>% unlist()
  mapply(seq,from= rep(1,length(b)),to= 1:length(b),by=1)  %>%
    map(~b[.x]) %>%
    map_chr(~paste(.x,collapse = "/")) %>%
    map(~create_dir(dir = .x,repos_id = repos_id,
                    seafile_url = seafile_url,
                    token = token))

}

#' Does this path exist
#'
#' @param dir directory to techeckst
#' @param repos_id redos id
#' @param seafile_url seafile url
#' @param token seafile token
#'
#'
does_path_exist <- function(dir="/",
                            repos_id =get_repo_id(),
                            seafile_url = get_seafile_url()  ,
                            token = get_seafile_api_token()){

 ncol(get_dir(dir = dir,
              repos_id = repos_id,seafile_url = seafile_url,
              token = token)) !=1

}

#' ask for a token
#'
#' @param seafile_url seafile base url
#' @param username username
#' @param password password
#' @importFrom httr POST content
#' @importFrom rstudioapi askForPassword
#' @importFrom glue glue
#' @return a token as string
#' @export
#' @examples
#' \dontrun{
#' ask_seafile_api_token(seafile_url = "http://drop.legum.fr:8000/",username = "vincent@thinkr.fr")
#' }
ask_seafile_api_token <- function(username,password = rstudioapi::askForPassword(),seafile_url=get_seafile_url()){
  r <- httr::POST(url = glue::glue("{seafile_url}/api2/auth-token/"),
                  body=
                    list(
                      username = username,
                      password = password
                    )               )

  httr::content(r)$token

}

#' @title set_seafile_api_token
#' @description  set the seafile api token
#' @param token seafile api token
#' @importFrom magrittr %>%
#' @import assertthat
#' @export
set_seafile_api_token <- function(token,
                                  username,password = rstudioapi::askForPassword(),seafile_url=get_seafile_url()

                                  ){


  if ( missing(token) ){
    token <- ask_seafile_api_token(username=username,password = password,seafile_url=seafile_url)
  }
  if (is.null(token)){return(invisible(NULL))}

  delete_seafile_api_token()
  assert_that(is.character(token))
  token %>% key_set_with_value(service = "seafile_api_token",password = .)

  token
}


#' get seafile api token
#'
#' @param ask booleen ask if missing
#' @param username username
#' @param password password
#' @param seafile_url seafile base url
#'
#' @export
#'
get_seafile_api_token <- function(ask=TRUE,
                                  username=rstudioapi::askForPassword(prompt = "set username"),password = rstudioapi::askForPassword(),seafile_url=get_seafile_url()


                                  ){
  token <-NULL

  try(token<-key_get(service = "seafile_api_token"),silent=TRUE)


  if ( is.null(token) & ask){
    delete_seafile_api_token()
    token <- ask_seafile_api_token(username=username,password = password,seafile_url=seafile_url)
    token %>% key_set_with_value(service = "seafile_api_token",password = .)

  }
  token
}


#' @title update_seafile_api_token
#' @description  update the seafile api token
#' @importFrom magrittr %>%
#' @export
update_seafile_api_token <- function(){
  delete_seafile_api_token()
  ask_seafile_api_token() %>% key_set_with_value(service = "seafile_api_token",password = .)
}

#' @title delete_seafile_api_token
#' @description  delete the seafile api token
#' @export
delete_seafile_api_token <- function(){
  try(key_delete("seafile_api_token"),silent=TRUE)
}


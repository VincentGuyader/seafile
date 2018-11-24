#' get seafile url
#'
#' @param ask booleen ask if missing
#'
#' @export
#'
get_seafile_url <- function(ask=TRUE){
    url <- NULL

    try(url <- key_get(service = "seafile_url"), silent=TRUE)


    if ( is.null(url) & ask){
      delete_seafile_url()
      url <- rstudioapi::askForPassword(prompt = "enter seafile url")
      url %>% key_set_with_value(service = "seafile_url",password = .)

    }
    url
  }


#' @title set_seafile_url
#' @description  set the seafile url
#' @param url seafile url
#' @importFrom magrittr %>%
#' @import assertthat
#' @export
set_seafile_url <- function(url){


  if ( missing(url) ){
    url <- get_seafile_url()
  }
  if (is.null(url)){return(invisible(NULL))}

  delete_seafile_url()
  assert_that(is.character(url))
  url %>% key_set_with_value(service = "seafile_url",password = .)

  url
}





#' @title update_seafile_url
#' @description  update the seafile api url
#' @importFrom magrittr %>%
#' @importFrom  keyring key_set_with_value
#' @export
update_seafile_url <- function(){
  delete_seafile_url()
  get_seafile_url() %>% key_set_with_value(service = "seafile_url",password = .)
}

#' @title delete_seafile_url
#' @description  delete the seafile api url
#' @importFrom  keyring key_delete
#' @export
delete_seafile_url <- function(){
  try(key_delete("seafile_url"),silent=TRUE)
}

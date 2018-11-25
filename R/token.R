#' Ask seafile for a token
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
#' ask_seafile_api_token(seafile_url = "http://cloud.seafile.com",username = "vincent@thinkr.fr")
#' }
ask_seafile_api_token <-
  function(username = rstudioapi::showPrompt(title = "username", message = "email"),
           password = rstudioapi::askForPassword(),
           seafile_url = get_seafile_url()) {
    r <- httr::POST(
      url = glue::glue("{seafile_url}/api2/auth-token/"),
      body =
        list(username = username,
             password = password)
    )

    httr::content(r)$token

  }

#' @title Set seafile api token
#' @description  set the seafile api token
#'
#' @param username seafile username or email adress
#' @param password seafile password
#' @param seafile_url seafile base url
#' @param token seafile api token
#'
#' @importFrom magrittr %>%
#' @import assertthat
#' @export
#' @examples
#' \dontrun{
#' set_seafile_api_token(usename="mymail@domain.fr")
#' }
set_seafile_api_token <- function(token,
                                  username = rstudioapi::showPrompt(title = "username", message = "email")
                                  ,
                                  password = rstudioapi::askForPassword(),
                                  seafile_url = get_seafile_url()) {
  if (missing(token)) {
    token <-
      ask_seafile_api_token(username = username,
                            password = password,
                            seafile_url = seafile_url)
  }
  if (is.null(token)) {
    return(invisible(NULL))
  }

  delete_seafile_api_token()
  assert_that(is.character(token))
  token %>% key_set_with_value(service = "seafile_api_token", password = .)

  token
}


#' Get seafile api token
#'
#' @param ask booleen ask if missing
#' @param username seafile username or email adress
#' @param password seafile password
#' @param seafile_url seafile base url
#' @importFrom keyring key_get key_set_with_value
#' @export
#'
get_seafile_api_token <- function(ask = TRUE,
                                  username = rstudioapi::showPrompt(title = "username", message = "email"),
                                  password = rstudioapi::askForPassword(),
                                  seafile_url = get_seafile_url()) {
  token <- NULL

  try(token <- key_get(service = "seafile_api_token"), silent = TRUE)


  if (is.null(token) & ask) {
    delete_seafile_api_token()
    token <-
      ask_seafile_api_token(username = username,
                            password = password,
                            seafile_url = seafile_url)
    token %>% key_set_with_value(service = "seafile_api_token", password = .)

  }
  token
}

#' @title Update seafile api token
#' @description  update the seafile api token
#' @importFrom magrittr %>%
#' @importFrom keyring key_set_with_value
#' @export
update_seafile_api_token <- function() {
  delete_seafile_api_token()
  ask_seafile_api_token() %>% key_set_with_value(service = "seafile_api_token", password = .)
}

#' @title Delete seafile api token
#' @description  delete the seafile api token
#' @export
delete_seafile_api_token <- function() {
  try(key_delete("seafile_api_token"), silent = TRUE)
}

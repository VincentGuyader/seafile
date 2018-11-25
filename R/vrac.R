#' Get ping using token
#'
#' @param seafile_url seafile base url
#' @param token token
#'
#' @return pong as string if OK
#' @export
#' @importFrom httr GET add_headers content
#' @importFrom glue glue
#' @examples
#' \dontrun{
#' ping(seafile_url = "http://drop.legum.fr:8000/",
#' token = ask_seafile_api_token(seafile_url = "http://drop.legum.fr:8000/",
#' username = "vincent@thinkr.fr"))
#' }

ping <-
  function(seafile_url = get_seafile_url()  ,
           token = get_seafile_api_token()) {
    template_base(base = "ping",
                        seafile_url = seafile_url,
                        token = token)

  }


#' @importFrom httr GET add_headers content
#' @importFrom glue glue
template_base <-
  function(base = "ping",
           seafile_url = get_seafile_url()  ,
           token = get_seafile_api_token()) {

    r <- httr::GET(
      glue::glue("{seafile_url}/api2/{base}/"),
      add_headers(Authorization = glue::glue("Token {token}"))
    )
    httr::content(r)

  }



#' List libraries
#'
#' @param seafile_url seafile base url
#' @param token seafil token
#' @importFrom dplyr bind_rows
#' @return a data.frame
#' @importFrom magrittr %>%
#' @export
#'
list_libraries <-
  function(seafile_url = get_seafile_url()  ,
           token = get_seafile_api_token()) {
    template_base(base = "repos",
                        seafile_url = seafile_url,
                        token = token) %>%
      bind_rows()
  }

APIVERSION <- 'v1'
APIHOST <- 'https://catalyst.uw.edu'
APIURL <- sprintf('%s/rest/webq/%s/survey/', APIHOST, APIVERSION)

### PRIVATE ###
webq_auth <- function() {
  key <- Sys.getenv('CATALYST_KEY')
  netid <- Sys.getenv('CATALYST_NETID')
  if (identical(key, "") | identical(netid, "")) {
    stop("Please set your Catalyst authentication using the webq_config function. If you do not have an API key, you can create one here: https://catalyst.uw.edu/rest_user.",
         call. = FALSE)
  }
  list(key = key, netid = netid)
}

#' webq_request
#'
#' @param endpoint
#'
#' @return HTML Response as a character string
#'
#'
webq_request <- function(endpoint){
  stopifnot(is.character(endpoint))

  auth <- webq_auth()
  date <- format(Sys.time(), tz = 'UTC', format = '%a, %d %b %Y %H:%M:%S UTC')
  url <- paste0(APIURL, endpoint)

  auth_signature <- openssl::sha1(
    paste(
      auth$key, 'GET', gsub(APIHOST, '', url),  date, '',
      sep = '\n'
    ),

  )

  resp <- httr::GET(
    url = url,
    httr::add_headers(
      Date = date,
      Authorization = paste0('SolAuth ', auth$netid, ':', auth_signature)
    )
  )

  if(httr::http_error(resp)){
    resp_error <- rvest::html_text2(rvest::html_elements(httr::content(resp, encoding = 'UTF-8'), "error_string"))
    stop(sprintf("Connection unsuccessful. API message:\n    %s", resp_error), call. = FALSE)
  }

  httr::content(resp, encoding = 'UTF-8')
}


### PUBLIC ###

#' webq_config
#'
#' @param netid Your UW Netid
#' @param key Your Catalyst API Key
#'
#' @return NULL
#' @export
#'
#' @examples webq_config('susieq', 'myapikey000111222333')
#'
#' @description You must run this function before you can use the other features of this package.
#' If you don't have a Catalyst API key, get one here: https://catalyst.uw.edu/rest_user.
#'
webq_config <- function(netid, key) {
  stopifnot(is.character(netid))
  stopifnot(is.character(key))
  Sys.setenv(CATALYST_NETID = netid)
  Sys.setenv(CATALYST_KEY = key)
}


#' webq_surveys
#'
#' @return a tibble
#' @export
#'
#' @examples webq_surveys()
#' @description Returns a tibble of WebQ surveys that the user is authorized to view.
#' The tibble has the following rows:
#'
#'  * name: the survey name
#'  * survey_id: the id that can be used as an argument to retrieve results with `webq_survey_results`
#'  * response_count: the current number of responses
webq_surveys <- function(){
  html <- webq_request('')
  name <- rvest::html_text(rvest::html_elements(html, 'a.survey'))
  survey_id <- gsub(APIURL, '', rvest::html_attr(rvest::html_elements(html, 'a.survey'), 'href'))
  response_count <- as.numeric(rvest::html_text(rvest::html_elements(html, 'span.response_count')))
  if(length(unique(c(length(name), length(survey_id), length(response_count)))) != 1){stop("Response was not formatted as expected.")}
  tibble::tibble(name = name, survey_id = survey_id, response_count = response_count)
}

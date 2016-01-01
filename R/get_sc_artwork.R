#' @title Get SoundCloud track urls
#' @description SoundCloud API wrapper function.
#' @export get_sc_artwork
#' @param  track_url character. sound track urls.
#' @param ci a character. vector that SoundCloud API client id.
#' @param size image file size.
#' @examples
#' \dontrun{
#' ci <- '<your client id>'
#' track_url <- c('http://ift.tt/1Vjezde')
#' track_url %>% purrr::flatmap(get_sc_artwork, ci)
#' }
get_sc_artwork <- function (track_url = NULL,
                            ci        = NULL,
                            size      = "t300x300.jpg") {
  track_url %<>% longurl::expand_urls() %$% expanded_url
  httr::GET(paste0("https://api.soundcloud.com/resolve.json?url=",
                   track_url,
                   "&client_id=",
                   ci)) %>% httr::content() %$% artwork_url -> artwork_url
  gsub(pattern = "large.jpg", replacement = size, artwork_url)
}

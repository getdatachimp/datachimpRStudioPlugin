#' upload data files to data chimp
#' @export
dc_upload <- function(path) {
  mirror_auth_token <- Sys.getenv("CHIMP_TOKEN")
  base_url <- Sys.getenv("DC_UPLOAD_URL", "https://the.datachimp.app")
  upload_request <- httr::POST(
    paste0(base_url, "dataFileUpload"),
    httr::add_headers("X-token" = mirror_auth_token),
    body = httr::upload_file(path)
  )
  status_code <- httr::status_code(upload_request)
  if (status_code != 200) {
    stop(paste("File upload failed with status code", status_code))
  }
  return(jsonlite::fromJSON(httr::content(upload_request, "text"), flatten = T)$key)
}

#' download files from data chimp
#' @importFrom magrittr %>%
#' @export
dc_download <- function(key) {
  mirror_auth_token <- Sys.getenv("CHIMP_TOKEN")
  base_url <- Sys.getenv("DC_UPLOAD_URL", "https://the.datachimp.app")
  download_request <- httr::GET(
    paste0(base_url, "dataFileDownload", "?", "key=", key),
    httr::add_headers("X-token" = mirror_auth_token)
  )
  status_code <- httr::status_code(download_request)
  if (status_code != 200) {
    stop(paste("File download failed with status code", status_code))
  }
  httr::content(download_request, "text") %>%
    readr::write_file(key)
  return(key)
}

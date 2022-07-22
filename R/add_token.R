#' adds CHIMP_TOKEN to your environ file for secure communication with data chimp servers
#' @export
add_token_to_renviron <- function(token) {
  path <- usethis:::scoped_path_r(c("user", "project"), ".Renviron", envvar = "R_ENVIRON_USER")
  write(paste0("CHIMP_TOKEN=", token), file = path, append = T)
  rstudioapi::restartSession()
}

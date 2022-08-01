dc_env <- new.env(parent = emptyenv())
dc_env$dc_ws <- NULL
dc_env$history_loop <- NULL
dc_env$old_file <- NULL

mirror_server_url <- ifelse(Sys.getenv("MIRROR_SERVER_URL") == "",
                            "wss://mirror.datachimp.app",
                            Sys.getenv("MIRROR_SERVER_URL"))

.onLoad <- function(libname, pkgname) {
  log("calling .onLoad")
  if (!is.null(dc_env$dc_ws)) {
    log("closing old websocket")
    dc_env$dc_ws$close()
  }
  if (!is.null(dc_env$history_loop)) {
    log("destorying old loop")
    later::destroy_loop(dc_env$history_loop)
  }
  log(mirror_server_url)
  dc_env$dc_ws <- websocket::WebSocket$new(mirror_server_url, autoConnect = FALSE)
  dc_env$history_loop <- later::create_loop()
}

#' connects to some stuff
#' @importFrom magrittr %>%
#' @export
connect <- function() {
  dc_env$dc_ws$onOpen(function(event) {
    log("sending auth token")
    mirrorAuthToken <- Sys.getenv("CHIMP_TOKEN")
    dc_env$dc_ws$send(jsonlite::toJSON(list(mirrorAuthToken = mirrorAuthToken),
                             auto_unbox = TRUE))
  })
  dc_env$dc_ws$onMessage(function(event) {
    log("received message")
    message <- jsonlite::fromJSON(event$data)
    if (message %>% pluck_str("type") == "sessionStatus" && message %>% pluck_str("envCreated") == T) {
      print("Connection opened\n")
      timestamp(prefix = "## datachimp_start -- ")
      send()
    }
    if (message %>% pluck_str("type") == "executionResult" && length(message %>% purrr::pluck("response", "result")) == 0) {
      warning(paste("Unable to execute", message %>% pluck_str("response", "cmd"), "remotely. Your environment may be out of sync with data chimp."))
    }
    if (message %>% pluck_str("type") == "codeFromVisualization") {
      log("inserting text")
      rstudioapi::insertText(message %>% pluck_str("code"))
    }
  })
  dc_env$dc_ws$onClose(function(event) {
    cat("Client disconnected with code ", event$code,
        " and reason ", event$reason, "\n",
        sep = ""
    )
    dc_env$dc_ws <- websocket::WebSocket$new(mirror_server_url, autoConnect = FALSE)
    later::destroy_loop(dc_env$history_loop)
    dc_env$history_loop <- later::create_loop()
  })
  dc_env$dc_ws$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
  })
  dc_env$dc_ws$connect()
}



#' @importFrom magrittr %>%
send <- function() {
  savehistory(file = ".DC_Rhistory")
  file_contents <- readr::read_file(".DC_Rhistory") %>%
    stringr::str_split("## datachimp_start --.+")[[2]]
  if (is.null(dc_env$old_file)) {
    lines <- stringr::str_split(file_contents, "\n")[[1]]
    if (safe_send(get_last_valid_command(lines))) {
      dc_env$old_file <- file_contents
    }
  } else if (dc_env$old_file != file_contents) {
    old_lines <- stringr::str_split(dc_env$old_file, "\n")[[1]]
    new_lines <- stringr::str_split(file_contents, "\n")[[1]]
    diff_lines <- NULL
    for (i in 1:length(new_lines)) {
      if (i > length(old_lines) || old_lines[[i]] != new_lines[[i]]) {
        diff_lines <- new_lines[i:length(new_lines)]
        break
      }
    }
    if (safe_send(get_last_valid_command(diff_lines))) {
      dc_env$old_file <- file_contents
    }
  }
  later::later(send, 1, dc_env$history_loop)
}

pluck_str <- function(lst, ...) {
  purrr::pluck(lst, ..., .default = "")
}

get_last_valid_command <- function(lines) {
  trimmed_lines <- lines %>%
    purrr::discard(~ . == "" ||
                     . == 'devtools::load_all(".")' ||
                     . == "datachimpR::connect()" ||
                     startsWith(., "?") ||
                     startsWith(., "datachimpR::add_token_to_renviron") ||
                     grepl("dc_upload", .)
                   )
  i <- length(trimmed_lines)
  if (i == 0) {
    return(NULL)
  }
  cmd <- trimmed_lines[[i]]
  if (i == 1 && !is_valid_command(cmd)) {
    return(NULL)
  }

  while(i > 1 && stringr::str_detect(trimmed_lines[[i - 1]], "(?:%>%$)|\\+$")) {
    cmd <- paste(trimmed_lines[[i - 1]], cmd, sep = "\n", collapse = "")
    i <- i - 1
  }
  if (is_valid_command(cmd)) {
    return(cmd)
  } else {
    return(NULL)
  }
}



is_valid_command <- function(cmd) {
  tryCatch({
    parse(text = cmd)
    return(T)
  }, error = function(e) {
    return(F)
  })
}

safe_send <- function(cmd) {
  if (!is.null(cmd) && stringr::str_length(cmd) > 0) {
    dc_env$dc_ws$send(jsonlite::toJSON(list(type = "execute", cmd = cmd), auto_unbox = T))
    return(T)
  }
  log("message is null or empty, not sending")
  return(F)
}



log <- function(txt) {
  enabled <- Sys.getenv("DC_DEBUG") != ""
  if (enabled) {
    print(txt)
  }
}

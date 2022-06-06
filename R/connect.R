ws <- NULL
history_loop <- NULL
old_file <- NULL

.onLoad <- function(libname, pkgname) {
  if (!is.null(ws)) {
    ws$close()
  }
  if (!is.null(history_loop)) {
    later::destroy_loop(history_loop)
  }
  ws <<- websocket::WebSocket$new("ws://localhost:8765/", autoConnect = FALSE)
  history_loop <<- later::create_loop()
}


#' connects to some stuff
#' @export
connect <- function() {
  ws$onOpen(function(event) {
    cat("Connection opened\n")
  })
  ws$onMessage(function(event) {
    if (event$data == "pong") {
      rstudioapi::insertText(
        "fixed_penguins %>%
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point()"
      )
    }
  })
  ws$onClose(function(event) {
    cat("Client disconnected with code ", event$code,
        " and reason ", event$reason, "\n",
        sep = ""
    )
    ws <- websocket::WebSocket$new("ws://localhost:8765/", autoConnect = FALSE)
  })
  ws$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
  })
  ws$connect()
}

#' @export
#' @importFrom magrittr %>%
send <- function(txt) {
  savehistory(file = "/tmp/.DC_Rhistory")
  file_contents <- readr::read_file("/tmp/.DC_Rhistory")
  if (is.null(old_file)) {
    lines <- stringr::str_split(file_contents, "\n")[[1]]
    if (safe_send(get_last_valid_command(lines))) {
      old_file <<- file_contents
    }
  } else if (old_file != file_contents) {
    old_lines <- stringr::str_split(old_file, "\n")[[1]]
    new_lines <- stringr::str_split(file_contents, "\n")[[1]]
    diff_lines <- NULL
    for (i in 1:length(new_lines)) {
      if (i > length(old_lines) || old_lines[[i]] != new_lines[[i]]) {
        diff_lines <- new_lines[i:length(new_lines)]
        break
      }
    }
    if (safe_send(get_last_valid_command(diff_lines))) {
      old_file <<- file_contents
    }
  }
  later::later(send, 1, history_loop)
}

get_last_valid_command <- function(lines) {

  trimmed_lines <- lines %>%
    purrr::discard(~ . == "" || . == 'devtools::load_all(".")')
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
    print(cmd)
    ws$send(cmd)
    return(T)
  }
  return(F)
}

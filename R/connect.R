ws <- websocket::WebSocket$new("ws://localhost:8765/", autoConnect = FALSE)
#' connects to some stuff
#' @export
connect <- function() {
  ws$onOpen(function(event) {
    cat("Connection opened\n")
  })
  ws$onMessage(function(event) {
    cat("Client got msg: ", event$data, "\n")
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

history_loop <- later::create_loop()

old_file <- NULL

#' @export
#' @importFrom magrittr %>%
send <- function(txt) {
  savehistory(file = "/tmp/.DC_Rhistory")
  file_contents <- readr::read_file("/tmp/.DC_Rhistory")
  if (is.null(old_file)) {
    ws$send(file_contents)
  } else if (old_file != file_contents) {
    old_lines <- stringr::str_split(old_file, "\n")
    new_lines <- stringr::str_split(file_contents, "\n") %>%
      purrr::discard(. %in% old_lines) %>%
      paste(sep = "\n", collapse = '')

    ws$send(new_lines)
  }
  old_file <<- file_contents
  later::later(send, 1, history_loop)
}

#' @export
stop_sending <- function() {
  later::destroy_loop(history_loop)
}

#' Create directory (if needed) and set working directory
#'
#' @param path Target directory path.
#' @return Invisibly returns normalized path.
#' @export
fxx.create_and_setwd <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Directory created: ", path)
  } else {
    message("Directory already exists: ", path)
  }
  setwd(path)
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

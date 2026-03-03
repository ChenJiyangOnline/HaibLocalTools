#' Sanitize text into a valid-ish variable name
#'
#' @param input_string Character vector.
#' @return Character vector with non-valid characters replaced by "_".
#' @export
fxx.sanitize_variable_name <- function(input_string) {
  if (!is.character(input_string)) {
    input_string <- as.character(input_string)
  }

  sanitized <- gsub("^[^A-Za-z]", "_", input_string)
  sanitized <- gsub("[^A-Za-z0-9_]", "_", sanitized)

  message("Sanitized variable name: ", paste(sanitized, collapse = ", "))
  sanitized
}

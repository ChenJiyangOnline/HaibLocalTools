#' Get many colors from ggsci palettes
#'
#' @param num Number of colors requested.
#' @return A character vector of hex colors.
#' @export
fxx.get_many_color <- function(num = 10) {
  if (!requireNamespace("ggsci", quietly = TRUE)) {
    stop("Package 'ggsci' is required for `fxx.get_many_color()`.", call. = FALSE)
  }

  num <- as.integer(num)
  if (is.na(num) || num < 1) {
    stop("`num` must be a positive integer.", call. = FALSE)
  }

  if (num <= 10) {
    return(ggsci::pal_npg()(num))
  }

  if (num <= 20) {
    return(c(
      ggsci::pal_npg()(10),
      ggsci::pal_futurama()(num - 10)
    ))
  }

  pool <- c(
    ggsci::pal_futurama()(10),
    ggsci::pal_uchicago()(9),
    ggsci::pal_cosmic("hallmarks_light")(10),
    ggsci::pal_locuszoom()(7),
    ggsci::pal_jco()(10),
    ggsci::pal_jama()(7),
    ggsci::pal_aaas()(10),
    ggsci::pal_nejm()(8),
    ggsci::pal_npg()(10)
  )

  if (num > length(pool)) {
    warning("`num` exceeds available palette pool; returning max available colors: ", length(pool))
    return(pool)
  }

  pool[seq_len(num)]
}

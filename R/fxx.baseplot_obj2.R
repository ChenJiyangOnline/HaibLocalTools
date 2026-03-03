#' Save a plot object to PNG and PDF
#'
#' @param plot A plot object. Supports ggplot, grid grob/gList, pheatmap result,
#'   and ComplexHeatmap objects.
#' @param filename Output file path without extension.
#' @param height,height_plot Plot height in inches.
#' @param width,width_plot Plot width in inches.
#' @param dpi Resolution for PNG.
#'
#' @return Invisibly returns NULL.
#' @importFrom grid grid.draw
#' @importFrom grDevices png pdf dev.off
#' @export
fxx.save_plot <- function(plot, filename, height, width, dpi = 600) {
  # inches -> pixels
  height_px <- height * dpi
  width_px  <- width * dpi
  draw_plot <- function(p) {
    if (inherits(p, "ggplot")) {
      print(p)
    } else if (inherits(p, "gList")) {
      grid::grid.draw(p)
    } else if (inherits(p, "pheatmap")) {
      grid::grid.draw(p$gtable)
    } else if (inherits(p, c("Heatmap", "AdditiveUnit", "HeatmapList"))) {
      if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
        stop("Package 'ComplexHeatmap' is required to draw Heatmap objects.")
      }
      ComplexHeatmap::draw(p)
    } else {
      tryCatch(
        print(p),
        error = function(e) grid::grid.draw(p)
      )
    }
  }
  # PNG
  grDevices::png(
    filename = paste0(filename, ".png"),
    height = height_px, width = width_px, units = "px", res = dpi
  )
  draw_plot(plot)
  grDevices::dev.off()
  # PDF
  grDevices::pdf(
    file = paste0(filename, ".pdf"),
    height = height, width = width
  )
  draw_plot(plot)
  grDevices::dev.off()
  invisible(NULL)
}



# 内部工具：解析配置（优先参数，其次全局变量）
.resolve_arg_or_global <- function(x, name) {
  if (!is.null(x)) return(x)

  if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    val <- get(name, envir = .GlobalEnv, inherits = FALSE)
    if (!is.null(val) && length(val) == 1 && nzchar(as.character(val))) {
      return(as.character(val))
    }
  }

  stop(
    sprintf(
      "`%s` is required. Please pass `%s=` or define `%s` in .GlobalEnv.",
      name, name, name
    ),
    call. = FALSE
  )
}




#' @importFrom ggplot2 last_plot
#' @export
fxx.save_baseplot_obj2res <- function(final_folder,
                                      p_filename,
                                      p = ggplot2::last_plot(),
                                      height,
                                      width,
                                      dpi = 300,
                                      root_path = NULL,
                                      res_folder = NULL) {
  root_path  <- .resolve_arg_or_global(root_path,  "root_path")
  res_folder <- .resolve_arg_or_global(res_folder, "res_folder")

  out_dir <- file.path(root_path, res_folder, final_folder)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  out_prefix <- file.path(out_dir, p_filename)
  fxx.save_plot(p, out_prefix, height, width, dpi)
  invisible(out_prefix)
}




#' @importFrom ggplot2 last_plot
#' @export
fxx.save_baseplot_obj2rdata <- function(final_folder,
                                        p_filename,
                                        p = ggplot2::last_plot(),
                                        height,
                                        width,
                                        dpi = 300,
                                        root_path = NULL,
                                        rdata_folder = NULL) {
  root_path    <- .resolve_arg_or_global(root_path,    "root_path")
  rdata_folder <- .resolve_arg_or_global(rdata_folder, "rdata_folder")

  out_dir <- file.path(root_path, rdata_folder, final_folder)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  out_prefix <- file.path(out_dir, p_filename)
  fxx.save_plot(p, out_prefix, height, width, dpi)
  invisible(out_prefix)
}



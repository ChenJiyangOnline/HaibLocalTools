#' t-SNE scatter plot helper with multiple styles
#'
#' @param tsne_data data.frame containing t-SNE coordinates and optional group column.
#' @param group character, grouping column name. If NULL, auto-detect a suitable column.
#' @param x_col character, x-axis column name. Default "TSNE1".
#' @param y_col character, y-axis column name. Default "TSNE2".
#' @param mycol character vector of colors. If NULL, generated automatically.
#'
#' @return A named list of ggplot objects.
#' @importFrom rlang .data
#' @export
#' @examples
#' if (requireNamespace("Rtsne", quietly = TRUE)) {
#'   set.seed(11)
#'   n <- 30; p <- 20
#'   mat <- matrix(rnorm(n * p), nrow = p, ncol = n)
#'   grp <- rep(c("QC", "Pg", "AE"), length.out = n)
#'   mat_t <- t(mat)
#'   tsne_out <- Rtsne::Rtsne(mat_t, dims = 2, perplexity = 5, verbose = FALSE)
#'   tsne_data <- data.frame(TSNE1 = tsne_out$Y[,1], TSNE2 = tsne_out$Y[,2], Group = grp)
#'   cols <- c("#27AE60", "#8F5F9F", "#2E86AB")
#'   pl <- fxx.plot_tsne(tsne_data, group = "Group", mycol = cols)
#'   pl$colored
#' }
#'
#'
#'
fxx.plot_tsne <- function(tsne_data,
                          group = NULL,
                          x_col = "TSNE1",
                          y_col = "TSNE2",
                          mycol = NULL) {

  if (!all(c(x_col, y_col) %in% colnames(tsne_data))) {
    stop("Required columns not found: ", x_col, " and/or ", y_col, call. = FALSE)
  }

  if (is.null(group)) {
    other_cols <- setdiff(colnames(tsne_data), c(x_col, y_col))
    tsne_data <- tsne_data[, c(x_col, y_col, other_cols), drop = FALSE]

    potential_group <- NULL
    for (col in colnames(tsne_data)[3:ncol(tsne_data)]) {
      n_unique <- length(unique(tsne_data[[col]]))
      if (n_unique > 1 && n_unique < nrow(tsne_data)) {
        potential_group <- col
        break
      }
    }

    if (is.null(potential_group)) {
      stop("No suitable grouping column found. Please specify 'group' manually.", call. = FALSE)
    }

    group <- potential_group
    message("Using column '", group, "' as grouping variable. ",
            "Number of groups: ", length(unique(tsne_data[[group]])))
  }

  if (!group %in% colnames(tsne_data)) {
    stop("Group column not found: ", group, call. = FALSE)
  }

  if (is.null(mycol)) {
    n_groups <- length(unique(tsne_data[[group]]))
    if (n_groups > 100) {
      stop("Too many groups (>100). Please specify 'mycol' manually.", call. = FALSE)
    }
    mycol <- fxx.get_many_color(n_groups)
  }

  plot_list <- list()

  mytheme <- ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "right",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 6),
      axis.text = ggplot2::element_text(size = 6),
      legend.text = ggplot2::element_text(size = 6),
      plot.title = ggplot2::element_text(size = 6),
      strip.text = ggplot2::element_text(size = 6)
    )

  p <- ggplot2::ggplot(tsne_data, ggplot2::aes(.data[[x_col]], .data[[y_col]])) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::labs(x = x_col, y = y_col) +
    mytheme
  plot_list[["base"]] <- p

  p1 <- p +
    ggplot2::geom_point(ggplot2::aes(color = .data[[group]]), size = 2.2) +
    ggplot2::scale_color_manual(values = mycol)
  plot_list[["colored"]] <- p1

  p2 <- p1 +
    ggplot2::aes(shape = .data[[group]]) +
    ggplot2::scale_shape_manual(values = 15:(14 + length(mycol)))
  plot_list[["shaped"]] <- p2

  p3 <- p2 +
    ggplot2::stat_ellipse(ggplot2::aes(color = .data[[group]]), level = 0.95, linetype = 2)
  plot_list[["ellipse"]] <- p3

  p4 <- p2 +
    ggplot2::stat_ellipse(ggplot2::aes(fill = .data[[group]]), level = 0.95,
                          geom = "polygon", alpha = 0.2) +
    ggplot2::scale_fill_manual(values = mycol)
  plot_list[["filled_ellipse"]] <- p4

  p5 <- p2 +
    ggplot2::stat_ellipse(ggplot2::aes(fill = .data[[group]], color = .data[[group]]),
                          level = 0.95, linetype = 2, geom = "polygon", alpha = 0.2) +
    ggplot2::scale_fill_manual(values = mycol)
  plot_list[["filled_bordered_ellipse"]] <- p5

  formula_str <- paste(paste(x_col, y_col, sep = " + "), group, sep = " ~ ")
  tsne_mean <- doBy::summaryBy(stats::as.formula(formula_str), data = tsne_data, FUN = mean)
  tsne <- merge(tsne_data, tsne_mean, by = group)

  mean_x_col <- paste0(x_col, ".mean")
  mean_y_col <- paste0(y_col, ".mean")

  p6 <- ggplot2::ggplot(tsne, ggplot2::aes(.data[[x_col]], .data[[y_col]])) +
    ggplot2::geom_point(ggplot2::aes(color = .data[[group]], shape = .data[[group]]), size = 2.2) +
    ggplot2::scale_shape_manual(values = 15:(14 + length(mycol))) +
    ggplot2::scale_color_manual(values = mycol) +
    ggplot2::labs(x = x_col, y = y_col) +
    mytheme
  plot_list[["firework_base"]] <- p6

  p7 <- p6 +
    ggplot2::geom_segment(
      data = tsne,
      ggplot2::aes(
        x = .data[[mean_x_col]], y = .data[[mean_y_col]],
        xend = .data[[x_col]], yend = .data[[y_col]], color = .data[[group]]
      ),
      alpha = 0.6, show.legend = FALSE
    )
  plot_list[["firework_centroid"]] <- p7

  p8 <- p6 +
    ggplot2::stat_ellipse(
      ggplot2::aes(fill = .data[[group]]),
      level = 0.95, show.legend = FALSE, geom = "polygon", alpha = 0.2
    ) +
    ggplot2::scale_fill_manual(values = mycol) +
    ggplot2::geom_segment(
      data = tsne,
      ggplot2::aes(
        x = .data[[mean_x_col]], y = .data[[mean_y_col]],
        xend = .data[[x_col]], yend = .data[[y_col]], color = .data[[group]]
      ),
      alpha = 0.6, show.legend = FALSE
    )
  plot_list[["firework_ellipse"]] <- p8

  plot_list
}




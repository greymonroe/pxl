#' Plot a nucleotide data.table as image-like text art
#'
#' Uses \code{geom_text()} to draw nucleotide letters colored by the original
#' pixel hex. Y is reversed to match image coordinates.
#'
#' @param DT data.table from \code{image_nt_dt()} with columns x, y, col, nt.
#' @param txt_size Numeric. Text size for the letters.
#' @param family Character. Font family (default "mono").
#'
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' DT <- image_nt_dt("~/Desktop/Pistachio-Nuts.tiff",
#'                   nt_vec = sample(c("A","T","C","G"), 1e7, TRUE),
#'                   stride = 10)
#' p <- plot_nt_dt(DT, txt_size = 10)
#' }
#' @export
plot_nt_dt <- function(DT, txt_size = 10, family = "mono") {
  ggplot2::ggplot(DT, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(
      ggplot2::aes(label = nt, color = col, fontface = "bold"),
      family = family, size = txt_size, show.legend = FALSE
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_equal() +
    ggplot2::theme_void()
}

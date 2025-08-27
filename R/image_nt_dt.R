#' Build a nucleotide data.table from an image (drop pure black/white pixels)
#'
#' Reads a TIFF image, samples pixels on a grid, converts each sampled pixel
#' to a hex color, drops pure black/white (`#000000`, `#FFFFFF`), and assigns
#' nucleotides in order from `nt_vec`.
#'
#' @param path Character. Path to a TIFF image.
#' @param nt_vec Character vector of nucleotides/labels. Must be at least as long
#'   as the number of kept pixels (no recycling).
#' @param stride Integer (>=1). Subsampling step; 1 = every pixel.
#'
#' @return A data.table with columns \code{x}, \code{y}, \code{col}, \code{nt}.
#' @examples
#' \dontrun{
#' DT <- image_nt_dt("~/Desktop/Pistachio-Nuts.tiff",
#'                   nt_vec = sample(c("A","T","C","G"), 1e7, TRUE),
#'                   stride = 10)
#' }
#' @export
image_nt_dt <- function(path, nt_vec, stride = 1L) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (stride < 1L) stop("stride must be >= 1.")

  nut  <- tiff::readTIFF(path, as.is = TRUE)
  dims <- dim(nut)
  if (length(dims) < 2L) stop("Unexpected image dimensions.")

  h <- dims[1]; w <- dims[2]

  # grid (optionally downsampled)
  rows <- seq.int(1L, h, by = stride)
  cols <- seq.int(1L, w, by = stride)
  grid <- as.matrix(expand.grid(row = rows, col = cols))

  # per-pixel color (RGB or grayscale)
  if (length(dims) >= 3L && dims[3] >= 3L) {
    pix_col <- mapply(function(r, c) grDevices::rgb(nut[r, c, 1], nut[r, c, 2], nut[r, c, 3]),
                      grid[,1], grid[,2])
  } else {
    vals <- nut[cbind(grid[,1], grid[,2])]
    pix_col <- grDevices::gray(vals)
  }

  DT <- data.table::data.table(
    y   = grid[,1],
    x   = grid[,2],
    col = pix_col
  )

  # drop ONLY pure black & pure white
  DT <- DT[!col %in% c("#000000", "#FFFFFF")]

  N <- nrow(DT)
  if (length(nt_vec) < N) {
    stop("nt_vec length (", length(nt_vec), ") is smaller than number of kept pixels (", N, ").")
  }

  DT[, nt := nt_vec[seq_len(N)]]
  DT[]
}

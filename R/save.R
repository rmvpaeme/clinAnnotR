#' Save a clinical figure to file
#'
#' A thin wrapper around [ggplot2::ggsave()] with clinically sensible defaults
#' and automatic device detection from the file extension.
#'
#' @param figure A [ggplot2::ggplot()] or
#'   [patchwork][patchwork::patchwork-package] object, typically the output of
#'   [make_clinical_figure()].
#' @param path Character. Output file path. The device is inferred from the
#'   extension: `.pdf`, `.svg`, `.png`, `.tiff`, `.jpg`, `.eps` are all
#'   supported via [ggplot2::ggsave()].
#' @param width Numeric. Figure width. Default: `7`.
#' @param height Numeric. Figure height. Default: `8`.
#' @param units Character. Units for `width` and `height`. One of `"in"`,
#'   `"cm"`, `"mm"`, `"px"`. Default: `"in"`.
#' @param dpi Numeric. Resolution in dots per inch (raster outputs only).
#'   Default: `300`.
#' @param device Character or function. Passed to [ggplot2::ggsave()]. `NULL`
#'   = inferred from the file extension. Default: `NULL`.
#' @param ... Additional arguments forwarded to [ggplot2::ggsave()].
#'
#' @return Invisibly returns `path`.
#'
#' @examples
#' \dontrun{
#' lab <- example_data("lab")
#' tx  <- example_data("treatment")
#' fig <- make_clinical_figure(
#'   lab, tx,
#'   lab_panels = list(lab_panel(line_params = "WBC (/\u00b5L)"))
#' )
#' save_clinical_figure(fig, "figure.pdf")
#' save_clinical_figure(fig, "figure.png", dpi = 150)
#' save_clinical_figure(fig, "figure.svg", width = 14, height = 16)
#' }
#'
#' @export
save_clinical_figure <- function(
    figure,
    path,
    width  = 7,
    height = 8,
    units  = "in",
    dpi    = 300,
    device = NULL,
    ...
) {
  if (is.null(device)) {
    ext    <- tolower(tools::file_ext(path))
    device <- switch(ext,
      pdf  = "pdf",
      svg  = "svg",
      png  = "png",
      tiff = "tiff",
      tif  = "tiff",
      jpg  = "jpg",
      jpeg = "jpeg",
      eps  = "eps",
      NULL   # ggsave will guess
    )
  }

  ggsave(
    filename = path,
    plot     = figure,
    width    = width,
    height   = height,
    units    = units,
    dpi      = dpi,
    device   = device,
    ...
  )
  invisible(path)
}

lifebrain_colors <- c(grey = "#f7ec48", greylow = "#DAA520", yellow = "#3f3f3f")

lifebrain_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (lifebrain_colors)
  
  lifebrain_colors[cols]
}

lifebrain_palettes <- list(
  main  = lifebrain_cols(names(lifebrain_colors)),
  grey  = lifebrain_cols("grey", "greylow"),
  light   = lifebrain_cols("greylow", "yellow"),
  dark = lifebrain_cols("grey", "yellow")
)

lifebrain_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- lifebrain_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_colour_lifebrain <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lifebrain_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", palette = pal, ...)
  } else {
    scale_colour_gradientn(colours = pal(256), ...)
  }
}

scale_fill_lifebrain <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lifebrain_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

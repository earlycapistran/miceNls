# =============================================================================
# c_mydas ggplot theme
# earlycapistran@comunidad.unam.mx - March 2021
# =============================================================================

#' @title Custom ggplot theme for Conservation Letters
#' @description Custom ggplot theme based on 'theme_light'
#' 
#' @param base_size Base fontsize
#' @param base_family Base font family
#' @param base_line_size Base line size
#' @param base_rect_size Base rect size
#' @export
#' 
#' @import ggplot2
#' @import grDevices

# generating new theme
theme_cmydas <- function(base_size = 22,
                      base_family = "Lato",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170){
  theme_light(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 500), 
        face = "bold",
        hjust = 0),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 500),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(105, 105, 105, maxColorValue = 500),
        size = rel(0.55), angle = 90),
      axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
      panel.grid.major = element_blank(),   
      panel.grid.minor = element_blank(), 
      legend.position = "bottom",
      legend.title = element_text(color = rgb(105, 105, 105, maxColorValue = 500), 
                                              size = rel(0.65)),
      legend.text = element_text(color = rgb(105, 105, 105, maxColorValue = 500),
                                             size = rel(0.65)),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(0,0,0,0),
      legend.box = "vertical",
      legend.spacing.y = unit(1, "mm"),
      complete = TRUE
    )
}

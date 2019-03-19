#-----------------------------------------------------------------------
# Eduardo Jr's theme for ggplot2
#-----------------------------------------------------------------------

#--------------------------------------------
# My color pallete (based on stata)
ej_pallete <- function(n) {
  if (n > 15) {
    warning("The pallete has 15 colors. You have supplied ",
            n, ". The colors will be repeated.")
  }
  colors <- c("#1a476f", "#90353b", "#55752f", "#e37e00", "#6e8e84",
              "#c10534", "#938dd2", "#cac27e", "#a0522d", "#7b92a8",
              "#2d6d66", "#9c8847", "#bfa19c", "#ffd200", "#d9e6eb")
  rep(colors,  length.out = n)
}

#--------------------------------------------
# Update default scales (based on lattice)
scale_shape <- function(..., solid = FALSE)
  gggplo2::scale_shape(..., solid = solid)
scale_shape_discrete <- function(..., solid = FALSE)
  ggplot2::scale_shape_discrete(..., solid = solid)
scale_color_discrete <- function(...) {
  ggplot2::discrete_scale("colour", "ejscale", ej_pallete, ...)
}
scale_colour_discrete <- function(...) {
  ggplot2::discrete_scale("colour", "ejscale", ej_pallete, ...)
}
scale_fill_discrete <- function(...) {
  ggplot2::discrete_scale("fill", "ejscale", ej_pallete, ...)
}

#--------------------------------------------
# Vertical facet on the left
facet_grid <- function (..., switch = "y") {
  ggplot2::facet_grid(..., switch = switch)
}

#--------------------------------------------
# Update default geoms (based on lattice)
update_geom_defaults("point", list(size = 2, shape = 1))
update_geom_defaults("smooth", list(color = "black", size = .7, span = 2/3))

#--------------------------------------------
# Smoother curves in geom_smooth(method = loess) (matches to lattice)
geom_smooth <- function(...,
                        se = FALSE,
                        method = "loess",
                        method.args = list()) {
  if (identical(method, "loess")) {
    method.args <- modifyList(list(degree = 1L, span = 2/3),
                              method.args)
    ggplot2::geom_smooth(...,
                         se = se,
                         method = method,
                         method.args = method.args)
  } else {
    ggplot2::geom_smooth(...,
                         se = se,
                         method = method,
                         method.args = method.args)
  }
}

#--------------------------------------------
# Define theme
ejtheme <-
  theme_bw(base_family = "Palatino", base_size = 11) +
  theme(panel.grid.minor = element_line(colour = "grey90",
                                        size = .3,
                                        linetype = "solid"),
        panel.grid.major = element_line(colour = "grey90",
                                        size = .3,
                                        linetype = "solid"),
        strip.text = element_text(size = 10.5, color = "black"),
        # plot.margin = margin(t =  5.5, r = 5.5, b = 5.5, l = 5.5),
        plot.tag = element_text(face = "bold"),
        plot.tag.position = "bottomleft",
        legend.key.width = grid::unit(1.5, "line"))

#--------------------------------------------
theme_set(ejtheme)

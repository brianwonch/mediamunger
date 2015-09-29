#' ggplot2 theme with pretty default settings
#'
#' @param base_size reference font size.  Default is 20.
#' @param base_family reference font family
#'
#' @return set of modified ggplot2 theme elements
#' @export
#' @import ggplot2
#' @examples ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() + theme_nice(base_size=12)
theme_nice = function(base_size = 20, base_family = ""){
    theme(
        line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
        rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
        text = element_text(family = base_family, face = "plain", colour = "#564D49", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),

        axis.text = element_text(size = rel(1), colour = "grey50"),
        strip.text = element_text(size = rel(1.4)),
        axis.line = element_line(size=.5,color="#2B221E"),
        axis.text.x = element_text(vjust = 1),
        axis.text.y = element_text(hjust = 1),
        axis.ticks = element_line(colour = "grey50"),
        axis.title.x = element_text(size=rel(1.2), face="bold"),
        axis.title.y = element_text(angle = 90,size=rel(1.2), face="bold"),
        axis.ticks.length = grid:::unit(0.15, "cm"),
        axis.ticks.margin = grid:::unit(0.2, "cm"),

        legend.background = element_rect(colour = NA),
        legend.margin = grid:::unit(0.3, "cm"),
        legend.key = element_rect(fill = "grey95", colour = "white"),
        legend.key.size = grid:::unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(1.2)),
        legend.text.align = NULL,
        legend.title = element_text(size = rel(1.2), face = "bold", hjust = 0),
        legend.title.align = NULL,
        legend.position = "bottom",
        legend.direction = NULL,
        legend.justification = "center",
        legend.box = NULL,

        panel.background = element_rect(fill = "white",colour = NA),
        panel.border = element_rect(size=.5,color="#564D49",fill=NA),
        panel.grid.major = element_line(colour = "#F5F5F5", size = 0.04),
        panel.grid.minor = element_line(colour = "#F5F5F5", size = 0.15),
        panel.margin = grid:::unit(2, "lines"),
        panel.margin.x = NULL,
        panel.margin.y = NULL,

        strip.background = element_rect(size=.5, color="#564D49", fill = "grey80", colour = NA),
        strip.text.x = element_text(size=rel(.75)),
        strip.text.y = element_text(angle = -90, size=rel(1.2)),

        plot.background = element_rect(colour = "white"),
        plot.title = element_text(size = rel(1.7)),
        plot.margin = grid:::unit(c(1, 1, 1, 1), "lines"), complete = TRUE)
}

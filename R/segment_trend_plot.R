#' Trended geom_barplot with segmented trended geom_barplots on top
#'
#' @param df input data frame
#' @param trend_col column with top-level trends
#' @param segmentation_col column for lower_level segmentation
#'
#' @return printed plot
#' @import ggplot2
#' @importFrom scales comma
#'
#' @export
segment_trend_plot <- function(df, trend_col, segmentation_col){
    ggplot(aes_string(x = trend_col)) +
    geom_bar(stat="identity",
             aes_string(y="value"),
             fill="#FF8E00",
             alpha = .3) +
    geom_bar(stat="identity", position="dodge", color="#DDDDDD",
             aes_string(y="value", fill=segmentation_col)) +
    scale_fill_brewer(palette="Blues") +
    facet_wrap(~metric, scales="free_y", nrow=2) +
    theme_icrossing() +
    scale_y_continuous(labels = comma) +
    # scale_fill_manual(values = belk_colors) +
    theme(axis.title = element_blank())

if (!is.null(title)){
    p <- p + ggtitle(title)
}

print(p)
}
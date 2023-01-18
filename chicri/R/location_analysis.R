
#' Producing a heatmap based on a variable
#'
#' @param data A `data.frame` containing where the shapefiles are contained in the `geometry` column
#' @param variable A string of the column name of the variable to be used to create the heat map
#' @param legend.title A string of the title of the legend to be included in the plot
#' @param trans A string indicating the transformation to be applied to the scale (options are "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time")
#'
#' @return A `ggplot` object with a heat map of the shapefiles in `data` with the fill colour determined by `variable`
#'
#' @import ggplot2 sf
#'
#' @export
#'
#' @examples
#'
#'
plot_heat_map <- function(data, variable, legend.title = variable ,trans = "identity"){

   p<- ggplot() +
    geom_sf(data = data, aes(fill = get(variable))) +
    scale_fill_viridis_c(name = legend.title,option = "magma", trans = trans) +
    theme_void()

    return(p)
}



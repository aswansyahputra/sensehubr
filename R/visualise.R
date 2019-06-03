#' Visualise product or sensory attributes
#' 
#' Make comparison between products and sensory attributes in visual representation.
#' 
#' @param res result of local or global analysis
#' @param ... other arguments to pass on specific visualisation method
#' 
#' @export
visualise <- function(res, ...){
  UseMethod("visualise")
}

#' @export
visualise.default <- function(res, ...) {
  stop("`res` is not a result of local nor global analysis.", call. = FALSE)
}


#' Visualise sensory properties
#'
#' Create representation of sensory properties of each product in form of radar plot.
#'
#' @param res result of local or global analysis
#' @param min_scales minimum value of the scales
#' @param max_scales maximum value of the scales
#' @param point_size point size
#' @param line_width line width
#' @param title a title to use in plot
#' @param legend_position position of legend, valid values are "top", "right", "bottom", and "left"
#' @param ... not yet implemented
#'
#' @import dplyr
#' @importFrom ggradar ggradar
#' @importFrom scales rescale
#'
#' @return a ggplot object
#' 
#' @name visualise-local
#'
#' @export
#' @examples 
#' data(perfume_qda_experts)
#' perfume_qda_experts %>% 
#' specify(
#'   panelist = panelist,
#'   product = product,
#'   session = session,
#'   pres_order = rank,
#'   attribute = spicy:wrapping,
#'   method = "QDA"
#' ) %>% 
#' analyse_local() %>% 
#' visualise()
visualise.tbl_sensory_local <- function(res, min_scales = 0, max_scales = 10, point_size = 4 , line_width = 1, title = "Radar plot of sensory properties", legend_position = "bottom", ...) {
  res <- 
    res %>% 
    select(-statistic, -p.value) %>%
    gather("product", "values", -attribute) %>% 
    spread(attribute, values) %>% 
    mutate_if(is.numeric, ~ rescale(.x, to = c(min_scales, max_scales), from = c(min_scales, max_scales))) %>% 
    ggradar(font.radar = "sans", 
            grid.label.size = 5,
            grid.min = min_scales, 
            grid.mid = (min_scales + max_scales) / 2, 
            grid.max = max_scales,
            values.radar = c(min_scales, (min_scales + max_scales) / 2, max_scales),
            group.point.size = point_size,
            group.line.width = line_width, 
            plot.title = title, 
            legend.position = legend_position)
  
  return(res)
}

#' Visualise sensory space
#' 
#' Plot product representations or attributes on sensory spaces.
#'
#' @param res output of global analysis
#' @param choice component of interest, valid values are "product" and "attribute"
#' @param dimension dimension to focus, integer vector of length 2 
#' @param repel avoid overplotting of text label
#' @param colour_by colour point or line based on specific properties, valid values are "quality" and "contribution"
#' @param title a title to use in plot
#' @param ... not yet implemented
#'
#' @importFrom factoextra fviz
#' @importFrom ggplot2 labs scale_colour_viridis_c
#' 
#' @return a ggplot object
#' @export
#' 
#' @name visualise-global
#'
#' @examples
#' data(perfume_qda_experts)
#' perfume_qda_experts %>% 
#' specify(
#'   panelist = panelist,
#'   product = product,
#'   session = session,
#'   pres_order = rank,
#'   attribute = spicy:wrapping,
#'   method = "QDA"
#' ) %>% 
#' analyse_global() %>% 
#' visualise(choice = "attribute", colour_by = "contribution")
visualise.tbl_sensory_global <- function(res, choice = c("product", "attribute"), dimension = c(1, 2), repel = FALSE, colour_by =  c("none", "quality", "contribution"), title = "default", ...) {
  res_global <- res$res_global
  
  if (choice[[1]] == "product") {
    element <- switch(class(res_global)[[1]],
                      "PCA" = "ind",
                      "MCA" = "ind",
                      "CA"  = "row") 
  } else if (choice[[1]] == "attribute") {
    element <- switch(class(res_global)[[1]],
                      "PCA" = "var",
                      "MCA" = "var",
                      "CA"  = "col")
  } 
  
  res <- fviz(res_global, 
              element = element,
              axes = dimension, 
              repel = repel, 
              color = switch(colour_by[[1]],
                             "none" = "black",
                             "quality" = "cos2",
                             "contribution" = "contrib")) +
    labs(
      title = ifelse(title == "default",
                     ifelse(choice == "product",
                            "Representation of products",
                            "Correlation circle of sensory attributes"
                            ),
                     title),
      colour = switch(colour_by[[1]],
                      "none" = "",
                      "quality" = "Quality",
                      "contribution" = "Contribution")
    ) +
    if (colour_by[[1]] %in% c("quality", "contribution")) {
      scale_colour_viridis_c(direction = -1)
    }
  return(res)
}
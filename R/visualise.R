#' Visualise product or sensory attributes
#'
#' Make comparison between products and sensory attributes in visual representation.
#'
#' @param res result of local or global analysis
#' @param ... other arguments to pass on specific visualisation method
#'
#' @export
visualise <- function(res, ...) {
  UseMethod("visualise")
}

#' @export
visualise.default <- function(res, ...) {
  stop("`res` is invalid.", call. = FALSE)
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
#' @importFrom dplyr select mutate_if
#' @importFrom tidyr gather spread
#' @importFrom ggradar ggradar
#' @importFrom scales rescale
#'
#' @return a ggplot object
#'
#' @name visualise-local
#'
#' @export
#' @examples
#' perfume_qda_experts %>%
#'   specify(
#'     sensory_method = "QDA",
#'     panelist = panelist,
#'     product = product,
#'     session = session,
#'     pres_order = rank,
#'     attribute = spicy:wrapping
#'   ) %>%
#'   analyse_local() %>%
#'   visualise()
visualise.tbl_sensory_local <- function(res, min_scales = 0, max_scales = 10, point_size = 4, line_width = 1, title = "Radar plot of sensory properties", legend_position = "bottom", ...) {
  res <-
    res %>%
    select(-statistic, -p.value) %>%
    gather("product", "values", -attribute) %>%
    spread(attribute, values) %>%
    mutate_if(is.numeric, ~ rescale(.x, to = c(min_scales, max_scales), from = c(min_scales, max_scales))) %>%
    ggradar(
      font.radar = "sans",
      grid.label.size = 5,
      grid.min = min_scales,
      grid.mid = (min_scales + max_scales) / 2,
      grid.max = max_scales,
      values.radar = c(min_scales, (min_scales + max_scales) / 2, max_scales),
      group.point.size = point_size,
      group.line.width = line_width,
      plot.title = title,
      legend.position = legend_position
    )

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
#' @importFrom rlang arg_match
#' @importFrom factoextra fviz
#' @importFrom ggplot2 ggplot aes geom_col geom_hline scale_x_continuous scale_y_continuous sec_axis scale_colour_viridis_c labs theme_minimal
#' @importFrom scales percent_format
#'
#' @return a ggplot object
#' @export
#'
#' @name visualise-global
#'
#' @examples
#' perfume_qda_experts %>%
#'   specify(
#'     sensory_method = "QDA",
#'     panelist = panelist,
#'     product = product,
#'     session = session,
#'     pres_order = rank,
#'     attribute = spicy:wrapping
#'   ) %>%
#'   analyse_global() %>%
#'   visualise(choice = "attribute", colour_by = "contribution")
visualise.tbl_sensory_global <- function(res, choice = c("product", "attribute", "eigenvalue"), dimension = c(1, 2), repel = FALSE, colour_by = c("none", "quality", "contribution"), title = "default", ...) {
  res_global <- res$res_global

  choice <- arg_match(choice)
  colour_by <- arg_match(colour_by)

  if (choice[[1]] == "eigenvalue") {
    tbl <- res_global %>%
      inspect_space()

    max_dim <- NROW(tbl)

    res <- ggplot(tbl, aes(dimension, eigenvalue)) +
      geom_col(fill = "lightblue") +
      scale_x_continuous(breaks = seq_len(max_dim)) +
      scale_y_continuous(
        sec.axis = sec_axis(~ . / max_dim,
          name = "Explained variance",
          labels = percent_format()
        )
      ) +
      labs(
        x = "Dimension",
        y = "Eigenvalue",
        title = ifelse(title == "default", "Screeplot", title)
      ) +
      theme_minimal()
  } else {
    if (choice[[1]] == "product") {
      element <- switch(class(res_global)[[1]],
        "PCA" = "ind",
        "MCA" = "ind",
        "CA" = "row"
      )
    } else if (choice[[1]] == "attribute") {
      element <- switch(class(res_global)[[1]],
        "PCA" = "var",
        "MCA" = "var",
        "CA" = "col"
      )
    }
    res <- fviz(res_global,
      element = element,
      axes = dimension,
      repel = repel,
      color = switch(colour_by[[1]],
        "none" = "black",
        "quality" = "cos2",
        "contribution" = "contrib"
      )
    ) +
      labs(
        title = ifelse(title == "default",
          ifelse(choice == "product",
            "Representation of products",
            "Correlation circle of sensory attributes"
          ),
          title
        ),
        colour = switch(colour_by[[1]],
          "none" = "",
          "quality" = "Quality",
          "contribution" = "Contribution"
        )
      ) +
      if (colour_by[[1]] %in% c("quality", "contribution")) {
        scale_colour_viridis_c(direction = -1)
      }
  }

  return(res)
}

#' @importFrom dplyr select
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal
#' @importFrom scales rescale
#' 
#' @export
visualise.tbl_sensory_liking <- function(res, xlab = "", ylab = "Mean of hedonic rating", title = "", ...) {
  res <-
    res %>% 
    select(-statistic, -p.value) %>%
    gather("product", "values", -attribute) %>%
    ggplot(aes(x = product, y = values)) +
    geom_col() +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_minimal()
  
  return(res)
}

#' @importFrom rlang arg_match
#' @export
visualise.tbl_sensory_preference <- function(res, choice = c("product", "panelist", "eigenvalue"), dimension = c(1, 2), repel = FALSE, colour_by = c("none", "quality", "contribution"), title = "default", ...) {
  res$res_global <- res$res_preference
  
  choice <- arg_match(choice)
  colour_by <- arg_match(colour_by)
  
  choice <- switch(choice[[1]],
                   "product" = "attribute",
                   "panelist" = "product",
                   "eigenvalue" = "eigenvalue")
  
  if (title == "default") {
    title <- "Representation of panelists"
    if (choice == "attribute") {
      title <- "Representation of products"
    }
  }
  
  res <- visualise.tbl_sensory_global(res = res, 
                                      choice = choice,
                                      dimension = dimension, 
                                      repel = repel,
                                      colour_by = colour_by,
                                      title = title)
  return(res)
}

#' Visualise penalty
#'
#' Plot liking drop and citing frequency from penalty analysis.
#'
#' @param res output penalty analysis
#' @param product product of interest (only one product)
#' @param frequency_threshold threshold for citing frequency
#' @param drop_threshold threshold for iking drop
#' @param title a title to use in plot
#' @param xlab label for x-axis
#' @param ylab label for y-axis
#' @param ... not yet implemented
#'
#' @importFrom rlang arg_match
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_vline geom_hline scale_x_continuous scale_colour_manual labs theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales percent_format
#'
#' @return a ggplot object
#' @export
#'
#' @name visualise-penalty
#'
#' @examples
#' perfume_jar %>%
#'   specify(
#'     sensory_method = "JAR",
#'     panelist = consumer,
#'     product = product,
#'     attribute = intensity:green,
#'     hedonic = liking
#'   ) %>%
#'   analyse_penalty(reference_value = 0) %>% 
#'   visualise("Chanel N5")
visualise.tbl_sensory_penalty <- function(res, product, frequency_threshold = 20, drop_threshold = 1, title = "Penalty analysis", xlab = "Citing frequency (%)", ylab = "Mean of liking drop", ...) {
  subproduct <- arg_match(product, values = unique(res$product))
  
  if (length(product) > 1) {
    stop("Please select only one product.", call. = FALSE)
  }
  
  res <- res %>%
    filter(product == subproduct) %>% 
    mutate(attribute = ifelse(p.value <= 0.05, paste0(attribute, "*"), attribute)) %>% 
    ggplot(aes(x = frequency, y = penalty, colour = category)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = attribute), show.legend = FALSE) +
    geom_vline(xintercept = frequency_threshold, lty = 2, colour = "grey30") +
    geom_hline(yintercept = drop_threshold, lty = 2, colour = "grey30") +
    scale_x_continuous(labels = percent_format()) +
    scale_colour_manual(values = c("blue", "red")) +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      colour = NULL
    ) +
    theme_minimal()
    
  return(res)
}

#' Visualise External Preference Map
#' 
#' Plot preference map of products in sensory space.
#' 
#' @param res result of external preference mapping
#' @param title a title to use in plot
#' @param xlab label for x-axis
#' @param ylab label for y-axis
#' @param ... not yet implemented
#' 
#' @importFrom ggplot2 ggplot aes geom_raster geom_contour scale_fill_viridis_c labs theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales percent_format
#' 
#' @export
visualise.tbl_sensory_prefmap <- function(res, title = "External Preference Mapping", xlab = "Dim 1", ylab = "Dim 2", ...) {
  tbl_above_average <- res$above_average
  tbl_product <- res$product
  
  res <- ggplot(mapping = aes(x = dim1, y = dim2)) +
    geom_raster(aes(fill = prop_panelist), data = tbl_above_average) +
    geom_contour(aes(z = prop_panelist), data = tbl_above_average, colour = "gray80") +
    geom_point(data = tbl_product, shape = 15, size = 3, colour = "white") +
    geom_text_repel(aes(label = product), data = tbl_product, size = 4, colour = "white") +
    scale_fill_viridis_c(labels = percent_format()) +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      fill = "Proportion of panelist"
    ) +
    theme_minimal()
  return(res)
}

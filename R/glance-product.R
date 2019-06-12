#' Glance product
#' 
#' Evaluate product in global analysis.
#'
#' @param res_global output of global analysis
#' @param dimension dimension to focus, integer vector of length 2
#' 
#' @export
glance_product <- function(res_global, dimension = c(1, 2)) {
  UseMethod("glance_product")
}

#' @importFrom dplyr mutate left_join select rename_at arrange vars desc
#' @importFrom factoextra facto_summarize
#' @importFrom tibble new_tibble
#' 
#' @export
glance_product.default <- function(res_global, dimension = c(1, 2)) {
  if (!is.numeric(dimension)) {
    stop("`dimension` should be an integer vector.", call. = FALSE)
  }
  
  if (length(dimension) != 2) {
    stop("`dimension` should be an integer vector of length 2.", call. = FALSE)
  }
  
  if (identical(dimension[1], dimension[2])) {
    stop("`dimension` should not idenctical.", call. = FALSE)
  }
  
  if (dimension[1] != dimension[2] - 1) {
    stop("`dimension` should be subsequential with increase of 1.", call. = FALSE)
  }
  
  if (any(class(res_global) %in% "PCA")) {
    element <- "ind"
  } else if (any(class(res_global) %in% "CA")) {
    element <- "row"
  }
  
  coord <- facto_summarize(res_global, element = element, result = "coord", axes = dimension) %>% 
    mutate(name = as.character(name))
  
  cos2 <- facto_summarize(res_global, element = element, result = "cos2", axes = dimension) %>% 
    mutate(name = as.character(name))
  
  contrib <- facto_summarize(res_global, element = element, result = "contrib", axes = dimension) %>% 
    mutate(name = as.character(name))
  
  tbl <- 
    coord %>% 
    left_join(cos2, by = "name") %>% 
    left_join(contrib, by = "name") %>% 
    select(
      attribute = name,
      everything(),
      quality = cos2,
      contribution = contrib,
      -coord
    ) %>% 
    rename_at(vars(starts_with("Dim")), ~tolower(sub("\\.", "", .x))) %>% 
    arrange(desc(contribution))
  
  res <- new_tibble(tbl,
                    "n_product" = NROW(tbl),
                    "dimension" = c(dimension[[1]], dimension[[2]]),
                    nrow = NROW(tbl), 
                    class = "tbl_sensory_global_product")
  return(res)
}

#' @export
glance_product.tbl_sensory_global <- function(res_global, dimension = c(1, 2)) {
  res_global_extracted <- res_global$res_global
  res <- glance_product(res_global_extracted, dimension = dimension)
  return(res)
}

#' Glance product
#' 
#' Evaluate product in global analysis.
#'
#' @param res_global output of global analysis
#' @param dimension dimension to focus, integer vector of length 2
#' 
#' @import dplyr
#' @importFrom factoextra facto_summarize
#' @importFrom tibble as_tibble trunc_mat
#'
#' @return a dataframe
#' @export

glance_product <- function(res_global, dimension = c(1, 2)) {
  UseMethod("glance_product")
}

#' @export
glance_product.default <- function(res_global, dimension = c(1, 2)) {
  stop("`res_global` is not valid.", call. = FALSE)
}

#' @export
glance_product.PCA <- function(res_global, dimension = c(1, 2)) {
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
  
  coord <- facto_summarize(res_global, element = "ind", result = "coord", axes = dimension) %>% 
    mutate(name = as.character(name))
  
  cos2 <- facto_summarize(res_global, element = "ind", result = "cos2", axes = dimension) %>% 
    mutate(name = as.character(name))
  
  contrib <- facto_summarize(res_global, element = "ind", result = "contrib", axes = dimension) %>% 
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
  
  res <- trunc_mat(as_tibble(tbl))
  
  res$summary <- c("Results of" = paste("product", angle_brackets(paste("dim", dimension[1], "and", dimension[2]))))
  
  return(res)
}

# @export
glance_product.tbl_sensory_global <- function(res_global, dimension = c(1, 2)) {
  res_global_extracted <- res_global$res_global
  res <- glance_product(res_global_extracted, dimension = dimension)
  return(res)
}

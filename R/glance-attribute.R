#' Glance sensory attribute
#' 
#' Evaluate sensory attribute in global analysis.
#'
#' @param res_global output of global analysis
#' @param dimension dimension to focus, integer vector of length 2
#' 
#' @import dplyr
#' @importFrom factoextra facto_summarize get_pca_var
#' @importFrom tibble as_tibble trunc_mat
#'
#' @return a dataframe
#' @export

glance_attribute <- function(res_global, dimension = c(1, 2)) {
  UseMethod("glance_attribute")
}

#' @export
glance_attribute.default <- function(res_global, dimension = c(1, 2)) {
  stop("`res_global` is not valid.", call. = FALSE)
}

#' @export
glance_attribute.PCA <- function(res_global, dimension = c(1, 2)) {
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
  
  coord <- facto_summarize(res_global, element = "var", result = "coord", axes = dimension) %>% 
    mutate(name = as.character(name))
  
  cor <- get_pca_var(res_global)[["cor"]][,dimension] %>% 
    as_tibble(rownames = "name") %>% 
    rename_at(vars(starts_with("Dim")), ~paste0("cor_to_", tolower(sub("\\.", "", .x))))
  
  cos2 <- facto_summarize(res_global, element = "var", result = "cos2", axes = dimension) %>% 
    mutate(name = as.character(name))
  
  contrib <- facto_summarize(res_global, element = "var", result = "contrib", axes = dimension) %>% 
    mutate(name = as.character(name))
  
  tbl <- 
    coord %>% 
    left_join(cor, by = "name") %>% 
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
  
  res <- trunc_mat(as_tibble(tbl), width = Inf)
  
  res$summary <- c("Results of" = paste("sensory attributes", angle_brackets(paste("dim", dimension[1], "and", dimension[2]))))
  
  return(res)
}

#' @export
glance_attribute.tbl_sensory_global <- function(res_global, dimension = c(1, 2)) {
  res_global_extracted <- res_global$res_global
  res <- glance_attribute(res_global_extracted, dimension = dimension)
  return(res)
}


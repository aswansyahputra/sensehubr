#' Inspect liking
#'
#' Evaluate panelist in liking analysis.
#'
#' @param res_liking output of preference analysis
#' @param dimension dimension to focus, integer vector of length 2
#'
#' @export
inspect_panelist_liking <- function(res_liking, dimension = c(1, 2)) {
  UseMethod("inspect_panelist_liking")
}

#' @importFrom dplyr mutate left_join select rename_at arrange vars desc
#' @importFrom factoextra facto_summarize get_pca_var
#' @importFrom tibble new_tibble
#' @export
inspect_panelist_liking.default <- function(res_liking, dimension = c(1, 2)) {
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
  
  if (any(class(res_liking) %in% "PCA")) {
    element <- "ind"
  } else if (any(class(res_liking) %in% "CA")) {
    element <- "row"
  }
  
  coord <- facto_summarize(res_liking, element = element, result = "coord", axes = dimension) %>%
    mutate(name = as.character(name))
  
  cos2 <- facto_summarize(res_liking, element = element, result = "cos2", axes = dimension) %>%
    mutate(name = as.character(name))
  
  contrib <- facto_summarize(res_liking, element = element, result = "contrib", axes = dimension) %>%
    mutate(name = as.character(name))
  
  tbl <-
    coord %>%
    left_join(cos2, by = "name") %>%
    left_join(contrib, by = "name") %>%
    select(
      panelist = name,
      everything(),
      quality = cos2,
      contribution = contrib,
      -coord
    ) %>%
    rename_at(vars(starts_with("Dim")), ~ tolower(sub("\\.", "", .x))) %>%
    arrange(desc(contribution))
  
  res <- new_tibble(tbl,
                    "n_product" = NROW(tbl),
                    "dimension" = c(dimension[[1]], dimension[[2]]),
                    nrow = NROW(tbl),
                    class = "tbl_sensory_preference_panelist"
  )
  return(res)
}

#' @export
inspect_panelist_liking.tbl_sensory_liking <- function(res_liking, dimension = c(1, 2)) {
  res_liking_extracted <- res_liking$res_liking_global
  res <- inspect_panelist_liking(res_liking_extracted, dimension = dimension)
  return(res)
}

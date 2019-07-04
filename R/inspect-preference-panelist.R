#' Inspect preference
#'
#' Evaluate panelist in preference analysis.
#'
#' @param res_preference output of preference analysis
#' @param dimension dimension to focus, integer vector of length 2
#'
#' @export
inspect_preference_panelist <- function(res_preference, dimension = c(1, 2)) {
  UseMethod("inspect_preference_panelist")
}

#' @importFrom dplyr mutate left_join select rename_at arrange vars desc
#' @importFrom factoextra facto_summarize
#' @importFrom tibble new_tibble
#'
#' @export
inspect_preference_panelist.default <- function(res_preference, dimension = c(1, 2)) {
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
  
  if (any(class(res_preference) %in% "PCA")) {
    element <- "ind"
  } else if (any(class(res_preference) %in% "CA")) {
    element <- "row"
  }
  
  coord <- facto_summarize(res_preference, element = element, result = "coord", axes = dimension) %>%
    mutate(name = as.character(name))
  
  cos2 <- facto_summarize(res_preference, element = element, result = "cos2", axes = dimension) %>%
    mutate(name = as.character(name))
  
  contrib <- facto_summarize(res_preference, element = element, result = "contrib", axes = dimension) %>%
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
                    "n_panelist" = NROW(tbl),
                    "dimension" = c(dimension[[1]], dimension[[2]]),
                    nrow = NROW(tbl),
                    class = "tbl_sensory_preference_panelist"
  )
  return(res)
}

#' @export
inspect_preference_panelist.tbl_sensory_preference <- function(res_preference, dimension = c(1, 2)) {
  res_preference_extracted <- res_preference$res_preference
  res <- inspect_preference_panelist(res_preference_extracted, dimension = dimension)
  return(res)
}

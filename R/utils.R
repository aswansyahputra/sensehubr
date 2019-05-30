angle_brackets <- function(x) {
  paste0("<", x, ">")
}

#' Capture metadata
#'
#' @param x a dataframe of class `tbl_sensory`
#' @param meta metadata, one of "panelist", "product", "attribute", or "hedonic"
#' 
#' @importFrom stringr str_trunc
#'
#' @return a metadata

meta_info <- function(x, meta = c("dimension", "panelist", "product", "attribute", "hedonic")) {
  if (!meta[[1]] %in% c("dimension", "panelist", "product", "attribute", "hedonic")) {
    stop("Unknown metadata", call. = FALSE)
  }
  
  if (meta[[1]] == "dimension") {
    res <- paste(NROW(x), "x", NCOL(x))
  }
  
  if (meta[[1]] == "panelist") {
    res <- paste(attr(x, "panelist"), angle_brackets(paste(attr(x, "n_panelist"), "subjects")))
  }
  
  if (meta[[1]] == "product") {
    res <- paste(attr(x, "product"), angle_brackets(paste(attr(x, "n_product"), "items")))
  }
  
  if (meta[[1]] == "attribute") {
    res <- paste(
      stringr::str_trunc(paste(attr(x, "attribute"), collapse = ", "), 35),
      angle_brackets(paste(paste(attr(x, "n_attribute"), "lexicons")))
    )
  }
  
  if (meta[[1]] == "hedonic") {
    if (attr(x, "hedonic") == "NULL") {
      res <- "None"
    } else {
      res <- attr(x, "hedonic")
    }
  }
  return(res)
}

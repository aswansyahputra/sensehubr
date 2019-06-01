angle_brackets <- function(x) {
  paste0("<", x, ">")
}

#' @importFrom utils getFromNamespace
print.tbl <- getFromNamespace("print.tbl", "tibble")

#' @importFrom pillar style_subtle

cat_subtle <- function(...) {
  cat(pillar::style_subtle(paste0(...))) 
}

#' @importFrom stringr str_trunc

meta_info <- function(x, meta = c("sensory_table", "dimension", "method", "panelist", "product", "attribute", "hedonic")) {
  if (!meta[[1]] %in% c("sensory_table", "dimension", "method", "panelist", "product", "attribute", "hedonic")) {
    stop("Unknown metadata", call. = FALSE)
  }
  
  if (meta[[1]] == "sensory_table") {
    res <- paste(attr(x, "method"), angle_brackets(paste(NROW(x), "x", NCOL(x))))
  }
  
  if (meta[[1]] == "dimension") {
    res <- paste(NROW(x), "x", NCOL(x))
  }
  
  if (meta[[1]] == "method") {
    res <- attr(x, "method")
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

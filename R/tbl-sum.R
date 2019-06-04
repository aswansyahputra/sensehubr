#' @export
tbl_sum.tbl_sensory_design <- function(x){
  c(
    "A sensory table" = "Design of Experiment",
    "Panelist" = paste(attr(x, "n_panelist"), "subjects"),
    "Product" = paste(attr(x, "n_product"), "items")
  )
}

#' @export
tbl_sum.tbl_sensory_template <- function(x){
  c(
    "A sensory table" = "Design of Experiment",
    "Panelist" = paste(attr(x, "n_panelist"), "subjects"),
    "Product" = paste(attr(x, "n_product"), "items"),
    "Attribute" = paste(attr(x, "n_attribute"), "lexicons") 
  )
}

#' @export
tbl_sum.tbl_sensory_qda <- function(x){
  c(
    "A sensory table" = meta_info(x, "sensory_table"),
    "Panelist" = meta_info(x, "panelist"),
    "Product" = meta_info(x, "product"),
    "Attribute" = meta_info(x, "attribute"),
    "Hedonic" = meta_info(x, "hedonic")
  )
}

#' @export
tbl_sum.tbl_sensory_local <- function(x) {
  c(
    "A sensory table" = meta_info(x, "method"),
    "Type" = "Local analysis",
    "Method" = attr(x, "method_local"),
    "Model" = attr(x, "model")
  )
}

#' @export
tbl_sum.tbl_sensory_global_eigenvalue <- function(x) {
  c(
    "Description of" = "Eigenvalue",
    "Number of dimension" = attr(x, "n_dimension")
  )
}

#' @export
tbl_sum.tbl_sensory_global_product <- function(x) {
  c(
    "Description of" = paste("Product", 
                             angle_brackets(paste(attr(x, "n_product"), "items"))),
    "Dimension" = paste("Dim", 
                        attr(x, "dimension")[[1]], 
                        "x", 
                        "Dim", 
                        attr(x, "dimension")[[2]])
  )
}

#' @export
tbl_sum.tbl_sensory_global_product <- function(x) {
  c(
    "Description of" = paste("Product", 
                             angle_brackets(paste(attr(x, "n_product"), "items"))),
    "Dimension" = paste("Dim", 
                        attr(x, "dimension")[[1]], 
                        "x", 
                        "Dim", 
                        attr(x, "dimension")[[2]])
  )
}

#' @export
tbl_sum.tbl_sensory_global_attribute <- function(x) {
  c(
    "Description of" = paste("Sensory attribute", 
                             angle_brackets(paste(attr(x, "n_attribute"), "lexicons"))),
    "Dimension" = paste("Dim", 
                        attr(x, "dimension")[[1]], 
                        "x", 
                        "Dim", 
                        attr(x, "dimension")[[2]])
  )
}

#' @importFrom stringr str_pad
#' @export
print.tbl_sensory_global <- function(x, ...){
  cat_subtle(paste("#", str_pad("A sensory table:", width = 23, side = "right"), attr(x, "method"), "\n"))
  cat_subtle(paste("#", str_pad("Type:", width = 23, side = "right"), "Global analysis", "\n"))
  cat_subtle(paste("#", str_pad("Method:", width = 23, side = "right"), attr(x, "method_global"), "\n"))
  cat_subtle(paste("#", str_pad("Active individual:", width = 23, side = "right"), attr(x, "n_product"), "products", "\n"))
  cat_subtle(paste("#", str_pad("Active variable:", width = 23, side = "right"), attr(x, "n_attribute"), "sensory attributes", "\n"))
  cat_subtle(paste("#", str_pad("Supplementary variable:", width = 23, side = "right"), ifelse(attr(x, "hedonic") == "NULL", "None", attr(x, "hedonic")), "\n"))
  cat_subtle("#\n")
  print.tbl(x$eigenvalue)
  cat_subtle("#\n")
  print.tbl(x$product)
  cat_subtle("#\n")
  print.tbl(x$attribute)
  invisible(x)
}

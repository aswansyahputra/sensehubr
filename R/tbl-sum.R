#' @export
tbl_sum.tbl_sensory_design <- function(x){
  c(
    "Design of Experiment" = print_meta(x, "dimension"),
    "Panelist" = print_meta(x, "n_panelist"),
    "Product" = print_meta(x, "n_product")
  )
}

#' @export
tbl_sum.tbl_sensory_template <- function(x){
  c(
    "A sensory table" = print_meta(x, "dimension"),
    "Panelist" = print_meta(x, "panelist"),
    "Product" = print_meta(x, "product"),
    "Attribute" = print_meta(x, "attribute") 
  )
}

#' @export
tbl_sum.tbl_sensory_qda <- function(x){
  c(
    "A sensory table" = print_meta(x, "dimension"),
    "Sensory method" = print_meta(x, "sensory_method"),
    "Panelist" = print_meta(x, "panelist"),
    "Product" = print_meta(x, "product"),
    "Attribute" = print_meta(x, "attribute"),
    "Hedonic" = print_meta(x, "hedonic")
  )
}

#' @export
tbl_sum.tbl_sensory_cata <- function(x){
  c(
    "A sensory table" = print_meta(x, "dimension"),
    "Sensory method" = print_meta(x, "sensory_method"),
    "Panelist" = print_meta(x, "panelist"),
    "Product" = print_meta(x, "product"),
    "Attribute" = print_meta(x, "attribute"),
    "Hedonic" = print_meta(x, "hedonic")
  )
}

#' @export
tbl_sum.tbl_sensory_local <- function(x) {
  c(
    "Local analysis" = "",
    "Sensory method" = print_meta(x, "sensory_method"),
    "Analytical method" = print_meta(x, "method_local"),
    "Model" = print_meta(x, "model")
  )
}

#' @export
tbl_sum.tbl_sensory_global_eigenvalue <- function(x) {
  c(
    "Description of" = "Eigenvalue",
    "Number of dimension" = attr(x, "n_dimension")
  )
}

#' @importFrom glue glue
#' @export
tbl_sum.tbl_sensory_global_product <- function(x) {
  c(
    "Description of" = glue("Product <{print_meta(x, 'n_product')}>"),
    "Dimension" = print_meta(x, "dimension")
  )
}

#' @importFrom glue glue
#' @export
tbl_sum.tbl_sensory_global_attribute <- function(x) {
  c(
    "Description of" = glue("Sensory attribute <{print_meta(x, 'n_attribute')}>"),
    "Dimension" = print_meta(x, "dimension")
  )
}

print.tbl_sensory_global <- function(x, ...) {
  cat_subtle(
    glue(
      "
    {pad('# Global analysis:')}
    {pad('# Sensory method:')} {sensory_method}
    {pad('# Method:')} {method_global}
    {pad('# Active individual:')} {active_individual}
    {pad('# Active variable:')} {active_variable}
    {pad('# Supplementary variable:')} {supplementary_variable}
    #
    
    ",
      sensory_method = print_meta(x, "sensory_method"),
      method_global = print_meta(x, "method_global"),
      active_individual = print_meta(x, "n_product"),
      active_variable = print_meta(x, "n_attribute"),
      supplementary_variable = print_meta(x, "hedonic")
    )
  )
  print(x$eigenvalue)
  cat_subtle("#\n")
  print(x$product)
  cat_subtle("#\n")
  print(x$attribute)
  
  invisible(x)
}

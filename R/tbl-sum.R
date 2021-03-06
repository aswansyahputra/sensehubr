#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_discrim <- function(x) {
  c(
    "Sensory method" = print_meta(x, "sensory_method"),
    "Analytical method" = print_meta(x, "method_local")
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_design <- function(x) {
  c(
    "Design of Experiment" = print_meta(x, "dimension"),
    "Panelist" = print_meta(x, "n_panelist"),
    "Product" = print_meta(x, "n_product")
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_template <- function(x) {
  c(
    "A sensory table" = print_meta(x, "dimension"),
    "Panelist" = print_meta(x, "panelist"),
    "Product" = print_meta(x, "product"),
    "Attribute" = print_meta(x, "attribute")
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_performance_panel <- function(x) {
  c(
    "Description of" = "Panel performance",
    "Metric" = "Discrimination, Agreement, Consistency"
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_performance_panelist <- function(x) {
  c(
    "Description of" = "Panelist performance",
    "Metric" = switch(attr(x, "metric"),
      "discrimination" = "Discrimination",
      "agreement" = "Agreement",
      "consistency" = "Consistency"
    )
  )
}

#' @export
print.tbl_sensory_performance <- function(x, ...) {
  cat_subtle(
    glue(
      "
    {pad('# Performance analysis:')}
    {pad('# Method:')} {method_local}
    {pad('# Model for panel:')} {panel_model}
    {pad('# Model for panelist:')} {panelist_model}
    #
    
    ",
      method_local = print_meta(x, "method_local"),
      panel_model = print_meta(x, "panel_model"),
      panelist_model = print_meta(x, "panelist_model")
    )
  )
  print(x$panel)
  cat_subtle("#\n")
  print(x$panelist_discrimination)
  cat_subtle("#\n")
  print(x$panelist_agreement)
  cat_subtle("#\n")
  print(x$panelist_consistency)

  invisible(x)
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_qda <- function(x) {
  c(
    "A sensory table" = print_meta(x, "dimension"),
    "Sensory method" = print_meta(x, "sensory_method"),
    "Panelist" = print_meta(x, "panelist"),
    "Product" = print_meta(x, "product"),
    "Attribute" = print_meta(x, "attribute"),
    "Hedonic" = print_meta(x, "hedonic")
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_cata <- function(x) {
  c(
    "A sensory table" = print_meta(x, "dimension"),
    "Sensory method" = print_meta(x, "sensory_method"),
    "Panelist" = print_meta(x, "panelist"),
    "Product" = print_meta(x, "product"),
    "Attribute" = print_meta(x, "attribute"),
    "Hedonic" = print_meta(x, "hedonic")
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_jar <- function(x) {
  c(
    "A sensory table" = print_meta(x, "dimension"),
    "Sensory method" = print_meta(x, "sensory_method"),
    "Panelist" = print_meta(x, "panelist"),
    "Product" = print_meta(x, "product"),
    "Attribute" = print_meta(x, "attribute"),
    "Hedonic" = print_meta(x, "hedonic")
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_penalty <- function(x) {
  c(
    "Penalty analysis" = "",
    "Sensory method" = print_meta(x, "sensory_method"),
    "Analytical method" = print_meta(x, "method_local"),
    "Model" = print_meta(x, "model")
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_local <- function(x) {
  c(
    "Local analysis" = "",
    "Sensory method" = print_meta(x, "sensory_method"),
    "Analytical method" = print_meta(x, "method_local"),
    "Model" = print_meta(x, "model")
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_liking <- function(x) {
  c(
    "Liking analysis" = "",
    "Sensory method" = print_meta(x, "sensory_method"),
    "Analytical method" = print_meta(x, "method_local"),
    "Model" = print_meta(x, "model")
  )
}

#' @importFrom tibble tbl_sum
#' @importFrom glue glue
#' @export
tbl_sum.tbl_sensory_preference_product <- function(x) {
  c(
    "Description of" = glue("Product <{print_meta(x, 'n_product')}>"),
    "Dimension" = print_meta(x, "dimension")
  )
}

#' @importFrom tibble tbl_sum
#' @importFrom glue glue
#' @export
tbl_sum.tbl_sensory_preference_panelist <- function(x) {
  c(
    "Description of" = glue("Panelist <{print_meta(x, 'n_panelist')}>"),
    "Dimension" = print_meta(x, "dimension")
  )
}

#' @importFrom glue glue
#' @export
print.tbl_sensory_preference <- function(x, ...) {
  cat_subtle(
    glue(
      "
    {pad('# Internal Preference Mapping:')}
    {pad('# Sensory method:')} {sensory_method}
    {pad('# Analytical method:')} {method_global}
    #
    
    ",
      sensory_method = print_meta(x, "sensory_method"),
      method_global = print_meta(x, "method_global")
    )
  )
  print(x$eigenvalue)
  cat_subtle("#\n")
  print(x$product)
  cat_subtle("#\n")
  print(x$panelist)
  
  invisible(x)
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_sensory_global_eigenvalue <- function(x) {
  c(
    "Description of" = "Eigenvalue",
    "Number of dimension" = attr(x, "n_dimension")
  )
}

#' @importFrom tibble tbl_sum
#' @importFrom glue glue
#' @export
tbl_sum.tbl_sensory_global_product <- function(x) {
  c(
    "Description of" = glue("Product <{print_meta(x, 'n_product')}>"),
    "Dimension" = print_meta(x, "dimension")
  )
}

#' @importFrom tibble tbl_sum
#' @importFrom glue glue
#' @export
tbl_sum.tbl_sensory_global_attribute <- function(x) {
  c(
    "Description of" = glue("Sensory attribute <{print_meta(x, 'n_attribute')}>"),
    "Dimension" = print_meta(x, "dimension")
  )
}

#' @importFrom glue glue
#' @export
print.tbl_sensory_global <- function(x, ...) {
  cat_subtle(
    glue(
      "
    {pad('# Global analysis:')}
    {pad('# Sensory method:')} {sensory_method}
    {pad('# Analytical method:')} {method_global}
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

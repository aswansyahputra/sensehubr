#' Liking analysis
#' 
#' Perform liking analysis on hedonic score or rating.
#' 
#' @param tbl_sensory a sensory table
#' 
#' @importFrom dplyr select group_by ungroup mutate arrange summarise
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble column_to_rownames
perform_liking_analysis <- function(tbl_sensory) {
  if (is.null(parse_meta(tbl_sensory, "hedonic"))) {
    stop("No hedonic data is available in sensory table", call. = FALSE)
  }
  
  meta_panelist <- parse_meta(tbl_sensory, "panelist")
  meta_product <- parse_meta(tbl_sensory, "product")
  meta_session <- parse_meta(tbl_sensory, "session")
  meta_pres_order <- parse_meta(tbl_sensory, "pres_order")
  meta_hedonic <- parse_meta(tbl_sensory, "hedonic")
  
  if (!is.null(meta_session)) {
    if (!is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist + session + panelist:product + panelist:session + product:session + pres_order"
    } else if (is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist + session + panelist:product + panelist:session + product:session"
    }
  } else if (is.null(meta_session)) {
    if (!is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist + pres_order"
    } else if (is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist"
    }
  }
  
  res_liking_local <- tbl_sensory %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      session = meta_session,
      pres_order = meta_pres_order,
      meta_hedonic
    ) %>%
    gather("attribute", "value", meta_hedonic) %>%
    group_by(attribute) %>%
    nest() %>%
    mutate(
      model = map(
        data,
        ~ aov(as.formula(fmla),
              data = .x
        )
      ),
      stats = map(
        model,
        ~ anova(.x) %>%
          tidy() %>%
          filter(term == "product") %>%
          select(statistic, p.value)
      ),
      values = map(
        data,
        ~ group_by(.x, product) %>%
          summarise(value = mean(value, na.rm = TRUE)) %>%
          spread(product, value)
      )
    ) %>%
    select(attribute, stats, values) %>%
    unnest(stats, values) %>%
    arrange(desc(statistic))
  
  res_liking_global <- tbl_sensory %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      liking = meta_hedonic
    ) %>% 
    group_by(panelist, product) %>% 
    summarise(liking = mean(liking)) %>% 
    ungroup() %>% 
    spread(product, liking) %>% 
    as.data.frame() %>% 
    column_to_rownames("panelist") %>% 
    PCA(graph = FALSE)
  
  res <- list(
    tbl_liking = res_liking_local,
    tbl_space = inspect_space(res_liking_global),
    tbl_product = inspect_product_liking(res_liking_global),
    tbl_panelist = inspect_panelist_liking(res_liking_global),
    res_liking_global = res_liking_global
  )
  
  attr(res, "sensory_method") <- parse_meta(tbl_sensory, "sensory_method")
  attr(res, "method_local") <- "Analysis of variance"
  attr(res, "method_global") <- "Principal Component Analysis"
  attr(res, "n_product") <- parse_meta(tbl_sensory, "n_product")
  attr(res, "n_panelist") <- parse_meta(tbl_sensory, "n_panelist")
  attr(res, "hedonic") <- parse_meta(tbl_sensory, "hedonic")
  class(res) <- append(class(res), "tbl_sensory_liking")
    
  
  return(res)
}



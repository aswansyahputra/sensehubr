#' @importFrom pillar style_subtle
#' @importFrom glue glue
cat_subtle <- function(...) {
  cat(style_subtle(glue(..., "\n")))
}

#' @importFrom stringr str_pad
#' @importFrom glue glue
pad <- function(...) {
  str_pad(glue(...), side = "right", width = 25)
}


#' @importFrom rlang arg_match
parse_meta <- function(x, meta = c("sensory_method", "method_local", "model", "method_global", "panelist", "n_panelist", "product", "n_product", "session", "pres_order", "attribute", "n_attribute", "hedonic")) {
  meta <- arg_match(meta)

  res <- attr(x, meta)
  if (length(res) == 1 && res == "NULL") {
    res <- NULL
  }
  return(res)
}

#' @importFrom rlang arg_match
#' @importFrom glue glue glue_collapse
print_meta <- function(x, meta = c("dimension", "sensory_method", "panel_model", "panelist_model", "method_local", "model", "method_global", "panelist", "n_panelist", "product", "n_product", "attribute", "n_attribute", "hedonic")) {
  meta <- arg_match(meta)

  if (meta[[1]] == "dimension") {
    res <- glue("<{NROW(x)} x {NCOL(x)}>")
    if (any(class(x) %in% c("tbl_sensory_global_product", "tbl_sensory_global_attribute"))) {
      res <- glue("Dim {attr(x, 'dimension')[[1]]} x {attr(x, 'dimension')[[2]]}")
    }
  }

  if (meta[[1]] == "sensory_method") {
    res <- switch(parse_meta(x, "sensory_method"),
      "QDA" = "Quantitative Descriptive Analysis",
      "CATA" = "Check-all-that-Apply",
      "RATA" = "Rate-all-that-Apply",
      "FCP" = "Free Choice Profiling",
      "FP" = "Flash Profiling",
      "JAR" = "Just-about-Right",
      "IPM" = "Ideal Profile Method"
    )
  }

  if (meta[[1]] == "panel_model") {
    res <- switch(attr(x, "panel_model"),
      "~ product + panelist" = "Attribute ~ Product + Panelist",
      "~ product + panelist + pres_order" = "Attribute ~ Product + Panelist + Presentation Order",
      "~ product + panelist + session + panelist:product + panelist:session + product:session" = "Attribute ~ Product + Panelist + Session + PanelistXProduct + PanelistXSession + ProductXSession",
      "~ product + panelist + session + panelist:product + panelist:session + product:session + pres_order" = "Attribute ~ Product + Panelist + Session + PanelistXProduct + PanelistXSession + ProductXSession + Presentation Order"
    )
  }

  if (meta[[1]] == "panelist_model") {
    res <- switch(attr(x, "panelist_model"),
      "~ product" = "Attribute ~ Product",
      "~ product + pres_order" = "Attribute ~ Product + Presentation Order",
      "~ product + session" = "Attribute ~ Product + Session",
      "~ product + session + pres_order" = "Attribute ~ Product + Session + Presentation Order"
    )
  }



  if (meta[[1]] == "method_local") {
    res <- parse_meta(x, "method_local")
  }

  if (meta[[1]] == "model") {
    res <- switch(attr(x, "model"),
      "value ~ product + panelist" = "Attribute ~ Product + Panelist",
      "value ~ product + panelist + pres_order" = "Attribute ~ Product + Panelist + Presentation Order",
      "value ~ product + panelist + session + panelist:product + panelist:session + product:session" = "Attribute ~ Product + Panelist + Session + PanelistXProduct + PanelistXSession + ProductXSession",
      "value ~ product + panelist + session + panelist:product + panelist:session + product:session + pres_order" = "Attribute ~ Product + Panelist + Session + PanelistXProduct + PanelistXSession + ProductXSession + Presentation Order",

      "value ~ product | panelist" = "Attribute ~ Product | Panelist"
    )
  }

  if (meta[[1]] == "method_global") {
    res <- parse_meta(x, "method_global")
  }

  if (meta[[1]] == "panelist") {
    res <- glue("{parse_meta(x, 'panelist')} <{parse_meta(x, 'n_panelist')} subjects>")
  }

  if (meta[[1]] == "n_panelist") {
    res <- glue("{parse_meta(x, 'n_panelist')} subjects")
  }

  if (meta[[1]] == "product") {
    res <- glue("{parse_meta(x, 'product')} <{parse_meta(x, 'n_product')} items>")
  }

  if (meta[[1]] == "n_product") {
    res <- glue("{parse_meta(x, 'n_product')} items")
  }

  if (meta[[1]] == "attribute") {
    res <- glue(glue_collapse(glue("{parse_meta(x, 'attribute')}"), sep = ", ", width = 35), "<{parse_meta(x, 'n_attribute')} lexicons>")
  }

  if (meta[[1]] == "n_attribute") {
    res <- glue("{parse_meta(x, 'n_attribute')} lexicons")
  }
  if (meta[[1]] == "hedonic") {
    res <- parse_meta(x, "hedonic")
    if (is.null(res)) {
      res <- "None"
    }
  }
  return(res)
}

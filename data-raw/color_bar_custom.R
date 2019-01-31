color_bar_custom <- function(bg.color = "lightgray", text.col = "black", fun = "proportion", ...) {
  fun <- match.fun(fun)
  formatter("span", style = function(x) formattable::style(
      display = "inline-block",
      direction = "rtl", "border-radius" = "4px", "padding-right" = "2px",
      "background-color" = csscolor(bg.color), color = csscolor(text.col), width = percent(fun(
        as.numeric(x),
        ...
      ))
    ))
}

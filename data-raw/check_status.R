check_status <- Vectorize(
  function(x, ref) {
    if (x == 0 && ref == 0) {
      "P(No)|(No)"
    } else if (x == 0 && ref == 1) {
      "P(No)|(Yes)"
    } else if (x == 1 && ref == 0) {
      "P(Yes)|(No)"
    } else if (x == 1 && ref == 1) {
      "P(Yes)|(Yes)"
    }
  } 
)
mh_distance <- function(x, y) {
  #Check NA, NaN, Inf, -Inf
  if (is.na(x) | is.na(y) | is.nan(x) | is.nan(y) | is.infinite(x)
      | is.infinite(y)) {
    warning("x or y is NA, NaN, Inf, or -Inf")
    return(-1)
  }
  #Check logical, numeric, character
  if (typeof(x) != typeof(y)) {
    warning("x and y are different types")
    return(-1)
  }
  if (!is.logical(x) & !is.numeric(x) & !is.character(x)) {
    warning("x is not logical, numeric, or character")
    return(-1)
  }
  if (!is.logical(y) & !is.numeric(y) & !is.character(y)) {
    warning("y is not logical, numeric, or character")
    return(-1)
  }
  #Check decimals
  if (is.numeric(x) && (round(x) != x)) {
    warning("x and/or y is a decimal")
    return(-1)
  }
  if (is.numeric(y) && (round(y) != y)) {
    warning("y is a decimal")
    return(-1)
  }
  #Check lengths
  if (is.numeric(x) & is.numeric(y)) {
    if (nchar(abs(x)) != nchar(abs(y))) {
      warning("x and y have different lengths")
      return(-1)
    }
  }
  if (is.character(x) & is.character(y)) {
    if (nchar(x) != nchar(y)) {
      warning("x and y have different lengths")
      return(-1)
    }
  }
  #Return logicals
  if (is.logical(x) & is.logical(y)) {
    if (x != y) {
      return(1)
    }
    else {
      return(0)
    }
  }
  #Return numeric and characters
  if (is.numeric(x) & is.numeric(y)) {
    a <- strsplit(as.character(abs(x)), split = "")[[1]]
    b <- strsplit(as.character(abs(y)), split = "")[[1]]
  }
  if (is.character(x) & is.character(y)) {
    a <- strsplit(as.character(x), split = "")[[1]]
    b <- strsplit(as.character(y), split = "")[[1]]
  }
  hamming <- 0
  for (i in 1:length(a)) {
    if (a[i] != b[i]) {
      hamming <- hamming + 1
    }
  }
  hamming
}
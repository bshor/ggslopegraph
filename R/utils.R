justifyme <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    first <- tolower(substr(x, 1L, 1L))
    if (first == "l") {
      return(0)
    }
    if (first == "c") {
      return(0.5)
    }
    if (first == "r") {
      return(1)
    }
  }

  if (is.numeric(x) && length(x) == 1L && x >= 0 && x <= 1) {
    return(x)
  }

  0.5
}

require_suggested <- function(package, theme_choice) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "ThemeChoice = \"", theme_choice, "\" requires the ",
      package, " package to be installed.",
      call. = FALSE
    )
  }
}

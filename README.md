# ggslopegraph

`ggslopegraph` is a small extraction of
`CGPfunctions::newggslopegraph()` as a standalone function for drawing
Tufte-style slopegraphs with `ggplot2`.

## Installation

```r
install.packages("devtools")
devtools::install_github("bshor/ggslopegraph")
```

## Example

```r
library(ggslopegraph)

example <- data.frame(
  year = ordered(rep(c("2020", "2024"), 3), levels = c("2020", "2024")),
  value = c(10, 14, 8, 7, 15, 18),
  group = rep(c("A", "B", "C"), each = 2)
)

ggslopegraph(
  example,
  year,
  value,
  group,
  Title = "Example slopegraph",
  SubTitle = NULL,
  Caption = NULL
)
```

## Credits

This package traces back to Thomas J. Leeper's
[`slopegraph`](https://github.com/leeper/slopegraph) package. The current
function is adapted from Chuck Powell's
[`CGPfunctions::newggslopegraph()`](https://github.com/ibecav/CGPfunctions).

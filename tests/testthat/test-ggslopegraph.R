test_that("ggslopegraph returns a ggplot", {
  dat <- data.frame(
    time = ordered(rep(c("Before", "After"), 3), levels = c("Before", "After")),
    value = c(10, 14, 8, 7, 15, 18),
    group = rep(c("A", "B", "C"), each = 2)
  )

  p <- ggslopegraph(dat, time, value, group, Title = NULL, SubTitle = NULL, Caption = NULL)

  expect_s3_class(p, "ggplot")
})

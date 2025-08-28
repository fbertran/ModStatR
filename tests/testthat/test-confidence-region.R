test_that("my.confidence.region runs without error on a simple lm", {
  skip_on_cran()
  set.seed(99)
  d <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  g <- lm(y ~ x1 + x2, data=d)
  # Use a null PDF device to avoid creating files
  pdf(file=NULL)
  expect_silent(ModStatR::my.confidence.region(g, a=2, b=3, which=0, col="pink"))
  dev.off()
})
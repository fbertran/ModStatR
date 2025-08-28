test_that("rho matches cor() on indexed subset", {
  set.seed(42)
  x <- rnorm(50)
  y <- x*0.6 + rnorm(50, sd=0.8)
  idx <- sample(1:50, 30, replace=FALSE)
  got <- ModStatR::rho(x,y,indices=idx)
  exp <- cor(x[idx], y[idx], method="pearson")
  expect_equal(as.numeric(got), as.numeric(exp), tolerance=1e-12)
})

test_that("rho.mult returns upper-triangle correlations in order", {
  set.seed(123)
  mat <- matrix(rnorm(5*30), ncol=5)
  idx <- sample(1:nrow(mat), 25)
  v <- ModStatR::rho.mult(mat, indices=idx)
  # Expected length is choose(p,2)
  expect_length(v, choose(ncol(mat), 2))
  # Check first pair equals cor of columns 1 and 2
  exp12 <- cor(mat[idx,1], mat[idx,2], use="pairwise.complete.obs")
  expect_equal(as.numeric(v[1]), as.numeric(exp12), tolerance=1e-12)
})
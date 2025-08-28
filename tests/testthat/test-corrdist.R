test_that("corrdist integrates to ~1 on [-1,1]", {
  for(r0 in c(0, 0.5, -0.3)){
    for(n in c(10, 20, 50)){
      f <- function(r) ModStatR::corrdist(r, rho_0=r0, n=n)
      val <- integrate(f, lower=-1, upper=1)$value
      expect_equal(val, 1, tolerance = 1e-3)
    }
  }
})

test_that("approximations are close to exact density", {
  set.seed(123)
  grid <- seq(-0.9, 0.9, length.out = 13)
  for(r0 in c(0, 0.4)){
    n <- 30
    exact <- sapply(grid, function(r) ModStatR::corrdist(r, r0, n))
    approx1 <- sapply(grid, function(r) ModStatR::corrdistapprox(r, r0, n))
    approx2 <- sapply(grid, function(r) ModStatR::corrdistapprox2(r, r0, n))
    expect_equal(approx1, exact, tolerance = 5e-2)
    expect_equal(approx2, exact, tolerance = 5e-2)
  }
})
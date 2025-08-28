test_that("boot.mcor.ic returns 4 CI matrices with correct shapes", {
  skip_if_not_installed("boot")
  set.seed(7)
  n <- 60; p <-4
  mat <- matrix(rnorm(n*p), ncol=p)
  # Statistic: vectorize upper-tri correlations
  stat_fun <- function(data, idx){
    d <- data[idx, , drop=FALSE]
    as.vector(cor(d, method="pearson", use="pairwise.complete.obs"))[outer(1:ncol(d),1:ncol(d),">")]
  }
  boot_res <- boot::boot(data=mat, statistic=stat_fun, R=99)
  out <- ModStatR::boot.mcor.ic(mat=mat, boot.mcor.res=boot_res, conflevel=0.90)
  expect_true(all(dim(out$cor.ic.percentile.low) == c(p,p)))
  expect_true(all(dim(out$cor.ic.percentile.up) == c(p,p)))
  expect_true(all(dim(out$cor.ic.BCa.low) == c(p,p)))
  expect_true(all(dim(out$cor.ic.BCa.up) == c(p,p)))
  expect_equal(diag(out$cor.ic.percentile.low), rep(1, p))
  expect_equal(diag(out$cor.ic.percentile.up), rep(1, p))
  expect_equal(diag(out$cor.ic.BCa.low), rep(1, p))
  expect_equal(diag(out$cor.ic.BCa.up), rep(1, p))
})

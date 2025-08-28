test_that("Gauss2F1 and Gauss2F1gsl agree on typical inputs", {
  skip_if_not_installed("gsl")
  skip_if_not_installed("hypergeo")
  vals <- list(
    list(a=0.5,b=0.5,c=19,x=1-0.75^2),
    list(a=0.5,b=0.5,c=1.5,x=0.3),
    list(a=1.2,b=0.7,c=2.5,x=0.6)
  )
  for(v in vals){
    r1 <- ModStatR::Gauss2F1(v$a, v$b, v$c, v$x)
    r2 <- ModStatR::Gauss2F1gsl(v$a, v$b, v$c, v$x)
    expect_true(is.finite(r1))
    expect_true(is.finite(r2))
    expect_equal(r1, r2, tolerance = 1e-5)
  }
})

test_that("Gauss2F1(a,b;c;0) = 1", {
  expect_equal(ModStatR::Gauss2F1(0.3, 0.8, 1.7, 0), 1, tolerance=1e-12)
})
test_that("p-value = 1 when corobs == rho0", {
  for(n in c(10, 25, 50)){
    expect_equal(ModStatR::ref.cor.test(corobs=0.2, rho_0=0.2, n=n), 1, tolerance=1e-10)
  }
})

test_that("p-value decreases as |corobs - rho0| increases", {
  n <- 40; r0 <- 0.1
  p1 <- ModStatR::ref.cor.test(corobs=r0 + 0.05, rho_0=r0, n=n)
  p2 <- ModStatR::ref.cor.test(corobs=r0 + 0.20, rho_0=r0, n=n)
  expect_true(p2 < p1)
})
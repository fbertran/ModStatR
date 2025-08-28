# Run coverage locally
if (requireNamespace("covr", quietly = TRUE)) {
  covres <- covr::package_coverage()
  print(covres)
  covr::report(covres, file = "coverage.html")
} else {
  message("Package 'covr' not available. Install with install.packages('covr')")
}

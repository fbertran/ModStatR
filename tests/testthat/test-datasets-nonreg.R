md5_expect <- function(filename, expected_md5) {
  path <- system.file("data", filename, package = "ModStatR")
  expect_true(file.exists(path), info = paste("Missing data file:", filename))
  cur <- as.character(tools::md5sum(path))
  expect_equal(unname(cur), expected_md5, info = filename)
}

test_that("CancerSein structure and summaries", {
  data("CancerSein", package="ModStatR")
  expect_equal(nrow(CancerSein), 62)
  expect_equal(ncol(CancerSein), 3)
  expect_true(is.factor(CancerSein$Traitement))
  expect_equal(nlevels(CancerSein$Traitement), 3)
  expect_true(is.numeric(CancerSein$Age))
  expect_true(is.numeric(CancerSein$Survie))
})

test_that("SidaChat structure and summaries", {
  data("SidaChat", package="ModStatR")
  expect_equal(nrow(SidaChat), 32)
  expect_equal(ncol(SidaChat), 3)
  expect_true(is.factor(SidaChat$Sexe))
  expect_equal(nlevels(SidaChat$Sexe), 2)
  expect_true(is.integer(SidaChat$Jours) || is.numeric(SidaChat$Jours))
  expect_true(is.numeric(SidaChat$LnT4))
})

test_that("chal structure", {
  data("chal", package="ModStatR")
  expect_equal(nrow(chal), 24)
  expect_equal(ncol(chal), 2)
  expect_true(all(c("Temperature","Defaillance") %in% names(chal)))
  expect_true(is.numeric(chal$Temperature))
  expect_true(is.numeric(chal$Defaillance) || is.integer(chal$Defaillance))
})

test_that("polypes structure and levels", {
  data("polypes", package="ModStatR")
  expect_equal(nrow(polypes), 20)
  expect_equal(ncol(polypes), 3)
  expect_true(is.factor(polypes$traitement))
  # levels are expected to include 'placebo' and 'medicament'
  if (!is.null(levels(polypes$traitement))) {
    expect_true(all(c("placebo","medicament") %in% levels(polypes$traitement)))
  }
  # counts are non-negative
  expect_true(all(polypes$nombre >= 0, na.rm=TRUE))
})

test_that("vitamines structure and levels", {
  data("vitamines", package="ModStatR")
  expect_equal(nrow(vitamines), 32)
  expect_equal(ncol(vitamines), 3)
  expect_true(all(sort(unique(vitamines$Calorie)) %in% c(1,2)))
  expect_true(all(sort(unique(vitamines$Vitamine)) %in% c(1,2)))
  expect_true(is.numeric(vitamines$Poids) || is.integer(vitamines$Poids))
})

test_that("d_hotels structure", {
  data("d_hotels", package="ModStatR")
  expect_equal(nrow(d_hotels), 39)
  expect_equal(ncol(d_hotels), 9)
  expect_true(is.factor(d_hotels$NOM))
  expect_true(is.factor(d_hotels$PAYS))
  expect_equal(nlevels(d_hotels$PAYS), 5)
  expect_true(is.numeric(d_hotels$PRIX) || is.integer(d_hotels$PRIX))
  expect_true(all(d_hotels$PRIX >= 0, na.rm=TRUE))
})

test_that("ecole2 structure", {
  data("ecole2", package="ModStatR")
  expect_equal(nrow(ecole2), 119)
  expect_equal(ncol(ecole2), 2)
  expect_true(all(c("Maths","Sport") %in% names(ecole2)))
  expect_true(is.numeric(ecole2$Maths))
  expect_true(is.numeric(ecole2$Sport))
})

test_that("d_vac structure and non-negativity", {
  data("d_vac", package="ModStatR")
  expect_equal(nrow(d_vac), 8)
  expect_equal(ncol(d_vac), 8)
  expect_true(all(unlist(lapply(d_vac, function(x) all(x >= 0)))))
})
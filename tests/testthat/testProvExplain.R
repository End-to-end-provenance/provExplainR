library(provExplainR)
library(testthat)

context("provExplain() - main method exposed to user")

source("initTest.R")

# provenance directory paths for testing
old.prov.dir <- get.test.prov.dirs ("prov_HF-data_2019-06-10T15.32.25EDT")
new.prov.dir <- get.test.prov.dirs ("prov_HF-data")

test_that("warning is shown if two directories are the same", {
	expect_warning(escape.value1 <- prov.explain(olderProv.dir = old.prov.dir, newerProv.dir = old.prov.dir))
	expect_equal(escape.value1, NA)
	expect_warning(escape.value2 <- prov.explain(olderProv.dir = new.prov.dir, newerProv.dir = new.prov.dir))
	expect_equal(escape.value2, NA)
})

# test_that("environment variables are correctly set", {
	
# })


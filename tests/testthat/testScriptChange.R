library(provExplainR)
library(testthat)

context("Finding Script Changes")

source("initTest.R")

# provenance directory paths for testing
# old.prov.dir <- get.test.prov.dirs ("prov_HF-data_2019-06-10T15.32.25EDT")
# new.prov.dir <- get.test.prov.dirs ("prov_HF-data")
test.prov.dir <- get.test.prov.dirs("prov_test1")

# ProvInfo objects
# old.prov.info <- get.test.prov.info ("prov_HF-data_2019-06-10T15.32.25EDT")
# new.prov.info <- get.test.prov.info ("prov_HF-data")
test.prov.info <- get.test.prov.info("prov_test1")

# script data frames for each provenance
# old.origin.scripts.df <- provParseR::get.scripts(old.prov.info)
# new.origin.scripts.df <- provParseR::get.scripts(new.prov.info)
test.origin.scripts.df <- provParseR::get.scripts(test.prov.info)

expected.script.df <- data.frame(script = c("/Users/khanhl.ngo/HarvardForest/provExplainR-dev/inst/testdata/prov_test1/scripts/test1.R",
		"/Users/khanhl.ngo/HarvardForest/provExplainR-dev/inst/testdata/prov_test1/scripts/test2.R"),
		timestamp = c("2019-07-22T11.20.52EDT", "2019-07-22T11.18.37EDT"), stringsAsFactors = FALSE)

test_that("correctly extracts script paths relatively to provenance folders", {
	actual.test.df <- get.copied.script.path(prov.dir = test.prov.dir,origin.script.df = test.origin.scripts.df)
	expect_equivalent(actual.test.df, expected.script.df)
})

test_that("correctly computes hash values for each script", {
	actual.script.df <- expected.script.df
	expected.script.df$hashValue <- c("7be48a56beba80e814c1f57887b3dba1", "d24ab89249763832d467b7d36ce7e6db")
	actual.script.df <- compute.script.hash.value(script.df = actual.script.df)
	expect_equivalent(actual.script.df, expected.script.df)
})

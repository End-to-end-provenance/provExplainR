library(provExplainR)
library(testthat)

context("Finding Script Changes")

source("initTest.R")

# START PREPARING TEST DATA
################################################################################################

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

# case 0: different content, same name
old.script.case0.df <- data.frame(script = c("/Users/khanhl.ngo/oldProv/MainScript.R", "/Users/khanhl.ngo/newProv/SourcedScript1.R"),
	timestamp = c("2019-07-22T11.20.52EDT", "2019-07-22T11.18.37EDT"),
	hashValue = c("7be48a56beba80e814c1f57887b3dba1", "e14ab89249763832d467b7d36ce7e6db"), stringsAsFactors = FALSE)
new.script.case0.df <- data.frame(script = c("/Users/khanhl.ngo/oldProv/MainScript.R", "/Users/khanhl.ngo/newProv/SourcedScript1.R"),
	timestamp = c("2019-07-22T12.20.52EDT", "2019-07-22T11.20.37EDT"),
	hashValue = c("8ce48a56beba80e814c1f57887b3dba1", "d24ab89249763832d467b7d36ce7e6db"), stringsAsFactors = FALSE)

# case 1: different content, different name
old.script.case1.df <- old.script.case0.df
new.script.case1.df <- new.script.case0.df
new.script.case1.df$script[1] = "/Users/khanhl.ngo/oldProv/MainRenamedScript.R"

# case 2: same content, different name
old.script.case2.df <- old.script.case1.df
new.script.case2.df <- new.script.case1.df
new.script.case2.df$hashValue[1] = old.script.case2.df$hashValue[1] 

# case 3: same content, same name
old.script.case3.df <- old.script.case2.df
new.script.case3.df <- new.script.case2.df
new.script.case3.df$script[1] = old.script.case3.df$script[1]

# FINISH PREPARING TEST DATA
################################################################################################

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

test_that("compares main script and returns corresponding integer values", {
	expect_equal(compare.main.script(olderProv.main.script.df = old.script.case0.df[1, ], newerProv.main.script.df = new.script.case0.df[1, ]), 0)
	expect_equal(compare.main.script(olderProv.main.script.df = old.script.case1.df[1, ], newerProv.main.script.df = new.script.case1.df[1, ]), 1)
	expect_equal(compare.main.script(olderProv.main.script.df = old.script.case2.df[1, ], newerProv.main.script.df = new.script.case2.df[1, ]), 2)
	expect_equal(compare.main.script(olderProv.main.script.df = old.script.case3.df[1, ], newerProv.main.script.df = new.script.case3.df[1, ]), 3)
})



#' This test file tests behavior of all functions related to detecting 
#' script changes based on the collected provenance.
#' @author Khanh Ngo

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
old.script.case0.df <- data.frame(script = c("/Users/khanhl.ngo/oldProv/MainScript.R", "/Users/khanhl.ngo/oldProv/SourcedScript1.R"),
	timestamp = c("2019-07-22T11.20.52EDT", "2019-07-22T11.18.37EDT"),
	hashValue = c("7be48a56beba80e814c1f57887b3dba1", "e14ab89249763832d467b7d36ce7e6db"), stringsAsFactors = FALSE)
new.script.case0.df <- data.frame(script = c("/Users/khanhl.ngo/newProv/MainScript.R", "/Users/khanhl.ngo/newProv/SourcedScript1.R"),
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

# test_that("correctly extracts script paths relatively to provenance folders", {
# 	actual.test.df <- get.copied.script.path(prov.dir = test.prov.dir, origin.script.df = test.origin.scripts.df)
# 	expect_equivalent(actual.test.df, expected.script.df)
# })

test_that("correctly computes hash values for each script", {
	actual.script.df <- expected.script.df
	expected.script.df$hashValue <- c("7be48a56beba80e814c1f57887b3dba1", "d24ab89249763832d467b7d36ce7e6db")
	actual.script.df <- compute.script.hash.value(script.df = actual.script.df)
	expect_equivalent(actual.script.df, expected.script.df)
})

test_that("compares main script and returns corresponding status", {
	expect_equal(compare.main.script(olderProv.main.script.df = old.script.case0.df[1, ], newerProv.main.script.df = new.script.case0.df[1, ]), 0)
	expect_equal(compare.main.script(olderProv.main.script.df = old.script.case1.df[1, ], newerProv.main.script.df = new.script.case1.df[1, ]), 1)
	expect_equal(compare.main.script(olderProv.main.script.df = old.script.case2.df[1, ], newerProv.main.script.df = new.script.case2.df[1, ]), 2)
	expect_equal(compare.main.script(olderProv.main.script.df = old.script.case3.df[1, ], newerProv.main.script.df = new.script.case3.df[1, ]), 3)
})

test_that("displays main script changes: case 0 - different content, same name", {
	actual.message <- capture_output(print.main.script.change(main.script.change.result = 0, olderProv.main.script.df = old.script.case0.df[1, ],newerProv.main.script.df = new.script.case0.df[1, ]))
	expected.message <- "\nThe content of the main script MainScript.R has changed\n### Old script MainScript.R was last modified at: 2019-07-22T11.20.52EDT\n### New script MainScript.R was last modified at: 2019-07-22T12.20.52EDT"
	expect_equal(actual.message, expected.message)
})

test_that("displays main script changes: case 1 - different content, different name", {
	actual.message <- capture_output(print.main.script.change(main.script.change.result = 1, olderProv.main.script.df = old.script.case1.df[1, ], newerProv.main.script.df = new.script.case1.df[1, ]))
	expected.message <- "\nMain script has been renamed from MainScript.R to MainRenamedScript.R\nThe content of the main script has changed\n### Old script MainScript.R was last modified at: 2019-07-22T11.20.52EDT\n### New script MainRenamedScript.R was last modified at: 2019-07-22T12.20.52EDT"
	expect_equal(actual.message, expected.message)
})

test_that("displays main script changes: case 2 - same content, different name", {
	actual.message <- capture_output(print.main.script.change(main.script.change.result = 2, olderProv.main.script.df = old.script.case2.df[1, ], newerProv.main.script.df = new.script.case2.df[1, ]))
	expected.message <- "\nMain script has been renamed from MainScript.R to MainRenamedScript.R\nNo change detected in main script\n### Old script MainScript.R was last modified at: 2019-07-22T11.20.52EDT\n### New script MainRenamedScript.R was last modified at: 2019-07-22T12.20.52EDT"
	expect_equal(actual.message, expected.message)
})

test_that("displays main script changes: case 3 - same content, same name", {
	actual.message <- capture_output(print.main.script.change(main.script.change.result = 3, olderProv.main.script.df = old.script.case3.df[1, ], newerProv.main.script.df = new.script.case3.df[1, ]))
	expected.message <- "\nNo change detected in main script MainScript.R\n### Old script MainScript.R was last modified at: 2019-07-22T11.20.52EDT\n### New script MainScript.R was last modified at: 2019-07-22T12.20.52EDT"
	expect_equal(actual.message, expected.message)
})

test_that("compares sourced scripts: both old and new data frames are empty", {
	no.sourced.script <- old.script.case0.df
	no.sourced.script <- no.sourced.script[-2, ] # remove the only sourced script 

	sourced.script.change.list <- compare.sourced.scripts(no.sourced.script[-1, ], no.sourced.script[-1, ])
	expect_equal(nrow(sourced.script.change.list[[1]]), 0)
	expect_equal(nrow(sourced.script.change.list[[2]]), 0)
	expect_equal(nrow(sourced.script.change.list[[3]]), 0)
	expect_equal(nrow(sourced.script.change.list[[4]]), 0)
})

test_that("compares sourced script: old data frame is empty", {
	no.old.sourced.script <- old.script.case0.df
	no.old.sourced.script <- no.old.sourced.script[-2, ] # remove the only sourced script
	expected.unmatched.new.script.df <- data.frame(script = c("SourcedScript1.R"), 
		timestamp = c("2019-07-22T11.20.37EDT"),
		hashValue = c("d24ab89249763832d467b7d36ce7e6db"), stringsAsFactors = FALSE)

	sourced.script.change.list <- compare.sourced.scripts(no.old.sourced.script[-1, ], new.script.case0.df[-1, ])
	expect_equal(nrow(sourced.script.change.list[[1]]), 0)
	expect_equal(nrow(sourced.script.change.list[[2]]), 0)
	expect_equal(nrow(sourced.script.change.list[[3]]), 0)
	expect_equal(sourced.script.change.list[[4]], expected.unmatched.new.script.df)
})

test_that("compares sourced script: new data frame is empty", {
	no.new.sourced.script <- new.script.case0.df
	no.new.sourced.script <- no.new.sourced.script[-2, ] # remove the only sourced script
	expected.unmatched.old.script.df <- data.frame(script = c("SourcedScript1.R"), 
		timestamp = c("2019-07-22T11.18.37EDT"),
		hashValue = c("e14ab89249763832d467b7d36ce7e6db"), stringsAsFactors = FALSE)

	sourced.script.change.list <- compare.sourced.scripts(old.script.case0.df[-1, ], no.new.sourced.script[-1, ])
	expect_equal(nrow(sourced.script.change.list[[1]]), 0)
	expect_equal(nrow(sourced.script.change.list[[2]]), 0)
	expect_equal(sourced.script.change.list[[3]], expected.unmatched.old.script.df)
	expect_equal(nrow(sourced.script.change.list[[4]]), 0)
})

test_that("compares sourced script: four cases are non-empty", {
	multiple.old.sourced.script.df <- old.script.case0.df
	old.extension.df <- data.frame(script = c("/Users/khanhl.ngo/oldProv/SourcedScript2.R", 
										"/Users/khanhl.ngo/oldProv/SourcedScript3.R", 
										"/Users/khanhl.ngo/oldProv/SourcedScript5.R"),
							timestamp = c("2019-07-22T10.10.37EDT", 
										"2019-07-22T09.09.37EDT",
										"2019-07-22T12.00.37EDT"),
							hashValue = c("a00ab89249763832d467b7d36ce7e6db", 
										"b01ab89249763832d467b7d36ce7e6db",
										"c02ab89249763832d467b7d36ce7e6db"), 
							stringsAsFactors = FALSE)

	multiple.old.sourced.script.df <- rbind(multiple.old.sourced.script.df, old.extension.df)

	multiple.new.sourced.script.df <- new.script.case0.df	
	new.extension.df <- data.frame(script = c("/Users/khanhl.ngo/newProv/SourcedScript2.R", 
										"/Users/khanhl.ngo/newProv/SourcedScript4.R", 
										"/Users/khanhl.ngo/newProv/SourcedScript6.R"),
							timestamp = c("2019-07-22T12.12.37EDT", 
										"2019-07-22T13.01.37EDT",
										"2019-07-22T09.00.37EDT"),
							hashValue = c("a00ab89249763832d467b7d36ce7e6db", 
										"b01ab89249763832d467b7d36ce7e6db",
										"d00ab89249763832d467b7d36ce7e6db"), 
							stringsAsFactors = FALSE)
	multiple.new.sourced.script.df <- rbind(multiple.new.sourced.script.df, new.extension.df)

	expected.first.df <- data.frame(script = c("SourcedScript1.R", "SourcedScript2.R"),
							old.timestamp = c("2019-07-22T11.18.37EDT", "2019-07-22T10.10.37EDT"),
							old.hashValue = c("e14ab89249763832d467b7d36ce7e6db", "a00ab89249763832d467b7d36ce7e6db"),
							new.timestamp = c("2019-07-22T11.20.37EDT", "2019-07-22T12.12.37EDT"), 
							new.hashValue = c("d24ab89249763832d467b7d36ce7e6db", "a00ab89249763832d467b7d36ce7e6db"),
							stringsAsFactors = FALSE)
	expected.second.df <- data.frame(old.script = c("SourcedScript3.R"),
							old.timestamp = c("2019-07-22T09.09.37EDT"),
							hashValue = c("b01ab89249763832d467b7d36ce7e6db"),
							new.script = c("SourcedScript4.R"),
							new.timestamp = c("2019-07-22T13.01.37EDT"),
							stringsAsFactors = FALSE)
	expected.third.df <- data.frame(script = c("SourcedScript5.R"),
							timestamp = c("2019-07-22T12.00.37EDT"),
							hashValue = c("c02ab89249763832d467b7d36ce7e6db"),
							stringsAsFactors = FALSE)

	expected.fourth.df <- data.frame(script = c("SourcedScript6.R"),
							timestamp = c("2019-07-22T09.00.37EDT"),
							hashValue = c("d00ab89249763832d467b7d36ce7e6db"),
							stringsAsFactors = FALSE)

	sourced.script.change.list <- compare.sourced.scripts(multiple.old.sourced.script.df[-1, ], multiple.new.sourced.script.df[-1, ])
	expect_equivalent(sourced.script.change.list[[1]], expected.first.df)
	expect_equivalent(sourced.script.change.list[[2]], expected.second.df)
	expect_equivalent(sourced.script.change.list[[3]], expected.third.df)
	expect_equivalent(sourced.script.change.list[[4]], expected.fourth.df)
})

test_that("displays sourced scripts: same name", {
	# case: data frame is non-empty
	df <- data.frame(script = c("s1.R", "s2.R", "s3.R"),
					old.timestamp = c("12", "1", "2"), 
                 	old.hashValue = c("abc", "cde", "xyz"), 
                 	new.timestamp = c("12", "2", "4"), 
                 	new.hashValue = c("abc", "efg", "mno"), 
                 	stringsAsFactors = FALSE)
	actual.message <- capture_output(print.same.name.sourced.scripts(df))
	expected.message <- "\nSourced script s2.R has changed\n### Old version s2.R was last modified at: 1\n### New version s2.R was last modified at: 2"
	expected.message <- paste(expected.message, "\nSourced script s3.R has changed\n### Old version s3.R was last modified at: 2\n### New version s3.R was last modified at: 4\nNo change detected in sourced script s1.R", sep = "")
	expect_equal(actual.message, expected.message)
})

test_that("displays sourced scripts: renamed", {
	# case: data frame is non-empty
	df <- data.frame(old.script = c("s0.R", "s2.R"), 
					old.timestamp = c("12", "10"), 
					hashValue = c("abc", "xyz"),
					new.script = c("s1.R", "s3.R"),
					new.timestamp = c("1", "11"),
					stringsAsFactors = FALSE)
	actual.message <- capture_output(print.renamed.sourced.scripts(df))
	expected.message <- "\nSourced script s0.R has been renamed to s1.R\n### Old version s0.R was last modified at: 12\n### New version s1.R was last modified at: 1"
	expected.message <- paste(expected.message, "\nSourced script s2.R has been renamed to s3.R\n### Old version s2.R was last modified at: 10\n### New version s3.R was last modified at: 11", sep = "")
	expect_equal(actual.message, expected.message)
})

test_that("displays sourced scripts: unmatched", {
	df <- data.frame(script = c("s0.R", "s2.R"), 
					timestamp = c("12", "10"), 
					hashValue = c("abc", "xyz"),
					stringsAsFactors = FALSE)

	# case: non-empty old unmatched data frame
	actual.message1 <- capture_output(print.unmatched.sourced.scripts(status = "old", unmatched.script.df = df))
	expected.message1 <- "\nSourced script s0.R has been renamed or removed\n### s0.R was last modified at: 12"
	expected.message1 <- paste(expected.message1, "\nSourced script s2.R has been renamed or removed\n### s2.R was last modified at: 10", sep = "")
	expect_equal(actual.message1, expected.message1)

	# case: non-empty new unmatched data frame
	actual.message2 <- capture_output(print.unmatched.sourced.scripts(status = "new", unmatched.script.df = df))
	expected.message2 <- "\nSourced script s0.R has been renamed or added\n### s0.R was last modified at: 12"
	expected.message2 <- paste(expected.message2, "\nSourced script s2.R has been renamed or added\n### s2.R was last modified at: 10", sep = "")
	expect_equal(actual.message2, expected.message2)
})

test_that("checks if a script data frame is valid", {
	expect_warning(escape.value1 <- is.valid.script.df(aspect = "same-name scripts", script.df = NULL),
		regexp = paste("data frame of same-name scripts is NULL"))
	expect_equal(escape.value1, FALSE)

	test.df <- data.frame(name = c("hello"), value = c("world"), stringsAsFactors = FALSE)
	test.list <- list(test.df)
	expect_equal(typeof(test.list), "list")
	expect_warning(escape.value2 <- is.valid.script.df(aspect = "same-name scripts", script.df = test.list[1]), 
		regexp = paste("argument is not a data frame, aspect = same-name scripts\n"))
	expect_equal(escape.value2, FALSE)

	expect_equal(is.valid.script.df(aspect = "same-name scripts", script.df = test.df), TRUE)
})



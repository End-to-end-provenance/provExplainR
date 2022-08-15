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
test.prov.dir <- get.test.prov.dirs("prov_test1")

# ProvInfo objects
test.prov.info <- get.test.prov.info("prov_test1")

# script data frames for each provenance
test.origin.scripts.df <- provParseR::get.scripts(test.prov.info)

expected.script.df <- data.frame(script = c(paste0(test.prov.dir, "/scripts/test1.R"),
		paste0(test.prov.dir, "/scripts/test2.R")),
		timestamp = c("2019-07-22T11.20.52EDT", "2019-07-22T11.18.37EDT"), stringsAsFactors = FALSE)

# case 0: different content, same name
first.script.case0.df <- data.frame(script = c("/Users/khanhl.ngo/oldProv/MainScript.R", "/Users/khanhl.ngo/oldProv/SourcedScript1.R"),
	timestamp = c("2019-07-22T11.20.52EDT", "2019-07-22T11.18.37EDT"),
	hashValue = c("7be48a56beba80e814c1f57887b3dba1", "e14ab89249763832d467b7d36ce7e6db"), stringsAsFactors = FALSE)
second.script.case0.df <- data.frame(script = c("/Users/khanhl.ngo/newProv/MainScript.R", "/Users/khanhl.ngo/newProv/SourcedScript1.R"),
	timestamp = c("2019-07-22T12.20.52EDT", "2019-07-22T11.20.37EDT"),
	hashValue = c("8ce48a56beba80e814c1f57887b3dba1", "d24ab89249763832d467b7d36ce7e6db"), stringsAsFactors = FALSE)

# case 1: different content, different name
first.script.case1.df <- first.script.case0.df
second.script.case1.df <- second.script.case0.df
second.script.case1.df$script[1] = "/Users/khanhl.ngo/oldProv/MainRenamedScript.R"

# case 2: same content, different name
first.script.case2.df <- first.script.case1.df
second.script.case2.df <- second.script.case1.df
second.script.case2.df$hashValue[1] = first.script.case2.df$hashValue[1] 

# case 3: same content, same name
first.script.case3.df <- first.script.case2.df
second.script.case3.df <- second.script.case2.df
second.script.case3.df$script[1] = first.script.case3.df$script[1]

# FINISH PREPARING TEST DATA
################################################################################################

# test_that("correctly extracts script paths relatively to provenance folders", {
# 	skip_on_travis()
# 	actual.test.df <- get.copied.script.path(prov.dir = test.prov.dir, origin.script.df = test.origin.scripts.df)
# 	expect_equivalent(actual.test.df, expected.script.df)
# })

test_that("compares main script and returns corresponding status", {
	expect_equal(compare.main.script(first.main.script.df = first.script.case0.df[1, ], second.main.script.df = second.script.case0.df[1, ]), 0)
	expect_equal(compare.main.script(first.main.script.df = first.script.case1.df[1, ], second.main.script.df = second.script.case1.df[1, ]), 1)
	expect_equal(compare.main.script(first.main.script.df = first.script.case2.df[1, ], second.main.script.df = second.script.case2.df[1, ]), 2)
	expect_equal(compare.main.script(first.main.script.df = first.script.case3.df[1, ], second.main.script.df = second.script.case3.df[1, ]), 3)
})

test_that("displays main script changes: case 0 - different content, same name", {
	actual.message <- get.main.script.change(main.script.change.result = 0, first.main.script.df = first.script.case0.df[1, ], second.main.script.df = second.script.case0.df[1, ])
	expected.message <- c("The content of the main script MainScript.R has changed",
	    "Run prov.diff.script to see the changes.",
		"### dir1 main script MainScript.R was last modified at: 2019-07-22T11.20.52EDT",
		"### dir2 main script MainScript.R was last modified at: 2019-07-22T12.20.52EDT")
	expect_equal(actual.message, expected.message)
})

test_that("displays main script changes: case 1 - different content, different name", {
	actual.message <- get.main.script.change(main.script.change.result = 1, first.main.script.df = first.script.case1.df[1, ], second.main.script.df = second.script.case1.df[1, ])
	expected.message <- c("Main script has different name",
		"### dir1 main script name: MainScript.R",
		"### dir2 main script name: MainRenamedScript.R",
		"The content of the main script has changed",
	    "Run prov.diff.script to see the changes.",
		"### dir1 main script MainScript.R was last modified at: 2019-07-22T11.20.52EDT",
		"### dir2 main script MainRenamedScript.R was last modified at: 2019-07-22T12.20.52EDT")
	expect_equal(actual.message, expected.message)
})

test_that("displays main script changes: case 2 - same content, different name", {
	actual.message <- get.main.script.change(main.script.change.result = 2, first.main.script.df = first.script.case2.df[1, ], second.main.script.df = second.script.case2.df[1, ])
	expected.message <- c("Main script has different name",
		"### dir1 main script name: MainScript.R",
		"### dir2 main script name: MainRenamedScript.R",
		"No change detected in the content of the main script",
		"### dir1 main script MainScript.R was last modified at: 2019-07-22T11.20.52EDT",
		"### dir2 main script MainRenamedScript.R was last modified at: 2019-07-22T12.20.52EDT")
	expect_equal(actual.message, expected.message)
})

test_that("displays main script changes: case 3 - same content, same name", {
	actual.message <- get.main.script.change(main.script.change.result = 3, first.main.script.df = first.script.case3.df[1, ], second.main.script.df = second.script.case3.df[1, ])
	expected.message <- c("No change detected in the content of the main script MainScript.R",
		"### dir1 main script MainScript.R was last modified at: 2019-07-22T11.20.52EDT",
		"### dir2 main script MainScript.R was last modified at: 2019-07-22T12.20.52EDT")
	expect_equal(actual.message, expected.message)
})

test_that("compares sourced scripts: both data frames are empty", {
	no.sourced.script <- first.script.case0.df
	no.sourced.script <- no.sourced.script[-2, ] # remove the only sourced script 

	sourced.script.change.list <- compare.sourced.scripts(no.sourced.script[-1, ], no.sourced.script[-1, ])
	expect_equal(nrow(sourced.script.change.list[[1]]), 0)
	expect_equal(nrow(sourced.script.change.list[[2]]), 0)
	expect_equal(nrow(sourced.script.change.list[[3]]), 0)
	expect_equal(nrow(sourced.script.change.list[[4]]), 0)
})

test_that("compares sourced script: dir1 data frame is empty", {
	no.dir1.sourced.script <- first.script.case0.df
	no.dir1.sourced.script <- no.dir1.sourced.script[-2, ] # remove the only sourced script
	expected.unmatched.dir2.script.df <- data.frame(script = c("SourcedScript1.R"), 
		timestamp = c("2019-07-22T11.20.37EDT"),
		hashValue = c("d24ab89249763832d467b7d36ce7e6db"), stringsAsFactors = FALSE)

	sourced.script.change.list <- compare.sourced.scripts(no.dir1.sourced.script[-1, ], second.script.case0.df[-1, ])
	expect_equal(nrow(sourced.script.change.list[[1]]), 0)
	expect_equal(nrow(sourced.script.change.list[[2]]), 0)
	expect_equal(nrow(sourced.script.change.list[[3]]), 0)
	expect_equal(sourced.script.change.list[[4]], expected.unmatched.dir2.script.df)
})

test_that("compares sourced script: dir2 data frame is empty", {
	no.dir2.sourced.script <- second.script.case0.df
	no.dir2.sourced.script <- no.dir2.sourced.script[-2, ] # remove the only sourced script
	expected.unmatched.dir1.script.df <- data.frame(script = c("SourcedScript1.R"), 
		timestamp = c("2019-07-22T11.18.37EDT"),
		hashValue = c("e14ab89249763832d467b7d36ce7e6db"), stringsAsFactors = FALSE)

	sourced.script.change.list <- compare.sourced.scripts(first.script.case0.df[-1, ], no.dir2.sourced.script[-1, ])
	expect_equal(nrow(sourced.script.change.list[[1]]), 0)
	expect_equal(nrow(sourced.script.change.list[[2]]), 0)
	expect_equal(sourced.script.change.list[[3]], expected.unmatched.dir1.script.df)
	expect_equal(nrow(sourced.script.change.list[[4]]), 0)
})

test_that("compares sourced script: four cases are non-empty", {
	multiple.first.sourced.script.df <- first.script.case0.df
	first.extension.df <- data.frame(script = c("/Users/khanhl.ngo/oldProv/SourcedScript2.R", 
										"/Users/khanhl.ngo/oldProv/SourcedScript3.R", 
										"/Users/khanhl.ngo/oldProv/SourcedScript5.R"),
							timestamp = c("2019-07-22T10.10.37EDT", 
										"2019-07-22T09.09.37EDT",
										"2019-07-22T12.00.37EDT"),
							hashValue = c("a00ab89249763832d467b7d36ce7e6db", 
										"b01ab89249763832d467b7d36ce7e6db",
										"c02ab89249763832d467b7d36ce7e6db"), 
							stringsAsFactors = FALSE)

	multiple.first.sourced.script.df <- rbind(multiple.first.sourced.script.df, first.extension.df)

	multiple.second.sourced.script.df <- second.script.case0.df	
	second.extension.df <- data.frame(script = c("/Users/khanhl.ngo/newProv/SourcedScript2.R", 
										"/Users/khanhl.ngo/newProv/SourcedScript4.R", 
										"/Users/khanhl.ngo/newProv/SourcedScript6.R"),
							timestamp = c("2019-07-22T12.12.37EDT", 
										"2019-07-22T13.01.37EDT",
										"2019-07-22T09.00.37EDT"),
							hashValue = c("a00ab89249763832d467b7d36ce7e6db", 
										"b01ab89249763832d467b7d36ce7e6db",
										"d00ab89249763832d467b7d36ce7e6db"), 
							stringsAsFactors = FALSE)
	multiple.second.sourced.script.df <- rbind(multiple.second.sourced.script.df, second.extension.df)

	expected.first.df <- data.frame(script = c("SourcedScript1.R", "SourcedScript2.R"),
							dir1.timestamp = c("2019-07-22T11.18.37EDT", "2019-07-22T10.10.37EDT"),
							dir1.hashValue = c("e14ab89249763832d467b7d36ce7e6db", "a00ab89249763832d467b7d36ce7e6db"),
							dir2.timestamp = c("2019-07-22T11.20.37EDT", "2019-07-22T12.12.37EDT"), 
							dir2.hashValue = c("d24ab89249763832d467b7d36ce7e6db", "a00ab89249763832d467b7d36ce7e6db"),
							stringsAsFactors = FALSE)
	expected.second.df <- data.frame(dir1.script = c("SourcedScript3.R"),
							dir1.timestamp = c("2019-07-22T09.09.37EDT"),
							hashValue = c("b01ab89249763832d467b7d36ce7e6db"),
							dir2.script = c("SourcedScript4.R"),
							dir2.timestamp = c("2019-07-22T13.01.37EDT"),
							stringsAsFactors = FALSE)
	expected.third.df <- data.frame(script = c("SourcedScript5.R"),
							timestamp = c("2019-07-22T12.00.37EDT"),
							hashValue = c("c02ab89249763832d467b7d36ce7e6db"),
							stringsAsFactors = FALSE)

	expected.fourth.df <- data.frame(script = c("SourcedScript6.R"),
							timestamp = c("2019-07-22T09.00.37EDT"),
							hashValue = c("d00ab89249763832d467b7d36ce7e6db"),
							stringsAsFactors = FALSE)

	sourced.script.change.list <- compare.sourced.scripts(multiple.first.sourced.script.df[-1, ], multiple.second.sourced.script.df[-1, ])
	expect_equivalent(sourced.script.change.list[[1]], expected.first.df)
	expect_equivalent(sourced.script.change.list[[2]], expected.second.df)
	expect_equivalent(sourced.script.change.list[[3]], expected.third.df)
	expect_equivalent(sourced.script.change.list[[4]], expected.fourth.df)
})

test_that("displays sourced scripts: same name", {
	# case: data frame is non-empty
	df <- data.frame(script = c("s1.R", "s2.R", "s3.R"),
					dir1.timestamp = c("12", "1", "2"), 
                 	dir1.hashValue = c("abc", "cde", "xyz"), 
                 	dir2.timestamp = c("12", "2", "4"), 
                 	dir2.hashValue = c("abc", "efg", "mno"), 
                 	stringsAsFactors = FALSE)
	actual.message <- get.same.name.sourced.scripts(df)
	expected.message <- c("Sourced script s2.R has changed",
	                      "Run prov.diff.script to see the changes.",
						  "### dir1 s2.R was last modified at: 1",
						  "### dir2 s2.R was last modified at: 2",
						  "Sourced script s3.R has changed",
	                      "Run prov.diff.script to see the changes.",
						  "### dir1 s3.R was last modified at: 2",
						  "### dir2 s3.R was last modified at: 4",
						  "No change detected in sourced script s1.R")
	expect_equal(actual.message, expected.message)
})

test_that("displays sourced scripts: renamed", {
	# case: data frame is non-empty
	df <- data.frame(dir1.script = c("s0.R", "s2.R"), 
					dir1.timestamp = c("12", "10"), 
					hashValue = c("abc", "xyz"),
					dir2.script = c("s1.R", "s3.R"),
					dir2.timestamp = c("1", "11"),
					stringsAsFactors = FALSE)
	actual.message <- get.renamed.sourced.scripts(df)
	expected.message <- c("Sourced script has same content but different names:",
						  "### dir1 sourced script name: s0.R",
						  "### dir2 sourced script name: s1.R",
						  "### s0.R was last modified at: 12",
						  "### s1.R was last modified at: 1",
						  "",
						  "Sourced script has same content but different names:",
						  "### dir1 sourced script name: s2.R",
						  "### dir2 sourced script name: s3.R",
						  "### s2.R was last modified at: 10",
						  "### s3.R was last modified at: 11", 
						  "")
	expect_equal(actual.message, expected.message)
})

test_that("displays sourced scripts: unmatched", {
	df <- data.frame(script = c("s0.R", "s2.R"), 
					timestamp = c("12", "10"), 
					hashValue = c("abc", "xyz"),
					stringsAsFactors = FALSE)

	# case: non-empty first unmatched data frame
	actual.message1 <- get.unmatched.sourced.scripts(status = "dir1", unmatched.script.df = df)
	expected.message1 <- c("Sourced scripts in dir1 but not in dir2:",
						   "### s0.R, which was last modified at: 12",
						   "### s2.R, which was last modified at: 10")
	expect_equal(actual.message1, expected.message1)

	# case: non-empty second unmatched data frame
	actual.message2 <- get.unmatched.sourced.scripts(status = "dir2", unmatched.script.df = df)
	expected.message2 <- c("Sourced scripts in dir2 but not in dir1:",
						   "### s0.R, which was last modified at: 12",
						   "### s2.R, which was last modified at: 10")
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



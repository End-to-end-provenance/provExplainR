#' This test file tests behavior of functions exposed to users.
#' @author Khanh Ngo

library(provExplainR)
library(testthat)

context("main methods exposed to user")

source("initTest.R")

# provenance directory paths for testing
dir1 <- get.test.prov.dirs ("prov_HF-data_2019-06-10T15.32.25EDT")
dir2 <- get.test.prov.dirs ("prov_HF-data")

test_that("prov.explain(): warning is shown if two directories are the same", {
	expect_warning(escape.value1 <- prov.explain(dir1, dir1), regexp = paste(dir1, "and", dir1, "are the same directories\n"))
	expect_equal(escape.value1, NA)
	expect_warning(escape.value2 <- prov.explain(dir2, dir2), regexp = paste(dir2, "and", dir2, "are the same directories\n"))
	expect_equal(escape.value2, NA)
})

test_that("prov.diff.script(): error message for non-existent directories", {
	# non-existent directory paths for testing
	error.prov.dir1 <- "testdata/error_dir1"
	error.prov.dir2 <- "testdata/error_dir2"

	expect_error(prov.diff.script(first.script = "non-existent.R", dir1 = dir1, dir2 = error.prov.dir1), 
		regexp = paste(error.prov.dir1, "directory not found\n"))
	expect_error(prov.diff.script(first.script = "non-existent.R", dir1 = error.prov.dir2, dir2 = dir2), 
		regexp = paste(error.prov.dir2, "directory not found\n"))
	expect_error(prov.diff.script(first.script = "non-existent.R", dir1 = error.prov.dir1, dir2 = error.prov.dir2), 
		regexp = paste(error.prov.dir1, " directory not found\n", error.prov.dir2, " directory not found\n", sep = ""))
	expect_error(prov.diff.script(first.script = "non-existent.R", dir1 = error.prov.dir2, dir2 = error.prov.dir1), 
		regexp = paste(error.prov.dir2, " directory not found\n", error.prov.dir1, " directory not found\n", sep = ""))
})

test_that("prov.diff.script(): error message for non-existent scripts", {
	expect_error(prov.diff.script(first.script = "/Users/khanhl.ngo/scripts/non-existent.R", dir1 = dir1, dir2 = dir2),
		regexp = paste("non-existent.R not found in", dir1, "\n"))
	expect_error(prov.diff.script(first.script = "/Users/khanhl.ngo/scripts/HF-data.R", dir1 = dir1, dir2 = dir2, second.script = "/Users/khanhl.ngo/scripts/non-existent.R"),
		regexp = paste("non-existent.R not found in", dir2, "\n"))
})



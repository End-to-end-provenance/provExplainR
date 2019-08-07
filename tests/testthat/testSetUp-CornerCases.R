#' this test file tests behavior of helper functions that handle corner
#' cases in provExplainR.R, which includes checking the existence of 
#' directories, data frames, getting ProvInfo object from provParseR, etc.
#' @author Khanh Ngo
 
library(provExplainR)
library(testthat)

context("Corner cases: directories, json files, data frames, custom print")

source("initTest.R")

# provenance directory paths for testing
first.prov.dir <- get.test.prov.dirs ("prov_HF-data_2019-06-10T15.32.25EDT")
second.prov.dir <- get.test.prov.dirs ("prov_HF-data")


########## test checking existence of two directories ##########
test_that("error message is empty for existing directories", {
	expect_equal(check.dir.existence(first.prov.dir, second.prov.dir), "")
	expect_equal(check.dir.existence(second.prov.dir, first.prov.dir), "")
})

test_that("error message is shown for non-existent directories with corresponding names", {
	# non-existent directory paths for testing
	error.prov.dir1 <- "testdata/error_dir1"
	error.prov.dir2 <- "testdata/error_dir2"

	expect_error(check.dir.existence(first.prov.dir, error.prov.dir1), regexp = paste(error.prov.dir1, "directory not found\n"))
	expect_error(check.dir.existence(error.prov.dir2, second.prov.dir), regexp = paste(error.prov.dir2, "directory not found\n"))
	expect_error(check.dir.existence(error.prov.dir1, error.prov.dir2), regexp = paste(error.prov.dir1, " directory not found\n", 
		error.prov.dir2, " directory not found\n", sep = ""))
	expect_error(check.dir.existence(error.prov.dir2, error.prov.dir1), paste(error.prov.dir2, " directory not found\n",
		error.prov.dir1, " directory not found\n", sep = ""))
})


########## test getting the ProvInfo objects ##########
test_that("error message is shown when the json file does not exist", {
	# provenance directory without json file
	no.json.prov.dir <- get.test.prov.dirs ("prov_HF-data_no-json")

	expect_error(get.prov.info.object(no.json.prov.dir), regexp = paste("prov.json file in ", no.json.prov.dir, "is not found\n"))
})

########## test checking existence of data frames ##########
test_that("warning is shown for non-existent data frames", {
	# data frames for testing
	first.df <- data.frame(row1 = c(1,2,3), row2 = c("a", "b", "c"))
	second.df <- data.frame(row1 = c(4,5,6), row2 = c("d", "e", "f"))

	expect_warning(return.value1 <- check.df.existence(aspect = "Environment", df1 = NULL, df2 = first.df), 
		regexp = paste("Environment data frames returned by provParseR is NULL\n"))
	expect_false(return.value1)
	expect_warning(return.value2 <- check.df.existence(aspect = "Environment", df1 = NULL, df2 = NULL), 
		regexp = paste("Environment data frames returned by provParseR is NULL\n"))
	expect_false(return.value2)
	expect_warning(return.value3 <- check.df.existence(aspect = "Environment", df1 = second.df, df2 = NULL), 
		regexp = paste("Environment data frames returned by provParseR is NULL\n"))
	expect_false(return.value3)
	expect_true(check.df.existence(aspect = "Environment", df1 = first.df, df2 = second.df))
})

########## test checking if a data frame is empty
test_that("warning is shown for empty data frames", {
	first.df <- data.frame(name = c("throw.away"), value = c("throw.away"), stringsAsFactors = FALSE)
	first.df <- first.df[-1, ]
	second.df <- data.frame(name = c("greetings"), value = c("hello"), stringsAsFactors = FALSE)

	expect_warning(escape.value1 <- check.df.empty(aspect = "script", df1 = first.df, df2 = second.df),
		regexp = paste("no script was recorded in data frame returned by provParseR\n"))
	expect_false(escape.value1)
	expect_warning(escape.value2 <- check.df.empty(aspect = "environment factor", df1 = second.df, df2 = first.df), 
		regexp = paste("no environment factor was recorded in data frame returned by provParseR\n"))
	expect_false(escape.value2)
	expect_true(check.df.empty(aspect = "library", second.df, second.df))
})


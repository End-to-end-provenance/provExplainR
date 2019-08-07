#' This test file tests behavior of all functions related to detecting changes
#' in the loaded libraries based on collect provenance.
#' @author Khanh Ngo

library(provExplainR)
library(testthat)

context("Finding Library Changes")

source("initTest.R")

# ProvInfo objects
first.prov.info <- get.test.prov.info ("prov_HF-data_2019-06-10T15.32.25EDT")
second.prov.info <- get.test.prov.info ("prov_HF-data")

# library data frames for each provenance
first.lib.df <- provParseR::get.libs(first.prov.info)
second.lib.df <- provParseR::get.libs(second.prov.info)

test_that("test init is fine", {
	expect_false(is.null(first.lib.df))
	expect_false(is.null(second.lib.df))
	expect_true(is.data.frame(first.lib.df))
	expect_true(is.data.frame(second.lib.df))
})

test_that("correctly outputs 3 library data frames: differences, in dir2, in dir1", {
	# get the actual returned list
	actual.lib.change.list <- find.library.changes(first.lib.df = first.lib.df, second.lib.df = second.lib.df)
	actual.lib.difference.df <- as.data.frame(actual.lib.change.list[1])
	actual.lib.dir2.df <- as.data.frame(actual.lib.change.list[2])
	actual.lib.dir1.df <- as.data.frame(actual.lib.change.list[3])

	# expected data frames
	expected.lib.difference.df <- data.frame(name = c("provSummarizeR", "rdtLite"), dir1.version = c("1.0", "1.0.2"), dir2.version = c("1.1", "1.1.0"), stringsAsFactors = FALSE)
	expected.lib.dir2.df <- data.frame(name = c("throw.away"), version = c("throw.away"), stringsAsFactors = FALSE)
	expected.lib.dir2.df <- expected.lib.dir2.df[-1, ]
	expected.lib.dir1.df <- data.frame(name = c("provDebugR"), version = c("0.1.2.9000"), stringsAsFactors = FALSE)

	# sort in alphabetical order to make sure 2 data frames are in same order
	expected.lib.difference.df <- expected.lib.difference.df[order(expected.lib.difference.df$name), ]
	actual.lib.difference.df <- actual.lib.difference.df[order(actual.lib.difference.df$name), ]

	expect_equivalent(actual.lib.difference.df, expected.lib.difference.df)
	expect_equivalent(actual.lib.dir2.df, expected.lib.dir2.df)
	expect_equivalent(actual.lib.dir1.df, expected.lib.dir1.df)
})

test_that("warning message is shown when library data frame is NULL", {
	expect_warning(escape.value1 <- find.library.changes(first.lib.df = NULL, second.lib.df = second.lib.df), 
		regexp = paste("Library data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value1, NULL)

	expect_warning(escape.value2 <- find.library.changes(first.lib.df = NULL, second.lib.df = NULL), 
		regexp = paste("Library data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value2, NULL)

	expect_warning(escape.value <- find.library.changes(first.lib.df = first.lib.df, second.lib.df = NULL), 
		regexp = paste("Library data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value, NULL)
})


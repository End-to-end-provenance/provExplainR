#' This test file tests behavior of all functions related to detecting changes
#' in the loaded libraries based on collect provenance.
#' @author Khanh Ngo
#' @version 7/9/19

library(provExplainR)
library(testthat)

context("Finding Library Changes")

source("initTest.R")

# ProvInfo objects
old.prov.info <- get.test.prov.info ("prov_HF-data_2019-06-10T15.32.25EDT")
new.prov.info <- get.test.prov.info ("prov_HF-data")

# library data frames for each provenance
old.libs.df <- provParseR::get.libs(old.prov.info)
new.libs.df <- provParseR::get.libs(new.prov.info)

test_that("test init is fine", {
	expect_false(is.null(old.libs.df))
	expect_false(is.null(new.libs.df))
	expect_true(is.data.frame(old.libs.df))
	expect_true(is.data.frame(new.libs.df))
})

test_that("correctly outputs 3 library data frames: updates, additions, removals", {
	# get the actual returned list
	actual.lib.change.list <- find.library.changes(olderProv.lib.df = old.libs.df, newerProv.lib.df = new.libs.df)
	actual.lib.update.df <- as.data.frame(actual.lib.change.list[1])
	actual.lib.add.df <- as.data.frame(actual.lib.change.list[2])
	actual.lib.remove.df <- as.data.frame(actual.lib.change.list[3])

	# expected data frames
	expected.lib.update.df <- data.frame(name = c("provSummarizeR", "rdtLite"), old.version = c("1.0", "1.0.2"), new.version = c("1.1", "1.1.0"), stringsAsFactors = FALSE)
	expected.lib.add.df <- data.frame(name = c("throw.away"), version = c("throw.away"), stringsAsFactors = FALSE)
	expected.lib.add.df <- expected.lib.add.df[-1, ]
	expected.lib.remove.df <- data.frame(name = c("provDebugR"), version = c("0.1.2.9000"), stringsAsFactors = FALSE)

	# sort in alphabetical order to make sure 2 data frames are in same order
	expected.lib.update.df <- expected.lib.update.df[order(expected.lib.update.df$name), ]
	actual.lib.update.df <- actual.lib.update.df[order(actual.lib.update.df$name), ]

	expect_equivalent(actual.lib.update.df, expected.lib.update.df)
	expect_equivalent(actual.lib.add.df, expected.lib.add.df)
	expect_equivalent(actual.lib.remove.df, expected.lib.remove.df)
})

test_that("warning message is shown when library data frame is NULL", {
	expect_warning(escape.value1 <- find.library.changes(olderProv.lib.df = NULL, newerProv.lib.df = new.libs.df), 
		regexp = paste("Library data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value1, NULL)

	expect_warning(escape.value2 <- find.library.changes(olderProv.lib.df = NULL, newerProv.lib.df = NULL), 
		regexp = paste("Library data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value2, NULL)

	expect_warning(escape.value <- find.library.changes(olderProv.lib.df = old.libs.df, newerProv.lib.df = NULL), 
		regexp = paste("Library data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value, NULL)
})


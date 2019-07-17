#' This test file tests behavior of all functions related to detecting changes
#' in the environment at the time users collect the provenances.
#' @author Khanh Ngo
#' @version 7/16/19

library(testthat)
library(provExplainR)

context("Finding Environment Changes")

source("initTest.R")

# ProvInfo objects
old.prov.info <- get.test.prov.info ("prov_HF-data_2019-06-10T15.32.25EDT")
new.prov.info <- get.test.prov.info ("prov_HF-data")

# library data frames for each provenance
old.environment.df <- provParseR::get.environment(old.prov.info)
new.environment.df <- provParseR::get.environment(new.prov.info)

test_that("test init is fine", {
	expect_false(is.null(old.environment.df))
	expect_false(is.null(new.environment.df))
	expect_true(is.data.frame(old.environment.df))
	expect_true(is.data.frame(new.environment.df))
})

test_that("warning shown if environment data frames returned by provParseR is NULL", {
	expect_warning(escape.value1 <- find.environment.changes(olderProv.env.df = NULL, newerProv.env.df = new.environment.df), 
		regexp = paste("Environment data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value1, NULL)

	expect_warning(escape.value2 <- find.environment.changes(olderProv.env.df = NULL, newerProv.env.df = NULL), 
		regexp = paste("Environment data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value2, NULL)

	expect_warning(escape.value3 <- find.environment.changes(olderProv.env.df = old.environment.df, newerProv.env.df = NULL), 
		regexp = paste("Environment data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value3, NULL)
})

test_that("correctly outputs comparison of environment values", {
	actual.environment.change.list <- find.environment.changes(olderProv.env.df = old.environment.df, newerProv.env.df = new.environment.df)
	actual.environment.update.df <- as.data.frame(actual.environment.change.list[1])
	actual.environment.added.df <- as.data.frame(actual.environment.change.list[2])
	actual.environment.removed.df <- as.data.frame(actual.environment.change.list[3])

	# expected data frame
	expected.environment.update.df <- data.frame(label = c("provDirectory", "provTimestamp"), 
		old.value = c("/Users/khanhl.ngo/HarvardForest/Day3Exercise/prov_HF-data_2019-06-10T15.32.25EDT", "2019-06-10T15.32.25EDT"), 
		new.value = c("/Users/khanhl.ngo/HarvardForest/Day3Exercise/prov_HF-data", "2019-06-28T10.17.18EDT"),
		stringsAsFactors = FALSE)
	expected.environment.added.df <- data.frame(label = c("totalElapsedTime"), value = c("5.058"), stringsAsFactors = FALSE)
	expected.environment.removed.df <- data.frame(label = c("throw.away"), value = c("throw.away"), stringsAsFactors = FALSE)
	expected.environment.removed.df <- expected.environment.removed.df[-1, ]

	# sort in alphabetical order to make sure 2 data frames are in same order
	expected.environment.update.df <- expected.environment.update.df[order(expected.environment.update.df$label), ]
	actual.environment.update.df <- actual.environment.update.df[order(actual.environment.update.df$label), ]
	expected.environment.added.df <- expected.environment.added.df[order(expected.environment.added.df$label), ]
	actual.environment.added.df <- actual.environment.added.df[order(actual.environment.added.df$label), ]

	expect_equivalent(actual.environment.update.df, expected.environment.update.df)
	expect_equivalent(actual.environment.added.df, expected.environment.added.df)
	expect_equivalent(actual.environment.removed.df, expected.environment.removed.df)
})


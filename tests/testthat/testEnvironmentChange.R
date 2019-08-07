#' This test file tests behavior of all functions related to detecting changes
#' in the environment at the time users collect the provenances.
#' @author Khanh Ngo

library(testthat)
library(provExplainR)

context("Finding Environment Changes")

source("initTest.R")

# ProvInfo objects
first.prov.info <- get.test.prov.info ("prov_HF-data_2019-06-10T15.32.25EDT")
second.prov.info <- get.test.prov.info ("prov_HF-data")

# library data frames for each provenance
first.environment.df <- provParseR::get.environment(first.prov.info)
second.environment.df <- provParseR::get.environment(second.prov.info)

test_that("test init is fine", {
	expect_false(is.null(first.environment.df))
	expect_false(is.null(second.environment.df))
	expect_true(is.data.frame(first.environment.df))
	expect_true(is.data.frame(second.environment.df))
})

test_that("warning shown if environment data frames returned by provParseR is NULL", {
	expect_warning(escape.value1 <- find.environment.changes(first.env.df = NULL, second.env.df = second.environment.df), 
		regexp = paste("Environment data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value1, NULL)

	expect_warning(escape.value2 <- find.environment.changes(first.env.df = NULL, second.env.df = NULL), 
		regexp = paste("Environment data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value2, NULL)

	expect_warning(escape.value3 <- find.environment.changes(first.env.df = first.environment.df, second.env.df = NULL), 
		regexp = paste("Environment data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value3, NULL)
})

test_that("correctly outputs comparison of environment values", {
	actual.environment.change.list <- find.environment.changes(first.env.df = first.environment.df, second.env.df = second.environment.df)
	actual.environment.difference.df <- as.data.frame(actual.environment.change.list[1])
	actual.environment.dir2.df <- as.data.frame(actual.environment.change.list[2])
	actual.environment.dir1.df <- as.data.frame(actual.environment.change.list[3])

	# expected data frame
	expected.environment.difference.df <- data.frame(label = c("provenance directory", "provenance collection time"), 
		dir1.value = c("/Users/khanhl.ngo/HarvardForest/Day3Exercise/prov_HF-data_2019-06-10T15.32.25EDT", "2019-06-10T15.32.25EDT"), 
		dir2.value = c("/Users/khanhl.ngo/HarvardForest/Day3Exercise/prov_HF-data", "2019-06-28T10.17.18EDT"),
		stringsAsFactors = FALSE)
	expected.environment.dir2.df <- data.frame(label = c("total elapsed time"), value = c("5.058"), stringsAsFactors = FALSE)
	expected.environment.dir1.df <- data.frame(label = c("throw.away"), value = c("throw.away"), stringsAsFactors = FALSE)
	expected.environment.dir1.df <- expected.environment.dir1.df[-1, ]

	# sort in alphabetical order to make sure 2 data frames are in same order
	expected.environment.difference.df <- expected.environment.difference.df[order(expected.environment.difference.df$label), ]
	actual.environment.difference.df <- actual.environment.difference.df[order(actual.environment.difference.df$label), ]
	expected.environment.dir2.df <- expected.environment.dir2.df[order(expected.environment.dir2.df$label), ]
	actual.environment.dir2.df <- actual.environment.dir2.df[order(actual.environment.dir2.df$label), ]

	expect_equivalent(actual.environment.difference.df, expected.environment.difference.df)
	expect_equivalent(actual.environment.dir2.df, expected.environment.dir2.df)
	expect_equivalent(actual.environment.dir1.df, expected.environment.dir1.df)
})


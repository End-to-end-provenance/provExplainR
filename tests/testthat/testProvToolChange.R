#' This test file tests behavior of all functions related to detecting changes
#' in the provenance tools which were used to collect the provenance.
#' @author Khanh Ngo

library(testthat)
library(provExplainR)

context("Finding Provenance Tool Changes")

source("initTest.R")

# ProvInfo objects
first.prov.info <- get.test.prov.info ("prov_HF-data_2019-06-10T15.32.25EDT")
second.prov.info <- get.test.prov.info ("prov_HF-data")

# library data frames for each provenance
first.tools.df <- provParseR::get.tool.info(first.prov.info)
second.tools.df <- provParseR::get.tool.info(second.prov.info)

test_that("test init is fine", {
	expect_false(is.null(first.tools.df))
	expect_false(is.null(second.tools.df))
	expect_true(is.data.frame(first.tools.df))
	expect_true(is.data.frame(second.tools.df))
})

test_that("warning shown if provenance-tool data frames returned by provParseR is NULL", {
	expect_warning(escape.value1 <- find.prov.tool.changes(first.tool.df = NULL, second.tool.df = second.tools.df), 
		regexp = paste("Provenance tool data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value1, NULL)

	expect_warning(escape.value2 <- find.prov.tool.changes(first.tool.df = NULL, second.tool.df = NULL), 
		regexp = paste("Provenance tool data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value2, NULL)

	expect_warning(escape.value3 <- find.prov.tool.changes(first.tool.df = first.tools.df, second.tool.df = NULL), 
		regexp = paste("Provenance tool data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value3, NULL)
})

test_that("warning shown if no provenance tool was recorded by provParseR", {
	empty.tool.df1 <- data.frame()
	empty.tool.df2 <- data.frame()
	expect_warning(escape.message1 <- find.prov.tool.changes(first.tool.df = empty.tool.df1, second.tool.df = empty.tool.df2), 
		regexp = paste("no provenance tool was recorded in data frame returned by provParseR\n"))
	expect_equal(escape.message1, NULL)
	expect_warning(escape.message2 <- find.prov.tool.changes(first.tool.df = first.tools.df, second.tool.df = empty.tool.df2), 
		regexp = paste("no provenance tool was recorded in data frame returned by provParseR\n"))
	expect_equal(escape.message2, NULL)
	expect_warning(escape.message2 <- find.prov.tool.changes(first.tool.df = empty.tool.df1, second.tool.df = second.tools.df), 
		regexp = paste("no provenance tool was recorded in data frame returned by provParseR\n"))
	expect_equal(escape.message2, NULL)
})

test_that("correctly outputs comparison of provenance tools and JSON file versions", {
	actual.tool.change.list <- find.prov.tool.changes(first.tool.df = first.tools.df, second.tool.df = second.tools.df)
	actual.tool.difference.df <- as.data.frame(actual.tool.change.list[1])
	actual.dir2.tool.df <- as.data.frame(actual.tool.change.list[2])
	actual.dir1.tool.df <- as.data.frame(actual.tool.change.list[3])

	# expected data frame
	expected.tool.difference.df <- data.frame(tool.name = c("rdtLite"), dir1.tool.version = c("1.0.2"), dir2.tool.version = c("1.1.0"),
		dir1.json.version = c("2.1"), dir2.json.version = c("2.2"), stringsAsFactors = FALSE)
	expected.dir2.tool.df <- data.frame(tool.name = c("throw.away"), tool.version = c("throw.away"), json.version = c("json.version"), 
		stringsAsFactors = FALSE)
	expected.dir2.tool.df <- expected.dir2.tool.df[-1, ]

	# sort in alphabetical order to make sure 2 data frames are in same order
	expected.tool.difference.df <- expected.tool.difference.df[order(expected.tool.difference.df$tool.name), ]
	actual.tool.difference.df <- actual.tool.difference.df[order(actual.tool.difference.df$tool.name), ]

	expect_equivalent(actual.tool.difference.df, expected.tool.difference.df)
	expect_equivalent(actual.dir2.tool.df, expected.dir2.tool.df)
	expect_equivalent(actual.dir1.tool.df, expected.dir2.tool.df)
})

test_that("mannual test: correct outputs for provenance tools and JSON file versions", {
	mannual.first.tools.df <- data.frame(tool.name = c("rdt", "rdtLite"), tool.version = c("1.0", "1.1"), json.version = c("1.1", "2.0"), stringsAsFactors = FALSE)
	mannual.second.tools.df <- data.frame(tool.name = c("rdtLite", "futureRDT"), tool.version = c("1.1", "2.0"), json.version = c("2.1", "2.1"), stringsAsFactors = FALSE)

	mannual.tool.change.list <- find.prov.tool.changes(first.tool.df = mannual.first.tools.df, second.tool.df = mannual.second.tools.df)
	mannual.actual.tool.difference.df <- as.data.frame(mannual.tool.change.list[1])
	mannual.actual.dir2.tool.df <- as.data.frame(mannual.tool.change.list[2])
	mannual.actual.dir1.tool.df <- as.data.frame(mannual.tool.change.list[3])

	mannual.expected.tool.difference.df <- data.frame(tool.name = c("rdtLite"), dir1.tool.version = c("1.1"), dir2.tool.version = c("1.1"),
		dir1.json.version = c("2.0"), dir2.json.version = c("2.1"), stringsAsFactors = FALSE)
	mannual.expected.dir2.tool.df <- data.frame(tool.name = c("futureRDT"), tool.version = c("2.0"), json.version = c("2.1"), stringsAsFactors = FALSE)
	mannual.expected.dir1.tool.df <- data.frame(tool.name = c("rdt"), tool.version = c("1.0"), json.version = c("1.1"), stringsAsFactors = FALSE)

	expect_equivalent(mannual.actual.tool.difference.df, mannual.expected.tool.difference.df)
	expect_equivalent(mannual.actual.dir2.tool.df, mannual.expected.dir2.tool.df)
	expect_equivalent(mannual.actual.dir1.tool.df, mannual.expected.dir1.tool.df)
})


#' This test file tests behavior of all functions related to detecting changes
#' in the provenance tools which were used to collect the provenance.
#' @author Khanh Ngo
#' @version 7/11/19

library(testthat)
library(provExplainR)

context("Finding Provenance Tool Changes")

source("initTest.R")

# ProvInfo objects
old.prov.info <- get.test.prov.info ("prov_HF-data_2019-06-10T15.32.25EDT")
new.prov.info <- get.test.prov.info ("prov_HF-data")

# library data frames for each provenance
old.tools.df <- provParseR::get.tool.info(old.prov.info)
new.tools.df <- provParseR::get.tool.info(new.prov.info)

test_that("test init is fine", {
	expect_false(is.null(old.tools.df))
	expect_false(is.null(new.tools.df))
	expect_true(is.data.frame(old.tools.df))
	expect_true(is.data.frame(new.tools.df))
})

test_that("warning shown if provenance-tool data frames returned by provParseR is NULL", {
	expect_warning(escape.value1 <- find.prov.tool.changes(olderProv.tool.df = NULL, newerProv.tool.df = new.libs.df), 
		regexp = paste("Provenance tool data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value1, NULL)

	expect_warning(escape.value2 <- find.prov.tool.changes(olderProv.tool.df = NULL, newerProv.tool.df = NULL), 
		regexp = paste("Provenance tool data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value2, NULL)

	expect_warning(escape.value3 <- find.prov.tool.changes(olderProv.tool.df = old.tools.df, newerProv.tool.df = NULL), 
		regexp = paste("Provenance tool data frames returned by provParseR is NULL\n"))
	expect_equal(escape.value3, NULL)
})

test_that("warning shown if no provenance tool was recorded by provParseR", {
	empty.tool.df1 <- data.frame()
	empty.tool.df2 <- data.frame()
	expect_warning(escape.message1 <- find.prov.tool.changes(olderProv.tool.df = empty.tool.df1, newerProv.tool.df = empty.tool.df2), 
		regexp = paste("no provenance tool was recorded in data frame returned by provParseR\n"))
	expect_equal(escape.message1, NULL)
	expect_warning(escape.message2 <- find.prov.tool.changes(olderProv.tool.df = old.tools.df, newerProv.tool.df = empty.tool.df2), 
		regexp = paste("no provenance tool was recorded in data frame returned by provParseR\n"))
	expect_equal(escape.message2, NULL)
	expect_warning(escape.message2 <- find.prov.tool.changes(olderProv.tool.df = empty.tool.df1, newerProv.tool.df = new.tools.df), 
		regexp = paste("no provenance tool was recorded in data frame returned by provParseR\n"))
	expect_equal(escape.message2, NULL)
})

test_that("correctly outputs comparison of provenance tools and JSON file versions", {
	actual.tool.change.list <- find.prov.tool.changes(olderProv.tool.df = old.tools.df, newerProv.tool.df = new.tools.df)
	actual.tool.update.df <- as.data.frame(actual.tool.change.list[1])
	actual.tool.added.df <- as.data.frame(actual.tool.change.list[2])
	actual.tool.removed.df <- as.data.frame(actual.tool.change.list[3])

	# expected data frame
	expected.tool.update.df <- data.frame(tool.name = c("rdtLite"), old.tool.version = c("1.0.2"), new.tool.version = c("1.1.0"),
		old.json.version = c("2.1"), new.json.version = c("2.2"), stringsAsFactors = FALSE)
	expected.tool.added.df <- data.frame(tool.name = c("throw.away"), tool.version = c("throw.away"), json.version = c("json.version"), 
		stringsAsFactors = FALSE)
	expected.tool.added.df <- expected.tool.added.df[-1, ]

	# sort in alphabetical order to make sure 2 data frames are in same order
	expected.tool.update.df <- expected.tool.update.df[order(expected.tool.update.df$tool.name), ]
	actual.tool.update.df <- actual.tool.update.df[order(actual.tool.update.df$tool.name), ]

	expect_equivalent(actual.tool.update.df, expected.tool.update.df)
	expect_equivalent(actual.tool.added.df, expected.tool.added.df)
	expect_equivalent(actual.tool.removed.df, expected.tool.added.df)
})

test_that("mannual test: correct outputs for provenance tools and JSON file versions", {
	mannual.old.tools.df <- data.frame(tool.name = c("rdt", "rdtLite"), tool.version = c("1.0", "1.1"), json.version = c("1.1", "2.0"), stringsAsFactors = FALSE)
	mannual.new.tools.df <- data.frame(tool.name = c("rdtLite", "futureRDT"), tool.version = c("1.1", "2.0"), json.version = c("2.1", "2.1"), stringsAsFactors = FALSE)

	mannual.tool.change.list <- find.prov.tool.changes(olderProv.tool.df = mannual.old.tools.df, newerProv.tool.df = mannual.new.tools.df)
	mannual.actual.tool.update.df <- as.data.frame(mannual.tool.change.list[1])
	mannual.actual.tool.added.df <- as.data.frame(mannual.tool.change.list[2])
	mannual.actual.tool.removed.df <- as.data.frame(mannual.tool.change.list[3])

	mannual.expected.tool.update.df <- data.frame(tool.name = c("rdtLite"), old.tool.version = c("1.1"), new.tool.version = c("1.1"),
		old.json.version = c("2.0"), new.json.version = c("2.1"), stringsAsFactors = FALSE)
	mannual.expected.tool.added.df <- data.frame(tool.name = c("futureRDT"), tool.version = c("2.0"), json.version = c("2.1"), stringsAsFactors = FALSE)
	mannual.expected.tool.removed.df <- data.frame(tool.name = c("rdt"), tool.version = c("1.0"), json.version = c("1.1"), stringsAsFactors = FALSE)

	expect_equivalent(mannual.actual.tool.update.df, mannual.expected.tool.update.df)
	expect_equivalent(mannual.actual.tool.added.df, mannual.expected.tool.added.df)
	expect_equivalent(mannual.actual.tool.removed.df, mannual.expected.tool.removed.df)
})


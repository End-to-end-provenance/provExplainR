#' This test file tests behavior of all functions related to detecting changes
#' in the loaded libraries based on collect provenance.
#' @author Khanh Ngo
#' @version 7/9/19

library(testthat)
library(provExplainR)

context("Finding Library Changes")

# provenance directory paths for testing
old.prov.dir <- system.file("testdata", "prov_HF-data_2019-06-10T15.32.25EDT", package = "provExplainR")
new.prov.dir <- system.file("testdata", "prov_HF-data", package = "provExplainR")

# ProvInfo objects
old.prov.info <- provParseR::prov.parse(paste(old.prov.dir, "/prov.json", sep = ""))
new.prov.info <- provParseR::prov.parse(paste(new.prov.dir, "/prov.json", sep = ""))

# library data frames for each provenance
old.libs.df <- provParseR::get.libs(old.prov.info)
new.libs.df <- provParseR::get.libs(new.prov.info)


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

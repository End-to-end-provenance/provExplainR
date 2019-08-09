# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2019.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

###############################################################################

#' Provenance explanation function
#' 
#' prov.explain reads two provenance collections and finds differences between these two versions
#' 
#' This function use provenance collected using the rdtLite or rdt packages.
#' 
#' Factors under examination includes:
#' \itemize{
#'   \item Environmental information identifying when the scripts were executed, the version of R,
#' 		the computing systems, the tool and version used to collect the provenances, the location
#' 		of the provenance file, and the hash algorithm used to hash data files.
#'   \item Versions of libraries loaded
#'   \item Versions of provenance tools
#'   \item Contents and names of main and sourced scripts 
#' }
#' 
#' @param dir1 path of first provenance directory
#' @param dir2 path of second provenance directory
#' @param save if true saves the report to the file prov-explain.txt in the first directory 
#' @export
#' @examples
#' \dontrun{prov.explain("first.test.dir", "second.test.dir")}
prov.explain <- function (dir1, dir2, save = FALSE){
	# check the existence of two given directories
	check.dir.existence(dir1, dir2)

	# case: two directories are the same
	if (dir1 == dir2){
		warning (paste(dir1, "and", dir2, "are the same directories\n"))
		return (NA)
	}

	# detecting changes 
	if(save == TRUE){
		save.to.text.file(dir1, dir2)
	}else{
		detect.changes(dir1, dir2)
	}
}

#' Provenance Script Diff function
#' 
#' prov.diff.script visualizes the differences between the contents of two scripts based on given provenance collections.
#' 
#' This function uses provenance collected using the rdtLite or rdt packages.
#' 
#' Users must specify name of the first script, 
#' first provenance directory path and second provenance directory 
#' path. Name of second script is optional. If second script 
#' is specified, provExplainR assumes first script is located in the 
#' first provenance directory and second script is located in 
#' the second provenance directory. Otherwise, provExplainR assumes 
#' both scripts share the same name.
#' @param first.script name of first script 
#' @param dir1 path of first provenance directory
#' @param dir2 path of second provenance directory
#' @param second.script name of second script 
#' @export
prov.diff.script <- function(first.script, dir1, dir2, second.script = NULL) {
	# check the existence of two given directories
	check.dir.existence(dir1, dir2)

	# extract script name and change paths to first script saved in prov folders
	first.script <- basename(first.script)
	first.full.script <- paste(dir1, "/scripts/", first.script, sep = "")
	
	# extract script name and change paths to second script saved in prov folders
	if(FALSE == is.null(second.script)){
		second.script <- basename(second.script)
	}else{
		second.script <- first.script
	}
	second.full.script <- paste(dir2, "/scripts/", second.script, sep = "")

	# check existence of 2 scripts 
	if(FALSE == file.exists(first.full.script)){
		stop(paste(first.script, "not found in", dir1, "\n"))
	}

	if(FALSE == file.exists(second.full.script)){
		stop(paste(second.script, "not found in", dir2, "\n"))
	}

	# show the diff
	diffobj::diffFile(target = first.full.script, current = second.full.script, mode = "sidebyside")
}

#' detect.changes gets ProvInfo objects from provParseR
#' and calls other methods to find changes in different aspects
#' @param dir1 path to first prov directory
#' @param dir2 path to second prov directory
#' @noRd
detect.changes <- function (dir1, dir2){
	cat(paste("\nYou entered:\ndir1 =", dir1, "\ndir2 =", dir2))

	# gets the ProvInfo objects
	first.prov.info <- get.prov.info.object(dir1)
	second.prov.info <- get.prov.info.object(dir2)

	# detect changes in different aspects
	print.script.changes (provParseR::get.scripts(first.prov.info), provParseR::get.scripts(second.prov.info), dir1, dir2)
	print.library.changes (provParseR::get.libs(first.prov.info), provParseR::get.libs(second.prov.info))
	print.environment.changes (provParseR::get.environment(first.prov.info), provParseR::get.environment(second.prov.info))
	print.prov.tool.changes (provParseR::get.tool.info(first.prov.info), provParseR::get.tool.info(second.prov.info))
}

#' save.to.text.file outputs comparison results to the console 
#' and saves them into a text file named prov-explain.txt located 
#' in the first provenance directory
#' @param dir1 first provenance directory
#' @param dir2 second provenance directory 
#' @noRd
save.to.text.file <- function(dir1, dir2) {
	# gets the full path of first provenance directory 
	explain.file <- paste(dir1, "/prov-explain.txt", sep = "")
	sink(explain.file, split = TRUE)
	detect.changes(dir1, dir2)
	sink()
	cat(paste("\n\nSaving comparison results in", explain.file))
}

#' print.library.changes gets changes in library by calling a helper
#' method find.library.changes, and prints out the result
#' @param first.lib.df first library data frame
#' @param second.lib.df second library data frame
#' @noRd
print.library.changes <- function (first.lib.df, second.lib.df){
	cat ("\n\nLIBRARY CHANGES: ")
	# get the list of changes
	lib.change.list <- find.library.changes(first.lib.df, second.lib.df)

	# if list is null, R returns empty (non-NULL) data frames
	lib.difference.df <- as.data.frame(lib.change.list[1])
	lib.dir2.df <- as.data.frame(lib.change.list[2])
	lib.dir1.df <- as.data.frame(lib.change.list[3])

	cat ("\nLibrary version differences:\n")
	if(nrow(lib.difference.df) == 0){
		cat("No differences in library versions have been detected")
	}else{
		print.data.frame(lib.difference.df, row.names = FALSE)
	}

	cat ("\n\nLibraries in dir2 but not in dir1:\n")
	if(nrow(lib.dir2.df) == 0){
		cat("No such libraries were found")
	}else{
		print.data.frame(lib.dir2.df, row.names = FALSE)
	}

	cat ("\n\nLibraries in dir1 but not in dir2:\n")
	if(nrow(lib.dir1.df) == 0){
		cat("No such libraries were found")
	}else{
		print.data.frame(lib.dir1.df, row.names = FALSE)
	}
}


#' find.library.changes detects changes in libraries used based on the 
#' collected provenance from two provenance folders.
#' The method returns a list of 3 data frames: library version differences,
#' libraries in dir2 but not in dir1, libraries in dir1 but not in dir2
#' @param first.lib.df first library data frame
#' @param second.lib.df second library data frame
#' @noRd
find.library.changes <- function (first.lib.df, second.lib.df) {
	# case: input data frame(s) do(es) not exist, stop the function immediately
	if (FALSE == check.df.existence("Library", first.lib.df, second.lib.df)) {
		return(NULL)
	}

	# rare case: no libraries were recorded by provParseR
	if (FALSE == check.df.empty("library", first.lib.df, second.lib.df)) {
		return(NULL)
	}

	# the input data frames have 3 columns: id, name, version
	# removes unneccesary id rows
	first.lib.df <- subset(first.lib.df, select = -1)
	second.lib.df <- subset(second.lib.df, select = -1)

	# find library differences
	# join two data frames by same "name"
	same.name.libs.df <- dplyr::inner_join(first.lib.df, second.lib.df, by = "name")
	# detect differences in corresponding version values of each lib name
	lib.difference.df <- same.name.libs.df[same.name.libs.df$version.x != same.name.libs.df$version.y, ]
	# rename the columns for easier reading 
	colnames(lib.difference.df) <- c("name", "dir1.version", "dir2.version")

	# find libraries in dir2 but not in dir1
	# get rows in 2nd df but not in 1st df
	lib.dir2.df <- dplyr::anti_join(second.lib.df, first.lib.df, by = "name")

	# find libraries in dir1 but not in dir2
	# get rows in 1st df but not in 2nd df
	lib.dir1.df <- dplyr::anti_join(first.lib.df, second.lib.df, by = "name")

	return (list(lib.difference.df, lib.dir2.df, lib.dir1.df))
}

#' print.environment.changes gets environment changes by calling
#' helper method find.environment.changes and prints out the result
#' @param first.env.df first environment data frame
#' @param second.env.df second environment data frame 
#' @noRd
print.environment.changes <- function(first.env.df, second.env.df) {
	cat ("\n\nENVIRONMENT CHANGES: ")
	env.change.list <- find.environment.changes(first.env.df, second.env.df)

	# as.data.frame returns an empty data frame if the given data frame is null
	# so no need to handle null case here
	env.difference.df <- as.data.frame(env.change.list[1])
	env.dir2.df <- as.data.frame(env.change.list[2])
	env.dir1.df <- as.data.frame(env.change.list[3])

	cat("\nValue differences: ") 
	# prints out the update 
	if(nrow(env.difference.df) == 0){
		cat("No differences have been detected")
	}else{
		for(i in 1:nrow(env.difference.df)){
			cat(paste("\nAttribute:", env.difference.df$label[i]))
			cat(paste("\n### dir1 value:", env.difference.df$dir1.value[i]))
			cat(paste("\n### dir2 value:", env.difference.df$dir2.value[i]))
			cat("\n")
		}
	}

	# rare case: environment factors in dir2 but not in dir1,
	# only prints out when found such factor
	if(nrow(env.dir2.df) != 0){
		cat("\nAttributes in dir2 but not in dir1:\n")
		print.data.frame(env.dir2.df, row.names = FALSE)
	}

	# rare case: environment factors in dir1 but not in dir2,
	# only prints out when found such factor
	if(nrow(env.dir1.df) != 0){
		cat("\nAttributes in dir1 but not in dir2:\n")
		print.data.frame(env.dir1.df, row.names = FALSE)
	}
}

#' find.environment.changes detects changes in the environment in which 
#' the provenance was collected.
#' The method returns a list of 3 main information:
#' environment value changes, any environment factors only in dir2 and
#' any environment factors only in dir1
#' @param first.env.df first environment data frame
#' @param second.env.df second environment data frame 
#' @noRd
find.environment.changes <- function (first.env.df, second.env.df) {
	# case: input data frame(s) do(es) not exist
	if (FALSE == check.df.existence("Environment", first.env.df, second.env.df)) {
		return(NULL)
	}

	# rare case: no environment factors were recorded by provParseR, returns immediately
	if (FALSE == check.df.empty("environment factor", first.env.df, second.env.df)) {
		return(NULL)
	}

	# clean data frames of environment factors
	first.env.df <- clean.environment.df(first.env.df)
	second.env.df <- clean.environment.df(second.env.df)

	# find environment changes
	# join two data frames by "label"
	same.label.env.df <- dplyr::inner_join(first.env.df, second.env.df, by = "label")
	env.difference.df <- same.label.env.df[same.label.env.df$value.x != same.label.env.df$value.y, ]
	# rename the columns for easier reading
	colnames(env.difference.df) <- c("label", "dir1.value", "dir2.value")

	# find environment values in dir2 but not in dir1
	dir2.env.df <- dplyr::anti_join(second.env.df, first.env.df, by = "label")

	# find environment values in dir1 but not in dir2
	dir1.env.df <- dplyr::anti_join(first.env.df, second.env.df, by = "label")

	return (list(env.difference.df, dir2.env.df, dir1.env.df))
}


#' clean.environment.df reads an environment data frame, remove
#' rows with name "script" and "scriptTimeStamp", then rename 
#' some environment factors into helpful message for later printing 
#' @param env.df environment data frame 
#' @noRd
clean.environment.df <- function(env.df) {
	# remove script and script timestamp rows
	env.df <- env.df[env.df$label != "script", ]
	env.df <- env.df[env.df$label != "scriptTimeStamp", ]

	# rename some of the environment factors
	env.df[env.df$label == "operatingSystem", "label"] <- "operating system"
	env.df[env.df$label == "langVersion", "label"] <- "language version"
	env.df[env.df$label == "workingDirectory", "label"] <- "working directory"
	env.df[env.df$label == "provDirectory", "label"] <- "provenance directory"
	env.df[env.df$label == "provTimestamp", "label"] <- "provenance collection time"
	env.df[env.df$label == "hashAlgorithm", "label"] <- "hash algorithm"
	env.df[env.df$label == "totalElapsedTime", "label"] <- "total elapsed time"
	return(env.df)
}


#' print.prov.tool.changes gets prov tool changes by calling
#' a helper method find.prov.tool.changes and prints out the result
#' @param first.tool.df first tool data frame
#' @param second.tool.df second tool data frame
#' @noRd
print.prov.tool.changes <- function (first.tool.df, second.tool.df) {
	cat ("\n\nPROVENANCE TOOL CHANGES: ")
	tool.change.list <- find.prov.tool.changes(first.tool.df, second.tool.df)
	# if the list returned is null, as.data.frame creates an empty data frame,
	# so no need to handle null case here
	tool.difference.df <- as.data.frame(tool.change.list[1])
	dir2.tool.df <- as.data.frame(tool.change.list[2])
	dir1.tool.df <- as.data.frame(tool.change.list[3])

	cat("\nTool differences: ")
	# prints out the update 
	if(nrow(tool.difference.df) == 0){
		cat("\nNo differences have been detected")
	}else{
		for(i in 1:nrow(tool.difference.df)){
			cat(paste("\nName:", tool.difference.df$tool.name[i]))
			cat(paste("\n### dir1 tool version:", tool.difference.df$dir1.tool.version[i], 
				"; dir1 json version:", tool.difference.df$dir1.json.version[i]))
			cat(paste("\n### dir2 tool version:", tool.difference.df$dir2.tool.version[i], 
				"; dir2 json version:", tool.difference.df$dir2.json.version[i]))
			cat("\n")
		}
	}

	# case: tool in dir2 but not in dir1 (for example one used rdt, the other used rdtLite)
	if(nrow(dir2.tool.df) != 0){
		cat ("\nTool in dir2 but not in dir1:\n")
		print.data.frame(dir2.tool.df, row.names = FALSE)
	}

	# case: tool in dir1 but not in dir2
	if(nrow(dir1.tool.df) != 0){
		cat("\nTool in dir1 but not in dir2:\n")
		print.data.frame(dir1.tool.df, row.names = FALSE)
	}
}

#' prov.tool.changes checks for changes in provenance tool:
#' currently rdt or rdtLite 
#' @param first.tool.df first tool data frame
#' @param second.tool.df second tool data frame
#' @noRd
find.prov.tool.changes <- function (first.tool.df, second.tool.df) {
	# case: input data frame(s) do(es) not exist, returns immediately
	if (FALSE == check.df.existence("Provenance tool", first.tool.df, second.tool.df)){
		return (NULL)
	}

	# rare case: no provenance tools are shown in the data frame, returns immediately
	if (FALSE == check.df.empty("provenance tool", first.tool.df, second.tool.df)){
		return(NULL)
	}

	# finds the tool and json versions in each provenance
	same.tool.df <- dplyr::inner_join(first.tool.df, second.tool.df, by = "tool.name")
	colnames(same.tool.df) <- c("tool.name", "dir1.tool.version", "dir1.json.version", 
		"dir2.tool.version", "dir2.json.version")
	same.tool.df <- same.tool.df[ , c(1, 2, 4, 3, 5)] # swaps column for nicer output

	# case: if there are no updates, returns an empty data frame 
	if (same.tool.df$dir1.tool.version == same.tool.df$dir2.tool.version
		&& same.tool.df$dir1.json.version == same.tool.df$dir2.json.version){
		same.tool.df <- data.frame()
	}

	# case: for future updates, show tools that are in dir2 but not in dir1
	dir2.tool.df <- dplyr::anti_join(second.tool.df, first.tool.df, by = "tool.name")
	
	# case: for future updates, show tools that are in dir1 but not in dir2
	dir1.tool.df <- dplyr::anti_join(first.tool.df, second.tool.df, by = "tool.name")

	return (list(same.tool.df, dir2.tool.df, dir1.tool.df))
}

#' print.script.changes finds differences between 2 R scripts 
#' and calls other helper printing functions to display the changes
#' @param first.script.df first script data frame
#' @param second.script.df second script data frame
#' @param dir1 path of first provenance directory
#' @param dir2 path of second provenance directory 
#' @noRd
print.script.changes <- function(first.script.df, second.script.df, dir1, dir2) {
	cat("\n\nSCRIPT CHANGES: ")

	# check the existence of the 2 data frames
	if(FALSE == check.df.existence("Script", first.script.df, second.script.df)){
		cat("\nNA")
		return(NULL)
	}

	# rare case : no scripts are recorded in the data frame
	if(FALSE == check.df.empty("script", first.script.df, second.script.df)){
		cat("\nNA")
		return(NULL)
	}

	script.change.list <- find.script.changes(first.script.df, second.script.df, dir1, dir2)
	main.script.change.result <- as.double(script.change.list[1])
	sourced.script.change.list <- script.change.list[[2]]

	# prints out the result
	print.main.script.change(main.script.change.result, first.script.df[1, ], second.script.df[1, ])
	print.same.name.sourced.scripts(same.name.script.df = sourced.script.change.list[[1]])
	print.renamed.sourced.scripts(renamed.script.df = sourced.script.change.list[[2]])
	print.unmatched.sourced.scripts(status = "dir1", sourced.script.change.list[[3]])
	print.unmatched.sourced.scripts(status = "dir2", sourced.script.change.list[[4]])
}

#' print.main.script.change prints out changes in main script
#' based on the status value returned by compare.main.script method.
#' The message should display the both name if the script got 
#' renamed, whether the content of the script has changed, and the 
#' timestamp for each script version
#' @param main.script.change.result given status value
#' @param first.main.script.df first data frame with only main script
#' @param second.main.script.df second data frame with only main script
#' @noRd
print.main.script.change <- function(main.script.change.result, first.main.script.df, second.main.script.df){
	# extract the name of main scripts from a full path 
	first.main.script.df$script <- basename(first.main.script.df$script)
	second.main.script.df$script <- basename(second.main.script.df$script)

	renamed <- FALSE
	# case: script got renamed
	if(main.script.change.result == 1 || main.script.change.result == 2){
		cat("\nMain script has different name")
		cat(paste("\n### dir1 main script name:", first.main.script.df$script))
		cat(paste("\n### dir2 main script name:", second.main.script.df$script))
		renamed <- TRUE
	}

	# case: the content of script changed
	if(main.script.change.result == 1 || main.script.change.result == 0){
		msg <- "\nThe content of the main script"
		# case: if script was not renamed, prints out the name of the script along with the message
		if(FALSE == renamed){
			cat(paste(msg, second.main.script.df$script, "has changed"))
		}else{
			cat(paste(msg, "has changed"))
		}
	}else{ # case: the content is not changed (value 2 or 3)
		msg <- "\nNo change detected in the content of the main script"
		# case: if script was not renamed, prints out the name of the script along with the message
		if(FALSE == renamed){
			cat(paste(msg, second.main.script.df$script))
		}else{
			cat(msg)
		}
	}
		
	cat(paste("\n### dir1 main script", first.main.script.df$script, "was last modified at:", first.main.script.df$timestamp))
	cat(paste("\n### dir2 main script", second.main.script.df$script, "was last modified at:", second.main.script.df$timestamp))
	cat("\n")
}

#' find.script.changes find changes in both main and sourced scripts.
#' The method calls other helper functions to get full path of each 
#' script in the provenance directories, generate hash value for each 
#' script, find changes in main and sourced scripts, then returns a list
#' containing comparison results of main script and sourced scripts. 
#' @param first.script.df first data frame with main and sourced scripts
#' @param second.script.df second data frame with main and sourced scripts
#' @param dir1 path to first provenance directory
#' @param dir2 path to second provenance directory
#' @noRd
find.script.changes <- function(first.script.df, second.script.df, dir1, dir2) {
	# get right paths for copied scripts located in the provenance folders
	first.script.df <- get.copied.script.path(dir1, first.script.df)
	second.script.df <- get.copied.script.path(dir2, second.script.df)

	# generate hash value for each script in the data frame
	first.script.df <- compute.script.hash.value(first.script.df)
	second.script.df <- compute.script.hash.value(second.script.df)

	#find script changes
	main.script.result <- compare.main.script(first.script.df[1, ], second.script.df[1, ])
	sourced.script.result.list <- compare.sourced.scripts(first.script.df[-1, ], second.script.df[-1, ])
	return (list(main.script.result, sourced.script.result.list))
}

#' compare.main.script find changes in the content and name of main script
#' The function returns 4 values reprensenting 4 status:
#' 0 = different script, same name
#' 1 = different script, different name
#' 2 = same script, different name
#' 3 = same script, same name 
#' @param first.main.script.df first data frame which contains only main script
#' @param second.main.script.df second data frame which contains only main script
#' @noRd
compare.main.script <- function(first.main.script.df, second.main.script.df) {
	# extract the name of main scripts from a full path 
	first.main.script.df$script <- basename(first.main.script.df$script)
	second.main.script.df$script <- basename(second.main.script.df$script)

	if(first.main.script.df$hashValue != second.main.script.df$hashValue
		&& first.main.script.df$script == second.main.script.df$script){
		return (0)
	}

	if(first.main.script.df$hashValue != second.main.script.df$hashValue
		&& first.main.script.df$script != second.main.script.df$script){
		return (1)
	}

	if(first.main.script.df$hashValue == second.main.script.df$hashValue
		&& first.main.script.df$script != second.main.script.df$script){
		return (2)
	}

	if(first.main.script.df$hashValue == second.main.script.df$hashValue
		&& first.main.script.df$script == second.main.script.df$script){
		return (3)
	}
}

#' compare.sourced.scripts detects changes in sourced script data frames.
#' The method returns a list of 4 data frames:
#' 1. data frame containing scripts with same name
#' 2. data frame containing renamed scripts with same hash values
#' 3. data frame containing unmatched scripts in the first prov version
#' 4. data frame containing unmatched scripts in the second prov version
#' Note: this method replaces full script path with script name. In other words,
#' returned data frames now contain only script name, not full script path
#' @param first.sourced.script.df first data frame containing only sourced scripts
#' @param second.sourced.script.df second data frame containing only sourced scripts
#' @noRd
compare.sourced.scripts <- function(first.sourced.script.df, second.sourced.script.df) {
	# extract the names of each script from their full path
	first.sourced.script.df$script <- basename(first.sourced.script.df$script)
	second.sourced.script.df$script <- basename(second.sourced.script.df$script)

	# case: no sourced scripts were used in both prov versions
	if(nrow(first.sourced.script.df) == 0 && nrow(second.sourced.script.df) == 0){
		return (list(data.frame(), data.frame(), data.frame(), data.frame()))
	}

	# case: scripts with same name (same or different hash values)
	same.name.script.df <- dplyr::inner_join(first.sourced.script.df, second.sourced.script.df, by = "script")
	colnames(same.name.script.df) <- c("script", "dir1.timestamp", "dir1.hashValue", "dir2.timestamp", "dir2.hashValue")

	# case: scripts with different name but with same hash values (scripts got renamed)
	first.different.name.script.df <- dplyr::anti_join(first.sourced.script.df, second.sourced.script.df, by = "script")
	second.different.name.script.df <- dplyr::anti_join(second.sourced.script.df, first.sourced.script.df, by = "script")
	renamed.script.df <- dplyr::inner_join(first.different.name.script.df, second.different.name.script.df, by = "hashValue")
	colnames(renamed.script.df) <- c("dir1.script", "dir1.timestamp", "hashValue", "dir2.script", "dir2.timestamp")

	# case: scripts with different name and different hash values
	first.unmatched.script.df <- dplyr::anti_join(first.different.name.script.df, second.different.name.script.df, by = "hashValue")
	second.unmatched.script.df <- dplyr::anti_join(second.different.name.script.df, first.different.name.script.df, by = "hashValue")

	return(list(same.name.script.df, renamed.script.df, first.unmatched.script.df, second.unmatched.script.df))
}

#' print.same.name.sourced.scripts takes in a script data frame 
#' with same names and reports to users if the scripts has changed 
#' with the corresponding timestamp
#' @param same.name.script.df a same-name script data frame 
#' @noRd
print.same.name.sourced.scripts <- function(same.name.script.df) {
	if(FALSE == is.valid.script.df("same-name scripts", same.name.script.df)) {
		return ("\nNA")
	}

	# case: data frame must be non-empty
	if(nrow(same.name.script.df) != 0) {
		# extract rows with different hash values
		modified.script.df <- dplyr::filter(same.name.script.df, same.name.script.df$dir1.hashValue != same.name.script.df$dir2.hashValue)
		if(nrow(modified.script.df) != 0){
			for(i in 1:nrow(modified.script.df)){
				cat(paste("\nSourced script", modified.script.df$script[i], "has changed"))
			  	cat(paste("\n### dir1", modified.script.df$script[i], "was last modified at:", modified.script.df$dir1.timestamp[i]))
			  	cat(paste("\n### dir2", modified.script.df$script[i], "was last modified at:", modified.script.df$dir2.timestamp[i]))
			}
		}

		# extract rows with same hash values
		identical.script.df <- dplyr::filter(same.name.script.df, same.name.script.df$dir1.hashValue == same.name.script.df$dir2.hashValue)
		if(nrow(identical.script.df) != 0){
			for(i in 1:nrow(identical.script.df)){
				cat(paste("\nNo change detected in sourced script", identical.script.df$script[i]))
			}
		}
	}
}

#' print.renamed.sourced.scripts takes in a data frame 
#' of renamed scripts and reports changes to users
#' with the corresponding timestamp
#' @param renamed.script.df a data frame of renamed scripts
#' @noRd
print.renamed.sourced.scripts <- function(renamed.script.df) {
	if(FALSE == is.valid.script.df("renamed scripts", renamed.script.df)) {
		return ("\nNA")
	}

	# case : data frame must be non-empty 
	if(nrow(renamed.script.df) != 0) {
		for(i in 1:nrow(renamed.script.df)) {
			cat(paste("\nSourced script has same content but different names:"))
			cat(paste("\n### dir1 sourced script name:", renamed.script.df$dir1.script[i]))
			cat(paste("\n### dir2 sourced script name:", renamed.script.df$dir2.script[i]))
			cat(paste("\n###", renamed.script.df$dir1.script[i], "was last modified at:", renamed.script.df$dir1.timestamp[i]))
			cat(paste("\n###", renamed.script.df$dir2.script[i], "was last modified at:", renamed.script.df$dir2.timestamp[i]))
			cat("\n")
		}
	}
}

#' print.unmatched.sourced.scripts takes in a data frame
#' of unmatched scripts, reports changes to users based on 
#' the given status (either dir1 or dir2)
#' @param status dir1 or dir2
#' @param unmatched.script.df data frame of unmatched scripts
#' @noRd
print.unmatched.sourced.scripts <- function(status, unmatched.script.df) {
	if(FALSE == is.valid.script.df(aspect = paste(status, "unmatched scripts"), script.df = unmatched.script.df)) {
		return ("\nNA")
	}

	# case: data frame must be non-empty
	if(nrow(unmatched.script.df) != 0) {
		if(status == "dir1") {
			cat("\nSourced scripts in dir1 but not in dir2:")
		}else if (status == "dir2"){
			cat("\nSourced scripts in dir2 but not in dir1:")
		}

		for(i in 1:nrow(unmatched.script.df)) {
			cat(paste("\n### ", unmatched.script.df$script[i], ", which was last modified at: ", unmatched.script.df$timestamp[i], sep = ""))
		}
	}
}

#' is.valid.script.df is a helper function for script printing functions.
#' This checks the existence and type of the given script data frame
#' @param aspect what the script data frame is about
#' @param script.df the script data frame
#' @noRd
is.valid.script.df <- function(aspect, script.df) {
	if(is.null(script.df)){
		warning(paste("data frame of", aspect, "is NULL\n"))
		return (FALSE)
	}

	if(FALSE == is.data.frame(script.df)){
		warning(paste("argument is not a data frame, aspect = ", aspect, "\n", sep = ""))
		return (FALSE)
	}
	return (TRUE)
}

#' compute.script.hash.value generates hash value for each script
#' based on their path in the provenance directory and store these 
#' values in a new column in the given data frame
#' @param script.df data frame with scripts
#' @noRd 
compute.script.hash.value <- function(script.df) {
	hash.values.vector <- sapply(script.df$script, FUN = function(X){
		digest::digest(file = X, algo = "md5")
	})

	script.df$hashValue <- hash.values.vector
	return (script.df)
}


#' get.copied.script.path takes in the path of the provenance directory and
#' a data frame about the locations of the original scripts, then returns
#' a data frame with exact locations of the scripts in the provenance directory,
#' while preserving all columns in the original data frame
#' @param prov.dir provenance directory
#' @param origin.script.df original script data frame
#' @noRd
get.copied.script.path <- function(prov.dir, origin.script.df) {
	# extract script names
	origin.script.df$script <- basename(origin.script.df$script)
	origin.script.df$script <- sapply(origin.script.df$script, insert.path <- function(script.name){
		script.name <- paste(prov.dir, "/scripts/", script.name, sep = "")
	})
	return (origin.script.df)
}

#' check.dir.existence checks if two given directories exists
#' and stops the program when the directories are non-existent
#' @param dir1 the first directory
#' @param dir2 the second directory
#' @noRd
check.dir.existence <- function (dir1, dir2) {
	error.message <- ""
	if (!dir.exists(dir1)) {
		error.message <- paste(dir1, "directory not found\n")
	}

	if (!dir.exists(dir2)) {
		error.message <- paste(error.message, dir2, " directory not found\n", sep = "")
	}

	if (error.message != "") {
		stop(error.message)
	}
	return (error.message)
}

#' get.prov.info.object accesses the JSON file from the given directory,
#' and returns a ProvInfo object from provParseR
#' @param directory the provenance directory
#' @return ProvInfo object
#' @noRd
get.prov.info.object <- function (directory) {
	# gets the path of two json files
	json.file <- paste(directory, "/prov.json", sep = "")

	# case: json file does not exist
	if(!file.exists(json.file)){
		stop(paste("prov.json file in ", directory, "is not found\n"))
	}

	# returns the ProvInfo object returned by provParseR
	return (provParseR::prov.parse(json.file))
}

#' check.df.existence checks if two given data frames are not null. 
#' If one of them are null, outputs a warning, returns false to 
#' stop the caller of given aspect and continue the program.
#' Otherwise, returns true 
#' @param aspect overview of what the data frames are about
#' @param df1 first data frame
#' @param df2 second data frame
#' @noRd
check.df.existence <- function (aspect, df1, df2) {
	if(is.null(df1) || is.null(df2)){
		warning (paste(aspect, "data frames returned by provParseR is NULL\n"))
		return (FALSE)
	}
	return (TRUE)
}

#' check.df.empty checks if two given data frames are empty.
#' If one of them are null, outputs a warning, returns false to
#' stop the caller of given aspect and continue the program.
#' Otherwise, returns true
#' @param aspect overview of what the data frames are about
#' @param df1 first data frame
#' @param df2 second data frame
#' @noRd
check.df.empty <- function (aspect, df1, df2) {
	if(nrow(df1) == 0 || nrow(df2) == 0){
		warning (paste("no", aspect, "was recorded in data frame returned by provParseR\n"))
		return (FALSE)
	}
	return (TRUE)
}



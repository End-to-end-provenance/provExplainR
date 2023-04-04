# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2019, 2020, 2021, 2022.

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

#' Provenance comparison functions
#' 
#' prov.explain reads two provenance collections and finds differences between these two versions.
#'
#' prov.explain and prov.diff.script are intended to help a user determine what
#' has changed if multiple executions of a script lead to different results.  prov.explain
#' does this by comparing provenance collected using the rdtLite or rdt packages.  
#' prov.diff.script compares copies of the R scripts saved in provenance directories
#' at the time that the scripts were executed. 
#' 
#' The types of differences that prov.explain can find include:
#' \itemize{
#'   \item Environmental information identifying when the scripts were executed, the version of R,
#' 		the computing systems, the tool and version used to collect the provenance, the location
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
#' @rdname explain
prov.explain <- function (dir1, dir2, testing = FALSE, save = FALSE){
	# check the existence of two given directories
	check.dir.existence(dir1, dir2)

	# case: two directories are the same
	if (dir1 == dir2){
		warning (paste(dir1, "and", dir2, "are the same directories\n"))
		return (NA)
	}
  if (testing) {
    first.prov.info <- get.prov.info.object(dir1)
    second.prov.info <- get.prov.info.object(dir2)
    return(get.data.differences(first.prov.info, second.prov.info))
#    get.error.differences(provParseR::get.error.nodes(first.prov.info), provParseR::get.error.nodes(second.prov.info))
  }

	# detecting changes 
	if(save == TRUE){
		save.to.text.file(dir1, dir2)
	}else{
		detect.changes(dir1, dir2, compare_errors)
	}
}

#' Provenance Script Diff function
#' 
#' prov.diff.script visualizes the differences between two versions of a script 
#' that were previously executed.
#' 
#' The prov.diff.script compares two versions of a script.
#' Users must specify the name of the first script, 
#' the provenance directory associated with the first execution of
#' the script, and the provenance directory associated with the second execution of
#' the script. The name of the second script is optional. If it
#' is omitted, the same script name is looked for in the second provenance
#' directory
#' @param dir1 path to first provenance directory
#' @param dir2 path to second provenance directory
#' @param first.script name of first script.  If no value is passed in, it will use the
#'    main script
#' @param second.script name of second script.  If both first and second script name
#'    are NULL, it will use the main script form the second directory.  If second
#'    script name is NULL, but first script name is not, it will use first script name.
#' @export
#' @rdname explain
prov.diff.script <- function(dir1, dir2, first.script = NULL, second.script = NULL) {
	# check the existence of two given directories
	check.dir.existence(dir1, dir2)

	# extract script name and change paths to first script saved in prov folders
	if (FALSE == is.null(first.script)){
	    use.main.script <- FALSE
		first.script <- basename(first.script)
	}
	else {
		use.main.script <- TRUE
		first.prov.info <- get.prov.info.object(dir1)
		environment <- provParseR::get.environment(first.prov.info)
		script.path <- environment[environment$label == "script", ]$value
		first.script <- basename(script.path)
	}
	first.full.script <- paste(dir1, "/scripts/", first.script, sep = "")
	
	# extract script name and change paths to second script saved in prov folders
	if(FALSE == is.null(second.script)){
		second.script <- basename(second.script)
	}else{
	    if (use.main.script) {
			second.prov.info <- get.prov.info.object(dir2)
			environment <- provParseR::get.environment(second.prov.info)
			script.path <- environment[environment$label == "script", ]$value
			second.script <- basename(script.path)
	    }
	    else {
			second.script <- first.script
		}
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
detect.changes <- function (dir1, dir2, compare_errors){
	cat("\nYou entered:\ndir1 =", dir1, "\ndir2 =", dir2)

	# gets the ProvInfo objects
	first.prov.info <- get.prov.info.object(dir1)
	second.prov.info <- get.prov.info.object(dir2)

	# detect changes in different aspects
	get.script.changes (provParseR::get.scripts(first.prov.info), provParseR::get.scripts(second.prov.info), dir1, dir2)
	get.library.changes (provParseR::get.libs(first.prov.info), provParseR::get.libs(second.prov.info))
	get.input.files.changes (provParseR::get.input.files(first.prov.info), provParseR::get.input.files(second.prov.info))
	get.environment.changes (provParseR::get.environment(first.prov.info), provParseR::get.environment(second.prov.info))
	get.prov.tool.changes (provParseR::get.tool.info(first.prov.info), provParseR::get.tool.info(second.prov.info))
	get.data.differences(first.prov.info, second.prov.info)
	
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
    sink(explain.file, split=TRUE)
	detect.changes(dir1, dir2)
    sink()
	message(paste("\n\nSaving comparison results in", explain.file))
}

#' get.library.changes gets changes in library by calling a helper
#' method find.library.changes, and prints out the result
#' @param first.lib.df first library data frame
#' @param second.lib.df second library data frame
#' @noRd
get.library.changes <- function (first.lib.df, second.lib.df){
	cat("\nLIBRARY CHANGES: ")
	# get the list of changes
	lib.change.list <- find.library.changes(first.lib.df, second.lib.df)

	# if list is null, R returns empty (non-NULL) data frames
	lib.difference.df <- as.data.frame(lib.change.list[1])
	lib.dir2.df <- as.data.frame(lib.change.list[2])
	lib.dir1.df <- as.data.frame(lib.change.list[3])

	cat("\nLibrary version differences:\n")
	if(nrow(lib.difference.df) == 0){
		cat("No differences in library versions have been detected")
	}else{
		print.data.frame(lib.difference.df, row.names = FALSE)
	}

	cat("\nLibraries in dir2 but not in dir1:\n")
	if(nrow(lib.dir2.df) == 0){
		cat("No such libraries were found")
	}else{
		print.data.frame(lib.dir2.df, row.names = FALSE)
	}

	cat("\nLibraries in dir1 but not in dir2:")
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

	# keep the 3 columns we are interested in: id, name, version
	first.lib.df <- subset(first.lib.df, select = c("id", "name", "version"))
	second.lib.df <- subset(second.lib.df, select = c("id", "name", "version"))

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

#' get.environment.changes gets environment changes by calling
#' helper method find.environment.changes and prints out the result
#' @param first.env.df first environment data frame
#' @param second.env.df second environment data frame 
#' @noRd
get.environment.changes <- function(first.env.df, second.env.df) {
	cat("\nENVIRONMENT CHANGES: ")
	env.change.list <- find.environment.changes(first.env.df, second.env.df)

	# as.data.frame returns an empty data frame if the given data frame is null
	# so no need to handle null case here
	env.difference.df <- as.data.frame(env.change.list[1])
	env.dir2.df <- as.data.frame(env.change.list[2])
	env.dir1.df <- as.data.frame(env.change.list[3])

	cat("Value differences: \n") 
	# prints out the update 
	if(nrow(env.difference.df) == 0){
		cat("No differences have been detected")
	}else{
		for(i in 1:nrow(env.difference.df)){
			cat("Attribute:", env.difference.df$label[i], "\n")
			cat("### dir1 value:", env.difference.df$dir1.value[i], "\n")
			cat("### dir2 value:", env.difference.df$dir2.value[i], "\n")
		}
	}

	# rare case: environment factors in dir2 but not in dir1,
	# only prints out when found such factor
	if(nrow(env.dir2.df) != 0){
		cat("Attributes in dir2 but not in dir1:\n")
		print.data.frame(env.dir2.df, row.names = FALSE)
	}

	# rare case: environment factors in dir1 but not in dir2,
	# only prints out when found such factor
	if(nrow(env.dir1.df) != 0){
		cat("Attributes in dir1 but not in dir2:\n")
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
get.prov.tool.changes <- function (first.tool.df, second.tool.df) {
	cat("\nPROVENANCE TOOL CHANGES: ")
	tool.change.list <- find.prov.tool.changes(first.tool.df, second.tool.df)
	# if the list returned is null, as.data.frame creates an empty data frame,
	# so no need to handle null case here
	tool.difference.df <- as.data.frame(tool.change.list[1])
	dir2.tool.df <- as.data.frame(tool.change.list[2])
	dir1.tool.df <- as.data.frame(tool.change.list[3])

	cat("Tool differences: ")
	# prints out the update 
	if(nrow(tool.difference.df) == 0){
		cat("No differences have been detected")
	}else{
		for(i in 1:nrow(tool.difference.df)){
			cat("Name:", tool.difference.df$tool.name[i])
			cat("### dir1 tool version:", tool.difference.df$dir1.tool.version[i], 
				"; dir1 json version:", tool.difference.df$dir1.json.version[i])
			cat("### dir2 tool version:", tool.difference.df$dir2.tool.version[i], 
				"; dir2 json version:", tool.difference.df$dir2.json.version[i])
		}
	}

	# case: tool in dir2 but not in dir1 (for example one used rdt, the other used rdtLite)
	if(nrow(dir2.tool.df) != 0){
		cat("Tool in dir2 but not in dir1:\n")
		print.data.frame(dir2.tool.df, row.names = FALSE)
	}

	# case: tool in dir1 but not in dir2
	if(nrow(dir1.tool.df) != 0){
		cat("Tool in dir1 but not in dir2:\n")
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
get.script.changes <- function(first.script.df, second.script.df, dir1, dir2) {
	cat("\nSCRIPT CHANGES: ")

	# check the existence of the 2 data frames
	if(FALSE == check.df.existence("Script", first.script.df, second.script.df)){
		cat("NA")
		return()
	}

	# rare case : no scripts are recorded in the data frame
	if(FALSE == check.df.empty("script", first.script.df, second.script.df)){
		cat("\nNA")
		return()
	}

	script.change.list <- find.script.changes(first.script.df, second.script.df, dir1, dir2)
	main.script.change.result <- as.double(script.change.list[1])
	sourced.script.change.list <- script.change.list[[2]]

	# prints out the result
	get.main.script.change(main.script.change.result, first.script.df[1, ], second.script.df[1, ])
	get.same.name.sourced.scripts(same.name.script.df = sourced.script.change.list[[1]])
	get.renamed.sourced.scripts(renamed.script.df = sourced.script.change.list[[2]])
	get.unmatched.sourced.scripts(status = "dir1", sourced.script.change.list[[3]])
	get.unmatched.sourced.scripts(status = "dir2", sourced.script.change.list[[4]])

}

#' get.main.script.change prints out changes in main script
#' based on the status value returned by compare.main.script method.
#' The message should display the both name if the script got 
#' renamed, whether the content of the script has changed, and the 
#' timestamp for each script version
#' @param main.script.change.result given status value
#' @param first.main.script.df first data frame with only main script
#' @param second.main.script.df second data frame with only main script
#' @noRd
get.main.script.change <- function(main.script.change.result, first.main.script.df, second.main.script.df){
	# extract the name of main scripts from a full path 
	first.main.script.df$script <- basename(first.main.script.df$script)
	second.main.script.df$script <- basename(second.main.script.df$script)

	renamed <- FALSE
	msg = character()
	# case: script got renamed
	if(main.script.change.result == 1 || main.script.change.result == 2){
		msg = c(msg, "Main script has different name")
		msg = c(msg, paste("### dir1 main script name:", first.main.script.df$script))
		msg = c(msg, paste("### dir2 main script name:", second.main.script.df$script))
		renamed <- TRUE
	}

	# case: the content of script changed
	if(main.script.change.result == 1 || main.script.change.result == 0){
		# case: if script was not renamed, prints out the name of the script along with the message
		if(FALSE == renamed){
			msg = c(msg, paste ("The content of the main script", second.main.script.df$script, "has changed"))
		}else{
			msg = c(msg, "The content of the main script has changed")
		}
		msg = c(msg, "Run prov.diff.script to see the changes.")
	}else{ # case: the content is not changed (value 2 or 3)
		# case: if script was not renamed, prints out the name of the script along with the message
		if(renamed){
			msg = c(msg, "No change detected in the content of the main script")
		}
		else {
			msg = c(msg, paste ("No change detected in the content of the main script", second.main.script.df$script))
		}	
	}
		
	msg = c(msg, paste("### dir1 main script", first.main.script.df$script, "was last modified at:", first.main.script.df$timestamp))
	msg = c(msg, paste("### dir2 main script", second.main.script.df$script, "was last modified at:", second.main.script.df$timestamp))
	writeLines (msg)
	return (msg)
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

	# If there are stored hash values, use those.  Otherwise, generate hash value 
	# for each script to be sure the same algorithm is being used for both.
	if (first.script.df$hash != "" || second.script.df$hash != "") {
		first.script.df <- compute.script.hash.value(first.script.df)
		second.script.df <- compute.script.hash.value(second.script.df)
	}

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

	if(first.main.script.df$hash != second.main.script.df$hash
		&& first.main.script.df$script == second.main.script.df$script){
		return (0)
	}

	if(first.main.script.df$hash != second.main.script.df$hash
		&& first.main.script.df$script != second.main.script.df$script){
		return (1)
	}

	if(first.main.script.df$hash == second.main.script.df$hash
		&& first.main.script.df$script != second.main.script.df$script){
		return (2)
	}

	if(first.main.script.df$hash == second.main.script.df$hash
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

#' get.same.name.sourced.scripts takes in a script data frame 
#' with same names and reports to users if the scripts has changed 
#' with the corresponding timestamp
#' @param same.name.script.df a same-name script data frame 
#' @noRd
get.same.name.sourced.scripts <- function(same.name.script.df) {
	if(FALSE == is.valid.script.df("same-name scripts", same.name.script.df)) {
		cat ("NA")
		return ("NA")
	}
	
	msg = character()
	# case: data frame must be non-empty
	if(nrow(same.name.script.df) != 0) {
		# extract rows with different hash values
		modified.script.df <- dplyr::filter(same.name.script.df, same.name.script.df$dir1.hashValue != same.name.script.df$dir2.hashValue)
		if(nrow(modified.script.df) != 0){
			for(i in 1:nrow(modified.script.df)){
				msg = c(msg, paste("Sourced script", modified.script.df$script[i], "has changed"))
				msg = c(msg, "Run prov.diff.script to see the changes.")
			  	msg = c(msg, paste("### dir1", modified.script.df$script[i], "was last modified at:", modified.script.df$dir1.timestamp[i]))
			  	msg = c(msg, paste("### dir2", modified.script.df$script[i], "was last modified at:", modified.script.df$dir2.timestamp[i]))
			}
		}

		# extract rows with same hash values
		identical.script.df <- dplyr::filter(same.name.script.df, same.name.script.df$dir1.hashValue == same.name.script.df$dir2.hashValue)
		if(nrow(identical.script.df) != 0){
			for(i in 1:nrow(identical.script.df)){
				msg = c(msg, paste("No change detected in sourced script", identical.script.df$script[i]))
			}
		}
		writeLines (msg)
	}
	return (msg)
}

#' get.renamed.sourced.scripts takes in a data frame 
#' of renamed scripts and reports changes to users
#' with the corresponding timestamp
#' @param renamed.script.df a data frame of renamed scripts
#' @noRd
get.renamed.sourced.scripts <- function(renamed.script.df) {
	if(FALSE == is.valid.script.df("renamed scripts", renamed.script.df)) {
		cat ("NA")
		return("NA")
	}

	# case : data frame must be non-empty 
    changes <- character(0)
	if(nrow(renamed.script.df) != 0) {

		for(i in 1:nrow(renamed.script.df)) {
			changes <- c(changes, "Sourced script has same content but different names:")
			changes <- c(changes, paste("### dir1 sourced script name:", renamed.script.df$dir1.script[i]))
			changes <- c(changes, paste("### dir2 sourced script name:", renamed.script.df$dir2.script[i]))
			changes <- c(changes, paste("###", renamed.script.df$dir1.script[i], "was last modified at:", renamed.script.df$dir1.timestamp[i]))
			changes <- c(changes, paste("###", renamed.script.df$dir2.script[i], "was last modified at:", renamed.script.df$dir2.timestamp[i]))
			changes <- c (changes, "")
		}
		writeLines(changes)
	}
	return (changes)
}

#' get.unmatched.sourced.scripts takes in a data frame
#' of unmatched scripts, reports changes to users based on 
#' the given status (either dir1 or dir2)
#' @param status dir1 or dir2
#' @param unmatched.script.df data frame of unmatched scripts
#' @noRd
get.unmatched.sourced.scripts <- function(status, unmatched.script.df) {
	if(FALSE == is.valid.script.df(aspect = paste(status, "unmatched scripts"), script.df = unmatched.script.df)) {
		cat ("NA")
		return()
	}

	# case: data frame must be non-empty
	msg <- character()
	if(nrow(unmatched.script.df) != 0) {
		if(status == "dir1") {
			msg <- c(msg, "Sourced scripts in dir1 but not in dir2:")
		}else if (status == "dir2"){
			msg <- c(msg, "Sourced scripts in dir2 but not in dir1:")
		}

		for(i in 1:nrow(unmatched.script.df)) {
			msg <- c(msg, paste ("### ", unmatched.script.df$script[i], ", which was last modified at: ", unmatched.script.df$timestamp[i], sep = ""))
		}
		writeLines (msg)
	}
	return (msg)
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

	script.df$hash <- hash.values.vector
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

#' get.input.files.changes checks if input files in two data provenance
#' collections are different based on their hash values.
#' NOTE: this function currently does not work for URL as input.
#' @param input.df1 data frame of input files in first data provenance collection
#' @param input.df2 data frame of input files in second data provenance collection
#' @noRd
get.input.files.changes <- function (input.df1, input.df2) {
# QUESTION: are inputs in sourced scripts recorded?
	if(FALSE == check.df.existence("Input file", input.df1, input.df2)) {
		return ("")
	}

	cat("\n\nINPUT FILE CHANGES:\n")

	# process input files (not URL)
	input.files.df1 <- input.df1[input.df1$type == "File", ]
	input.files.df2 <- input.df2[input.df2$type == "File", ]

	empty <- FALSE
	if(nrow(input.files.df1) == 0) {
		cat("No input files were found in dir 1\n")
		empty <- TRUE
	}
	else {
		get.input.file.changes(input.files.df1, "1")
	}
	
	if(nrow(input.files.df2) == 0) {
		cat("No input files were found in dir 2\n")
		empty <- TRUE
	}
	else {
		get.input.file.changes(input.files.df2, "2")
	}
	
	if(empty) return()

	# if reached here, both data frames must have some input files
	# compare files with same name
	same.name.files.df <- dplyr::inner_join(input.files.df1, input.files.df2, by = "name")
	compare.input.files.same.name(same.name.files.df)

	# compare files with different name
	different.name.files.df1 <- dplyr::anti_join(input.files.df1, input.files.df2, by = "name")
	different.name.files.df2 <- dplyr::anti_join(input.files.df2, input.files.df1, by = "name")
	compare.input.files.different.name(different.name.files.df1, different.name.files.df2)

}

#' compare.input.files.same.name takes in a data frame of input files
#' of dir1 and dir2 that have the same name. The function then compares
#' the content of these files to see if they have been modified. If yes,
#' corresponding modification times are printed.
#' @param same.name.files.df data frame of input files with same names 
#' @noRd
compare.input.files.same.name <- function (same.name.files.df) {
	if(FALSE == is.null(same.name.files.df) && nrow(same.name.files.df) != 0) {
		# case: same hash value, thus no change detected
		same.hash.df <- dplyr::filter(same.name.files.df, same.name.files.df$hash.x == same.name.files.df$hash.y)
		if (nrow(same.hash.df) > 0) {
			for(i in 1:nrow(same.hash.df)) {
				cat("No change detected in the input file", same.hash.df$name[i])
			}
		}

		# case: different hash value, thus content changed
		different.hash.df <- dplyr::filter(same.name.files.df, same.name.files.df$hash.x != same.name.files.df$hash.y)
		if (nrow(different.hash.df) > 0) {
			for(i in 1:nrow(different.hash.df)) {
				row <- different.hash.df[i, ]
				cat("\nThe content of the input file", row$name, "has changed")
				cat("### dir1", row$name, "was last modified at:", row$timestamp.x)
				cat("### dir2", row$name, "was last modified at:", row$timestamp.y)
			}
		}
	}

}

#' compare.input.files.different.name takes in 2 data frames of input files:
#' one containing files with names in dir1 but not in dir2, the other containing 
#' files with names in dir2 but not in dir1. The function then compares
#' hash values of these files to see if there are some files that are the same but 
#' have been renamed. As for files with different names and different hash values,
#' the function simply returns information of the files to user.
#' @param different.name.files.df1 data frame of input files whose names are exclusive to dir1
#' @param different.name.files.df2 data frame of input files whose names are exclusive to dir2
#' @noRd
compare.input.files.different.name <- function(different.name.files.df1, different.name.files.df2) {
	# case: files with same content but different name
	same.hash.df <- dplyr::inner_join(different.name.files.df1, different.name.files.df2, by = "hash")
	if(FALSE == is.null(same.hash.df) && nrow(same.hash.df) != 0) {
		for(i in 1:nrow(same.hash.df)) {
			cat("Content of two input files", same.hash.df$name.x[i], "(dir1) and", same.hash.df$name.y[i], "(dir 2) is the same")
		}
	}

	# case: files with different hash values and names (dir1)
	exclusive.files1 <- dplyr::anti_join(different.name.files.df1, different.name.files.df2, by = "hash")
	if(FALSE == is.null(exclusive.files1) && nrow(exclusive.files1) != 0) {
		cat("\nInput files in dir1 but not in dir2:")
		for(i in 1:nrow(exclusive.files1)) {
			cat("### ", exclusive.files1$name[i], ", which was last modified at ", exclusive.files1$timestamp[i], sep = "")
		}
	}

	# case: files with different hash values and names (dir2)
	exclusive.files2 <- dplyr::anti_join(different.name.files.df2, different.name.files.df1, by = "hash")
	if(FALSE == is.null(exclusive.files2) && nrow(exclusive.files2) != 0) {
		cat("\nInput files in dir2 but not in dir1:")
		for(i in 1:nrow(exclusive.files2)) {
			cat("### ", exclusive.files2$name[i], ", which was last modified at ", exclusive.files2$timestamp[i], sep = "")
		}
	}

}

#' get.input.file.changes prints out all input files recorded in
#' a given data frame, corresponding to a folder/directory number. If the 
#' data frame is empty, a helpful message is returned to user. 
#' @param input.df data frame of input files
#' @param dir current working provenance directory
#' @noRd
get.input.file.changes <- function(input.df, dir) {
	cat("List of input files in dir", dir, ":\n", sep = "")
	for(i in 1:nrow(input.df)) {
		row <- input.df[i, ]
		cat("### Name:", row$name, "\n")
		cat("### Last modification time:", row$timestamp, "\n")
		cat("### Location:", row$location, "\n\n")
	}
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
#' If one of them is empty, outputs a warning, returns false to
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

#####################################################################################
#' IN DEVELOPMENT - PROCEDURE NODES

#' find.changes.proc.nodes inspects procedure nodes of both provenance 
#' collections and finds the first point in which the procedure node 
#' data frames start diverging. 
#' @param prov.info1 prov info object of dir1
#' @param prov.info2 prov info object of dir2
#' @noRd
find.changes.proc.nodes <- function (prov.info1, prov.info2) {
	# get the corresponding procedure node data frames
	proc.node.df1 <- provParseR::get.proc.nodes(prov.info1)
	proc.node.df2 <- provParseR::get.proc.nodes(prov.info2)

	# filter out Start and Finish nodes, only consider Operation nodes
	proc.node.df1 <- dplyr::filter(proc.node.df1, proc.node.df1$type == "Operation")
	proc.node.df2 <- dplyr::filter(proc.node.df2, proc.node.df2$type == "Operation")

	# get the script data frames 
	scripts.df1 <- provParseR::get.saved.scripts(prov.info1)
	scripts.df2 <- provParseR::get.saved.scripts(prov.info2)

	# check existence and emptiness status of the data frames
	if(FALSE == check.df.existence("Procedure node", proc.node.df1, proc.node.df2)
		|| FALSE == check.df.empty("procedure node", proc.node.df1, proc.node.df2)
		|| FALSE == check.df.existence("Saved script", scripts.df1, scripts.df2)
		|| FALSE == check.df.empty("saved script", scripts.df1, scripts.df2)) {
		return ()
	}

	# for each script in each provenance folder, get its array of lines 
	all.scripts.array1 <- get.array.of.arrays.of.lines(scripts.df1)
	all.scripts.array2 <- get.array.of.arrays.of.lines(scripts.df2)

	# loop through procedure nodes with the shorter length of the two data frames
	for(i in 1:min(nrow(proc.node.df1), nrow(proc.node.df2))){
		proc.node.info1 <- proc.node.df1[i, ]
		proc.node.info2 <- proc.node.df2[i, ]
		# get full info for the current proc nodes 
		detailed.info1 <- get.proc.node.full.info(proc.node.info1, all.scripts.array1)
		detailed.info2 <- get.proc.node.full.info(proc.node.info2, all.scripts.array2)

		# compare two pieces of information
		if(identical(detailed.info1, detailed.info2) == FALSE){
			cat("First procedure node differences found:")
			# helpful output here (subject to change)
			cat("### Line", proc.node.info1$startLine, "in script", basename(scripts.df1$script[proc.node.info1$scriptNum]))
			cat("### versus line", proc.node.info2$startLine, "in script", basename(scripts.df2$script[proc.node.info2$scriptNum]))
			return()
		}
	}

	# lines are identical so far. Compare lengths
	if(nrow(proc.node.df1) == nrow(proc.node.df2)) {
		cat("All procedure nodes of dir1 and dir2 are identical")
		return()
	}

	# lengths are different
	cat("First procedure node differences found:")
	if(nrow(proc.node.df1) > nrow(proc.node.df2)) {
		temp.proc <- proc.node.df1[nrow(proc.node.df2) + 1, ]
		temp.script <- scripts.df1
	}else{
		temp.proc <- proc.node.df2[nrow(proc.node.df1) + 1, ]
		temp.script <- scripts.df2
	}
	cat("### Line", temp.proc$startLine, "in script", basename(temp.script$script[temp.proc$scriptNum]))
	return()
}

#' get.proc.node.full.info gets the detailed lines about a given procedure node.
#' If there are many lines within 1 procedure node, all lines will be collapsed into 
#' one line. Returned character does not contain any white spaces
#' @param proc.node.info current procedure node extracted from the proc node data frame
#' @param all.scripts.full.line an array of arrays of lines of all scripts in a data provenance collection
#' @noRd
get.proc.node.full.info <- function(proc.node.info, all.scripts.full.line) {
	current.script.line.array <- all.scripts.full.line[[proc.node.info$scriptNum]]
	if(proc.node.info$startLine - proc.node.info$endLine == 0) {
		full.line <- current.script.line.array[proc.node.info$startLine]
	}else{
		full.line <- current.script.line.array[proc.node.info$startLine:proc.node.info$endLine]
		paste(full.line, sep = "", collapse = "")
	}
	return (stringr::str_replace_all(full.line, pattern = " ", replacement = ""))
}

#' get.array.of.arrays.of.lines returns an array of arrays of lines for each script
#' in the given script data frame
#' @param scripts.df script data frame
#' @noRd
get.array.of.arrays.of.lines <- function(scripts.df) {
	array.of.arrays <- lapply(scripts.df$script, function(script) {
		file <- file(script, "r")
		lines <- readLines(file, warn = FALSE)
		close(file)
		return(lines)
	})
	return(array.of.arrays)
}

get.data.differences <- function(first.prov.info, second.prov.info){
  
  error.nodes.1 <- provParseR::get.error.nodes(first.prov.info)
  error.nodes.2 <- provParseR::get.error.nodes(second.prov.info)
  data.nodes.1 <- provParseR::get.data.nodes(first.prov.info)
  data.nodes.2 <- provParseR::get.data.nodes(second.prov.info)
  
  if (!identical(data.nodes.1, data.nodes.2)){
    #based off of provSummarize
    
    proc.data.edges.1 <- provParseR::get.proc.data(first.prov.info)
    proc.nodes.1 <- provParseR::get.proc.nodes(first.prov.info)
    
    proc.data.edges.2 <- provParseR::get.proc.data(second.prov.info)
    proc.nodes.2 <- provParseR::get.proc.nodes(second.prov.info)
    
    # Merge the data frames so that we have the output and the operation that
    # produced that output in 1 row
    error.report.1 <- merge(error.nodes.1, proc.data.edges.1, by.x="id", by.y="entity")
    error.report.1 <- merge(error.report.1, proc.nodes.1, by.x="activity", by.y="id")
    error.report.2 <- merge(error.nodes.2, proc.data.edges.2, by.x="id", by.y="entity")
    error.report.2 <- merge(error.report.2, proc.nodes.2, by.x="activity", by.y="id")
    
    data.nodes.1 <- data.nodes.1[data.nodes.1$type == "Data",]
    data.nodes.2 <- data.nodes.2[data.nodes.2$type == "Data",]
    
    data.report.1 <- merge(data.nodes.1, proc.data.edges.1, by.x="id", by.y="entity")
    data.report.1 <- merge(data.report.1, proc.nodes.1, by.x="activity", by.y="id")
    data.report.2 <- merge(data.nodes.2, proc.data.edges.2, by.x="id", by.y="entity")
    data.report.2 <- merge(data.report.2, proc.nodes.2, by.x="activity", by.y="id")
    
    data.report.1 <- subset(data.report.1, select = c("activity", "id", "name.x", "value", 
                                                     "valType", "type.x", "scope", "fromEnv",
                                                     "id.y", "name.y", "type.y", "scriptNum", "startLine", "endLine"))
    data.report.2 <- subset(data.report.2, select = c("activity", "id", "name.x", "value", 
                                                      "valType", "type.x", "scope", "fromEnv",
                                                      "id.y", "name.y", "type.y", "scriptNum", "startLine", "endLine"))
    
    # Get the scripts and remove the directory name
    scripts.1 <- provParseR::get.scripts(first.prov.info)
    scripts.1 <- sub(".*/", "", scripts.1$script)
    
    scripts.2 <- provParseR::get.scripts(second.prov.info)
    scripts.2 <- sub(".*/", "", scripts.2$script)
    
    if (!identical(error.report.1, error.report.2)){
      compare.error.nodes(error.report.1, error.report.2,
                          scripts.1, scripts.2)
    }
    if (!identical(data.report.1, data.report.2)){
      compare.data.nodes(data.report.1, data.report.2,
                         scripts.1, scripts.2)
    }
  }
}

compare.error.nodes <- function(error.report.1, error.report.2, scripts.1, scripts.2){
  cat("ERROR DIFFERENCE DETECTED:\n")
  if (nrow(error.report.1) >= nrow(error.report.2)){
    end <-  nrow(error.report.1)
    using.1 <- TRUE
  } else {
    end = nrow(error.report.2)
    using.1 <- FALSE
  }
  for (i in 1:end){ #fix for better comparison for when diff lengths
    script.name.1 <- scripts.1[error.report.1[i, "scriptNum"]]
    script.name.2 <- scripts.2[error.report.2[i, "scriptNum"]]
    if(!identical(error.report.1[i, "value"], error.report.2[i, "value"])){
      msg.1 <- error.report.1[i, "value"]
      msg.2 <- error.report.2[i, "value"]
      if(!is.na(error.report.1[i, "startLine"]) && !is.na(error.report.2[i, "startLine"])){
        sl.1 <- error.report.1[i, "startLine"]
        sl.2 <- error.report.2[i, "startLine"]
        if (using.1){
          cat(paste("Line ", sl.1, " of file in Directory 1:", script.name.1, " reported ",
                    msg.1, " while in Directory 2:", script.name.2, " reported "))
          if (!is.na(msg.2)){
            cat(paste(msg.2))
          } else {
            cat(paste("nothing."))
          }
        } else {
          cat(paste("Line ", sl.2, " of file in Directory 1:", script.name.2, " reported the following: '",
                    msg.2, "' while file in Directory 2:", script.name.1, " reported "))
          if (!is.na(msg.1)){
            cat(msg.2)
          } else {
            cat("nothing")
          }
        }
      }
    }
  }
  cat("\n")
}
compare.data.nodes <- function(data.report.1, data.report.2, scripts.1, scripts.2){
  cat("\nDATA NODE DIFFERENCE DETECTED:\n")
  
  if (nrow(data.report.1) >= nrow(data.report.2)){
    end <-  nrow(data.report.1)
    using.1 <- TRUE
  } else {
    end = nrow(data.report.2)
    using.1 <- FALSE
  }
  for (i in 1:end){
    script.name.1 <- scripts.1[data.report.1[i, "scriptNum"]]
    script.name.2 <- scripts.2[data.report.2[i, "scriptNum"]]
    
    if (!identical(data.report.1[i,], data.report.2[i,])){
      #File has node that other does not
      if (using.1 && is.na(data.report.2[i, "id"])){
        cat("Data node", data.report.1[i, "id"], "in Directory 1 File", script.name.1, 
        "does not exist in Directory 2 File", script.name.2)
      } else if (!using.1 && is.na(data.report.1[i, "id"])){
        cat("Data node", data.report.2[i, "id"], "in Directory 2 File", script.name.2, 
            "does not exist in Directory 1 File", script.name.1)
      }
      #Comparing file 1 to file 2, in this case, directory 1 data node is different from directory 2 data node
      diff.vector <- ""
      name.diff <- ""
      value.diff <- ""
      valType.diff <- ""
      
      if (!identical(data.report.1[i, "name.x"], data.report.2[i, "name.x"])){
        diff.vector <- paste(diff.vector, "name")
        name.diff <- paste(data.report.1[i, "id"] ,"Name:", data.report.1[i, "name"], "\n ", 
                           data.report.2[i, "id"],"Name:", data.report.2[i, "name"], "\n")
      }
      if (!identical(data.report.1[i, "value"], data.report.2[i, "value"])){
        diff.vector <- paste(diff.vector, "value")
        value.diff <- paste(data.report.1[i, "id"] ,"Value:", data.report.1[i, "value"], "\n ", 
                            data.report.2[i, "id"],"Value:", data.report.2[i, "value"], "\n")
      }
      if (!identical(data.report.1[i, "valType"], data.report.2[i, "valType"])){
        diff.vector <- paste(diff.vector, "valType")
        valType.diff <- paste(data.report.1[i, "id"] ,"valType:", data.report.1[i, "valType"], "\n ", 
                              data.report.2[i, "id"],"valType:", data.report.2[i, "valType"], "\n")
      }
      cat(data.report.1[i, "id"], "from Line", data.report.1[i, "startLine"], 
          "in Directory 1 File", script.name.1, 
          "has different:", diff.vector, "\ncompared to", 
          data.report.2[i, "id"], "from Line", data.report.2[i, "startLine"], 
          "in Directory 2 File", script.name.2, "\n")
      cat(paste(name.diff, value.diff, valType.diff))
      break
    }
  }
  
  
  
}
  

  








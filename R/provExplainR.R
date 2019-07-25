#' Immplementation of provExplainR: detect changes and report to users
#' @author Khanh Ngo

#' provExplainR takes in two provenance directories, detect changes, and
#' report to users. Set 'save' parameter to be true to have the results
#' written in a txt file.
#' @param olderProv.dir path to the directory that contains older provenance
#' @param newerProv.dir path to the directory that contains newer provenance
#' @param save if true saves the report to the file prov-explain.txt
#' in the current working directory
#' @export
#' @rdname explain
prov.explain <- function (olderProv.dir, newerProv.dir, save = FALSE){
	# check the existence of two given directories
	check.dir.existence (olderProv.dir, newerProv.dir)

	# case: two directories are the same
	if (olderProv.dir == newerProv.dir){
		warning (olderProv.dir, "and", newerProv.dir, "are the same directories\n")
		return (NA)
	}

	# check for changes
	detect.changes (olderProv.dir, newerProv.dir)
}

#' detect.changes gets ProvInfo objects from provParseR
#' and calls other methods to find changes in different aspects
#' @param olderProv.dir path to directory that contains older provenance
#' @param newerProv.dir path to directory that contains newer provenance
#' @noRd
detect.changes <- function (olderProv.dir, newerProv.dir){
	# gets the ProvInfo objects
	older.prov.info <- get.prov.info.object(olderProv.dir)
	newer.prov.info <- get.prov.info.object(newerProv.dir)

	# detect changes in different aspects
	print.environment.changes (provParseR::get.environment(older.prov.info), provParseR::get.environment(newer.prov.info))
	print.library.changes (provParseR::get.libs(older.prov.info), provParseR::get.libs(newer.prov.info))
	print.prov.tool.changes (provParseR::get.tool.info(older.prov.info), provParseR::get.tool.info(newer.prov.info))
	print.script.changes (provParseR::get.scripts(older.prov.info), provParseR::get.scripts(newer.prov.info), olderProv.dir, newerProv.dir)
}

#' print.library.changes gets changes in library by calling a helper
#' method find.library.changes, and prints out the result
#' @param olderProv.lib.df library data frame for older provenance
#' @param newerProv.lib.df library data frame for newer provenance
#' @noRd
print.library.changes <- function (olderProv.lib.df, newerProv.lib.df){
	cat ("\n\nLIBRARY CHANGES: ")
	# get the list of changes
	lib.change.list <- find.library.changes(olderProv.lib.df, newerProv.lib.df)

	# if list is null, R returns empty (non-NULL) data frames
	lib.updates.df <- as.data.frame(lib.change.list[1])
	lib.add.df <- as.data.frame(lib.change.list[2])
	lib.remove.df <- as.data.frame(lib.change.list[3])

	cat ("\nLibrary updates: ")
	display.custom.df(lib.updates.df)

	cat ("\nLibraries added: ")
	display.custom.df(lib.add.df)

	cat ("\nLibraries removed: ")
	display.custom.df(lib.remove.df)
}


#' find.library.changes detects changes in libraries used based on the 
#' collected provenance from two provenance folders.
#' The method returns a list of 3 data frames: library version updates,
#' added libraries, removed libraries
#' @param olderProv.lib.df library data frame for older provenance
#' @param newerProv.lib.df library data frame for newer provenance
#' @noRd
find.library.changes <- function (olderProv.lib.df, newerProv.lib.df) {
	# case: input data frame(s) do(es) not exist, stop the function immediately
	if (FALSE == check.df.existence("Library", olderProv.lib.df, newerProv.lib.df)) {
		return(NULL)
	}

	# rare case: no libraries were recorded by provParseR
	if (FALSE == check.df.empty("library", olderProv.lib.df, newerProv.lib.df)) {
		return(NULL)
	}

	# the input data frames have 3 columns: id, name, version
	# removes unneccesary id rows
	olderProv.lib.df <- subset(olderProv.lib.df, select = -1)
	newerProv.lib.df <- subset(newerProv.lib.df, select = -1)

	# find library updates
	# join two data frames by same "name"
	same.name.libs.df <- dplyr::inner_join(olderProv.lib.df, newerProv.lib.df, by = "name")
	# detect the differences in corresponding values of each lib name
	lib.updates.df <- same.name.libs.df[same.name.libs.df$version.x != same.name.libs.df$version.y, ]
	# rename the columns for easier reading 
	colnames(lib.updates.df) <- c("name", "old.version", "new.version")

	# find libraries added
	# get rows in 2nd df but not in 1st df
	added.lib.df <- dplyr::anti_join(newerProv.lib.df, olderProv.lib.df, by = "name")

	# find libraries removed
	# get rows in 1st df but not in 2nd df
	removed.lib.df <- dplyr::anti_join(olderProv.lib.df, newerProv.lib.df, by = "name")

	return (list(lib.updates.df, added.lib.df, removed.lib.df))
}

#' print.environment.changes gets environment changes by calling
#' helper method find.environment.changes and prints out the result
#' @param olderProv.env.df environment data frame for older provenance
#' @param newerProv.env.df environment data frame for newer provenance
#' @noRd
print.environment.changes <- function(olderProv.env.df, newerProv.env.df) {
	cat ("\n\nENVIRONMENT CHANGES: ")
	env.change.list <- find.environment.changes(olderProv.env.df, newerProv.env.df)

	env.updates.df <- as.data.frame(env.change.list[1])
	env.added.df <- as.data.frame(env.change.list[2])
	env.removed.df <- as.data.frame(env.change.list[3])

	cat("\nEnvironment updates: ") 
	display.custom.df(env.updates.df)

	# rare case: a new environment factor has been added,
	# only prints out when found such factor
	if(!is.null(env.added.df) && nrow(env.added.df) != 0){
		cat("\nNew environment factors added: ")
		display.custom.df(env.added.df)
	}

	# rare case: an environment factor has been removed,
	# only prints out when found such factor
	if(!is.null(env.removed.df) && nrow(env.removed.df) != 0){
		cat("\nRemoved environment factor: ")
		display.custom.df(env.removed.df)
	}
}

#' find.environment.changes detects changes in the environments in which 
#' the provenances were collected
#' The method returns a list of 3 main information:
#' environment value changes, any new added environment factors and
#' any removed environment factors by provenance tool
#' @param olderProv.env.df environment data frame for older provenance
#' @param newerProv.env.df environment data frame for newer provenance
#' @noRd
find.environment.changes <- function (olderProv.env.df, newerProv.env.df) {
	# case: input data frame(s) do(es) not exist
	if (FALSE == check.df.existence("Environment", olderProv.env.df, newerProv.env.df)) {
		return(NULL)
	}

	# rare case: no environment factors were recorded by provParseR, returns immediately
	if (FALSE == check.df.empty("environment factor", olderProv.env.df, newerProv.env.df)) {
		return(NULL)
	}

	# find environment changes
	# join two data frames by "label"
	same.label.env.df <- dplyr::inner_join(olderProv.env.df, newerProv.env.df, by = "label")
	env.changes.df <- same.label.env.df[same.label.env.df$value.x != same.label.env.df$value.y, ]
	# rename the columns for easier reading
	colnames(env.changes.df) <- c("label", "old.value", "new.value")

	# find possible added environment factor from the newer version
	added.env.df <- dplyr::anti_join(newerProv.env.df, olderProv.env.df, by = "label")

	# find possible removed environment factor from the older version
	removed.env.df <- dplyr::anti_join(olderProv.env.df, newerProv.env.df, by = "label")

	return (list(env.changes.df, added.env.df, removed.env.df))
}

#' print.prov.tool.changes gets prov tool changes by calling
#' a helper method find.prov.tool.changes and prints out the result
#' @param olderProv.tool.df tool data frame for older provenance
#' @param newerProv.tool.df tool data frame for newer provenance 
#' @noRd
print.prov.tool.changes <- function (olderProv.tool.df, newerProv.tool.df) {
	cat ("\n\nPROVENANCE TOOL CHANGES: ")
	tool.change.list <- find.prov.tool.changes(olderProv.tool.df, newerProv.tool.df)
	# if the list returned is null, as.data.frame creates an empty data frame,
	# so no need to handle null case here
	update.tool.df <- as.data.frame(tool.change.list[1])
	added.tool.df <- as.data.frame(tool.change.list[2])
	removed.tool.df <- as.data.frame(tool.change.list[3])

	cat("\nTool updates: ")
	display.custom.df(update.tool.df)

	# since this case is rare, only prints out when found a new tool
	if (!is.null(added.tool.df) && nrow(added.tool.df) != 0){
		cat ("\nNew provenance tool: \n")
		print.data.frame(added.tool.df, row.names = FALSE)
	}

	# since this case is rare, only prints out when found a removed tool 
	if (!is.null(removed.tool.df) && nrow(removed.tool.df) != 0){
		cat("\nRemoved provenance tool: \n")
		print.data.frame(removed.tool.df, row.names = FALSE)
	}
}

#' prov.tool.changes checks the changes in provenance tool:
#' currently rdt or rdtLite 
#' @param olderProv.tool.df tool data frame for older provenance
#' @param newerProv.tool.df tool data frame for newer provenance
#' @noRd
find.prov.tool.changes <- function (olderProv.tool.df, newerProv.tool.df) {
	# case: input data frame(s) do(es) not exist, returns immediately
	if (FALSE == check.df.existence("Provenance tool", olderProv.tool.df, newerProv.tool.df)){
		return (NULL)
	}

	# rare case: no provenance tools are shown in the data frame, returns immediately
	if (FALSE == check.df.empty("provenance tool", olderProv.tool.df, newerProv.tool.df)){
		return(NULL)
	}

	# finds the tool and json versions in each provenance
	same.tool.df <- dplyr::inner_join(olderProv.tool.df, newerProv.tool.df, by = "tool.name")
	colnames(same.tool.df) <- c("tool.name", "old.tool.version", "old.json.version", 
		"new.tool.version", "new.json.version")
	same.tool.df <- same.tool.df[ , c(1, 2, 4, 3, 5)] # swaps column for nicer output

	# case: if there are no updates, returns an empty data frame 
	if (same.tool.df$old.tool.version == same.tool.df$new.tool.version
		&& same.tool.df$old.json.version == same.tool.df$new.json.version){
		same.tool.df <- data.frame()
	}

	# case: for future updates, if new tools are added, show the new tool
	added.tool.df <- dplyr::anti_join(newerProv.tool.df, olderProv.tool.df, by = "tool.name")
	
	# rare case: provenance tool has been removed since the old prov version
	removed.tool.df <- dplyr::anti_join(olderProv.tool.df, newerProv.tool.df, by = "tool.name")

	return (list(same.tool.df, added.tool.df, removed.tool.df))
}

#' print.script.changes finds the difference between 2 R scripts 
print.script.changes <- function(olderProv.script.df, newerProv.script.df, olderProv.dir, newerProv.dir) {
	cat("\n\nSCRIPT CHANGES: ")

	# check the existence of the 2 data frames
	if(FALSE == check.df.existence("Script", olderProv.script.df, newerProv.script.df)){
		cat("\nNA")
		return(NULL)
	}

	# rare case : no scripts are recorded in the data frame
	if(FALSE == check.df.empty("script", olderProv.script.df, newerProv.script.df)){
		cat("\nNA")
		return(NULL)
	}

	script.change.list <- find.script.changes(olderProv.script.df, newerProv.script.df, olderProv.dir, newerProv.dir)
	main.script.change.result <- script.change.list[[1]]
	sourced.script.change.list <- script.change.list[[2]]

	# prints out the result
	print.main.script.change(main.script.change.result, olderProv.script.df[1, ], newerProv.script.df[1, ])
}

print.main.script.change <- function(main.script.change.result, olderProv.main.script.df, newerProv.main.script.df){
	# extract the name of main scripts from a full path 
	olderProv.main.script.df$script <- basename(olderProv.main.script.df$script)
	newerProv.main.script.df$script <- basename(newerProv.main.script.df$script)

	renamed <- FALSE
	# case: script got renamed
	if(main.script.change.result == 1 || main.script.change.result == 2){
		cat(paste("\nMain script has been renamed from", olderProv.main.script.df$script, "to", newerProv.main.script.df$script))
		renamed <- TRUE
	}

	# case: the content of script changed
	if(main.script.change.result == 1 || main.script.change.result == 0){
		msg <- "\nThe content of the main script"
		# case: if script was not renamed, prints out the name of the script along with the message
		if(FALSE == renamed){
			cat(paste(msg, newerProv.main.script.df$script, "has changed"))
		}else{
			cat(paste(msg, "has changed"))
		}
	}else{ # case: the content is not changed (value 2 or 3)
		msg <- "\nNo change deteced in main script"
		# case: if script was not renamed, prints out the name of the script along with the message
		if(FALSE == renamed){
			cat(paste(msg, newerProv.main.script.df$script))
		}else{
			cat(msg)
		}
	}

	# prints out timestamp for each script
	cat(paste("\nOld script", olderProv.main.script.df$script, "last modified at: ", olderProv.main.script.df$timestamp))
	cat(paste("\nNew script", newerProv.main.script.df$script, "last modified at: ", newerProv.main.script.df$timestamp))
}


#' Steps:
#' 1. Access the scripts
#' 2. Generate the hash value for each script
#' 3. compare hash value and return changes in form of a list 
#' 4. provide a function for users to view diff of 2 scripts 
find.script.changes <- function(olderProv.script.df, newerProv.script.df, olderProv.dir, newerProv.dir) {
	# get right paths for copied scripts located in the provenance folders
	olderProv.script.df <- get.copied.script.path(olderProv.dir, olderProv.script.df)
	newerProv.script.df <- get.copied.script.path(newerProv.dir, newerProv.script.df)

	# generate hash value for each script in the data frame
	olderProv.script.df <- compute.script.hash.value(olderProv.script.df)
	newerProv.script.df <- compute.script.hash.value(newerProv.script.df)

	#find script changes
	main.script.result <- compare.main.script(olderProv.script.df[1, ], newerProv.script.df[1, ])
	sourced.script.result.list <- compare.sourced.scripts(olderProv.script.df[-1, ], newerProv.script.df[-1, ])
	return (list(main.script.result, sourced.script.result.list))
}

#' compare.main.script find changes in the the content and name of main script
#' The function returns 4 values reprensting 4 status:
#' 0 = different script, same name
#' 1 = different script, different name
#' 2 = same script, different name
#' 3 = same script, same name 
#' @param olderProv.main.script.df data frame which contains only old main script
#' @param newerProv.main.script.df data frame which contains only new main script
#' @noRd
compare.main.script <- function(olderProv.main.script.df, newerProv.main.script.df) {
	# extract the name of main scripts from a full path 
	olderProv.main.script.df$script <- basename(olderProv.main.script.df$script)
	newerProv.main.script.df$script <- basename(newerProv.main.script.df$script)

	if(olderProv.main.script.df$hashValue != newerProv.main.script.df$hashValue
		&& olderProv.main.script.df$script == newerProv.main.script.df$script){
		return (0)
	}

	if(olderProv.main.script.df$hashValue != newerProv.main.script.df$hashValue
		&& olderProv.main.script.df$script != newerProv.main.script.df$script){
		return (1)
	}

	if(olderProv.main.script.df$hashValue == newerProv.main.script.df$hashValue
		&& olderProv.main.script.df$script != newerProv.main.script.df$script){
		return (2)
	}

	if(olderProv.main.script.df$hashValue == newerProv.main.script.df$hashValue
		&& olderProv.main.script.df$script == newerProv.main.script.df$script){
		return (3)
	}
}

#' compare.sourced.scripts detects changes in sourced script data frames.
#' The method returns a list of 4 data frames:
#' 1. data frame containing scripts with same name
#' 2. data frame containing renamed scripts with same hash values
#' 3. data frame containing unidentified scripts in the old prov version
#' 4. data frame containing unidentified scripts in the new prov version
#' @param olderProv.sourced.script.df data frame containing only sourced scripts in older prov version
#' @param newerProv.sourced.script.df data frame containing only sourced scripts in newer prov version
#' @noRd
compare.sourced.scripts <- function(olderProv.sourced.script.df, newerProv.sourced.script.df) {
	# case: no sourced scripts were used in both prov versions
	if(nrow(olderProv.sourced.script.df) == 0 && nrow(newerProv.sourced.script.df) == 0){
		return (list(data.frame(), data.frame(), data.frame(), data.frame()))
	}

	# case: scripts with same name (same or different hash values)
	same.name.script.df <- dplyr::inner_join(olderProv.sourced.script.df, newerProv.sourced.script.df, by = "script")
	colnames(same.name.script.df) <- c("script", "old.timestamp", "old.hashValue", "new.timestamp", "new.hashValue")

	# case: scripts with different name but with same hash values (scripts got renamed)
	different.name.old.script.df <- dplyr::anti_join(olderProv.sourced.script.df, newerProv.sourced.script.df, by = "script")
	different.name.new.script.df <- dplyr::anti_join(newerProv.sourced.script.df, olderProv.sourced.script.df, by = "script")
	script.renamed.df <- dplyr::inner_join(different.name.old.script.df, different.name.new.script.df, by = "hashValue")
	colnames(script.renamed.df) <- c("old.script", "old.timestamp", "hashValue", "new.script", "new.timestamp")

	# case: scripts with different name and different hash values
	unmatched.old.script.df <- dplyr::anti_join(different.name.old.script.df, different.name.new.script.df, by = "hashValue")
	unmatched.new.script.df <- dplyr::anti_join(different.name.new.script.df, different.name.old.script.df, by = "hashValue")

	return(list(same.name.script.df, script.renamed.df, unmatched.old.script.df, unmatched.new.script.df))
}

# TODO: FUNCTION FOR USERS TO VIEW DIFF BETWEEN TWO SCRIPTS
# diff.script <- function(first.script, olderProv.dir, newerProv.dir, second.script = NULL) {
	
# }

#' compute.script.hash.value generates hash value for each script
#' and store these values in a new column in the given data frame
#' @param script.df data frame with scripts
#' @noRd 
compute.script.hash.value <- function(script.df) {
	hash.values.vector <- sapply(script.df$script, FUN = function(X){
		digest::digest(file = X, algo = "md5")
	})

	script.df$hashValue <- hash.values.vector
	return(script.df)
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
	return(origin.script.df)
}

#' display.custom.df prints a data frame if nrow is larger than 0
#' @param specific.data.frame data frame to be printed out
#' @param has.row.names logical value to include row names or not
#' @noRd
display.custom.df <- function (specific.data.frame, has.row.names = FALSE) {
	if(is.null(specific.data.frame) || nrow(specific.data.frame) == 0){
		cat ("NONE\n")
	}else{
		cat ("\n")
		print.data.frame(specific.data.frame, row.names = has.row.names)
	}
}

#' TODO: display.custom.df.extension prints out result with nicer format 
# display.custom.df.extension <- function(aspect, specific.data.frame, col.num) {
# 	cat("\n")
# 	apply(specific.data.frame, function(item){
# 		cat (item)
# 	})
# }

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
	return(error.message)
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
	return(provParseR::prov.parse(json.file))
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



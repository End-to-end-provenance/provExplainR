#' Immplementation of provExplainR: detect changes and report to users
#' @author Khanh Ngo

#' provExplainR takes in two provenance directories, detect changes, and
#' report to users. Set 'save' parameter to be true to have the results
#' written in a txt file.
#' @param olderProv.dir path of older provenance directory
#' @param newerProv.dir path of newer provenance directory
#' @param save if true saves the report to the file prov-explain.txt
#' in the current working directory
#' @export
prov.explain <- function (olderProv.dir, newerProv.dir, save = FALSE){
	# check the existence of two given directories
	check.dir.existence(olderProv.dir, newerProv.dir)

	# case: two directories are the same
	if (olderProv.dir == newerProv.dir){
		warning (olderProv.dir, "and", newerProv.dir, "are the same directories\n")
		return (NA)
	}

	# check for changes
	detect.changes (olderProv.dir, newerProv.dir)
}

#' prov.diff.script visualizes the difference between the content of
#' two scripts. Users must specify name of the first script, 
#' old provenance directory path and new provenance directory 
#' path. Name of second script is optional. If second script 
#' is specified, provExplainR assumes first script locates in the 
#' old provenance folder and second script locates in 
#' the new provenance folder. Otherwise, provExplainR assumes 
#' both provenance folders share same script name.
#' @param first.script name of first script 
#' @param olderProv.dir path of older provenance directory
#' @param newerProv.dir path of newer provenance directory
#' @param second.script name of second script 
#' @export
prov.diff.script <- function(first.script, olderProv.dir, newerProv.dir, second.script = NULL) {
	# check the existence of two given directories
	check.dir.existence(olderProv.dir, newerProv.dir)

	# extract script name and get right full paths of each script
	first.script <- basename(first.script)
	old.script <- paste(olderProv.dir, "/scripts/", first.script, sep = "")
	if(!is.null(second.script)){
		second.script <- basename(second.script)
		new.script <- paste(newerProv.dir, "/scripts/", second.script, sep = "")
	}else{
		new.script <- paste(newerProv.dir, "/scripts/", first.script, sep = "")
	}

	# check existence of 2 scripts 
	if(!file.exists(old.script)){
		stop(paste(first.script, "not found in the given provenance folder\n"))
	}

	if(!file.exists(new.script)){
		stop(paste(new.script), "not found in the given provenance folder\n")
	}

	# show the diff
	diffobj::diffFile(target = old.script, current = new.script, mode = "sidebyside")
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
	lib.unloaded.df <- as.data.frame(lib.change.list[3])

	cat ("\nLibrary updates:\n")
	if(nrow(lib.updates.df) == 0){
		cat("No updates have been detected")
	}else{
		print.data.frame(lib.updates.df, row.names = FALSE)
	}

	cat ("\n\nLibraries added:\n")
	if(nrow(lib.add.df) == 0){
		cat("No libraries have been added")
	}else{
		print.data.frame(lib.add.df, row.names = FALSE)
	}

	cat ("\n\nLibraries unloaded:\n")
	if(nrow(lib.unloaded.df) == 0){
		cat("No libraries have been unloaded")
	}else{
		print.data.frame(lib.unloaded.df, row.names = FALSE)
	}
}


#' find.library.changes detects changes in libraries used based on the 
#' collected provenance from two provenance folders.
#' The method returns a list of 3 data frames: library version updates,
#' added libraries, unloaded libraries
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

	# find libraries unloaded
	# get rows in 1st df but not in 2nd df
	unloaded.lib.df <- dplyr::anti_join(olderProv.lib.df, newerProv.lib.df, by = "name")

	return (list(lib.updates.df, added.lib.df, unloaded.lib.df))
}

#' print.environment.changes gets environment changes by calling
#' helper method find.environment.changes and prints out the result
#' @param olderProv.env.df environment data frame for older provenance
#' @param newerProv.env.df environment data frame for newer provenance
#' @noRd
print.environment.changes <- function(olderProv.env.df, newerProv.env.df) {
	cat ("\nENVIRONMENT CHANGES: ")
	env.change.list <- find.environment.changes(olderProv.env.df, newerProv.env.df)

	# as.data.frame returns an empty data frame if the given data frame is null
	# so no need to handle null case here
	env.updates.df <- as.data.frame(env.change.list[1])
	env.added.df <- as.data.frame(env.change.list[2])
	env.removed.df <- as.data.frame(env.change.list[3])

	cat("\nEnvironment updates:") 
	# prints out the update 
	if(nrow(env.updates.df) == 0){
		cat("\nNo updates have been detected")
	}else{
		for(i in 1:nrow(env.updates.df)){
			cat(paste("\nEnvironment factor:", env.updates.df$label[i]))
			cat(paste("\n### Old value:", env.updates.df$old.value[i]))
			cat(paste("\n### New value:", env.updates.df$new.value[i]))
		}
	}

	# rare case: a new environment factor has been added,
	# only prints out when found such factor
	if(nrow(env.added.df) != 0){
		cat("\n\nNew environment factors added:\n")
		print.data.frame(env.added.df, row.names = FALSE)
	}

	# rare case: an environment factor has been removed,
	# only prints out when found such factor
	if(nrow(env.removed.df) != 0){
		cat("\n\nRemoved environment factor:\n")
		print.data.frame(env.removed.df, row.names = FALSE)
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
	# prints out the update 
	if(nrow(update.tool.df) == 0){
		cat("\nNo updates have been detected")
	}else{
		for(i in 1:nrow(update.tool.df)){
			cat(paste("\nTool name:", update.tool.df$tool.name[i]))
			cat(paste("\n### Old tool version:", update.tool.df$old.tool.version[i], 
				"; old json version:", update.tool.df$old.json.version[i]))
			cat(paste("\n### New tool version:", update.tool.df$new.tool.version[i], 
				"; new json version:", update.tool.df$new.json.version[i]))
		}
	}

	# since this case is rare, only prints out when found a new tool
	if (nrow(added.tool.df) != 0){
		cat ("\n\nNew provenance tool:\n")
		print.data.frame(added.tool.df, row.names = FALSE)
	}

	# since this case is rare, only prints out when found a removed tool 
	if (nrow(removed.tool.df) != 0){
		cat("\n\nRemoved provenance tool:\n")
		print.data.frame(removed.tool.df, row.names = FALSE)
	}
}

#' prov.tool.changes checks for changes in provenance tool:
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
#' and call other helper printing functions to display the change results
#' @param olderProv.script.df script data frame for older provenance
#' @param newerProv.script.df script data frame for newer provenance
#' @param olderProv.dir path to directory that contains older provenance
#' @param newerProv.dir path to directory that contains newer provenance
#' @noRd
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
	main.script.change.result <- as.double(script.change.list[1])
	sourced.script.change.list <- script.change.list[[2]]

	# prints out the result
	print.main.script.change(main.script.change.result, olderProv.script.df[1, ], newerProv.script.df[1, ])
	print.same.name.sourced.scripts(same.name.script.df = sourced.script.change.list[[1]])
	print.renamed.sourced.scripts(renamed.script.df = sourced.script.change.list[[2]])
	print.unmatched.sourced.scripts(status = "old", sourced.script.change.list[[3]])
	print.unmatched.sourced.scripts(status = "new", sourced.script.change.list[[4]])
}

#' print.main.script.change prints out changes in main script
#' based on the status value returned by compare.main.script method.
#' The message should display the old and new name if the script got 
#' renamed, whether the content of the script has changed, and the 
#' timestamp for each script version
#' @param main.script.change.result given status value
#' @param olderProv.main.script.df data frame which contains only old main script
#' @param newerProv.main.script.df data frame which contains only new main script
#' @noRd
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
		msg <- "\nNo change detected in main script"
		# case: if script was not renamed, prints out the name of the script along with the message
		if(FALSE == renamed){
			cat(paste(msg, newerProv.main.script.df$script))
		}else{
			cat(msg)
		}
	}

	# prints out timestamp for each script
	if(olderProv.main.script.df$timestamp == newerProv.main.script.df$timestamp){
		# case: timestamp of two script versions is the same
		cat(paste("\n### Main script"), newerProv.main.script.df$script, "was last modified at:", newerProv.main.script.df$timestamp)
	}else{
		cat(paste("\n### Old script", olderProv.main.script.df$script, "was last modified at:", olderProv.main.script.df$timestamp))
		cat(paste("\n### New script", newerProv.main.script.df$script, "was last modified at:", newerProv.main.script.df$timestamp))
	}
}

#' find.script.changes find changes in both main and sourced scripts.
#' The method calls other helper functions to get full path of each 
#' script in the provenance directories, generate hash value for each 
#' script, find changes in main and sourced scripts, then returns a list
#' containing comparison results of main script and sourced scripts. 
#' @param olderProv.script.df data frame of old main and sourced scripts
#' @param newerProv.script.df data frame of new main and sourced scripts
#' @param olderProv.dir path to older provenance directory
#' @param newerProv.dir path to newer provenance directory
#' @noRd
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
#' 3. data frame containing unmatched scripts in the old prov version
#' 4. data frame containing unmatched scripts in the new prov version
#' Note: this method replaces full script path with script name. In other words,
#' returned data frames now contain only script name, not full script path
#' @param olderProv.sourced.script.df data frame containing only sourced scripts in older prov version
#' @param newerProv.sourced.script.df data frame containing only sourced scripts in newer prov version
#' @noRd
compare.sourced.scripts <- function(olderProv.sourced.script.df, newerProv.sourced.script.df) {
	# extract the names of each script from their full path
	olderProv.sourced.script.df$script <- basename(olderProv.sourced.script.df$script)
	newerProv.sourced.script.df$script <- basename(newerProv.sourced.script.df$script)

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
	renamed.script.df <- dplyr::inner_join(different.name.old.script.df, different.name.new.script.df, by = "hashValue")
	colnames(renamed.script.df) <- c("old.script", "old.timestamp", "hashValue", "new.script", "new.timestamp")

	# case: scripts with different name and different hash values
	unmatched.old.script.df <- dplyr::anti_join(different.name.old.script.df, different.name.new.script.df, by = "hashValue")
	unmatched.new.script.df <- dplyr::anti_join(different.name.new.script.df, different.name.old.script.df, by = "hashValue")

	return(list(same.name.script.df, renamed.script.df, unmatched.old.script.df, unmatched.new.script.df))
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
		modified.script.df <- dplyr::filter(same.name.script.df, same.name.script.df$old.hashValue != same.name.script.df$new.hashValue)
		if(nrow(modified.script.df) != 0){
			for(i in 1:nrow(modified.script.df)){
				cat(paste("\nSourced script", modified.script.df$script[i], "has changed"))
			  	cat(paste("\n### Old version", modified.script.df$script[i], "was last modified at:", modified.script.df$old.timestamp[i]))
			  	cat(paste("\n### New version", modified.script.df$script[i], "was last modified at:", modified.script.df$new.timestamp[i]))
			}
		}

		# extract rows with same hash values
		identical.script.df <- dplyr::filter(same.name.script.df, same.name.script.df$old.hashValue == same.name.script.df$new.hashValue)
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
			cat(paste("\nSourced script", renamed.script.df$old.script[i], "has been renamed to", renamed.script.df$new.script[i]))
			cat(paste("\n### Old version", renamed.script.df$old.script[i], "was last modified at:", renamed.script.df$old.timestamp[i]))
			cat(paste("\n### New version", renamed.script.df$new.script[i], "was last modified at:", renamed.script.df$new.timestamp[i]))
		}
	}
}

#' print.unmatched.sourced.scripts takes in a data frame
#' of unmatched scripts, reports changes to users based on 
#' the given status (either old or new)
#' @param status old or new
#' @param unmatched.script.df data frame of unmatched scripts
#' @noRd
print.unmatched.sourced.scripts <- function(status, unmatched.script.df) {
	if(FALSE == is.valid.script.df(aspect = paste(status, "unmatched scripts"), script.df = unmatched.script.df)) {
		return ("\nNA")
	}

	# case: data frame must be non-empty
	if(nrow(unmatched.script.df) != 0) {
		if(status == "old") {
			result <- "renamed or removed"
		}else{
			result <- "renamed or added"
		}

		for(i in 1:nrow(unmatched.script.df)) {
			cat(paste("\nSourced script", unmatched.script.df$script[i], "has been", result))
			cat(paste("\n###", unmatched.script.df$script[i], "was last modified at:", unmatched.script.df$timestamp[i]))
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



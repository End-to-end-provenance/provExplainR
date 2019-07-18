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
		cat (olderProv.dir, "and", newerProv.dir, "are the same directories\n")
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
	if (check.df.existence("Library", olderProv.lib.df, newerProv.lib.df) == FALSE) {
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
	if (check.df.existence("Environment", olderProv.env.df, newerProv.env.df) == FALSE) {
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
	cat ("\n\nPROVENANCE TOOL CHANGES: \n")
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
	if (check.df.existence("Provenance tool", olderProv.tool.df, newerProv.tool.df) == FALSE){
		return (NULL)
	}

	# rare case: no provenance tools are shown in the data frame, returns immediately
	if (nrow(olderProv.tool.df) == 0 || nrow(newerProv.tool.df) == 0){
		warning("no provenance tool was recorded by provParseR\n")
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
#' If one or both of them are null, outputs a warning, returns false to 
#' stop the caller of given aspect and continue the program.
#' Otherwise, returns true 
#' @param aspect overview of what the data frames are about
#' @param df1 first data frame
#' @param df2 second data frame
#' @noRd
check.df.existence <- function (aspect, df1, df2){
	if(is.null(df1) || is.null(df2)){
		warning (paste(aspect, "data frames returned by provParseR is NULL\n"))
		return (FALSE)
	}
	return (TRUE)
}

# Immplementation of provExplainR: detect changes and report to users
# @author: Khanh Ngo
# @date: 06/24/19

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
	# case: directories do not exist
	error.message <- check.dir.existence(olderProv.dir, newerProv.dir)
	if(error.message != ""){
		stop(error.message)
	}

	# case: two directories are the same
	if(olderProv.dir == newerProv.dir){
		cat(olderProv.dir, "and", newerProv.dir, "are the same directories\n")
		return(NA)
	}

	# check for changes
	detect.changes(olderProv.dir, newerProv.dir)
}

#' detect.changes gets ProvInfo objects from provParseR
#' and calls other methods to find changes in different aspects
#' @param olderProv.dir path to directory that contains older provenance
#' @param newerProv.dir path to directory that contains newer provenance
#' @noRd
detect.changes <- function (olderProv.dir, newerProv.dir){
	# gets the path of two json files
	older.json.file <- paste(olderProv.dir, "/prov.json", sep = "")
	newer.json.file <- paste(newerProv.dir, "/prov.json", sep = "")

	# case: json file(s) do(es) not exist
	if(!file.exists(older.json.file) || !file.exists(newer.json.file)){
		stop("prov.json file in the given folders not found\n")
	}

	# gets the ProvInfo object returned by provParseR
	older.prov.info <- provParseR::prov.parse(older.json.file)
	newer.prov.info <- provParseR::prov.parse(newer.json.file)

	# detect changes in different aspects
	environment.changes(provParseR::get.environment(older.prov.info), provParseR::get.environment(newer.prov.info))
	libraries.changes(provParseR::get.libs(older.prov.info), provParseR::get.libs(newer.prov.info))
	prov.tool.changes(provParseR::get.tool.info(older.prov.info), provParseR::get.tool.info(newer.prov.info))
}

#' libraries.changes detects changes in libraries used based on 
#' the collected provenances from two provenance folders.
#' The method prints out 3 main information to compare differences 
#' between 2 library data frames:
#' library version updates, added libraries, removed libraries
#' @param olderProv.lib.df library data frame for older provenance
#' @param newerProv.lib.df library data frame for newer provenance
#' @noRd
libraries.changes <- function (olderProv.lib.df, newerProv.lib.df){
	cat ("\nLIBRARY CHANGES:\n")

	# case: input data frame(s) do(es) not exist
	if (check.df.existence("Library", olderProv.lib.df, newerProv.lib.df) == FALSE) {
		return(NULL)
	}

	# the input data frames have 3 columns: id, name, version
	# removes unneccesary id rows
	olderProv.lib.df <- subset(olderProv.lib.df, select = -id)
	newerProv.lib.df <- subset(newerProv.lib.df, select = -id)

	# find library updates
	# join two data frames by same "name"
	same.name.libs.df <- dplyr::inner_join(olderProv.lib.df, newerProv.lib.df, by = "name")
	# detect the differences in corresponding values of each lib name
	lib.updates.df <- same.name.libs.df[same.name.libs.df$version.x != same.name.libs.df$version.y, ]
	# rename the columns for easier reading 
	colnames(lib.updates.df) <- c("name", "old.version", "new.version")
	if (nrow(lib.updates.df) == 0) {
		cat ("Library updates: NONE\n")
	}else{
		cat ("Library updates: \n")
		print.data.frame(lib.updates.df, row.names = FALSE)
	}

	# find libraries added
	# get rows in 2nd df but not in 1st df
	added.lib.df <- dplyr::anti_join(newerProv.lib.df, olderProv.lib.df, by = "name")
	if (nrow(added.lib.df) == 0) {
		cat ("\nLibraries added: NONE\n")
	}else{
		cat ("\nLibraries added: \n")
		print.data.frame(added.lib.df, row.names = FALSE)
	}

	# find libraries removed
	# get rows in 1st df but not in 2nd df
	removed.lib.df <- dplyr::anti_join(olderProv.lib.df, newerProv.lib.df, by = "name")
	if (nrow(removed.lib.df) == 0) {
		cat ("\nLibraries removed: NONE\n")
	}else{
		cat ("\nLibraries removed: \n")
		print.data.frame(removed.lib.df, row.names = FALSE)
	}
}

#' environment.changes detects changes in the environments in which 
#' the provenances were collected
#' The method prints out 3 main information to compare differences 
#' between 2 environment data frames:
#' environment value changes, any new collected environment factors and
#' any removed environment factors by provenance collector tool
#' @param olderProv.lib.df environment data frame for older provenance
#' @param newerProv.lib.df environment data frame for newer provenance
#' @noRd
environment.changes <- function (olderProv.env.df, newerProv.env.df) {
	cat ("\nENVIRONMENT CHANGES:\n")
	# case: input data frame(s) do(es) not exist
	if (check.df.existence("Environment", olderProv.env.df, newerProv.env.df) == FALSE) {
		return(NULL)
	}

	# find environment changes
	# join two data frames by "label"
	same.label.env.df <- dplyr::inner_join(olderProv.env.df, newerProv.env.df, by = "label")
	env.changes.df <- same.label.env.df[same.label.env.df$value.x != same.label.env.df$value.y, ]
	# rename the columns for easier reading
	colnames(env.changes.df) <- c("name", "old.env.value", "new.env.value")
	if (nrow(env.changes.df) == 0){
		cat ("Environment value changes: NONE")
	}else{
		cat ("Environment value changes: \n")
		print.data.frame(env.changes.df, row.names = FALSE)
	}

	# find possible added environment factor from the newer version
	added.env.df <- dplyr::anti_join(newerProv.env.df, olderProv.env.df, by = "label")
	if (nrow(added.env.df) != 0){
		cat ("\nEnvironment factors added: \n")
		print.data.frame(added.env.df, row.names = FALSE)
	}

	# find possible removed environment factor from the older version
	removed.env.df <- dplyr::anti_join(olderProv.env.df, newerProv.env.df, by = "label")
	if (nrow(removed.env.df) != 0){
		cat ("\nEnvironment factors removed: \n")
		print.data.frame(removed.env.df, row.names = FALSE)
	}
}

#' prov.tool.changes checks the changes in provenance tool:
#' currently rdt or rdtLite 
#' @param olderProv.tool.df tool data frame for older provenance
#' @param newerProv.tool.df tool data frame for newer provenance
#' @noRd
#' 
prov.tool.changes <- function (olderProv.tool.df, newerProv.tool.df) {
	cat ("\nPROVENANCE TOOL CHANGES: \n")
	# case: input data frame(s) do(es) not exist 
	if (check.df.existence("Provenance tool", olderProv.tool.df, newerProv.tool.df) == FALSE){
		return (NULL)
	}

	# rare case: no provenance tools are shown in the data frame
	if (nrow(olderProv.tool.df) == 0 || nrow(newerProv.tool.df) == 0){
		warning("no provenance tool was recorded in one of the given provenance directory by provParseR")
		return(NULL)
	}

	# finds the tool and json versions in each provenance
	same.tool.df <- dplyr::inner_join(olderProv.tool.df, newerProv.tool.df, by = "tool.name")
	# renames the columns for nicer output
	colnames(same.tool.df) <- c("tool.name", "old.tool.version", "old.json.version", 
		"new.tool.version", "new.json.version")
	# case: if tool or json versions are different, show all (same and different) 
	if (same.tool.df$old.tool.version != same.tool.df$new.tool.version
		|| same.tool.df$old.json.version != same.tool.df$old.json.version){
		print.data.frame(same.tool.df, row.names = FALSE)
	}

	# case: for future updates, if new tools are added, show the new tool
	added.tool.df <- dplyr::anti_join(newerProv.tool.df, olderProv.tool.df, by = "tool.name")
	if (nrow(added.tool.df) != 0){
		cat ("\nNew tool used to collect provenance: \n")
		print.data.frame(added.tool.df, row.names = FALSE)
	}
}

#' check.dir.existence checks if two given directories exists
#' and returns the error message to the caller
#' @param dir1 the first directory
#' @param dir2 the second directory
#' @noRd
check.dir.existence <- function (dir1, dir2){
	error.message <- ""
	if (!dir.exists(dir1)) {
		error.message <- paste(dir1, "directory not found\n")
	}

	if (!dir.exists(dir2)) {
		error.message <- paste(error.message, dir2, " directory not found\n", sep = "")
	}
	return(error.message)
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
		warning (paste(aspect, "data frames returned by provParseR is null\n"))
		return (FALSE)
	}
	return (TRUE)
}

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
	# checks if passed in directories exists
	error.message <- check.dir.existence(olderProv.dir, newerProv.dir)
	if(error.message != ""){
		stop(error.message)
	}

	# checks if paths of two given directories are the same
	if(olderProv.dir == newerProv.dir){
		cat(olderProv.dir, "and", newerProv.dir, "are the same directories\n")
		return(NA)
	}

	# check for changes
	detect.changes(olderProv.dir, newerProv.dir)
}


detect.changes <- function (olderProv.dir, newerProv.dir){
	# get ProvInfo object of two directories from provParseR
	older.json.file <- paste(olderProv.dir, "/prov.json", sep = "")
	newer.json.file <- paste(newerProv.dir, "/prov.json", sep = "")

	if(!file.exists(older.json.file) || !file.exists(newer.json.file)){
		stop("prov.json file in the given folders not found")
	}

	older.prov.info <- provParseR::prov.parse(older.json.file)
	newer.prov.info <- provParseR::prov.parse(newer.json.file)

	# detect changes in different aspects
	libraries.changes(provParseR::get.libs(older.prov.info), provParseR::get.libs(newer.prov.info))
}

#' libraries.changes detects changes in libraries used based on 
#' the collected provenances from two provenance folders.
#' The method prints out 3 main information to compare differences 
#' between 2 library data frames:
#' library version updates, added libraries, removed libraries
#' @param olderProv.lib.df library data frame for older provenance obtained from provParseR
#' @param newerProv.lib.df library data frame for newer provenance obtained from provParseR
#' @noRd
libraries.changes <- function (olderProv.lib.df, newerProv.lib.df){
	# removes unneccesary id rows
	olderProv.lib.df <- subset(olderProv.lib.df, select = -id)
	newerProv.lib.df <- subset(newerProv.lib.df, select = -id)

	cat ("LIBRARY CHANGES:\n")
	# find library updates
	same.name.libs.df <- dplyr::inner_join(olderProv.lib.df, newerProv.lib.df, by = "name")
	lib.updates.df <- same.name.libs.df[same.name.libs.df$version.x != same.name.libs.df$version.y, ]
	colnames(lib.updates.df) <- c("name", "old.version", "new.version")
	if (nrow(lib.updates.df) == 0) {
		cat ("Library updates: NONE\n")
	}else{
		cat ("Library updates: \n")
		print.data.frame(lib.updates.df, row.names = FALSE)
	}

	# find libraries added
	added.lib.df <- dplyr::anti_join(newerProv.lib.df, olderProv.lib.df, by = "name")
	if (nrow(added.lib.df) == 0) {
		cat ("\nLibraries added: NONE\n")
	}else{
		cat ("\nLibraries added: \n")
		print.data.frame(added.lib.df, row.names = FALSE)
	}

	#find libraries removed
	removed.lib.df <- dplyr::anti_join(olderProv.lib.df, newerProv.lib.df, by = "name")
	if (nrow(removed.lib.df) == 0) {
		cat ("\nLibraries removed: NONE\n")
	}else{
		cat ("\nLibraries removed: \n")
		print.data.frame(removed.lib.df, row.names = FALSE)
	}
}

#' check.dir.existence checks if two given directories exists
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

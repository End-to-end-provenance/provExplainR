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

	#detect.changes(olderProv.dir, newerProv.dir)
}

#detect.changes <- function (olderProv.dir, newerProv.dir){

#}

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


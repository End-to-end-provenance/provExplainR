get.test.prov.dirs <- function (dir.name) {
	return (system.file("testdata", dir.name, package = "provExplainR"))	
}

get.test.prov.info <- function (dir.name) {
	dir.path <- get.test.prov.dirs (dir.name)
	prov.json.file <- paste(dir.path, "/prov.json", sep = "")
	return (provParseR::prov.parse(prov.json.file))
}


#' Gather and store data from the US Census API
#'
#' Pull specific ACS tables and/or variables at different spatial resolutions through the Census API. Option to store files in structured sub-directories.
#' @param year Numeric year value for ACS pull
#' @param geo State, county or tract
#' @param span Select 1 year or 5 year ACS
#' @param vars Select specific variables from a table, optional
#' @param state Select specific state. Required for tract pulls.
#' @param county Select specific county, optional
#' @param key Your Census API key 
#' @param save Whether you want to save your ACS files somewhere
#' @keywords ACS
#' @return An array of data and metadata for an ACS table
#' @export
#' @examples
#' acs_pull(year = 2018, geo = "state", span = "5", tab = "B01001", vars = NULL, state = "California", county = NULL, key = "YOUR_KEY_HERE", save = TRUE, dir = "C:/MyACSdata")


acs_table_search <- function(year = 2018, span = 5, keyword = ""){
  
  suppressWarnings(suppressMessages(require(censusapi)))
  suppressWarnings(suppressMessages(require(tidycensus)))
  
  span <- as.character(span)
  
  metadata <- load_variables(year, paste0("acs", span), cache = TRUE)
  metadata$table <- unlist(lapply(metadata$name, function(x) unlist(strsplit(x, "_"))[1]))
  metadata$label <- unlist(lapply(metadata$label, function(x) gsub("!!", " ", x)))

  metadata_part <- metadata[grep(keyword, metadata$concept, ignore.case = TRUE),]
  metadata_part <- metadata_part[,-c(1,2)]
  
  if(nrow(metadata_part) == 0){
    print("No tables matched this keyword")
  }
  
  return(metadata_part)
}
  
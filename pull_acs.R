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


acs_pull <- function(year = 2018, geo = "state", span = 5, tab = "B01001", vars = NULL, state = NULL, county = NULL, key = "YOUR_KEY_HERE", save = TRUE, dir = ""){
  
  suppressWarnings(suppressMessages(require(censusapi)))
  suppressWarnings(suppressMessages(require(tidycensus)))
  
  if(tail(unlist(strsplit(dir, "")), 1) != "/"){
    dir <- paste0(dir, "/")
  }
  
  span <- as.character(span)
  
  data <- get_acs(geography = geo, variables = vars, table = tab,
                  cache_table = FALSE, year = year, endyear = NULL,
                  output = "tidy", state = state, county = county, geometry = FALSE,
                  keep_geo_vars = FALSE, shift_geo = FALSE, summary_var = NULL,
                  key = key, moe_level = 90, survey = paste0("acs",span))
  
  NAMES <- unique(data$NAME)
  data_reworked <- NULL 
  
  for(i in NAMES){
    data_part <- data[which(data$NAME == i),]
    estimates <- t(data_part$estimate)
    names(estimates) <- data_part$variable
    moes <- t(data_part$moe)
    names(moes) <- paste0(data_part$variable, "_moe")
    GEOID <- data_part$GEOID[1]
    NAME <- data_part$NAME[1]
    row <- c(GEOID, NAME, estimates, moes)
    data_reworked <- rbind(data_reworked, row)
  }
  
  data_reworked <- as.data.frame(data_reworked, row.names = FALSE)
  
  tab_vars <- data.frame(name = unique(data$variable))
  all_vars <- load_variables(year, paste0("acs", span), cache = TRUE)
  metadata <- merge(all_vars, tab_vars, by = "name", all = FALSE)
  metadata <- metadata[,c(1,2)] #names will be different than what's downloaded through ACS
  
  if(save == TRUE){
    dir <- paste0(dir, "ACS", as.character(year), "_", span, "year_", gsub("(^[[:alpha:]])", "\\U\\1", geo, perl=TRUE), "Level")
    if(dir.exists(dir) == TRUE){
      dir.create(paste0(dir, "/Data/"))
      dir.create(paste0(dir, "/Metadata/"))
      write.csv(data, paste0(dir, "ACS_", paste(strsplit(as.character(year), "")[[1]][3:4], collapse = ""), "_", span, "YR_", tab, "_with_ann.csv"), row.names = FALSE)
      write.csv(metadata, paste0(dir, "/Data/Metadata/ACS_", paste(strsplit(as.character(year), "")[[1]][3:4], collapse = ""), "_", span, "YR_", tab, "_metadata.csv"), row.names = FALSE)
    }
    if(dir.exists(dir) == FALSE){
      dir.create(dir)
      dir.create(paste0(dir, "/Data/"))
      dir.create(paste0(dir, "/Metadata/"))
      write.csv(data, paste0(dir, "/Data/ACS_", paste(strsplit(as.character(year), "")[[1]][3:4], collapse = ""), "_", span, "YR_", tab, "_with_ann.csv"), row.names = FALSE)
      write.csv(metadata, paste0(dir, "/Metadata/ACS_", paste(strsplit(as.character(year), "")[[1]][3:4], collapse = ""), "_", span, "YR_", tab, "_metadata.csv"), row.names = FALSE)      
    }
    allDat <- list(data_reworked, metadata)
    return(allDat)
  }
  if(save == FALSE){
    allDat <- list(data_reworked, metadata)
    return(allDat)
  }
}



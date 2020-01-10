#' Fix Two Digit US state FIPS codes 
#'
#' Converts numeric US state FIPS codes to character, appends leading zeroes as needed
#' @param fips Numeric US state FIPS codes
#' @keywords geolocation
#' @return A character 2-digit US state FIPS code, with leading zeros
#' @export
#' @examples
#' state_fips_fix(2)
#' @seealso Find a list of US state FIPS codes here https://www.census.gov/geographies/reference-files/2017/demo/popest/2017-fips.html


state_fips_fix <- function(fips){
  fips <- as.character(fips)
  if(nchar(fips)==1){
    fips <- paste("0", fips, sep="")
  }
  
  else{
    fips <- fips
  }
  return(fips)
}
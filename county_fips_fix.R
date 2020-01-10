#' Fix Five Digit US County FIPS codes 
#'
#' Converts numeric US county FIPS codes to character, appends leading zeroes as needed
#' @param fips Numeric US county FIPS codes
#' @keywords geolocation
#' @return A character 5-digit US county FIPS code, with leading zeros
#' @export
#' @examples
#' county_fips_fix(1001)
#' @seealso Find a list of US county FIPS codes here https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697 


county_fips_fix <- function(fips){
  fips <- as.character(fips)
  if(nchar(fips)==4){
    fips <- paste("0", fips, sep="")
  }
  else{
    fips <- fips
  }
  return(fips)
}

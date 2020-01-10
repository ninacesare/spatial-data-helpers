#' Transform latitude and longitude to U.S. counties
#'
#' Transform latitude and longitude points to U.S. county FIPS codes using the Federal Communications Commission Block Conversion API
#' @param lat Point latitude
#' @param lon Point longitude
#' @keywords geolocation
#' @return A five-digit US county FIPS code or NA 
#' @export
#' @examples
#' reverseGeoCode(47.6062, 122.3321)
#' @seealso For more information on the FCC Block Conversion API, please see: https://www.fcc.gov/census-block-conversions-api

reverseGeoCode <- function(lat, lon){
  connectStr <- connectStr<-paste0("http://geo.fcc.gov/api/census/block/find?latitude=", lat, "&longitude=", lon, "&showall=TRUE&format=json")
  raw.result <- GET(connectStr)
  this.raw.content <- rawToChar(raw.result$content)
  this.content <- fromJSON(this.raw.content)
  fips <- this.content$County[1]
  return(fips)
}

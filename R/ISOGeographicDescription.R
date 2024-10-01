#' ISOGeographicDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO geographic description
#' @return Object of \code{\link{R6Class}} for modelling an ISO GeographicDescription
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOGeographicDescription$new()
#'   md$setGeographicIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeographicDescription <- R6Class("ISOGeographicDescription",
  inherit = ISOGeographicExtent,
  private = list(
    xmlElement = "EX_GeographicDescription",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "GEX"
    )
  ),
  public = list(
    #'@field geographicIdentifier geographicIdentifier [1..1]: character
    geographicIdentifier = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set geographic identifier
    #'@param geographicIdentifier geographic identifier, object of class \link{ISOMetaIdentifier}
    setGeographicIdentifier = function(geographicIdentifier){
      if(!is(geographicIdentifier, "ISOMetaIdentifier")){
        stop("The argument should be an object of class 'ISOMetaIdentifier'")
      }
      self$geographicIdentifier <- geographicIdentifier
    }
    
  )                                          
)
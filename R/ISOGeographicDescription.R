#' ISOGeographicDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO geographic description
#' @return Object of \code{\link{R6Class}} for modelling an ISO GeographicDescription
#' @format \code{\link{R6Class}} object.
#'
#' @field geographicIdentifier
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOGeographicDescription
#'  }
#' }
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
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #+ geographicIdentifier [1..1]: character
    geographicIdentifier = NULL,
    initialize = function(xml = NULL){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
    },
    
    #setGeographicIdentifier
    setGeographicIdentifier = function(geographicIdentifier){
      if(!is(geographicIdentifier, "ISOMetaIdentifier")){
        stop("The argument should be an object of class 'ISOMetaIdentifier'")
      }
      self$geographicIdentifier <- geographicIdentifier
    }
    
  )                                          
)
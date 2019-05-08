#' ISODistributionUnits
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO distribution units
#' @return Object of \code{\link{R6Class}} for modelling an ISO DistributionUnits
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISODistributionUnits
#'  }
#' }
#' 
#' @examples 
#'   unit <- ISODistributionUnits$new(value = "unit")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODistributionUnits <- R6Class("ISODistributionUnits",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MD_DistributionUnits",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value,
                       description = description)
    }
  )                        
)

ISODistributionUnits$values <- function(labels = FALSE){
  return(NULL)
}
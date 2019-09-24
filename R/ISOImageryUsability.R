#' ISOImageryUsability
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery usability
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery usability
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryUsability}}
#'  }
#' }
#' 
#' @section Methods inherited from \code{\link{ISODataQualityAbstractElement}}:
#' See methods description at \code{\link{ISODataQualityAbstractElement}}
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryUsability <- R6Class("ISOImageryUsability",
  inherit = ISODataQualityAbstractElement,
  private = list(
    xmlElement = "QE_Usability",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )                        
)
#' ISOImageryCoverageResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery coverage result
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery coverage result
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryCoverageResult}}
#'  }
#' }
#' 
#'  @section Methods inherited from \code{\link{ISOAbstractResult}}:
#' \describe{
#'  See methods description at \code{\link{ISOAbstractResult}}
#' }  
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryCoverageResult <- R6Class("ISOImageryCoverageResult",
  inherit = ISOAbstractResult,
  private = list(
    xmlElement = "QE_CoverageResult",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )                        
)
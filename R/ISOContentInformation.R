#' ISOContentInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO content information
#' @return Object of \code{\link{R6Class}} for modelling an ISOContentInformation
#' @format \code{\link{R6Class}} object.
#'
#' @section Abstract Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOContentInformation
#'  }
#' }
#' 
#' @note Abstract class. Used internally by \pkg{geometa}
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOContentInformation <- R6Class("ISOContentInformation",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "AbstractMD_ContentInformation",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )                        
)
#' ISOAbstractMDContentInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract content information
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOAbstractMDContentInformation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Abstract class. Used internally by \pkg{geometa}
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractMD_ContentInformation} 
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_AbstractMD_ContentInformation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractMDContentInformation <- R6Class("ISOAbstractMDContentInformation",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "AbstractMD_ContentInformation",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MRC"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )                        
)

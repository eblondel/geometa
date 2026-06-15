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
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )                        
)

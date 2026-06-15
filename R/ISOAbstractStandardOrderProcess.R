#' ISOAbstractStandardOrderProcess
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract standard order process
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract standard order process
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractStandardOrderProcess <- R6Class("ISOAbstractStandardOrderProcess",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "Abstract_StandardOrderProcess",
    xmlNamespacePrefix = list(
      "19115-3" = "MCC"
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

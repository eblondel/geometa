#' ISODefinitionSource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO definition source
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISODefinitionSource
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODefinitionSource <- R6Class("ISODefinitionSource",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "FC_DefinitionSource",
      xmlNamespacePrefix = "GFC"
    ),
    public = list(
      #'@field source source [0..1]: ISOCitation
      source = NULL,
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}
      #'@param source source object of class \link{ISOCitation}
      initialize = function(xml = NULL, source = NULL){
        super$initialize(xml = xml)
        if(!is.null(source)){
          self$setSource(source)
        }
      },
      
      #'@description Set source
      #'@param source object of class \link{ISOCitation}
      setSource = function(source){
        if(!is(source, "ISOCitation")){
          stop("The argument should be an object of class 'ISOCitation'")
        }
        self$source <- source
      }
    )                        
)

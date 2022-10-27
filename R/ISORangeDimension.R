#' ISORangeDimension
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO range dimension
#' @return Object of \code{\link{R6Class}} for modelling an ISORangeDimension
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'    #create dimension
#'    md <- ISORangeDimension$new()
#'    md$setSequenceIdentifier(ISOMemberName$new(aName = "name", attributeType = "type"))
#'    md$setDescriptor("descriptor")
#'    xml <- md$encode()
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORangeDimension <- R6Class("ISORangeDimension",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "MD_RangeDimension",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      #'@field sequenceIdentifier sequenceIdentifier
      sequenceIdentifier = NULL,
      #'@field descriptor descriptor
      descriptor = NULL,
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #'@description Set sequence identifier
      #'@param memberName object of class \link{ISOMemberName}
      setSequenceIdentifier = function(memberName){
        if(!is(memberName, "ISOMemberName")){
          stop("The argument should be an object of class 'ISOMemberName'")
        }
        self$sequenceIdentifier <- memberName
      },
      
      #'@description Set descriptor
      #'@param descriptor descriptor
      #'@param locales list of localized texts. Default is \code{NULL}
      setDescriptor = function(descriptor, locales = NULL){
        self$descriptor <- descriptor
        if(!is.null(locales)){
          self$descriptor <- self$createLocalisedProperty(descriptor, locales)
        }
      }
    )                        
)
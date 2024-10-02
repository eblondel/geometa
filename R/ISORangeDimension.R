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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_RangeDimension}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_RangeDimension}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORangeDimension <- R6Class("ISORangeDimension",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "MD_RangeDimension",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MRC"
      )
    ),
    public = list(
      #'@field sequenceIdentifier sequenceIdentifier
      sequenceIdentifier = NULL,
      #'@field descriptor descriptor (=> ISO 19139)
      descriptor = NULL,
      #'@field description description (=> ISO 19115-3)
      description = NULL,
      #'@field name name (=> ISO 19115-3)
      name = NULL,
      
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
        self$stopIfMetadataStandardIsNot("19139")
        self$descriptor <- descriptor
        if(!is.null(locales)){
          self$descriptor <- self$createLocalisedProperty(descriptor, locales)
        }
      },
      
      #'@description Set description
      #'@param description description
      #'@param locales list of localized texts. Default is \code{NULL}
      setDescription = function(description, locales = NULL){
        self$stopIfMetadataStandardIsNot("19115-3")
        self$description <- description
        if(!is.null(locales)){
          self$description <- self$createLocalisedProperty(description, locales)
        }
      },
      
      #'@description Set name
      #'@param name name
      #'@param locales list of localized texts. Default is \code{NULL}
      setName = function(name, locales = NULL){
        self$stopIfMetadataStandardIsNot("19115-3")
        self$name <- name
        if(!is.null(locales)){
          self$name <- self$createLocalisedProperty(name, locales)
        }
      }
    )                        
)
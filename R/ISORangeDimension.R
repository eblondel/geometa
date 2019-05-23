#' ISORangeDimension
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO range dimension
#' @return Object of \code{\link{R6Class}} for modelling an ISORangeDimension
#' @format \code{\link{R6Class}} object.
#'
#' @field sequenceIdentifier [\code{\link{ISOMemberName}}] sequence identifier
#' @field descriptor [\code{\link{character}}] description
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISORangeDimension
#'  }
#'  \item{\code{setSequenceIdentifier(memberName)}}{
#'    Sets the sequence identifier, object of class \code{ISOMemberName}
#'  }
#'  \item{\code{setDescriptor(descriptor ,locales)}}{
#'    Sets the descriptor, object of class \code{character}. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#' }
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
      sequenceIdentifier = NULL,
      descriptor = NULL,
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #setSequenceIdentifier
      setSequenceIdentifier = function(memberName){
        if(!is(memberName, "ISOMemberName")){
          stop("The argument should be an object of class 'ISOMemberName'")
        }
        self$sequenceIdentifier <- memberName
      },
      
      #setDescriptor
      setDescriptor = function(descriptor, locales = NULL){
        self$descriptor <- descriptor
        if(!is.null(locales)){
          self$descriptor <- self$createLocalisedProperty(descriptor, locales)
        }
      }
    )                        
)
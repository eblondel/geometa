#' ISOGeoreferenceable
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO grid spatial representation georeferenceable
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Georeferenceable
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOGeoreferenceable$new()
#'   
#'   #inherited methods from ISOGridSpatialRepresentation
#'   md$setNumberOfDimensions(1)
#'   dim1 <- ISODimension$new()
#'   dim1$setName("row")
#'   dim1$setSize(100)
#'   dim1$setResolution(ISOMeasure$new(value=1,uom="m"))
#'   md$addDimension(dim1)
#'   md$setCellGeometry("area")
#'   
#'   #parameters
#'   md$setControlPointAvailability(TRUE)
#'   md$setOrientationParameterAvailability(TRUE)
#'   md$setOrientationParameterDescription("description")
#'   md$setGeoreferencedParameters("record")
#'   ct <- ISOCitation$new()
#'   ct$setTitle("citation")
#'   md$addParameterCitation(ct)
#'   
#'   xml <- md$encode()
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_Georeferenceable}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_MD_Georeferenceable}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeoreferenceable <- R6Class("ISOGeoreferenceable",
    inherit = ISOGridSpatialRepresentation,
    private = list(
      xmlElement = "MD_Georeferenceable",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MSR"
      )
    ),
    public = list(
      
      #'@field controlPointAvailability controlPointAvailability: logical
      controlPointAvailability = NULL,
      #'@field orientationParameterAvailability orientationParameterAvailability : logical
      orientationParameterAvailability = NULL,
      #'@field orientationParameterDescription orientationParameterDescription [0..1] : character
      orientationParameterDescription = NULL,
      #'@field georeferencedParameters georeferencedParameters : ISORecord
      georeferencedParameters = NULL,
      #'@field parameterCitation parameterCitation [0..*] : ISOCitation
      parameterCitation = NULL,
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #'@description Set control point availability
      #'@param availability object of class \link{logical}
      setControlPointAvailability = function(availability){
        self$controlPointAvailability = as.logical(availability)
      },
      
      #'@description Set orientation parameter availability
      #'@param availability object of class \link{logical}
      setOrientationParameterAvailability = function(availability){
        self$orientationParameterAvailability = as.logical(availability)
      },
      
      #'@description Set orientation parameter description
      #'@param description description
      #'@param locales list of localized descriptions. Default is \code{NULL}
      setOrientationParameterDescription = function(description, locales = NULL){
        self$orientationParameterDescription = as.character(description)
        if(!is.null(locales)){
          self$orientationParameterDescription <- self$createLocalisedProperty(description, locales)
        }
      },
      
      #'@description Set georeferenced parameters
      #'@param record object of class \link{ISORecord}
      setGeoreferencedParameters = function(record){
        if(!is(record, "ISORecord")){
          record <- ISORecord$new(value = as(record,"character"))
        }
        self$georeferencedParameters = record
      },
      
      #'@description Adds parameter citation
      #'@param citation object of class \link{ISOCitation}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addParameterCitation = function(citation){
        if(!is(citation, "ISOCitation")){
          stop("Argument should be an object of class 'ISOCitation'")
        }
        return(self$addListElement("parameterCitation", citation))
      },
      
      #'@description Deletes parameter citation
      #'@param citation object of class \link{ISOCitation}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delParameterCitation = function(citation){
        if(!is(citation, "ISOCitation")){
          stop("Argument should be an object of class 'ISOCitation'")
        }
        return(self$delListElement("parameterCitation", citation))
      }
      
    )                        
)

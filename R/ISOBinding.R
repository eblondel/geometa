#' ISOBinding
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Binding
#' @return Object of \code{\link{R6Class}} for modelling an ISOBinding
#' @format \code{\link{R6Class}} object.
#'
#' @field description
#' @field globalProperty
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate an ISOBinding
#'  }
#'  \item{\code{setDescription(description, locales)}}{
#'    Set description of inheritance relation. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setPropertyType(propertyType)}}{
#'    Set global property, object of class \code{ISOPropertyType}
#'  }
#' }
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBinding <- R6Class("ISOBinding",
    inherit = ISOCarrierOfCharacteristics,
    private = list(
      xmlElement = "FC_Binding",
      xmlNamespacePrefix = "GFC"
    ),
    public = list(
      
      #+ description [0..1]: character
      description = NULL,
      #+ globalProperty [1..1]: ISOPropertyType
      globalProperty = NULL,

      #setDescription
      setDescription = function(description, locales = NULL){
        self$description <- as.character(description)
        if(!is.null(locales)){
          self$description <- self$createLocalisedProperty(description, locales)
        }
      },

      #setPropertyType
      setPropertyType = function(propertyType){
        if(!is(propertyType, "ISOPropertyType")){
          stop("The argument value should be an object of class 'ISOPropertyType'")
        }
        self$globalProperty <- propertyType
      }
      
    )         
)
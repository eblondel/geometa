#' ISOBinding
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Binding
#' @return Object of \code{\link{R6Class}} for modelling an ISOBinding
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBinding <- R6Class("ISOBinding",
    inherit = ISOAbstractCarrierOfCharacteristics,
    private = list(
      xmlElement = "FC_Binding",
      xmlNamespacePrefix = "GFC"
    ),
    public = list(
      
      #'@field description description [0..1]: character
      description = NULL,
      #'@field globalProperty globalProperty [1..1]: ISOPropertyType
      globalProperty = NULL,

      #'@description Set description 
      #'@param description description
      #'@param locales list of localized descriptions
      setDescription = function(description, locales = NULL){
        self$description <- as.character(description)
        if(!is.null(locales)){
          self$description <- self$createLocalisedProperty(description, locales)
        }
      },

      #'@description Set property type.
      #'@param propertyType property type, object of class \link{ISOPropertyType}
      setPropertyType = function(propertyType){
        if(!is(propertyType, "ISOPropertyType")){
          stop("The argument value should be an object of class 'ISOPropertyType'")
        }
        self$globalProperty <- propertyType
      }
      
    )         
)
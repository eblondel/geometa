#' ISOPropertyType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO property type
#' @return Object of \code{\link{R6Class}} for modelling an ISOPropertyType
#' @format \code{\link{R6Class}} object.
#'
#' @field memberName
#' @field definition
#' @field cardinality
#' @field definitionReference
#'
#' @section Inherited methods from \code{ISOCarrierOfCharacteristics}:
#' \describe{
#'  \item{\code{setFeatureType(featureType)}}{
#'    Sets a feature type, object of class \code{ISOFeatureType}
#'  }
#'  \item{\code{addConstraint(constraint)}}{
#'    Adds a constraint, object of class \code{ISOConstraint} or \code{character}
#'  }
#'  \item{\code{delConstraint(constraint)}}{
#'    Deletes a constraint, object of class \code{ISOConstraint} or \code{character}
#'  }
#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate an ISOPropertyType
#'  }
#'  \item{\code{setMemberName(memberName)}}{
#'    Sets the member name. Object of class \code{ISOLocalName} or \code{"character"}
#'  }
#'  \item{\code{setDefinition(definition, locales)}}{
#'    Sets the definition. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setCardinality(lower, upper)}}{
#'    Sets the cardinality boundaries lower and upper of class \code{numeric}
#'  }
#'  \item{\code{setDefinitionReference(definitionReference)}}{
#'    Sets the definition Reference, object of class \code{ISODefinitionReference}
#'  }
#' }
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOPropertyType <- R6Class("ISOPropertyType",
    inherit = ISOCarrierOfCharacteristics,
    private = list(
      xmlElement = "FC_PropertyType",
      xmlNamespacePrefix = "GFC"
    ),
    public = list(
      
      #+ typeName [1..1]: ISOLocalName
      memberName = NULL,
      #+ definition [0..1]: character
      definition = NULL,
      #+ cardinality [1..1]: ISOMultiplicity
      cardinality = NULL,
      #+ definitionReference [0..1]
      definitionReference = NULL,
      
      initialize = function(xml = NULL, defaults = NULL){
        if(is.null(defaults)) defaults <- list(cardinality = ISOMultiplicity$new(lower=1L,upper=1L))
        super$initialize(xml = xml, defaults = defaults)
      },
      
      #setMemberName
      setMemberName = function(memberName){
        if(!is(memberName,"ISOLocalName")) memberName <- ISOLocalName$new(value = memberName)
        self$memberName <- memberName
      },
      
      #setDefinition
      setDefinition = function(definition, locales = NULL){
        self$definition <- definition
        if(!is.null(locales)){
          self$definition <- self$createLocalisedProperty(definition, locales)
        }
      },
      
      #setCardinality
      setCardinality = function(lower, upper){
        self$cardinality = ISOMultiplicity$new(lower = lower, upper = upper)
      },
      
      #setDefinitionReference
      setDefinitionReference = function(definitionReference){
        if(!is(definitionReference, "ISODefinitionReference")){
          stop("The argument should be an object of class 'ISODefinitionReference'")
        }
        self$definitionReference = definitionReference
      }
    )         
)

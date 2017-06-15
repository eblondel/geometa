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
#' @field featureType
#' @field constrainedBy
#' @field definitionReference
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, namespace, defaults)}}{
#'    This method is used to instantiate an ISOPropertyType
#'  }
#'  \item{\code{setMemberName(memberName)}}{
#'    Sets the member name. Object of class \code{ISOLocalName} or \code{"character"}
#'  }
#'  \item{\code{setDefinition(definition)}}{
#'    Sets the definition
#'  }
#'  \item{\code{setCardinality(lower, upper)}}{
#'    Sets the cardinality boundaries lower and upper of class \code{numeric}
#'  }
#'  \item{\code{addFeatureType(featureType)}}{
#'    Adds a feature type, object of class \code{ISOFeatureType}
#'  }
#'  \item{\code{delFeatureType(featureType)}}{
#'    Deletes a feature type, object of class \code{ISOFeatureType}
#'  }
#'  \item{\code{addConstraint(constraint)}}{
#'    Adds a constraint, object of class \code{ISOConstraint} or \code{character}
#'  }
#'  \item{\code{delConstraint(constraint)}}{
#'    Deletes a constraint, object of class \code{ISOConstraint} or \code{character}
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
    inherit = ISOMetadataElement,
    private = list(
      xmlElement = "FC_PropertyType",
      xmlNamespacePrefix = "GFC"
    ),
    public = list(
      
      #+ typeName [1..1]: ISOLocalName
      memberName = NULL,
      #+ definition [0..1]: character
      definition = NULL,
      #+ cardinality [1..1]: ISOMultiplicityRange
      cardinality = NULL,
      #+ featureType [0..*]: ISOFeatureType
      featureType = list(),
      #+ constrainedBy [0..*]: ISOConstraint
      constrainedBy = list(),
      #+ definitionReference [0..1]
      definitionReference = NULL,
      
      initialize = function(xml = NULL,
                            element = NULL, namespace = NULL,
                            defaults = NULL){
        
        if(is.null(element)) element <- private$xmlElement
        if(is.null(namespace)) namespace <- getISOMetadataNamespace(private$xmlNamespacePrefix)
        if(is.null(defaults)) defaults <- list(cardinality = ISOMultiplicityRange$new(lower=1L,upper=1L))
        super$initialize(
          xml = xml,
          element = element,
          namespace = namespace,
          defaults = defaults
        )
      },
      
      #setMemberName
      setMemberName = function(memberName){
        if(!is(memberName,"ISOLocalName")) memberName <- ISOLocalName$new(value = memberName)
        self$memberName <- memberName
      },
      
      #setDefinition
      setDefinition = function(definition){
        self$definition <- definition
      },
      
      #setCardinality
      setCardinality = function(lower, upper){
        self$cardinality = ISOMultiplicity$new(lower = lower, upper = upper)
      },
      
      #addFeatureType
      addFeatureType = function(featureType){
        if(!is(featureType, "ISOFeatureType")){
          stop("The argument should be an object of class 'ISOFeatureType'")
        }
        return(self$addListElement("featureType", featureType))
      },
      
      #delFeatureType
      delFeatureType = function(featureType){
        if(!is(featureType, "ISOFeatureType")){
          stop("The argument should be an object of class 'ISOFeatureType'")
        }
        return(self$delListElement("featureType", featureType))
      },
      
      #addConstraint
      addConstraint = function(constraint){
        if(!is(constraint, "ISOConstraint")){
          constraint <- ISOConstraint$new(description = constraint)
        }
        return(self$addListElement("constrainedBy", constraint))
      },
      
      #delConstraint
      delConstraint = function(constraint){
        if(!is(constraint, "ISOConstraint")){
          constraint <- ISOConstraint$new(description = constraint)
        }
        return(self$delListElement("constrainedBy", constraint))
      },
      
      #setDefinitionReference
      setDefinitionReference = function(definitionReference){
        if(!is(definition, "ISODefinitionReference")){
          stop("The argument should be an object of class 'ISODefinitionReference'")
        }
        self$definitionReference = definitionReference
      }
    )         
)
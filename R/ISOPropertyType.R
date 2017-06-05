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
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
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
      
      initialize = function(xml = NULL){
        super$initialize(
          xml = xml,
          element = private$xmlElement,
          namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
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
        self$cardinality = ISOMultiplicityRange$new(lower = lower, upper = upper)
      }
    )         
)
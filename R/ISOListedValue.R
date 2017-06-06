#' ISOListedValue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO listed value
#' @return Object of \code{\link{R6Class}} for modelling an ISOListedValue
#' @format \code{\link{R6Class}} object.
#'
#' @field code
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOListedValue
#'  }
#'  \item{\code{setLabel(label)}}{
#'    Sets the label
#'  }
#'  \item{\code{setCode(code)}}{
#'    Sets the code
#'  }
#'  \item{\code{setDefinition(definition)}}{
#'    Sets the definition
#'  }
#'  \item{\code{setDefinitionReference(definitionReference)}}{
#'    Sets the definition reference
#'  }
#' }
#' 
#' @examples 
#'   val <- ISOListedValue$new()
#'   val$setCode("code1")
#'   val$setLabel("label1")
#'   val$setDefinition("definition1")
#'   xml <- val$encode()
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOListedValue <- R6Class("ISOListedValue",
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = "FC_ListedValue",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #+ label: character
     label = NULL,
     #+ code [0..1]: character
     code = NULL,
     #+ definition [0..1]: character
     definition = NULL,
     #+ definitionReference [0..1]: ISODefinitionReference
     definitionReference = NULL,
     
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setLabel
     setLabel = function(label){
       self$label = label
     },
     
     #setCode
     setCode = function(code){
       self$code <- code
     },
     
     #setDefinition
     setDefinition = function(definition){
       self$definition <- definition
     },
     
     #setDefinitionReference
     setDefinitionReference = function(definitionReference){
       if(!is(definitionReference, "ISODefinitionReference")){
         stop("The argument should be an object of class 'ISODefinitionReference'")
       }
       self$definitionReference <- definitionReference
     }
   )         
)
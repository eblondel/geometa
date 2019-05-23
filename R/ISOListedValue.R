#' ISOListedValue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO listed value
#' @return Object of \code{\link{R6Class}} for modelling an ISOListedValue
#' @format \code{\link{R6Class}} object.
#'
#' @field label [\code{\link{character}}] label
#' @field code [\code{\link{character}}] code
#' @field definition [\code{\link{character}}] definition
#' @field definitionReference [\code{\link{ISODefinitionReference}}] definition reference
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOListedValue}}
#'  }
#'  \item{\code{setLabel(label, locales)}}{
#'    Sets the label. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setCode(code, locales)}}{
#'    Sets the code. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setDefinition(definition, locales)}}{
#'    Sets the definition. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setDefinitionReference(definitionReference)}}{
#'    Sets the definition reference, object of class \code{\link{ISODefinitionReference}}
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
   inherit = ISOAbstractObject,
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
       super$initialize(xml = xml)
     },
     
     #setLabel
     setLabel = function(label, locales = NULL){
       self$label = label
       if(!is.null(locales)){
         self$label <- self$createLocalisedProperty(label, locales)
       }
     },
     
     #setCode
     setCode = function(code, locales = NULL){
       self$code <- code
       if(!is.null(locales)){
         self$code <- self$createLocalisedProperty(code, locales)
       }
     },
     
     #setDefinition
     setDefinition = function(definition, locales = NULL){
       self$definition <- definition
       if(!is.null(locales)){
         self$definition <- self$createLocalisedProperty(definition, locales)
       }
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
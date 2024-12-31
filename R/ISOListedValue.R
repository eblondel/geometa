#' ISOListedValue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO listed value
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOListedValue
#' @format \code{\link[R6]{R6Class}} object.
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
     
     #'@field label label: character
     label = NULL,
     #'@field code code [0..1]: character
     code = NULL,
     #'@field definition definition [0..1]: character
     definition = NULL,
     #'@field definitionReference definitionReference [0..1]: ISODefinitionReference
     definitionReference = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set label
     #'@param label label
     #'@param locales list of localized texts. Default is \code{NULL}
     setLabel = function(label, locales = NULL){
       self$label = label
       if(!is.null(locales)){
         self$label <- self$createLocalisedProperty(label, locales)
       }
     },
     
     #'@description Set code
     #'@param code code
     #'@param locales list of localized texts. Default is \code{NULL}
     setCode = function(code, locales = NULL){
       self$code <- code
       if(!is.null(locales)){
         self$code <- self$createLocalisedProperty(code, locales)
       }
     },
     
     #'@description Set definition
     #'@param definition definition
     #'@param locales list of localized texts. Default is \code{NULL}
     setDefinition = function(definition, locales = NULL){
       self$definition <- definition
       if(!is.null(locales)){
         self$definition <- self$createLocalisedProperty(definition, locales)
       }
     },
     
     #'@description Set definition reference
     #'@param definitionReference object of class \link{ISODefinitionReference}
     setDefinitionReference = function(definitionReference){
       if(!is(definitionReference, "ISODefinitionReference")){
         stop("The argument should be an object of class 'ISODefinitionReference'")
       }
       self$definitionReference <- definitionReference
     }
   )         
)

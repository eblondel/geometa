#' ISODefinitionReference
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO definition reference
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISODefinitionReference
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODefinitionReference <- R6Class("ISODefinitionReference",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "FC_DefinitionReference",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #'@field sourceIdentifier sourceIdentifier [0..1]: character
     sourceIdentifier = NULL,
     #'@field definitionSource definitionSource: ISODefinitionSource
     definitionSource = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set source identifier
     #'@param identifier identifier
     setSourceIdentifier = function(identifier){
       self$sourceIdentifier <- identifier
     },
     
     #'@description Set definition source
     #'@param source object of class \link{ISODefinitionSource} or \link{ISOCitation}
     setDefinitionSource = function(source){
       if(!is(source, "ISODefinitionSource")){
         if(is(source, "ISOCitation")){
           source <- ISODefinitionSource$new(source = source)
         }else{
           stop("The argument should be an object of class 'ISODefinitionSource' or 'ISOCitation'")
         }
       }
       self$definitionSource <- source
     }
   )                        
)

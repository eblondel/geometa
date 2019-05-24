#' ISODefinitionReference
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO definition reference
#' @return Object of \code{\link{R6Class}} for modelling an ISODefinitionReference
#' @format \code{\link{R6Class}} object.
#'
#' @field sourceIdentifier [\code{\link{character}}] source identifier
#' @field definitionSource [\code{\link{ISODefinitionSource}}] definition source
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISODefinitionReference}}
#'  }
#'  \item{\code{setSourceIdentifier(identifier)}}{
#'    Sets the source identifier as object of class \code{character}
#'  }
#'  \item{\code{setDefinitionSource(source)}}{
#'    Sets the definition source as object of class \code{\link{ISODefinitionSource}} or
#'    directly using a \code{\link{ISOCitation}}
#'  }
#' }
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
     
     #+ sourceIdentifier [0..1]: character
     sourceIdentifier = NULL,
     #+ definitionSource: ISODefinitionSource
     definitionSource = NULL,
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setSourceIdentifier
     setSourceIdentifier = function(identifier){
       self$sourceIdentifier <- identifier
     },
     
     #setDefinitionSource
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

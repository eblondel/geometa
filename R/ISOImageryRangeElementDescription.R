#' ISOImageryRangeElementDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery range element description
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOImageryRangeElementDescription
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'    #create object
#'    md <- ISOImageryRangeElementDescription$new()
#'    md$setName("name")
#'    md$setDefinition("description")
#'    md$addRangeElement("record1")
#'    md$addRangeElement("record2")
#'    xml <- md$encode()
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_RangeElementDescription}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MI_RangeElementDescription}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryRangeElementDescription <- R6Class("ISOImageryRangeElementDescription",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_RangeElementDescription",
     xmlNamespacePrefix = list(
       "19139" = "GMI",
       "19115-3" = "MRC"
     )
   ),
   public = list(
     
     #'@field name name [0..1] : character
     name = NULL,
     #'@field definition definition [0..1] : character
     definition = NULL,
     #'@field rangeElement rangeElement [0..*] : ISORecord
     rangeElement = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set name
     #'@param name name
     #'@param locales list of localized texts. Default is \code{NULL}
     setName = function(name, locales = NULL){
       if(!is.null(locales)){
         name <- self$createLocalisedProperty(name, locales)
       }
       self$name <- name
     },
     
     #'@description Set definition
     #'@param definition definition
     #'@param locales list of localized texts. Default is \code{NULL}
     setDefinition = function(definition, locales = NULL){
       if(!is.null(locales)){
         definition <- self$createLocalisedProperty(definition, locales)
       }
       self$definition <- definition
     },
     
     #'@description Adds range element
     #'@param record object of class \link{ISORecord} or \link{character}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addRangeElement = function(record){
       if(is(record, "character")){
         record <- ISORecord$new(value = record)
       }else{
         if(!is(record, "ISORecord")){
           stop("The argument should be an object of class 'character' or 'ISORecord'")
         }
       }
       return(self$addListElement("rangeElement", record))
     },
     
     #'@description Deletes range element
     #'@param record object of class \link{ISORecord} or \link{character}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delRangeElement = function(record){
       if(is(record, "character")){
         record <- ISORecord$new(value = record)
       }else{
         if(!is(record, "ISORecord")){
           stop("The argument should be an object of class 'character' or 'ISORecord'")
         }
       }
       return(self$delListElement("rangeElement", record))
     }
   )                        
)

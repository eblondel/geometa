#' ISOImageryRangeElementDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery range element description
#' @return Object of \code{\link{R6Class}} for modelling an ISOImageryRangeElementDescription
#' @format \code{\link{R6Class}} object.
#'
#' @field name [\code{\link{character}}]
#' @field description [\code{\link{character}}]
#' @field rangeElement [\code{\link{ISORecord}}]
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryRangeElementDescription}}
#'  }
#'  \item{\code{setName(name, locales)}}{
#'    Sets the name. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setDefinition(description, locales)}}{
#'    Sets the description. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{addRangeElement(record)}}{
#'    Add range element, object of class \code{\link{ISORecord}}
#'  }
#'  \item{\code{delRangeElement(record)}}{
#'    Deletes range element, object of class \code{\link{ISORecord}}
#'  }
#' }
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
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryRangeElementDescription <- R6Class("ISOImageryRangeElementDescription",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_RangeElementDescription",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     
     #+ name [0..1] : character
     name = NULL,
     #+ description [0..1] : character
     definition = NULL,
     #+ rangeElement [0..*] : ISORecord
     rangeElement = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setName
     setName = function(name, locales = NULL){
       if(!is.null(locales)){
         name <- self$createLocalisedProperty(name, locales)
       }
       self$name <- name
     },
     
     #setDefinition
     setDefinition = function(definition, locales = NULL){
       if(!is.null(locales)){
         definition <- self$createLocalisedProperty(definition, locales)
       }
       self$definition <- definition
     },
     
     #addRangeElement
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
     
     #delRangeElement
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
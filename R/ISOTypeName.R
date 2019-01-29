#' ISOTypeName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO typename
#' @return Object of \code{\link{R6Class}} for modelling an ISOTypeName
#' @format \code{\link{R6Class}} object.
#'
#' @field aName
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, aName)}}{
#'    This method is used to instantiate an ISOTypeName
#'  }
#'  \item{\code{setName(aName, locales)}}{
#'    Sets the aName. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#' }
#' 
#' @examples 
#'   typeName <- ISOTypeName$new(aName = "name")
#'   xml <- typeName$encode()
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTypeName <- R6Class("ISOTypeName",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "TypeName",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
     
     #+ aName: character
     aName = NULL,
     
     initialize = function(xml = NULL, aName = NULL){
       super$initialize(xml = xml)
       if(is.null(xml)){
         if(!is.null(aName)) self$setName(aName)
       }
     },
     
     #setName
     setName = function(aName, locales = NULL){
       self$aName <- aName
       if(!is.null(locales)){
         self$aName <- self$createLocalisedProperty(aName, locales)
       }
     }
   )         
)
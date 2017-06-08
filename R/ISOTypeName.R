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
#'  \item{\code{setName(aName)}}{
#'    Sets the aName
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
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = "TypeName",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
     
     #+ aName: character
     aName = NULL,
     
     initialize = function(xml = NULL, aName = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
       if(is.null(xml)){
         if(!is.null(aName)) self$setName(aName)
       }
     },
     
     #setName
     setName = function(aName){
       self$aName <- aName
     }
   )         
)
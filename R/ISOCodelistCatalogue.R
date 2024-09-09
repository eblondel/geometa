#' ISOCodelistCatalogue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO citation series
#' @return Object of \code{\link{R6Class}} for modelling an ISOCodelistCatalogue
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCodelistCatalogue <- R6Class("ISOCodelistCatalogue",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "CT_CodelistCatalogue",
     xmlNamespacePrefix = list(
       "19115-1/2" = "GMX",
       "19115-3" = "CAT"
     )
   ),
   public = list(
     #'@field name name 
     name = NULL,
     #'@field scope scope
     scope = NULL,
     #'@field fieldOfApplication field of application
     fieldOfApplication = NULL,
     #'@field versionNumber version number
     versionNumber = NULL,
     #'@field versionDate version date
     versionDate = NULL,
     #'@field codelistItem codelist items
     codelistItem = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Get codelists. The method ensure an harmonized output made of objects of
     #' class \link{ISOCodelist}. In the catalogue is built of objects of class \link{ISOCodeListDictionary}, these
     #' will be mapped as \link{ISOCodelist}, to facilitate the consumption of codelists by \pkg{geometa}
     getCodelists = function(){
        items <- lapply(self$codelistItem, function(item){
           the_item <- item
           if(is(item, "ISOCodeListDictionary")){
              the_item = item$toISOCodelist()
           }
           return(the_item)
        })
        return(items)
     },
     
     #'@description Get codelist by id.
     #'@param id id
     #'@return object of class \link{ISOCodelist}
     getCodelist = function(id){
        cls <- self$getCodelists()
        cls <- cls[sapply(cls, function(x){if(x$identifier$value == id) return(TRUE) else return(FALSE)})]
        cl <- NULL
        if(length(cls)>0) cl <- cls[[1]]
        return(cl)
     }
   )
)
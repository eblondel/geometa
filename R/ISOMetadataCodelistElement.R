#' ISOMetadataCodelistElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO code element
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata codelist element
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOMetadataCodelistElement
#'  }
#'  \item{\code{getAcceptedValues()}}{
#'    This method allows to get the codelist accepted values
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadataCodelistElement <- R6Class("ISOMetadataCodelistElement",
   inherit = ISOMetadataElement,
   public = list(
     codelistId = NULL,
     attrs = list(),
     value = NULL,
     initialize = function(xml = NULL, id, value, addCodeSpaceAttr = TRUE, setValue = TRUE){
       super$initialize(
         xml = xml,
         element = id,
         namespace = ISOMetadataNamespace$GMD
       )
       if(!is.null(xml)){
         value <- xmlGetAttr(xml, "codeListValue") #codeListValue should be as attribute
         if(is.null(value)) value <- xmlValue(xml) #try to pick up value instead
       }
       
       cl <- getISOCodelist(id)
       if(is.null(cl)){
         stop(sprintf("No ISO codelist for identifier '%s'", id))
       }
       self$codelistId = cl
       clEntry <- cl$entries[cl$entries$value == value,]
       if(nrow(clEntry)==0){
         stop(sprintf("No ISO '%s' codelist entry for value '%s'", id, value))
       }
       clEntry <- clEntry[1L,]
       self$attrs <- list(
        codeList = sprintf("%s/Codelist/%s#%s",
                           .geometa.iso$schemaBaseUrl, cl$refFile, id),
        codeListValue = clEntry$value
       )
       if(addCodeSpaceAttr){
         self$attrs <- c(self$attrs, codeSpace = "eng")
       }
       if(setValue){
         self$value <-clEntry$description
       }
     },
     
     #getAcceptedValues
     getAcceptedValues = function(){
       return(getISOCodelist(self$codelistId)$entries$value)
     }
   )                        
)

ISOMetadataCodelistElement$values = function(class, labels = FALSE){
  fields <- "value"
  if(labels) fields <- c(fields, "description")
  return(getISOCodelist(class$private_fields$xmlElement)$entries[,fields])
}
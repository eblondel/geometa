#' ISOCodeListValue
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
#'  \item{\code{new(xml, id, value, description, addCodeListAttrs, addCodeSpaceAttr, setValue)}}{
#'    This method is used to instantiate an ISOCodeListValue. By default,
#'    \code{addCodeListAttrs = TRUE}, to add codelist atributes to root XML. The 
#'    parameter \code{addCodeSpaceAttr = TRUE} by default, and ignored if the valueof
#'    \code{addCodeLisAttrs} is set to \code{FALSE}. The argument \code{setValue}
#'    sets the value as node text (defaut is \code{TRUE}).
#'    
#'  }
#'  \item{\code{getAcceptedValues()}}{
#'    This method allows to get the codelist accepted values
#'  }
#' }
#' 
#' @note Abstract ISO codelist class used internally by geometa
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCodeListValue <- R6Class("ISOCodeListValue",
   inherit = ISOAbstractObject,
   private = list(
     printAttrs = list()
   ),
   public = list(
     codelistId = NULL,
     attrs = list(),
     value = NULL,
     valueDescription = NULL,
     initialize = function(xml = NULL, id, value, description = NULL,
                           addCodeListAttrs = TRUE,
                           addCodeSpaceAttr = TRUE,
                           setValue = TRUE){
       super$initialize(xml = xml)
       if(!is.null(xml)){
         value <- xmlGetAttr(xml, "codeListValue") #codeListValue should be as attribute
         description <- xmlValue(xml) #try to pick up value instead
         if(is.null(value)) value <- description
       }
       if(id=="MD_ScopeCode") id <- "MX_ScopeCode"
       cl <- getISOCodelist(id)
       if(is.null(cl)){
         stop(sprintf("No ISO codelist for identifier '%s'", id))
       }
       self$codelistId = cl
       clEntry <- cl$entries[cl$entries$value == value,]
       clValue <- ""
       clDescription <- ""
       clCodeSpace <- ""
       if(nrow(clEntry)==0){
         warning(sprintf("No ISO '%s' codelist entry for value '%s'", id, value))
         clValue <- value
         if(!is.null(description)){
           setValue <- TRUE
           clDescription <- description
           self$valueDescription <- clDescription
         }
       }else{
         clCodeSpace <- cl$codeSpace
         clEntry <- clEntry[1L,]
         clValue <- clEntry$value
         clDescription <- clEntry$description
         self$valueDescription <- clDescription
       }
       
       if(id == "LanguageCode"){
         clUrl <- .geometa.iso$languageUrl
       }else{
         clUrl <- sprintf("%s/Codelist/%s#%s", .geometa.iso$schemaBaseUrl, cl$refFile, id)
       }
       
       if(addCodeListAttrs){
         self$attrs <- list(
           codeList = clUrl,
           codeListValue = clValue
         )
         if(addCodeSpaceAttr){
           self$attrs <- c(self$attrs, codeSpace = clCodeSpace)
         }
         if(setValue){
           self$value <-clDescription
         }
       }else{
         self$value <- clValue 
       }
       
       self$printAttrs <- list(
         codeList = clUrl,
         codeListValue = clValue,
         codeSpace = clCodeSpace
       )
       
     },
     
     #getAcceptedValues
     getAcceptedValues = function(){
       return(self$codelistId$entries$value)
     }
   )                        
)

ISOCodeListValue$values = function(class, labels = FALSE){
  fields <- "value"
  if(labels) fields <- c(fields, "description")
  element <- class$private_fields$xmlElement
  if(element == "MD_ScopeCode") element <- "MX_ScopeCode"
  return(getISOCodelist(element)$entries[,fields])
}
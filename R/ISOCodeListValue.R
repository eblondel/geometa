#' ISOCodeListValue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO code element
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata codelist element
#' @format \code{\link{R6Class}} object.
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
      printAttrs = list(),
      xmlElement = "CodeListValue",
      xmlNamespacePrefix = "GCO"
   ),
   public = list(
     #'@field codelistId codelist ID
     codelistId = NULL,
     #'@field attrs attrs
     attrs = list(),
     #'@field value value
     value = NULL,
     #'@field valueDescription value description
     valueDescription = NULL,
     
     #'@description Method used to instantiate an \link{ISOCodeListValue}. By default,
     #'    \code{addCodeListAttrs = TRUE}, to add codelist atributes to root XML. The 
     #'    parameter \code{addCodeSpaceAttr = TRUE} by default, and ignored if the valueof
     #'    \code{addCodeLisAttrs} is set to \code{FALSE}. The argument \code{setValue}
     #'    sets the value as node text (defaut is \code{TRUE}). The argument \code{setValueDescription}
     #'    allows to force having description set as value, default is \code{FALSE} in which case
     #'    the name will be preferred, and in case no name is provided, code value will be used.
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param id id
     #'@param value value
     #'@param description description
     #'@param addCodeListAttrs add codelist attributes?
     #'@param addCodeSpaceAttr add codespace attribute?
     #'@param setValue set value?
     #'@param setValueDescription set value description?
     initialize = function(xml = NULL, id, value = NULL, description = NULL,
                           addCodeListAttrs = TRUE,
                           addCodeSpaceAttr = TRUE,
                           setValue = TRUE,
                           setValueDescription = FALSE){
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
       clCodeSpace <- cl$codeSpace
       
       clValue <- ""
       clName <- NA
       clDescription <- ""
       
       if(length(cl$codeEntry)>0){
         if(!is.null(value)){
           clEntry <- cl$codeEntry[cl$codeEntry$value == value,]
           if(nrow(clEntry)==0){
             warning(sprintf("No ISO '%s' codelist entry for value '%s'", id, value))
             clValue <- value
             if(!is.null(description)){
               setValue <- TRUE
               clName <- description
               clDescription <- description
               self$valueDescription <- clDescription
             }
           }
         }else{
           clEntry <- cl$codeEntry[1,]
         }
         
         if(!is.null(value) & nrow(clEntry)>0){
           clEntry <- clEntry[1L,]
           clValue <- clEntry$value
           clName <- clEntry$name
           clDescription <- ifelse(!is.na(clName), clEntry$name, clEntry$description)
           if(setValueDescription) clDescription <- clEntry$description
           self$valueDescription <- clDescription
         }
       }else{
         clValue <- value
         if(!is.null(description)){
           setValue <- TRUE
           clName <- description
           clDescription <- description
           self$valueDescription <- clDescription
         }
       }
           
       isLocalFile <- !grepl("^http", cl$refFile) & !grepl("^https", cl$refFile)
       clUrl <- paste(cl$refFile, id, sep="#")
       clUrl <- gsub("ML_", "", clUrl)
       if(isLocalFile) clUrl <- paste(getGeometaOption("codelistUrl"), clUrl, sep="/")
       if(id == "LanguageCode"){
         langUrlOp <- getGeometaOption("languageUrl")
         if(!is.null(langUrlOp)) clUrl <- langUrlOp
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
           self$value <- ifelse(!is.na(clName), clName, clValue)
           if(setValueDescription) self$value <- clDescription
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
     
     #'@description Get accepted values
     #'@return a vector of class \link{character}
     getAcceptedValues = function(){
       return(self$codelistId$codeEntry$value)
     }
   )                        
)

ISOCodeListValue$values = function(class, labels = FALSE){
  fields <- "value"
  if(labels) fields <- c(fields, "name", "description")
  element <- class$private_fields$xmlElement
  if(element == "MD_ScopeCode") element <- "MX_ScopeCode"
  return(getISOCodelist(element)$codeEntry[,fields])
}
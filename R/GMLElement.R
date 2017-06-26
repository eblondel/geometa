#' GMLElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML element
#' @return Object of \code{\link{R6Class}} for modelling an GML element
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults)}}{
#'    This method is used to instantiate a GML element
#'  }
#' }
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLElement <- R6Class("GMLElement",
    inherit = GMLAbstractGML,
    lock_objects = FALSE,
    private = list(
      xmlElement = "Element",
      xmlNamespacePrefix = "GML"
    ),
    public = list(
      initialize = function(xml = NULL, element = NULL, attrs = list(), defaults = list()){
        super$initialize(xml = xml, element = element, attrs = attrs, defaults = defaults)
      },
      
      decode = function(xml){
        fieldName <- xmlName(xml)
        nsPrefix <- ""
        fNames <- unlist(strsplit(fieldName, ":"))
        if(length(fNames)>1){
          fieldName <- fNames[2]
        }
        self$element = fieldName
        
        #set attrs if any
        self$attrs <- as.list(xmlAttrs(xml, TRUE, FALSE))
        
        
        fieldValue <- xmlValue(xml, recursive = FALSE)
        if(length(fieldValue)>0){
          #set value if any
          fieldValue <- private$toComplexTypes(fieldValue)
          if(!is.na(fieldValue)) self$setValue(fieldValue)
        }else{
          #set children if any
          children <- xmlChildren(xml, encoding = private$encoding)
          if(length(children)>0){
            for(i in 1:length(children)){
              childXML <- children[[i]]
              childName <- names(children)[i]
              childElem <- GMLElement$new(element = childName)
              childElem$decode(xml = childXML)
              if(is(self[[childName]], "list")){
                self[[childName]] <- c(self[[childName]], childElem)
              }else{
                self[[childName]] <- childElem
              }
            }
          }
        }
      }
    )
)

GMLElement$create <- function(element, value = NULL,  attrs = list(), href = NULL, 
                              codeList = NULL, codeListValue = NULL, codeSpace = NULL){
  #element
  gmlElem <- GMLElement$new(element = element)
  #value
  if(!is.null(value)) gmlElem$setValue(value)
  #general attributes
  for(attrName in names(attrs)){
    gmlElem$setAttr(attrName, attrs[[attrName]])
  }
  #specific attributes
  if(!is.null(href)) gmlElem$setHref(href)
  if(!is.null(codeList)) gmlElem$setCodeList(codeList)
  if(!is.null(codeListValue)) gmlElem$setCodeListValue(codeListValue)
  if(!is.null(codeSpace)) gmlElem$setCodeSpace(codeSpace)
  
  return(gmlElem)
}
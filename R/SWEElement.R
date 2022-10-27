#' SWEElement
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
SWEElement <- R6Class("SWEElement",
  inherit = SWEAbstractObject,
  lock_objects = FALSE,
  private = list(
    xmlElement = "Element",
    xmlNamespacePrefix = "SWE"
  ),
  public = list(
    
    #'@description Initializes a generic abstract SWE element
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param element element
    #'@param attrs attrs
    #'@param defaults defaults
    #'@param xmlNamespacePrefix XML namespace prefix. Default is \code{"SWE"}
    initialize = function(xml = NULL, element = NULL, attrs = list(), defaults = list(), xmlNamespacePrefix = "SWE"){
      private$xmlNamespacePrefix <- xmlNamespacePrefix
      super$initialize(xml = xml, element = element, attrs = attrs, defaults = defaults, wrap = FALSE)
    },
    
    #'@description Decodes object from XML
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
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
        if(fieldValue %in% c("true","false")) fieldValue <- as.logical(fieldValue)
        fieldValue <- private$toComplexTypes(fieldValue)
        if(!is.na(fieldValue)) self$setValue(fieldValue)
      }else{
        #set children if any
        children <- xmlChildren(xml, encoding = private$encoding)
        if(length(children)>0){
          for(i in 1:length(children)){
            childXML <- children[[i]]
            childName <- names(children)[i]
            childElem <- SWEElement$new(element = childName)
            childElem$decode(xml = childXML)
            if(is(self[[childName]], "list") | !is.null(self[[childName]])){
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

SWEElement$create <- function(element, value = NULL,  attrs = list(), href = NULL, 
                              codeList = NULL, codeListValue = NULL, codeSpace = NULL,
                              xmlNamespacePrefix = "SWE"){
  #element
  sweElem <- SWEElement$new(element = element, xmlNamespacePrefix = xmlNamespacePrefix)
  #value
  if(!is.null(value)) sweElem$setValue(value)
  #general attributes
  for(attrName in names(attrs)){
    sweElem$setAttr(attrName, attrs[[attrName]])
  }
  #specific attributes
  if(!is.null(href)) sweElem$setHref(href)
  if(!is.null(codeList)) sweElem$setCodeList(codeList)
  if(!is.null(codeListValue)) sweElem$setCodeListValue(codeListValue)
  if(!is.null(codeSpace)) sweElem$setCodeSpace(codeSpace)
  
  return(sweElem)
}
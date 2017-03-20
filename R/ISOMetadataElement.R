#' ISOMetadataElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO metadata element
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata Element
#' @format \code{\link{R6Class}} object.
#'
#' @section Abstract Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate an ISOMetadataElement
#'  }
#'  \item{\code{decode(xml)}}{
#'    Decodes a ISOMetadata* R6 object from XML representation
#'  }
#'  \item{\code{encode()}}{
#'    Encodes a ISOMetadata* R6 object to XML representation
#'  }
#'  \item{\code{wrapBaseElement(field, fieldObj)}}{
#'    Wraps a base element type
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadataElement <- R6Class("ISOMetadataElement",
  public = list(
    element = NA,
    namespace = NA,
    initialize = function(xml = NULL, element, namespace){
      if(!is.null(xml)){
        self$decode(xml)
      }else{
        self$element = element
        self$namespace = namespace
      }
    },
    
    #getNamespaceDefinition
    getNamespaceDefinition = function(){
      return(self$namespace$getDefinition())
    },
    
    #decode
    decode = function(xml){
      self$element = xmlName(xml)
      ns <- xmlNamespaceDefinitions(xml)
      ns[[1]] = ns[[1]]$uri
      self$namespace = ns
      stop("Unimplement decoding")
    },
    
    #encode
    encode = function(addNS = TRUE){
      
      #list of fields to encode as XML
      fields <- rev(names(self))
      
      #root XML
      rootXML <- NULL
      rootXMLAttrs <- list()
      if("attrs" %in% fields){
        rootXMLAttrs <- self[["attrs"]]
      }
      rootNamespaces <- self$getNamespaceDefinition()
      if(self$element == "MD_Metadata"){
        rootNamespaces <- sapply(ISOMetadataNamespace$all(), function(x){x$getDefinition()})
      }
      if(addNS){
        rootXML <- xmlOutputDOM(
          tag = self$element,
          attrs = rootXMLAttrs,
          nameSpace = self$namespace$id,
          nsURI = rootNamespaces
        )
      }else{
        rootXML <- xmlOutputDOM(
          tag = self$element,
          attrs = rootXMLAttrs,
          nameSpace = self$namespace$id
        )
      }
      
      #fields
      fields <- fields[!sapply(fields, function(x){
        (class(self[[x]]) %in% c("environment", "function")) ||
          (x %in% c("element", "namespace", "attrs", "codelistId"))
      })]
      
      for(field in fields){
        fieldObj <- self[[field]]
        if(!is.null(fieldObj)){
          if(is(fieldObj, "ISOMetadataElement")){
            wrapperNode <- xmlOutputDOM(
              tag = field,
              nameSpace = ISOMetadataNamespace$GMD$id
            )
            wrapperNode$addNode(fieldObj$encode(addNS = FALSE))
            rootXML$addNode(wrapperNode$value())
          }else if(is(fieldObj, "list")){
            for(item in fieldObj){
              wrapperNode <- xmlOutputDOM(
                tag = field,
                nameSpace = ISOMetadataNamespace$GMD$id
              )
              nodeValue <- NULL
              if(is(item, "ISOMetadataElement")){
                nodeValue <- item$encode(addNS = FALSE)
              }else{
                nodeValue <- self$wrapBaseElement(field, item)
              }
              wrapperNode <- xmlOutputDOM(
                tag = field,
                nameSpace = ISOMetadataNamespace$GMD$id
              )
              wrapperNode$addNode(nodeValue)
              rootXML$addNode(wrapperNode$value())
            }
          }else{
            if(field == "value"){
              #special case of node value
              rootXML$addNode(xmlTextNode(fieldObj))
            }else{
              #general case of gco wrapper element
              wrapperNode <- xmlOutputDOM(
                tag = field,
                nameSpace = ISOMetadataNamespace$GMD$id
              )
              dataObj <- self$wrapBaseElement(field, fieldObj)
              wrapperNode$addNode(dataObj)
              rootXML$addNode(wrapperNode$value())
            }
            
          }
        }
      }

      return(rootXML$value())
    },
    
    #wrapBaseElement
    wrapBaseElement = function(field, fieldObj){
      dataType <- class(fieldObj)
      if(all(dataType == c("POSIXct","POSIXt"))) dataType <- "datetime"
      if(regexpr("^http*",fieldObj, TRUE) > 0 | regexpr("^ftp*",fieldObj, TRUE) > 0){
        dataType <- "url"
      }
      #exception for EPSG code uris
      #TODO think about cleaner solution for URLs + anchors
      if(is(self, "ISOIdentifier") && field == "code"){
        dataType <- "character"
      }
      dataObj <- switch(dataType,
                        "character" = ISOBaseCharacterString$new(value = fieldObj),
                        "numeric"   = ISOBaseDecimal$new(value = fieldObj),
                        "integer"   = ISOBaseInteger$new(value = fieldObj),
                        "logical"   = ISOBaseBoolean$new(value = fieldObj),
                        "datetime"  = ISOBaseDateTime$new(value = fieldObj),
                        "date"      = ISOBaseDate$new(value = fieldObj),
                        "url"       = ISOBaseURL$new(value = fieldObj),
                        NULL
      )
      if(!is.null(dataObj)){
        dataObj <- dataObj$encode(addNS = FALSE)
      }
      return(dataObj)
    }
  )                              
)
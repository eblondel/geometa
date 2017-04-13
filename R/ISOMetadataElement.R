#' ISOMetadataElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO metadata element
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata Element
#' @format \code{\link{R6Class}} object.
#'
#' @section Static Methods:
#' \describe{
#'  \item{\code{getISOClassByNode(node)}}{
#'    Inherit the ISO class matching an XML document or node
#'  }
#'  \item{\code{compare(metadataElement1, metadataElement2)}}{
#'    Compares two metadata elements objects. Returns TRUE if they are equal,
#'    FALSE otherwise. The comparison of object is done by comparing the XML 
#'    representation of the objects (since no R6 object comparison method seems 
#'    to exist)
#'  }
#' }
#'
#' @section Abstract Methods:
#' \describe{
#'  \item{\code{new(xml, element, namespace)}}{
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
#'  \item{\code{contains(field, metadataElement)}}{
#'    Indicates of the present class object contains an metadata element object
#'    for a particular list-based field.
#'  }
#'  \item{\code{addListElement(field, metadataElement)}}{
#'    Adds a metadata element to a list-based field. Returns TRUE if the element
#'    has been added, FALSE otherwise. In case an element is already added, the 
#'    element will not be added and this method will return FALSE.
#'  }
#'  \item{\code{delListElement(field, metadataElement)}}{
#'    Deletes a metadata element from a list-based field. Returns TRUE if the element
#'    has been deleted, FALSE otherwise. In case an element is abstent, this method 
#'    will return FALSE.
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadataElement <- R6Class("ISOMetadataElement",
  public = list(
    element = NA,
    namespace = NA,
    defaults = list(),
    initialize = function(xml = NULL, element, namespace, defaults = list()){
      self$element = element
      self$namespace = namespace
      self$defaults = defaults
      if(!is.null(xml)){
        self$decode(xml)
      }
    },
    
    #getNamespaceDefinition
    getNamespaceDefinition = function(){
      return(self$namespace$getDefinition())
    },
    
    #decode
    decode = function(xml){
      if(is(xml, "XMLInternalDocument")){
        xml <- xmlChildren(xml)[[1]]
      }
      for(child in xmlChildren(xml)){
        fieldName <- xmlName(child)
        if(!(fieldName %in% names(self))) next
        
        fieldClass <- NULL
        if(!is(child, "XMLInternalTextNode")){
          fieldClass <- ISOMetadataElement$getISOClassByNode(child)
          if(is.null(fieldClass)){
            child <- xmlChildren(child)[[1]]
            fieldClass <- ISOMetadataElement$getISOClassByNode(child)
          }
        }
        fieldValue <- NULL
        if(!is.null(fieldClass)){
          if(regexpr("^ISOBase.+", fieldClass$classname)>0){
            fieldValue <- xmlValue(child)
            fieldValue <- switch(fieldClass$classname,
                                 "ISOBaseBoolean" = as.logical(fieldValue),
                                 "ISOBaseInteger" = as.integer(fieldValue),
                                 "ISOBaseDecimal" = as.numeric(fieldValue),
                                 "ISOBaseDate" = as.Date(fieldValue),
                                 "ISOBaseDateTime" = as.POSIXct(strptime(fieldValue, "%Y-%m-%dT%H:%M:%S")),
                                 fieldValue
            )
          }else{
            if(fieldClass$classname == "ISOIdentifier"){
              prefix <- unlist(strsplit(xmlName(child),"_"))[1]
              fieldValue <- fieldClass$new(xml = child, prefix = prefix)
            }else{
              fieldValue <- fieldClass$new(xml = child)
            }
          }
          if(is(self[[fieldName]], "list")){
            self[[fieldName]] <- c(self[[fieldName]], fieldValue)
          }else{
            self[[fieldName]] <- fieldValue
          }
        }else{
          self$value <- as(child, "character") 
        }
        
      }
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
          (x %in% c("element", "namespace", "defaults", "attrs", "codelistId"))
      })]
      
      for(field in fields){
        fieldObj <- self[[field]]
        
        #default values management
        if(is.null(fieldObj) || (is.list(fieldObj) & length(fieldObj)==0)){
          if(field %in% names(self$defaults)){
            fieldObj <- self$defaults[[field]]
          }
        }
        
        #user values management
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
              #if(class(fieldObj)=="character"){
              #  if(Encoding(fieldObj)!="UTF-8") fieldObj <- iconv(fieldObj, from="ISO-8859-1", to="UTF-8")
              #}
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
      out <- rootXML$value()
      #if(addNS){
        out <- as(out, "XMLInternalNode")
      #}
      return(out)
    },
    
    #wrapBaseElement
    wrapBaseElement = function(field, fieldObj){
      dataType <- class(fieldObj)
      if(all(dataType == c("POSIXct","POSIXt"))) dataType <- "datetime"
      if((regexpr("^http*",fieldObj, TRUE) > 0 | regexpr("^ftp*",fieldObj, TRUE) > 0) && is(self, "ISOOnlineResource")){
        dataType <- "url"
      }
      dataObj <- switch(tolower(dataType),
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
    },
    
    #contains
    contains = function(field, metadataElement){
      out = FALSE
      if(length(self[[field]]) == 0){
        out = FALSE
      }else{
        out = any(sapply(self[[field]], function(x){
          ISOMetadataElement$compare(x, metadataElement)
        }))
      }
      return(out)
    },
    
    #addListElement
    addListElement = function(field, metadataElement){
      startNb <- length(self[[field]])
      if(!self$contains(field, metadataElement)){
        self[[field]] = c(self[[field]], metadataElement)
      }
      endNb = length(self[[field]])
      return(endNb == startNb+1)
    },
    
    #delListElement
    delListElement = function(field, metadataElement){
      startNb <- length(self[[field]])
      if(self$contains(field, metadataElement)){
        self[[field]] = self[[field]][!sapply(self[[field]], ISOMetadataElement$compare, metadataElement)]
      }
      endNb = length(self[[field]])
      return(endNb == startNb-1)
    }
  )                              
)

ISOMetadataElement$getISOClassByNode = function(node){
  outClass <- NULL
  if(!is(node, "XMLInternalDocument")) node <- xmlDoc(node)
  nodeElement <- xmlRoot(node)
  list_of_classes <- rev(ls("package:geometa"))
  list_of_classes <- list_of_classes[regexpr("^ISO.+", list_of_classes)>0]
  for(classname in list_of_classes){
    class <- eval(parse(text=classname))
    if(length(class$private_fields)>0
       && !is.null(class$private_fields$xmlElement)
       && !is.null(class$private_fields$xmlNamespacePrefix)){
      nodeElementName <- xmlName(nodeElement)
      nodeElementNames <- unlist(strsplit(xmlName(nodeElement), ":"))
      if(length(nodeElementNames)>1){
        nodeElementName <- nodeElementNames[2]
      }
      if(nodeElementName %in% class$private_fields$xmlElement){
        outClass <- class
        break
      }
    }
  }
  return(outClass)
}

ISOMetadataElement$compare = function(metadataElement1, metadataElement2){
  text1 <- NULL
  if(is(metadataElement1, "ISOMetadataElement")){
    text1 <- as(XML::xmlDoc(metadataElement1$encode(FALSE)), "character")
  }else{
    text1 <- as.character(metadataElement1)
  }
  text2 <- NULL
  if(is(metadataElement2, "ISOMetadataElement")){
    text2 <- as(XML::xmlDoc(metadataElement2$encode(FALSE)), "character")
  }else{
    text2 <- as.character(metadataElement2)
  }
  return(text1 == text2)
}
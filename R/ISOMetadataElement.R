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
#' @note Abstract ISO Metadata class used internally by geometa
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadataElement <- R6Class("ISOMetadataElement",
  private = list(
    encoding = options("encoding")
  ),
  public = list(
    wrap = TRUE,
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
    
    #getClassName
    getClassName = function(){
      return(class(self)[1])
    },
    
    #print
    print = function(..., depth = 1){
      #list of fields to encode as XML
      fields <- rev(names(self))
      
      #fields
      fields <- fields[!sapply(fields, function(x){
        (class(self[[x]]) %in% c("environment", "function")) ||
          (x %in% c("wrap", "element", "namespace", "defaults", "attrs", "codelistId", "measureType"))
      })]
      
      cat(sprintf("<%s>", self$getClassName()))
      if(is(self, "ISOMetadataCodelistElement")){
        clVal <- self$attrs$codeListValue
        clDes <- self$codelistId$entries[self$codelistId$entries$value == clVal,"description"]
        cat(paste0(": ", clVal, " {",clDes,"}"))
      }
      
      for(field in fields){
        fieldObj <- self[[field]]
        
        #default values management
        if(is.null(fieldObj) || (is.list(fieldObj) & length(fieldObj)==0)){
          if(field %in% names(self$defaults)){
            fieldObj <- self$defaults[[field]]
          }
        }
        
        #user values management
        shift <- "...."
        if(!is.null(fieldObj)){
          if(is(fieldObj, "ISOMetadataElement")){
            cat(paste0("\n", paste(rep(shift, depth), collapse=""),"|-- ", field, " "))
            fieldObj$print(depth = depth+1)
          }else if(is(fieldObj, "list")){
            for(item in fieldObj){
              if(is(item, "ISOMetadataElement")){
                cat(paste0("\n", paste(rep(shift, depth), collapse=""),"|-- ", field, " "))
                item$print(depth = depth+1)
                if(is(item, "ISOMetadataCodelistElement")){
                  clVal <- item$attrs$codeListValue
                  clDes <- item$codelistId$entries[item$codelistId$entries$value == clVal,"description"]
                  cat(paste0(": ", clVal, " {",clDes,"}"))
                }
              }else{
                cat(paste0("\n", paste(rep(shift, depth), collapse=""),"|-- ", field, ": ", item))
              }
            }
          }else{
            cat(paste0("\n",paste(rep(shift, depth), collapse=""),"|-- ", field, ": ", fieldObj))
          }
        }
      }
      invisible(self)
    },
    
    #decode
    decode = function(xml){
      if(is(xml, "XMLInternalDocument")){
        xml <- xmlChildren(xml, encoding = private$encoding)[[1]]
      }
      for(child in xmlChildren(xml, encoding = private$encoding)){
        fieldName <- xmlName(child)
        fNames <- unlist(strsplit(fieldName, ":"))
        if(length(fNames)>1){
         fieldName <- fNames[2]
        }
        
        if(!(fieldName %in% names(self)) & fieldName != "text") next
        
        fieldClass <- NULL
        if(!is(child, "XMLInternalTextNode")){
          fieldClass <- ISOMetadataElement$getISOClassByNode(child)
          if(is.null(fieldClass)){
            child <- xmlChildren(child, encoding = private$encoding)[[1]]
            fieldClass <- ISOMetadataElement$getISOClassByNode(child)
          }
        }
        fieldValue <- NULL
        if(!is.null(fieldClass)){
          if(regexpr("^ISOBase.+", fieldClass$classname)>0){
            fieldValue <- xmlValue(child)
            
            #coerceTimePosition util
            coerceTimePosition <- function(fieldValue){
              outval <- NULL
              if(nchar(fieldValue)==10){
                outval <- as.Date(fieldValue)
              }else{
                outval <- as.POSIXct(strptime(fieldValue, "%Y-%m-%dT%H:%M:%S"), tz = "GMT")
              }
              return(outval)
            }
            
            fieldValue <- switch(fieldClass$classname,
                                 "ISOBaseBoolean" = as.logical(fieldValue),
                                 "ISOBaseInteger" = as.integer(fieldValue),
                                 "ISOBaseReal" = as.numeric(fieldValue),
                                 "ISOBaseDecimal" = {
                                   fieldValue <- as.numeric(fieldValue)
                                   class(fieldValue) <- "decimal"
                                   fieldValue
                                 },
                                 "ISOBaseDate" = as.Date(fieldValue),
                                 "ISOBaseDateTime" = as.POSIXct(strptime(fieldValue, "%Y-%m-%dT%H:%M:%S"), tz = "GMT"),
                                 "ISOBaseTimeBeginPosition" = coerceTimePosition(fieldValue),
                                 "ISOBaseTimeEndPosition" = coerceTimePosition(fieldValue),
                                 fieldValue
            )
          }else{    
            fieldValue <- fieldClass$new(xml = child)
          }
          if(is(self[[fieldName]], "list")){
            self[[fieldName]] <- c(self[[fieldName]], fieldValue)
          }else{
            self[[fieldName]] <- fieldValue
          }
        }else{
          self[["value"]] <- as(child, "character") 
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
      
      if(self$getClassName() %in%  c("ISOMetadata", "ISOFeatureCatalogue")){
        rootNamespaces <- sapply(ISOMetadataNamespace$all(), function(x){x$getDefinition()})
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
          (x %in% c("wrap", "element", "namespace", "defaults", "attrs", "codelistId", "measureType"))
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
            if(fieldObj$wrap){
              wrapperNode <- xmlOutputDOM(
                tag = field,
                nameSpace = self$namespace$id
              )
              wrapperNode$addNode(fieldObj$encode(addNS = FALSE))
              rootXML$addNode(wrapperNode$value())
            }else{
              rootXML$addNode(fieldObj$encode(addNS = FALSE))
            }
          }else if(is(fieldObj, "list")){
            for(item in fieldObj){
              nodeValue <- NULL
              if(is(item, "ISOMetadataElement")){
                nodeValue <- item
              }else{
                nodeValue <- self$wrapBaseElement(field, item)
              }
              if(nodeValue$wrap){
                wrapperNode <- xmlOutputDOM(
                  tag = field,
                  nameSpace = self$namespace$id
                )
                wrapperNode$addNode(nodeValue$encode(addNS = FALSE))
                rootXML$addNode(wrapperNode$value())
              }else{
                rootXML$addNode(nodeValue$encode(addNS = FALSE))
              }
            }
          }else{
            if(field == "value"){
              rootXML$addNode(xmlTextNode(fieldObj))
            }else{
              dataObj <- self$wrapBaseElement(field, fieldObj)
              if(!is.null(dataObj)){
                if(dataObj$wrap){
                  #general case of gco wrapper element
                  wrapperNode <- xmlOutputDOM(
                    tag = field,
                    nameSpace = self$namespace$id
                  )
                  wrapperNode$addNode(dataObj$encode(addNS = FALSE))
                  rootXML$addNode(wrapperNode$value())
                }else{
                  rootXML$addNode(dataObj$encode(addNS = FALSE))
                }
              }
            }
            
          }
        }
      }
      out <- rootXML$value()
      out <- as(out, "XMLInternalNode")
      if(addNS & self$element != "MD_Metadata"){
       xmlNamespaces(out, set = TRUE) <- self$getNamespaceDefinition()
      }
      return(out)
    },
    
    #wrapBaseElement
    wrapBaseElement = function(field, fieldObj){
      dataType <- class(fieldObj)
      if(all(dataType == c("POSIXct","POSIXt"))) dataType <- "datetime"
      if((regexpr("^(https?://)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([/\\w \\.-]*)*/?$", fieldObj, TRUE) > 0 | regexpr("^ftp*",fieldObj, TRUE) > 0) && is(self, "ISOOnlineResource")){
        dataType <- "url"
      }
      
      #specific coercing
      if(field == "beginPosition") dataType <- "begintime"
      if(field == "endPosition") dataType <- "endtime"
      
      #wrapping
      dataObj <- switch(tolower(dataType),
                        "character" = ISOBaseCharacterString$new(value = iconv(fieldObj, to  = "UTF-8//IGNORE")),
                        "numeric"   = ISOBaseReal$new(value = fieldObj),
                        "decimal"   = ISOBaseDecimal$new(value = fieldObj), #Requires specific class call
                        "integer"   = ISOBaseInteger$new(value = fieldObj),
                        "unlimitedinteger" = ISOUnlimitedInteger$new(value = fieldObj),
                        "logical"   = ISOBaseBoolean$new(value = fieldObj),
                        "datetime"  = ISOBaseDateTime$new(value = fieldObj),
                        "date"      = ISOBaseDate$new(value = fieldObj),
                        "url"       = ISOBaseURL$new(value = fieldObj),
                        "begintime" = ISOBaseTimeBeginPosition$new(value = fieldObj),
                        "endtime"   = ISOBaseTimeEndPosition$new(value = fieldObj),
                        NULL
      )
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
  nodeElementName <- xmlName(nodeElement)
  nodeElementNames <- unlist(strsplit(nodeElementName, ":"))
  if(length(nodeElementNames)>1){
    nodeElementName <- nodeElementNames[2]
  }
  list_of_classes <- rev(ls("package:geometa"))
  list_of_classes <- list_of_classes[regexpr("^ISO.+", list_of_classes)>0]
  for(classname in list_of_classes){
    class <- eval(parse(text=classname))
    if(length(class$private_fields)>0
       && !is.null(class$private_fields$xmlElement)
       && !is.null(class$private_fields$xmlNamespacePrefix)){

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

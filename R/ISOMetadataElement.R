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
#'  \item{\code{INFO(text)}}{
#'    Logger to report information. Used internally
#'  }
#'  \item{\code{WARN(text)}}{
#'    Logger to report warnings. Used internally
#'  }
#'  \item{\code{ERROR(text)}}{
#'    Logger to report errors. Used internally
#'  }
#'  \item{\code{print()}}{
#'    Provides a custom print output (as tree) of the current class
#'  }
#'  \item{\code{decode(xml)}}{
#'    Decodes a ISOMetadata* R6 object from XML representation
#'  }
#'  \item{\code{encode(addNS, validate, strict)}}{
#'    Encodes a ISOMetadata* R6 object to XML representation. By default, namespace
#'    definition will be added to XML root (\code{addNS = TRUE}), and validation
#'    of object will be performed (\code{validate = TRUE}) prior to its XML encoding.
#'    The argument \code{strict} allows to stop the encoding in case object is not
#'    valid, with a default value set to \code{FALSE}.
#'  }
#'  \item{\code{validate(xml, strict)}}{
#'    Validates the encoded XML against ISO 19139 XML schemas. If \code{strict} is
#'    \code{TRUE}, a error will be raised. Default is \code{FALSE}. 
#'  }
#'  \item{\code{getNamespaceDefinition(recursive)}}{
#'    Gets the namespace definition of the current ISO* class. By default, only
#'    the namespace definition of the current element is retrieved (\code{recursive = FALSE}).
#'  }
#'  \item{\code{getClassName()}}{
#'    Gets the class name
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
    encoding = options("encoding"),
    logger = function(type, text){
      cat(sprintf("[geometa][%s] %s \n", type, text))
    }
  ),
  public = list(
    
    #logger
    INFO = function(text){private$logger("INFO", text)},
    WARN = function(text){private$logger("WARN", text)},
    ERROR = function(text){private$logger("ERROR", text)},
    
    
    #fields
    #---------------------------------------------------------------------------
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
    
    #Main methods
    #---------------------------------------------------------------------------
    
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
    encode = function(addNS = TRUE, validate = TRUE, strict = FALSE){

      #list of fields to encode as XML
      fields <- rev(names(self))
      
      #root XML
      rootXML <- NULL
      rootXMLAttrs <- list()
      if("attrs" %in% fields){
        rootXMLAttrs <- self[["attrs"]]
      }
      
      #fields
      fields <- fields[!sapply(fields, function(x){
        (class(self[[x]]) %in% c("environment", "function")) ||
          (x %in% c("wrap", "element", "namespace", "defaults", "attrs", "codelistId", "measureType"))
      })]
      
      if(self$getClassName() %in%  c("ISOMetadata", "ISOFeatureCatalogue")){
        rootNamespaces <- sapply(ISOMetadataNamespace$all(), function(x){x$getDefinition()})
        rootXML <- xmlOutputDOM(
          tag = self$element,
          nameSpace = self$namespace$id,
          nsURI = rootNamespaces
        )
      }else{
        if(addNS){
          nsdefs <- self$getNamespaceDefinition(recursive = TRUE)
          rootXML <- xmlOutputDOM(
            tag = self$element,
            nameSpace = self$namespace$id,
            nsURI = nsdefs
          )
        }else{
          rootXML <- xmlOutputDOM(
            tag = self$element,
            nameSpace = self$namespace$id
          )
        }
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
        if(!is.null(fieldObj)){
          if(is(fieldObj, "ISOMetadataElement")){
            if(fieldObj$wrap){
              wrapperNode <- xmlOutputDOM(
                tag = field,
                nameSpace = self$namespace$id
              )
              wrapperNode$addNode(fieldObj$encode(addNS = FALSE, validate = FALSE))
              rootXML$addNode(wrapperNode$value())
            }else{
              rootXML$addNode(fieldObj$encode(addNS = FALSE, validate = FALSE))
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
                wrapperNode$addNode(nodeValue$encode(addNS = FALSE, validate = FALSE))
                rootXML$addNode(wrapperNode$value())
              }else{
                rootXML$addNode(nodeValue$encode(addNS = FALSE, validate = FALSE))
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
                  wrapperNode$addNode(dataObj$encode(addNS = FALSE, validate = FALSE))
                  rootXML$addNode(wrapperNode$value())
                }else{
                  rootXML$addNode(dataObj$encode(addNS = FALSE, validate = FALSE))
                }
              }
            }
            
          }
        }
      }
      out <- rootXML$value()
      out <- as(out, "XMLInternalNode")
      if(length(rootXMLAttrs)>0){
        suppressWarnings(xmlAttrs(out) <- rootXMLAttrs)
      }
      
      #validation vs. ISO 19139 XML schemas
      #no GFC validation for the timebeing
      if(self$namespace$id == "gfc") validate <- FALSE
      if(validate){
        self$validate(xml = out, strict = strict)
      }
      
      return(out)
    },
    
    #validate
    validate = function(xml = NULL, strict = FALSE){
      
      #xml
      if(is.null(xml)){
        #no GFC validation for the timebeing
        if(self$namespace$id == "gfc") return(TRUE)
        xml <- self$encode(addNS = TRUE, validate = FALSE, strict = strict)
      }
      
      #proceed with schema xml schema validation
      xsd <- getISOSchemas()
      report <- xmlSchemaValidate(xsd, xmlDoc(xml))
      
      #check validity on self
      isValid <- report$status == 0
      if(!isValid){
        loggerType <- ifelse(strict, "ERROR", "WARN")
        for(error in report$errors){
          errorMsg <- paste0(substr(error$msg, 1, nchar(error$msg)-2), " at line ", error$line, ".")
          self[[loggerType]](errorMsg)
        }
        msg <- sprintf("Object '%s' is INVALID according to ISO 19139 XML schemas!", self$getClassName())
        if(strict){
          self$ERROR(msg)
          stop(msg)
        }else{
          self$WARN(msg)
        }
      }else{
        self$INFO(sprintf("Object '%s' is VALID according to ISO 19139 XML schemas!", self$getClassName()))
      }
      return(isValid)
    },
    
    #Util & internal methods
    #---------------------------------------------------------------------------
    
    #getNamespaceDefinition
    getNamespaceDefinition = function(recursive = FALSE){
      nsdefs <- NULL
      if(recursive){
        #list of fields
        fields <- rev(names(self))
        fields <- fields[!sapply(fields, function(x){
          (class(self[[x]]) %in% c("environment", "function")) ||
            (x %in% c("wrap", "element", "namespace", "defaults", "attrs", "codelistId", "measureType"))
        })]
        
        selfNsdef <- self$getNamespaceDefinition()
        nsdefs <- list()
        invisible(lapply(fields, function(x){
          xObj <- self[[x]]
          if(is.null(xObj) || (is.list(xObj) & length(xObj) == 0)){
            if(x %in% names(self$defaults)){
              xObj <- self$defaults[[x]]
            }
          }
          if(!is.null(xObj)){
            nsdef <- NULL
            if(is(xObj, "ISOMetadataElement")){
              nsdef <- xObj$getNamespaceDefinition(recursive = recursive)
            }else if(is(xObj, "list")){
              nsdef <- list()
              invisible(lapply(xObj, function(xObj.item){
                nsdef.item <- NULL
                if(is(xObj.item, "ISOMetadataElement")){
                  nsdef.item <- xObj.item$getNamespaceDefinition(recursive = recursive)
                }else{
                  nsdef.item <- ISOMetadataNamespace$GCO$getDefinition() 
                }
                for(item in names(nsdef.item)){
                  nsd <- nsdef.item[[item]]
                  if(!(nsd %in% nsdef)){
                    nsdef.new <- c(nsdef, nsd)
                    names(nsdef.new) <- c(names(nsdef), item)
                    nsdef <<- nsdef.new
                  }
                }
              }))
            }else{
              nsdef <- ISOMetadataNamespace$GCO$getDefinition()
            }
            for(item in names(nsdef)){
              nsdef.item <- nsdef[[item]]
              if(!(nsdef.item %in% nsdefs)){
                nsdefs.new <- c(nsdefs, nsdef.item)
                names(nsdefs.new) <- c(names(nsdefs), item)
                nsdefs <<- nsdefs.new
              }
            }
          }
        }))
        if(!(selfNsdef[[1]] %in% nsdefs)) nsdefs <- c(selfNsdef, nsdefs)
        nsdefs <- nsdefs[!sapply(nsdefs, is.null)]
      }else{
        nsdefs <- self$namespace$getDefinition()
      }
      return(nsdefs)
    },
    
    #getClassName
    getClassName = function(){
      return(class(self)[1])
    },
    
    #wrapBaseElement
    wrapBaseElement = function(field, fieldObj){
      dataType <- class(fieldObj)
      
      #specific coercing
      if(all(dataType == c("POSIXct","POSIXt"))) dataType <- "datetime"
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
    text1 <- as(XML::xmlDoc(metadataElement1$encode(addNS = TRUE, validate = FALSE)), "character")
  }else{
    text1 <- as.character(metadataElement1)
  }
  text2 <- NULL
  if(is(metadataElement2, "ISOMetadataElement")){
    text2 <- as(XML::xmlDoc(metadataElement2$encode(addNS = TRUE, validate = FALSE)), "character")
  }else{
    text2 <- as.character(metadataElement2)
  }
  return(text1 == text2)
}

#ISO 19139 schemas fetcher / getter
#===============================================================================
#fetchISOSchemas
fetchISOSchemas <- function(){
  packageStartupMessage("Loading ISO 19139 XML schemas... \n")
  xsdfile <- system.file("extdata/schemas/gmd", "gmd.xsd", package = "geometa", mustWork = TRUE)
  .geometa.iso.schemas <- tryCatch(
    XML::xmlTreeParse(
      xsdfile, isSchema = TRUE, xinclude = TRUE,
      error = function (msg, code, domain, line, col, level, filename, class = "XMLError"){}
    )
  )
}

#getISOSchemas
getISOSchemas <- function(){
  return(.geometa.iso.schemas)
}

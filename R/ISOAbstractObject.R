#' ISOAbstractObject
#'
#' @docType class
#' @importFrom utils packageDescription
#' @importFrom R6 R6Class
#' @importFrom methods is
#' @importFrom methods as
#' @importFrom utils read.csv
#' @import XML
#' @import httr
#' @import jsonlite
#' @import keyring
#' @export
#' @keywords ISO metadata element
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata Element
#' @format \code{\link{R6Class}} object.
#'
#' @section Static Methods:
#' \describe{
#'  \item{\code{getISOStandardByPrefix(prefix)}}{
#'    Inherit the ISO (and/or OGC) standard reference for a given standard prefix (e.g. GMD).
#'    The object returned is a \code{data.frame} containing the specification reference
#'    and title.
#'  }
#'  \item{\code{getISOStandard(clazz)}}{
#'    Inherit the ISO (and/or OGC) standard reference for a given \pkg{geometa} class.
#'    The object returned is a \code{data.frame} containing the specification reference
#'    and title.
#'  }
#'  \item{\code{getISOClasses(extended, pretty)}}{
#'    Get the list of classes supported by \pkg{geometa}. By default, \code{extended} is
#'    set to \code{FALSE} (restrained to \pkg{geometa} environment). If \code{TRUE}, this
#'    allows to list eventual classes loaded in your global environment and that extend
#'    \pkg{geometa} classes. The argument \code{pretty} gives a the list of classes and 
#'    associated ISO/OGC standard information as \code{data.frame}.
#'  }
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
#'  \item{\code{new(xml, element, namespace, defaults, attrs)}}{
#'    This method is used to instantiate an ISOAbstractObject
#'  }
#'  \item{\code{print()}}{
#'    Provides a custom print output (as tree) of the current class
#'  }
#'  \item{\code{decode(xml)}}{
#'    Decodes a ISOMetadata* R6 object from XML representation
#'  }
#'  \item{\code{encode(addNS, validate, strict, inspire, inspireValidator, resetSerialID, setSerialID, encoding)}}{
#'    Encodes a ISOMetadata* R6 object to XML representation. By default, namespace
#'    definition will be added to XML root (\code{addNS = TRUE}), and validation
#'    of object will be performed (\code{validate = TRUE}) prior to its XML encoding.
#'    The argument \code{strict} allows to stop the encoding in case object is not
#'    valid, with a default value set to \code{FALSE}. The argument \code{setSerialID}
#'    is used by \pkg{geometa} to generate automatically serial IDs associated to
#'    XML elements, in particular for GML, default value is \code{TRUE} (recommended value).
#'    The argument \code{resetSerialID} is used by \pkg{geometa} for reseting mandatory IDs
#'    associated to XML elements, such as GML objects, default value is \code{TRUE} 
#'    (recommended value).
#'    Setting \code{inspire} to TRUE (default FALSE), the metadata will be checked with
#'    the INSPIRE metadata validator (online web-service provided by INSPIRE). To check 
#'    metadata with the INSPIRE metadata validator, setting an INSPIRE metadata validator 
#'    is now required, and should be specified with the \code{inspireValidator}. See 
#'    \code{\link{INSPIREMetadataValidator}} for more details
#'  }
#'  \item{\code{validate(xml, strict, inspire, inspireValidator)}}{
#'    Validates the encoded XML against ISO 19139 XML schemas. If \code{strict} is
#'    \code{TRUE}, a error will be raised. Default is \code{FALSE}.
#'    Setting \code{inspire} to\code{TRUE} (default \code{FALSE}), the metadata will be 
#'    checked with the INSPIRE metadata validator (online web-service provided by INSPIRE).
#'    To check metadata with the INSPIRE metadata validator, setting an INSPIRE metadata validator 
#'    is now required, and should be specified with the \code{inspireValidator}. See 
#'    \code{\link{INSPIREMetadataValidator}} for more details
#'  }
#'  \item{\code{save(file, ...)}}{
#'    Saves the current metadata object XML representation to a file. This utility
#'    ensures proper indentation of XML file produced. Additional parameters from 
#'    \code{$encode()} method can be specified, such as \code{inspire} to check
#'    the INSPIRE metadata validity.
#'  }
#'  \item{\code{getNamespaceDefinition(recursive)}}{
#'    Gets the namespace definition of the current ISO* class. By default, only
#'    the namespace definition of the current element is retrieved (\code{recursive = FALSE}).
#'  }
#'  \item{\code{getClassName()}}{
#'    Gets the class name
#'  }
#'  \item{\code{getClass()}}{
#'    Gets the class
#'  }
#'  \item{\code{wrapBaseElement(field, fieldObj)}}{
#'    Wraps a base element type
#'  }
#'  \item{\code{setIsNull(isNull, reason)}}{
#'    Sets the object as null object for the XML. In case \code{isNull} is \code{TRUE},
#'    a reason should be specified among values 'inapplicable', 'missing', 'template',
#'    'unknown', 'withheld'. By default, the reason is set 'missing'.
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
#'  \item{\code{setAttr(attrKey, attrValue)}}{
#'    Set an attribute
#'  }
#'  \item{\code{addFieldAttrs(field, ...)}}{
#'    Allows to add one more xlink attributes a field (element property)
#'  }
#'  \item{\code{setId(id, addNS)}}{
#'    Set an id. By default \code{addNS} is \code{FALSE} (no namespace prefix added).
#'  }
#'  \item{\code{setHref(href)}}{
#'    Sets an href reference
#'  }
#'  \item{\code{setCodeList(codeList)}}{
#'    Sets a codeList
#'  }
#'  \item{\code{setCodeListValue(codeListValue)}}{
#'    Sets a codeList value
#'  }
#'  \item{\code{setCodeSpace(codeSpace)}}{
#'    Set a codeSpace
#'  }
#'  \item{\code{setValue(value)}}{
#'    Set a value
#'  }
#'  \item{\code{isDocument()}}{
#'    Indicates if the object is a metadata document, typically an object of class
#'    \code{ISOMetadata} or \code{ISOFeatureCatalogue}
#'  }
#'  \item{\code{isFieldInheritedFrom(field)}}{
#'    Gives the parent from which the field is inherited, otherwise return \code{NULL}.
#'  }
#'  \item{\code{createLocalisedProperty(text, locales)}}{
#'    Creates a localised property made of a default text and a list of localised texts.
#'  }
#' }
#' 
#' @note Abstract ISO Metadata class used internally by geometa
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractObject <- R6Class("ISOAbstractObject",
  inherit = geometaLogger,
  cloneable = FALSE,
  private = list(
    xmlElement = "AbstractObject",
    xmlNamespacePrefix = "GCO",
    encoding = options("encoding"),
    document = FALSE,
    system_fields = c("wrap", "value_as_field", "valueDescription",
                      "element", "namespace", "defaults", "attrs", "printAttrs", "parentAttrs",
                      "codelistId", "measureType", "isNull", "anyElement"),
    xmlComments = function(isoCompliant = NA, inspireReport = NULL){
      comments <- list()
      geometa <- packageDescription("geometa")
      title <- paste0("ISO 19139 XML generated by geometa R package - Version ", geometa$Version)
      
      isISOCompliant <- ifelse(is.na(isoCompliant),"NOT TESTED", ifelse(isoCompliant, "YES", "NO"))
      ISOCompliance <- paste0("ISO 19139 XML compliance: ", isISOCompliant)
      
      isINSPIRECompliant <- ifelse(is.null(inspireReport), "NOT TESTED", ifelse(inspireReport$Status=="PASSED", "YES", "NO"))
      INSPIRECompliance <- paste0("INSPIRE compliance: ", isINSPIRECompliant)
      
      createdOn <- paste0("Metadata Creation date/time: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
      geometaAuthor <- gsub(">","",gsub("<","",unlist(strsplit(as.character(eval(parse(text=geometa$Authors)))," \\["))[1]))
      author <- paste0("\tContact: ", geometaAuthor)
      infoPage <- paste0("\tURL: ", geometa$URL)
      bugReport <- paste0("\tBugReports: ", geometa$BugReports)
      idx <- 1
      comments[[idx]] <- createdOn; idx <- idx+1
      comments[[idx]] <- title; idx <- idx+1
      comments[[idx]] <- ISOCompliance; idx <- idx+1
      if(!is.null(inspireReport)){
        comments[[idx]] <- INSPIRECompliance; idx <- idx+1
        for(inspireAttr in names(inspireReport)){
          if(!(inspireAttr %in% c("raw", "status"))){
            comments[[idx]] <- sprintf("INSPIRE %s : %s", inspireAttr, inspireReport[[inspireAttr]])
            idx <- idx+1
          }
        }
      }
      comments[[idx]] <- paste("geometa R package information:", author, infoPage, bugReport, sep="\n")
      return(comments)
    },
    toComplexTypes = function(value){
      newvalue <- value
      #datetime types
      if(regexpr(pattern = "^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})", value)>0){
        if(endsWith(value, "Z")){
          newvalue <- as.POSIXct(strptime(value, "%Y-%m-%dT%H:%M:%S"), tz = "UTC")
        }else{
          if(regexpr(pattern = "[[:space:]]", value)){
            splits <- unlist(strsplit(value, " "))
            value <- splits[1]
            #TODO find a way to fetch "tzone" attribute -not solved for now
            newvalue <- as.POSIXct(strptime(value, "%Y-%m-%dT%H:%M:%S"), tz = "")
          }else{
            newvalue <- as.POSIXct(strptime(value, "%Y-%m-%dT%H:%M:%S"), tz = "")
          }
        }
        
      }else if(regexpr(pattern = "^(\\d{4})-(\\d{2})-(\\d{2})$", value)>0){
        newvalue <- as.Date(as.POSIXct(strptime(value, "%Y-%m-%d"), tz = "UTC"))
      }
      
      return(newvalue)
    },
    fromComplexTypes = function(value){
      #datetime types
      if(suppressWarnings(all(class(value)==c("POSIXct","POSIXt")))){
        tz <- attr(value, "tzone")
        if(length(tz)>1){
          if(tz %in% c("UTC","GMT")){
            value <- format(value,"%Y-%m-%dT%H:%M:%S")
            value <- paste0(value,"Z")
          }else{
            utc_offset <- format(value, "%z")
            utc_offset <- paste0(substr(utc_offset,1,3),":",substr(utc_offset,4,5))
            value <- paste0(format(value,"%Y-%m-%dT%H:%M:%S"), utc_offset)
          }
        }else{
          value <- format(value,"%Y-%m-%dT%H:%M:%S")
        }
      }else if(class(value)[1] == "Date"){
        value <- format(value,"%Y-%m-%d")
      }
      
      return(value)
    },
    xmlNodeToCharacter = function (x, ..., indent = "", tagSeparator = "\n") 
    {
      out <- ""
      if (length(xmlAttrs(x))) {
        tmp <- paste(names(xmlAttrs(x)), paste("\"", XML:::insertEntities(xmlAttrs(x)), 
                                               "\"", sep = ""), sep = "=", collapse = " ")
      } else{
        tmp <- ""
      }
      if (length(x$namespaceDefinitions) > 0) {
        k = as(x$namespaceDefinitions, "character")
        ns = paste("xmlns", ifelse(nchar(names(k)), ":", ""), 
                   names(k), "=", ddQuote(k), sep = "", collapse = " ")
      } else{
        ns <- ""
      }
      subIndent <- paste(indent, " ", sep = "")
      if (is.logical(indent) && !indent) {
        indent <- ""
        subIndent <- FALSE
      }
      if (length(xmlChildren(x)) == 0) {
        out <- paste(out,indent, paste("<", xmlName(x, TRUE), ifelse(tmp != 
                                                                       "", " ", ""), tmp, ifelse(ns != "", " ", ""), ns, 
                                       "/>", tagSeparator, sep = ""), sep = "")
      } else if (length(xmlChildren(x)) == 1 && inherits(xmlChildren(x)[[1]], "XMLTextNode")) {
        out <- paste(out,indent, paste("<", xmlName(x, TRUE), ifelse(tmp != 
                                                                       "", " ", ""), tmp, ifelse(ns != "", " ", ""), ns, 
                                       ">", sep = ""), sep = "")
        kid = xmlChildren(x)[[1]]
        if (inherits(kid, "EntitiesEscaped")) 
          txt = xmlValue(kid)
        else txt = XML:::insertEntities(xmlValue(kid))
        out <- paste(out,txt, sep = "")
        out <- paste(out,paste("</", xmlName(x, TRUE), ">", tagSeparator, 
                               sep = ""), sep = "")
      } else {
        out <- paste(out,indent, paste("<", xmlName(x, TRUE), ifelse(tmp != 
                                                                       "", " ", ""), tmp, ifelse(ns != "", " ", ""), ns, 
                                       ">", tagSeparator, sep = ""), sep = "")
        for (i in xmlChildren(x)){
          out_child <- NULL
          if(is(i,"XMLNode")){
            if(is(i,"XMLCommentNode")){
              out_child <- paste0(capture.output(i),collapse="")
            }else{
              out_child <- private$xmlNodeToCharacter(i)
            }
          }else{
            out_child <- paste(as(i,"character"),tagSeparator,sep="")
          }
          if(!is.null(out_child)) out <- paste(out, out_child, sep="") 
        }
        out<-paste(out,indent, paste("</", xmlName(x, TRUE), ">", tagSeparator, 
                                     sep = ""), sep = "")
      }
      return(out)
    }
  ),
  public = list(

    #fields
    #---------------------------------------------------------------------------
    wrap = TRUE,
    element = NA,
    namespace = NA,
    defaults = list(),
    attrs = list(),
    printAttrs = list(),
    parentAttrs = NULL,
    value = NULL,
    value_as_field = FALSE,
    isNull = FALSE,
    anyElement = FALSE,
    initialize = function(xml = NULL, element = NULL, namespace = NULL,
                          attrs = list(), defaults = list(),
                          wrap = TRUE, value_as_field = FALSE){
      if(!is.null(element)){ private$xmlElement <- element }
      if(!is.null(namespace)){ private$xmlNamespacePrefix <- toupper(namespace)}
      self$element = private$xmlElement
      self$namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      self$attrs = attrs
      self$defaults = defaults
      self$wrap = wrap
      self$value_as_field = value_as_field
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
        (class(self[[x]])[1] %in% c("environment", "function")) ||
        (x %in% private$system_fields)
      })]
      
      if(!inherits(self, "GMLElement") && !inherits(self, "SWEElement")) cat(crayon::white(paste0("<", crayon::underline(self$getClassName()), ">")))
      if(is(self, "ISOCodeListValue")){
        clVal <- self$printAttrs$codeListValue
        clDes <- self$codelistId$entries[self$codelistId$entries$value == clVal,"description"]
        if(length(clDes)==0){
          clDes <- self$valueDescription
        }
        cat(paste0(": ", clVal, crayon::cyan(paste0(" {",clDes,"}"))))
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
          if(is(fieldObj, "ISOAbstractObject")){
            attrs_str <- ""
            if(length(fieldObj$attrs)>0){
              attrs <- paste(sapply(names(fieldObj$attrs), function(attrName){paste0(attrName,"=",fieldObj$attrs[[attrName]])}), collapse=",")
              attrs_str <- paste0("[",attrs,"]")
            }
            cat(paste0("\n", paste(rep(shift, depth), collapse=""),"|-- ", crayon::italic(field), " ", attrs_str, " "))
            fieldObj$print(depth = depth+1)
          }else if(is(fieldObj, "ISOAttributes")){
            attrs <- paste(sapply(names(fieldObj$attrs), function(attrName){paste0(attrName,"=",fieldObj$attrs[[attrName]])}), collapse=",")
            cat(paste0("\n",paste(rep(shift, depth), collapse=""),"|-- ", crayon::italic(field), "[",attrs,"]"))
          }else if(is(fieldObj, "list")){
            for(item in fieldObj){
              if(is(item, "ISOAbstractObject")){
                attrs_str <- ""
                if(length(item$attrs)>0){
                  attrs <- paste(sapply(names(item$attrs), function(attrName){paste0(attrName,"=",item$attrs[[attrName]])}), collapse=",")
                  attrs_str <- paste0("[",attrs,"]")
                }
                cat(paste0("\n", paste(rep(shift, depth), collapse=""),"|-- ", crayon::italic(field), " ", attrs_str))
                item$print(depth = depth+1)
                if(is(item, "ISOCodeListValue")){
                  clVal <- item$printAttrs$codeListValue
                  clDes <- item$codelistId$entries[item$codelistId$entries$value == clVal,"description"]
                  cat(paste0(": ", clVal, crayon::cyan(paste0(" {",clDes,"}"))))
                }
              }else if(is(item, "ISOAttributes")){
                attrs <- paste(sapply(names(item$attrs), function(attrName){paste0(attrName,"=",item$attrs[[attrName]])}), collapse=",")
                cat(paste0("\n",paste(rep(shift, depth), collapse=""),"|-- ", crayon::italic(field), "[",attrs,"]"))
              }else if(is(item, "matrix")){
                m <- paste(apply(item, 1L, function(x){
                  x <- lapply(x, function(el){
                    if(is.na(suppressWarnings(as.numeric(el))) & !all(sapply(item,class)=="character")){
                      el <- paste0("\"",el,"\"")
                    }else{
                      if(!is.na(suppressWarnings(as.numeric(el)))){
                        el <- as.numeric(el)
                      }
                    }
                    return(el)
                  })
                  return(paste(x, collapse = " "))
                }), collapse = " ")
                cat(paste0("\n",paste(rep(shift, depth), collapse=""),"|-- ", crayon::italic(field), ": ", crayon::bgWhite(m)))
              }else{
                cat(paste0("\n", paste(rep(shift, depth), collapse=""),"|-- ", crayon::italic(field), ": ", crayon::bgWhite(item)))
              }
            }
          }else if (is(fieldObj,"matrix")){
            m <- paste(apply(fieldObj, 1L, function(x){
              x <- lapply(x, function(el){
                if(is.na(suppressWarnings(as.numeric(el)))& !all(sapply(fieldObj,class)=="character")){
                  el <- paste0("\"",el,"\"")
                }else{
                  if(!is.na(suppressWarnings(as.numeric(el)))){
                    el <- as.numeric(el)
                  }
                }
                return(el)
              })
              return(paste(x, collapse = " "))
            }), collapse = " ")
            cat(paste0("\n",paste(rep(shift, depth), collapse=""),"|-- ", crayon::italic(field), ": ", crayon::bgWhite(m)))
          }else{
            cat(paste0("\n",paste(rep(shift, depth), collapse=""),"|-- ", crayon::italic(field), ": ", crayon::bgWhite(fieldObj)))
          }
        }
      }
      invisible(self)
    },
    
    #decode
    decode = function(xml){
      
      #remove comments if any (in case of document)
      if(is(xml, "XMLInternalDocument")){
        children <- xmlChildren(xml, encoding = private$encoding, addFinalizer = FALSE)
        xml <- children[names(children) != "comment"][[1]]
      }
      xml_children <- xmlChildren(xml, encoding = private$encoding, addFinalizer = FALSE)
      for(child in xml_children){
        fieldName <- xmlName(child)
        childElement <- child
        nsPrefix <- ""
        fNames <- unlist(strsplit(fieldName, ":"))
        if(length(fNames)>1){
         fieldName <- fNames[2]
        }
        
        if(!is(self, "ISOElementSequence")) {
          if(!(fieldName %in% names(self)) & fieldName != "text" & !self$anyElement) next
        }
        
        wrap_fields <- FALSE  
        fieldClass <- NULL
        parentAttrs <- NULL
        if(!is(child, "XMLInternalTextNode")){
          fieldClass <- ISOAbstractObject$getISOClassByNode(child)
          nsPrefix <- names(xmlNamespace(child))
          if(is.null(nsPrefix)){
            #try to grab from ns prefix
            childName <- xmlName(child, full = TRUE)
            preftag <- unlist(strsplit(as(childName, "character"),":"))[1]
            if(preftag!=childName) nsPrefix <- substring(preftag, 2, nchar(preftag))
          }
          if(is.null(fieldClass)){
            parentAttrs <- as.list(xmlAttrs(child, TRUE, FALSE))
            if(length(parentAttrs)>0) parentAttrs <- parentAttrs[names(parentAttrs) != "xsi:type"]
            if(length(parentAttrs)==0) parentAttrs <- NULL
            children <- xmlChildren(child, encoding = private$encoding, addFinalizer = FALSE)
            if(length(children)>0){
              if(length(children)==1){
                childroot <- children[[1]]
                if(!is(childroot, "XMLInternalTextNode")){
                  child <- childroot
                  fieldClass <- ISOAbstractObject$getISOClassByNode(childroot)
                }
              }else{
                #more than one child, consider it as sequence
                fieldClass <- ISOElementSequence
              }
            }else{
              #if xlink:href attr available attempt to
              #href <- xmlGetAttr(child, "xlink:href")
              #if(!is.null(href)){
              #  self$INFO(sprintf("Fetching child element from xlink:href attribute '%s'", href))
              #  childXML <- try(XML::xmlParse(href))
              #  if(!is(childXML,"try-error")){
              #    child <- XML::xmlRoot(childXML)
              #    fieldClass <- ISOAbstractObject$getISOClassByNode(child)
              #  }
              #}
            }
            if(!is.null(fieldClass)) wrap_fields <- TRUE 
          }
        }
        
        #coercing
        fieldValue <- xmlValue(child, recursive = FALSE)
        if(length(fieldValue)>0){
          fieldValue <- private$toComplexTypes(fieldValue)
        }
        
        if(!is.null(fieldClass)){
          if(regexpr("^ISOBase.+", fieldClass$classname)>0){
            
            fieldValue <- switch(fieldClass$classname,
                                 "ISOBaseBoolean" = as.logical(fieldValue),
                                 "ISOBaseInteger" = as.integer(fieldValue),
                                 "ISOBaseReal" = as.numeric(fieldValue),
                                 "ISOBaseDecimal" = {
                                   fieldValue <- as.numeric(fieldValue)
                                   class(fieldValue) <- "decimal"
                                   fieldValue
                                 },
                                 fieldValue
            )
          }else{
            fieldValue <- fieldClass$new(xml = child)
            fieldValue$parentAttrs <- parentAttrs
            fieldValue$attrs <- as.list(xmlAttrs(child, TRUE, FALSE))
          }
          if(is(self[[fieldName]], "list")){
            self[[fieldName]] <- c(self[[fieldName]], fieldValue)
          }else{
			      if(is(self, "ISOElementSequence")){
              if(!wrap_fields){
                #means no wrapping of ElementSequence fields
                self[["_internal_"]] <- c(self[["_internal_"]], fieldValue)
              }else{
                self[[fieldName]] <- fieldValue
              }
            }else{
              self[[fieldName]] <- fieldValue
            }
          }
        }else{
          if(is.null(nsPrefix)) nsPrefix <- ""
          if(startsWith(nsPrefix,"gml") |inherits(self, "GMLAbstractObject")){
            if(is(self[[fieldName]], "matrix") & 
              (inherits(self,"GMLAbstractRing")|
               inherits(self,"GMLAbstractGeometricPrimitive")|
               inherits(self,"GMLEnvelope")|
               inherits(self,"GMLGeneralGridAxis"))){
              value <- xmlValue(child)
              if(value=="") value <- NA
              if(!is.na(value)){
                value_split <- unlist(strsplit(value," "))
                coercable <- !suppressWarnings(is.na(as.numeric(value_split)))
                values <- lapply(1:length(value_split), function(i){
                  out <- value_split[i]
                  if(coercable[i]) out <- as.numeric(out)
                  return(out)
                })
                if(all(!is.na(values)) & length(values)>1){
                  values <- lapply(values, function(x){if(is.character(x)){x <- gsub("\"","",x)};x})
                  if(is(self,"GMLEnvelope")){
                    m.values <- t(matrix(values))
                  }else{
                    dimension <- xmlGetAttr(xml, "srsDimension")
                    if(!is.null(dimension)) dimension <- as.integer(dimension)
                    if(is.null(dimension)){
                      dimension <- 1
                      if(inherits(self,"GMLAbstractGeometricPrimitive")){
                        if(is(self, "GMLPoint")){
                          dimension <- length(values)
                        }else{
                          self$WARN("No 'srsDimension' on geometry object. Impossible to decode coordinates!")
                        }
                      }
                    }
                    m.values <- matrix(values, length(values)/dimension, dimension, byrow = TRUE)
                  }
                  if(is(self[[fieldName]], "list")){
                    self[[fieldName]] <- c(self[[fieldName]], m.values)
                  }else{
                    self[[fieldName]] <- m.values
                  }
                }
              }else{
                #xmlNamespacePrefix <- "GML"
                xmlNamespacePrefix <- self$getClass()$private_fields$xmlNamespacePrefix
                if(startsWith(nsPrefix,"gml")) xmlNamespacePrefix <- toupper(nsPrefix)
                if(is.null(xmlNamespacePrefix)) xmlNamespacePrefix <- "GML"
                gmlElem <- GMLElement$new(element = fieldName, xmlNamespacePrefix = xmlNamespacePrefix)
                gmlElem$decode(xml = childElement)
                if(is(self[[fieldName]], "list")){
                  self[[fieldName]] <- c(self[[fieldName]], gmlElem)
                }else{
                  self[[fieldName]] <- gmlElem
                }
              }
            }else{
              #xmlNamespacePrefix <- "GML"
              #if(startsWith(nsPrefix,"gml")) xmlNamespacePrefix <- toupper(nsPrefix)
              xmlNamespacePrefix <- self$getClass()$private_fields$xmlNamespacePrefix
              if(startsWith(nsPrefix,"gml")) xmlNamespacePrefix <- toupper(nsPrefix)
              if(is.null(xmlNamespacePrefix)) xmlNamespacePrefix <- "GML"
              gmlElem <- GMLElement$new(element = fieldName, xmlNamespacePrefix = xmlNamespacePrefix)
              gmlElem$decode(xml = childElement)
              if(is(self[[fieldName]], "list")){
                self[[fieldName]] <- c(self[[fieldName]], gmlElem)
              }else{
                self[[fieldName]] <- gmlElem
              }
            }
           
          }else if(inherits(self, "SWEAbstractObject")){ 
            #TODO see how to improve encoding/decoding for GML/SWE objects
            xmlNamespacePrefix <- self$getClass()$private_fields$xmlNamespacePrefix
            if(startsWith(nsPrefix,"swe")) xmlNamespacePrefix <- toupper(nsPrefix)
            if(is.null(xmlNamespacePrefix)) xmlNamespacePrefix <- "SWE"
            sweElem <- SWEElement$new(element = fieldName, xmlNamespacePrefix = xmlNamespacePrefix)
            sweElem$decode(xml = childElement)
            if(is(self[[fieldName]], "list")){
              self[[fieldName]] <- c(self[[fieldName]], sweElem)
            }else{
              self[[fieldName]] <- sweElem
            }
          }else{
            value <- xmlValue(child)
            isList <- is.list(self$getClass()$public_fields[[fieldName]])
            if(value==""){
              value <- ifelse(isList, list(), NA)
              attrs <- xmlAttrs(child)
              if(!is.null(attrs)){
                attrNs <- attr(attrs,"namespaces")
                if(!is.null(attrNs)){
                  attr(attrs,"namespaces") <- NULL
                  names(attrs) <- paste(attrNs, names(attrs), sep=":")
                  #control mal-formed attributes (starting with :)
                  names(attrs) <- lapply(names(attrs), function(x){
                    out <- x 
                    if(startsWith(x,":")) out <- substr(x, 2, nchar(x))
                    return(out)
                  })
                }
                value <- ISOAttributes$new(attrs)
              }
            }
            if(fieldName == "text") fieldName <- "value"
            self[[fieldName]] <- if(is.list(self[[fieldName]])) c(self[[fieldName]], value) else value
          }
        }
        
      }
      
      #inherit attributes if any
      xmlattrs <- NULL
      if(!self$isDocument()) xmlattrs <- xmlAttrs(xml, TRUE, FALSE)
      if(is(self, "ISOFeatureCatalogue")){
        xmlattrs <- xmlAttrs(xml, TRUE, FALSE)
        if("uuid" %in% names(xmlattrs)){
          xmlattrs <- xmlattrs[names(xmlattrs)=="uuid"]
        }
      }
      
      self$attrs <- as.list(xmlattrs)
      if("gco:nilReason" %in% names(xmlattrs)) self$isNull <- TRUE
    },
    
    #encode
    encode = function(addNS = TRUE, validate = TRUE, strict = FALSE, inspire = FALSE, inspireValidator = NULL,
                      resetSerialID = TRUE, setSerialID = TRUE,
                      encoding = "UTF-8"){
      
      #management of GML ids
      if(resetSerialID) .geometa.gml$serialId <- 1L
      if(setSerialID){
        if(inherits(self, "GMLAbstractGML")){
          if(is.null(self$attrs[["gml:id"]])){
            serialId <- paste0("ID",.geometa.gml$serialId)
            self$setId(serialId,TRUE)
            .geometa.gml$serialId <- .geometa.gml$serialId+1
          }
        }
      }
      
      #list of fields to encode as XML
      fields <- rev(names(self))
      
      #root XML
      rootXML <- NULL
      rootXMLAttrs <- list()
      if("attrs" %in% fields){
        rootXMLAttrs <- self[["attrs"]]
        rootXMLAttrs <- rootXMLAttrs[!is.na(rootXMLAttrs)]
      }
      freeTextAttr <- list("xsi:type" = "gmd:PT_FreeText_PropertyType")
      
      #fields
      fields <- fields[!sapply(fields, function(x){
        (class(self[[x]])[1] %in% c("environment", "function")) ||
        (x %in% private$system_fields)
      })]
      
      if(self$isDocument()){
        rootNamespaces <- sapply(getISOMetadataNamespaces(), function(x){x$getDefinition()})
        rootXML <- xmlOutputDOM(
          tag = self$element,
          nameSpace = self$namespace$id,
          nsURI = rootNamespaces
        )
      }else{
        wrapperAttrs <- self$attrs
        if(self$isNull){
          wrapperAttrs <- self$attrs
          if(length(wrapperAttrs)>1) wrapperAttrs <- wrapperAttrs[names(wrapperAttrs)!="gco:nilReason"]
        }
        if(addNS){
          nsdefs <- self$getNamespaceDefinition(recursive = TRUE)
          if(!("xsi" %in% names(nsdefs))) nsdefs <- c(nsdefs, ISOMetadataNamespace$XSI$getDefinition())
          if(!("xlink" %in% names(nsdefs))) nsdefs <- c(nsdefs, ISOMetadataNamespace$XLINK$getDefinition())
          nsdefs <- nsdefs[order(names(nsdefs))]
          rootXML <- xmlOutputDOM(
            tag = self$element,
            nameSpace = self$namespace$id,
            nsURI = nsdefs,
            attrs = wrapperAttrs
          )
        }else{
          rootXML <- xmlOutputDOM(
            tag = self$element,
            nameSpace = self$namespace$id,
            attrs = wrapperAttrs
          )
        }
      }
      
      if(!self$isNull) for(field in fields){
        fieldObj <- self[[field]]
        
        #default values management
        if(is.null(fieldObj) || (is.list(fieldObj) & length(fieldObj)==0)){
          if(field %in% names(self$defaults)){
            fieldObj <- self$defaults[[field]]
          }
        }
        
        #user values management
        ns <- self$namespace$getDefinition()
        if(field != "value"){
          klass <- self$isFieldInheritedFrom(field)
          if(!is.null(klass)){
            ns <- ISOMetadataNamespace[[klass$private_fields$xmlNamespacePrefix]]$getDefinition()
          }
        }
        namespaceId <- names(ns)
        if(!is.null(fieldObj)){
          if(is(fieldObj, "ISOAbstractObject")){
            fieldObjXml <- fieldObj$encode(addNS = FALSE, validate = FALSE,
                                           resetSerialID = FALSE, setSerialID = setSerialID)
            if(is(fieldObj, "ISOElementSequence")){
              fieldObjXml.children <- xmlChildren(fieldObjXml, addFinalizer = FALSE)
      			  hasLocales <- FALSE
      				if(!is.null(fieldObj[["_internal_"]])){
        				if(any(sapply(fieldObj[["_internal_"]],function(x){class(x)[1]})=="ISOFreeText")){
        				  hasLocales <- TRUE
        				}
      			  }
              if(self$wrap){
                wrapperAttrs <- self$parentAttrs
                if(hasLocales) wrapperAttrs <- c(wrapperAttrs, freeTextAttr)
                wrapperNode <- xmlOutputDOM(
                  tag = field,
                  nameSpace = namespaceId, 
                  attrs = wrapperAttrs
                )
                for(child in fieldObjXml.children){
                  wrapperNode$addNode(child)
                }
                rootXML$addNode(wrapperNode$value())
              }else{
      				  if(hasLocales && !("xsi:type" %in% names(rootXMLAttrs))) rootXMLAttrs <- c(rootXMLAttrs, freeTextAttr)
                for(child in fieldObjXml.children){
                    rootXML$addNode(child)
                }
              }
        
              #fieldObjNames <- names(fieldObj)
              #fieldObjNames <- fieldObjNames[sapply(fieldObjNames, function(name){
              #  return(which(fieldObjNames == name) < which(fieldObjNames == ".__enclos_env__"))
              #})]
            }else{
              if(fieldObj$wrap){
                wrapperAttrs <- fieldObj$parentAttrs
                if(fieldObj$isNull){
                  wrapperAttrs <- fieldObj$attrs
                  if(length(wrapperAttrs)>1) wrapperAttrs <- wrapperAttrs[names(wrapperAttrs)!="gco:nilReason"]
                }
                wrapperNode <- xmlOutputDOM(
                  tag = field,
                  nameSpace = namespaceId,
                  attrs = wrapperAttrs
                )
                if(!fieldObj$isNull) wrapperNode$addNode(fieldObjXml)
                rootXML$addNode(wrapperNode$value())
                
              }else{
                rootXML$addNode(fieldObjXml)
              }
            }
          }else if(is(fieldObj, "list")){
            for(item in fieldObj){
              nodeValue <- NULL
              if(length(item)==0) item <- NA
              if(is(item, "ISOAttributes")){
                emptyNodeAttrs <- item$attrs
                emptyNode <- xmlOutputDOM(tag = field,nameSpace = namespaceId, attrs = emptyNodeAttrs)
                rootXML$addNode(emptyNode$value())
              }else if(suppressWarnings(all(is.na(item)))){
                emptyNodeAttrs <- c("gco:nilReason" = "missing")
                emptyNode <- xmlOutputDOM(tag = field,nameSpace = namespaceId, attrs = emptyNodeAttrs)
                rootXML$addNode(emptyNode$value())
              }else if(is(item, "matrix")){
                matrix_NA <- all(is.na(item))
                if(matrix_NA){
                  emptyNode <- xmlOutputDOM(tag = field,nameSpace = namespaceId)
                  rootXML$addNode(emptyNode$value())
                }else{
                  mts <- paste(apply(item, 1L, function(x){
                    x <- lapply(x, function(el){
                      if(!is.na(suppressWarnings(as.numeric(el)))){
                        el <- as.numeric(el)
                      }
                      return(el)
                    })
                    x <- lapply(x, function(el){
                      if(is.na(suppressWarnings(as.numeric(el))) & !all(sapply(x,class)=="character")){
                        el <- paste0("\"",el,"\"")
                      }
                      return(el)
                    })
                    return(paste(x, collapse = " "))
                  }), collapse = " ")
                  txtNode <- xmlTextNode(mts)
                  if(field == "value"){
                    rootXML$addNode(txtNode)
                  }else{
                    wrapperNode <- xmlOutputDOM(tag = field, nameSpace = namespaceId)
                    wrapperNode$addNode(txtNode)
                    rootXML$addNode(wrapperNode$value())
                  }
                }
              }else{
                if(is(item, "ISOAbstractObject")){
                  nodeValue <- item
                }else{
                  nodeValue <- self$wrapBaseElement(field, item)
                }
                nodeValueXml <- nodeValue$encode(addNS = FALSE, validate = FALSE,
                                                 resetSerialID = FALSE, setSerialID = setSerialID)
                if(is(item, "ISOElementSequence")){
                  nodeValueXml.children <- xmlChildren(nodeValueXml, addFinalizer = FALSE)

                  hasLocales <- FALSE
                  if(!is.null(item[["_internal_"]])){
					          if(any(sapply(item[["_internal_"]],function(x){class(x)[1]})=="ISOFreeText")){
						          hasLocales <- TRUE
					          }
				          }
				  
                  if(nodeValue$wrap){
                    wrapperAttrs <- nodeValue$parentAttrs
                    if(nodeValue$isNull){
                      wrapperAttrs <- nodeValue$attrs
                      if(length(wrapperAttrs)>1) wrapperAttrs <- wrapperAttrs[names(wrapperAttrs)!="gco:nilReason"]
                    }
					          wrapperAttrs <- c(wrapperAttrs,freeTextAttr)
                    wrapperNode <- xmlOutputDOM(tag = field,nameSpace = namespaceId, attrs = wrapperAttrs)
                    if(!nodeValue$isNull){
                      for(child in nodeValueXml.children){
                        wrapperNode$addNode(child)
                      }
                    }
                    rootXML$addNode(wrapperNode$value())
                  }else{
					          if(hasLocales && !("xsi:type" %in% names(rootXMLAttrs))) rootXMLAttrs <- c(rootXMLAttrs, freeTextAttr)
                    for(child in nodeValueXml.children){
                      rootXML$addNode(child)
                    }
                  }
                }else{
                  if(nodeValue$wrap && field != "_internal_"){
                    wrapperAttrs <- nodeValue$parentAttrs
                    if(nodeValue$isNull){
                      wrapperAttrs <- nodeValue$attrs
                      if(length(wrapperAttrs)>1) wrapperAttrs <- wrapperAttrs[names(wrapperAttrs)!="gco:nilReason"]
                    }
                    wrapperNode <- xmlOutputDOM(
                      tag = field,
                      nameSpace = namespaceId,
                      attrs = wrapperAttrs
                    )
                    if(!nodeValue$isNull) wrapperNode$addNode(nodeValueXml)
                    rootXML$addNode(wrapperNode$value())
                  }else{
                    rootXML$addNode(nodeValueXml)
                  }
                }
              }
            }
          }else if(is(fieldObj, "matrix")){
            matrix_NA <- all(is.na(fieldObj))
            if(matrix_NA){
              emptyNode <- xmlOutputDOM(tag = field,nameSpace = namespaceId)
              rootXML$addNode(emptyNode$value())
            }else{
              mts <- paste(apply(fieldObj, 1L, function(x){
                x <- lapply(x, function(el){
                  if(!is.na(suppressWarnings(as.numeric(el)))){
                    el <- as.numeric(el)
                  }
                  return(el)
                })
                x <- lapply(x, function(el){
                  if(is.na(suppressWarnings(as.numeric(el))) & !all(sapply(x,class)=="character")){
                    el <- paste0("\"",el,"\"")
                  }
                  return(el)
                })
                return(paste(x, collapse = " "))
              }), collapse = " ")
              txtNode <- xmlTextNode(mts)
              if(field == "value"){
                if(field == "value" && self$value_as_field){
                  wrapperNode <- xmlOutputDOM(
                    tag = field,
                    nameSpace = namespaceId
                  )
                  wrapperNode$addNode(txtNode)
                  rootXML$addNode(wrapperNode$value())
                }else{
                  rootXML$addNode(txtNode)
                }
              }else{
                wrapperNode <- xmlOutputDOM(tag = field, nameSpace = namespaceId)
                wrapperNode$addNode(txtNode)
                rootXML$addNode(wrapperNode$value())
              }
            }
          }else{
            if(length(fieldObj)==0) fieldObj <- NA
            if(is(fieldObj, "ISOAttributes")){
              emptyNodeAttrs <- fieldObj$attrs
              emptyNode <- xmlOutputDOM(tag = field,nameSpace = namespaceId, attrs = emptyNodeAttrs)
              rootXML$addNode(emptyNode$value())
            }else if(is.na(fieldObj)){
              emptyNodeAttrs <- c("gco:nilReason" = "missing")
              emptyNode <- xmlOutputDOM(tag = field,nameSpace = namespaceId, attrs = emptyNodeAttrs)
              rootXML$addNode(emptyNode$value())
            }else{
              if((field == "value"|| field == "_internal_")){
                if(is.logical(fieldObj)) fieldObj <- tolower(as.character(is.logical(fieldObj)))
                fieldObj <- private$fromComplexTypes(fieldObj)
                if(field == "value" && self$value_as_field){
                  wrapperNode <- xmlOutputDOM(
                    tag = field,
                    nameSpace = namespaceId
                  )
                  wrapperNode$addNode(xmlTextNode(fieldObj))
                  rootXML$addNode(wrapperNode$value())
                }else{
                  rootXML$addNode(xmlTextNode(fieldObj))
                }
              }else{
                dataObj <- self$wrapBaseElement(field, fieldObj)
                if(!is.null(dataObj)){
                  if(dataObj$wrap){
                    #general case of gco wrapper element
                    wrapperNode <- xmlOutputDOM(
                      tag = field,
                      nameSpace = namespaceId
                    )
                    wrapperNode$addNode(dataObj$encode(addNS = FALSE, validate = FALSE,
                                                       resetSerialID = FALSE, setSerialID = setSerialID))
                    rootXML$addNode(wrapperNode$value())
                  }else{
                    rootXML$addNode(dataObj$encode(addNS = FALSE, validate = FALSE,
                                                   resetSerialID = FALSE, setSerialID = setSerialID))
                  }
                }
              }
            }
          }
        }
      }
      
      #toXML (required for validation)
      out <- rootXML$value()
      out <- private$xmlNodeToCharacter(out)
      if(Encoding(out)!="UTF-8") out <- iconv(out, to = "UTF-8")
      out <- xmlParse(out, encoding = Encoding(out), error = function (msg, ...) {})
      out <- as(out, "XMLInternalNode") #to XMLInternalNode

      if(length(rootXMLAttrs)>0){
        suppressWarnings(xmlAttrs(out) <- rootXMLAttrs)
      }
	  
      #validation vs. ISO 19139 XML schemas + eventually INSPIRE
      compliant <- NA
      if(validate){
        compliant <- self$validate(xml = out, strict = strict, inspire = inspire, inspireValidator = inspireValidator)
      }
      if(self$isDocument()){
        if(!inspire){
          header_comments <- private$xmlComments(compliant)
        }else{
          if(is.list(compliant)){
            header_comments <- private$xmlComments(compliant$ISO, compliant$INSPIRE)
          }else{
            header_comments <- private$xmlComments(compliant)
          }
        }
        #process XML comments
        for(comment in header_comments){
          rootXML$addNode(xmlCommentNode(comment))
        }
        
        #toXML (regeneration with comments)
        out <- rootXML$value()
        out <- private$xmlNodeToCharacter(out)
        if(Encoding(out)!="UTF-8") out <- iconv(out, to = "UTF-8")
        out <- xmlParse(out, encoding = Encoding(out), error = function (msg, ...) {})
        out <- as(out, "XMLInternalNode") #to XMLInternalNode
        if(length(rootXMLAttrs)>0){
          suppressWarnings(xmlAttrs(out) <- rootXMLAttrs)
        }
      }

      return(out)
    },
    
    #validate
    validate = function(xml = NULL, strict = FALSE, inspire = FALSE, inspireValidator = NULL){
      
      #xml
      schemaNamespaceId <- NULL
      if(is.null(xml)){
        schemaNamespaceId <- self$namespace$id
        xml <- self$encode(addNS = TRUE, validate = FALSE, strict = strict)
      }else{
        #remove comments if any
        content <- as(xml, "character")
        content <- gsub("<!--.*?-->", "", content)
        xml <- xmlParse(content, encoding = private$encoding)
        schemaNamespaceId <- names(xmlNamespace(xmlRoot(xml)))
      }
      
      #proceed with schema xml schema validation
      xsd <- getISOMetadataSchemas()
      if(is(xml, "XMLInternalNode")) xml <- xmlDoc(xml)
      report <- xmlSchemaValidate(xsd, xml)
      
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
      
      if(inspire){
        if(!is.null(inspireValidator) && is(inspireValidator, "INSPIREMetadataValidator")){
          if(inspireValidator$running){
            inspireReport <- inspireValidator$getValidationReport(obj = self)
            isValid <- list(
              ISO = isValid,
              INSPIRE = inspireReport
            )
          }else{
            self$WARN(sprintf("INSPIRE Metadata validator service (%s) is not running", inspireValidator$url))
          }
        }else{
          self$WARN("No INSPIRE Metadata validator set, aborting INSPIRE metadata validation!")
        }
      }
      
      return(isValid)
    },
    
    #save
    save = function(file, ...){
      #encode as xml
      xml <- self$encode(...)
      xml_str <- as(xml, "character")
      #write file with writeBin to overcome writeChar size limitation
      writeBin(xml_str, con = file, useBytes = TRUE)
      #read file to replace C-style zero-terminated string
      r = readBin(file, raw(), file.info(file)$size)
      r[r==as.raw(0)] = as.raw(0x20) ## replace with 0x20 = <space>
      writeBin(r, file)
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
          (class(self[[x]])[1] %in% c("environment", "function")) ||
            (x %in% private$system_fields)
        })]
        
        selfNsdef <- self$getNamespaceDefinition()
        nsdefs <- list()
        if(length(fields)>0){
          invisible(lapply(fields, function(x){
            xObj <- self[[x]]
            if(is.null(xObj) || (is.list(xObj) & length(xObj) == 0)){
              if(x %in% names(self$defaults)){
                xObj <- self$defaults[[x]]
              }
            }
            hasContent <- !is.null(xObj)
            if(is(xObj, "ISOAbstractObject")){
              hasContent <- any(hasContent, length(xObj$attrs)>0)
            }
            if(hasContent){
              
              #add parent namespaces if any parent field
              if(x != "value"){
                klass <- self$isFieldInheritedFrom(x)
                if(!is.null(klass)){
                  ns <- ISOMetadataNamespace[[klass$private_fields$xmlNamespacePrefix]]$getDefinition()
                  if(!(ns %in% nsdefs)){
                    nsdefs <<- c(nsdefs, ns)
                  }
                }
              }
              
              #add namespaces
              nsdef <- NULL
              if(is(xObj, "ISOAbstractObject")){
                nsdef <- xObj$getNamespaceDefinition(recursive = recursive)
              }else if(is(xObj, "list")){
                nsdef <- list()
                invisible(lapply(xObj, function(xObj.item){
                  nsdef.item <- NULL
                  if(is(xObj.item, "ISOAbstractObject")){
                    nsdef.item <- xObj.item$getNamespaceDefinition(recursive = recursive)
                  }else{
                    if(!is(xObj.item, "matrix")) nsdef.item <- ISOMetadataNamespace$GCO$getDefinition() 
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
                if(!startsWith(names(selfNsdef)[1],"gml")){
                  nsdef <- ISOMetadataNamespace$GCO$getDefinition()
                }
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
        }
        if(!(selfNsdef[[1]] %in% nsdefs)) nsdefs <- c(selfNsdef, nsdefs)
        nsdefs <- nsdefs[!sapply(nsdefs, is.null)]
      }else{
        nsdefs <- self$namespace$getDefinition()
      }
      
      invisible(lapply(names(self$attrs), function(attr){
        str <- unlist(strsplit(attr,":", fixed=T))
        if(length(str)>1){
          nsprefix <- str[1]
          namespace <- ISOMetadataNamespace[[toupper(nsprefix)]]
          if(!is.null(namespace)){
            ns <- namespace$getDefinition()
            if(!(ns %in% nsdefs)) nsdefs <<- c(nsdefs, ns)
          }
        }
      }))
      nsdefs <- nsdefs[!duplicated(names(nsdefs))]
      return(nsdefs)
    },
    
    #getClassName
    getClassName = function(){
      return(class(self)[1])
    },
    
    #getClass
    getClass = function(){
      class <- eval(parse(text=self$getClassName()))
      return(class)
    },
    
    #wrapBaseElement
    wrapBaseElement = function(field, fieldObj){
      dataType <- class(fieldObj)
      
      #specific coercing
      if(all(dataType == c("POSIXct","POSIXt"))) dataType <- "datetime"
      
      #re-encoding (if needed)
      if(tolower(dataType)=="character"){
        if(Encoding(fieldObj)!="UTF-8") fieldObj <- iconv(fieldObj, to = "UTF-8")
      }
   
      #wrapping
      dataObj <- switch(tolower(dataType),
                        "character" = ISOBaseCharacterString$new(value = fieldObj),
                        "numeric"   = ISOBaseReal$new(value = fieldObj),
                        "decimal"   = ISOBaseDecimal$new(value = fieldObj), #Requires specific class call
                        "integer"   = ISOBaseInteger$new(value = fieldObj),
                        "unlimitedinteger" = ISOUnlimitedInteger$new(value = fieldObj),
                        "logical"   = ISOBaseBoolean$new(value = fieldObj),
                        "datetime"  = ISOBaseDateTime$new(value = fieldObj),
                        "date"      = ISOBaseDate$new(value = fieldObj),
                        NULL
      )
      return(dataObj)
    },
    
    #setIsNull
    setIsNull = function(isNull, reason = "missing"){
      if(isNull){
        allowedReasons <- c("inapplicable", "missing", "template", "unknown", "withheld")
        if(!(reason %in% allowedReasons)){
          stop(sprintf("The reason should be a value among [%s]", paste(allowedReasons, collapse=",")))
        }
      }
      self$isNull <- isNull
      if(self$isNull){
        self$setAttr("gco:nilReason", reason)
      }else{
        self$attrs <- self$attrs[names(self$attrs)!="gco:nilReason"]
      }
    },
    
    #contains
    contains = function(field, metadataElement){
      out = FALSE
      if(length(self[[field]]) == 0){
        out = FALSE
      }else{
        out = any(sapply(self[[field]], function(x){
          ISOAbstractObject$compare(x, metadataElement)
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
        self[[field]] = self[[field]][!sapply(self[[field]], ISOAbstractObject$compare, metadataElement)]
      }
      endNb = length(self[[field]])
      return(endNb == startNb-1)
    },
    
    #setAttr
    setAttr = function(attrKey, attrValue){
      self$attrs[[attrKey]] <- attrValue
    },
    
    #addFieldAttrs
    addFieldAttrs = function(field, ...){
      hasfield <- field %in% names(self$getClass()$public_fields)
      if(hasfield){
        if(is.list(self$getClass()$public_fields[[field]])){
          self[[field]] <- c(self[[field]], ISOAttributes$new(...))
        }else{
          self[[field]] <- ISOAttributes$new(...)
        }
      }else{
        stop(sprintf("Field '%s' is not a property of class '%s'", field, self$getClassName()))
      }
    },
    
    #setId
    setId = function(id, addNS = FALSE){
      attrKey <- "id"
      prefix <- tolower(private$xmlNamespacePrefix)
      if(startsWith(prefix, "gml")) prefix <- "gml"
      if(addNS) attrKey <- paste(prefix, attrKey, sep=":")
      self$attrs[[attrKey]] <- id
    },
    
    #setHref
    setHref = function(href){
      self$attrs[["xlink:href"]] <- href
    },
    
    #setCodeList
    setCodeList = function(codeList){
      self$attrs[["codeList"]] <- codeList
    },
    
    #setCodeListValue
    setCodeListValue = function(codeListValue){
      self$attrs[["codeListValue"]] <- codeListValue
    },
    
    #setCodeSpace
    setCodeSpace = function(codeSpace){
      self$attrs[["codeSpace"]] <- codeSpace
    },
    
    #setValue
    setValue = function(value){
      self$value <- value
    },
    
    #isDocument
    isDocument = function(){
      return(private$document)
    },
    
    #isFieldInheritedFrom
    isFieldInheritedFrom = function(field){
      parentClass <- NULL
      inherited <- !(field %in% names(self$getClass()$public_fields))
      if(inherited){
        classes <- class(self)
        classes <- classes[c(-1,-length(classes))]
        for(i in 1:length(classes)){
          cl <- eval(parse(text=classes[i]))
          if(field %in% names(cl$public_fields)){
            parentClass <- cl
            break
          }
        }
      }
      return(parentClass)
    },
	
    #createLocalisedProperty
    createLocalisedProperty = function(text, locales){
      if(!is(locales, "list")){
        stop("The argument 'locales' should be an object of class 'list'")
      }
      ft <- ISOFreeText$new()
      for(locale in names(locales)){
        localeValue <- locales[[locale]]
        if(!is(localeValue, "character")){
          stop("Each locale value should be of class 'character'")
        }
        localised <- ISOLocalisedCharacterString$new(locale = locale, value = localeValue)
        ft$addTextGroup(localised)
      }
      seq <- ISOElementSequence$new(xml=NULL, text, ft)
      return(seq)
    }
  )                              
)

ISOAbstractObject$getStandardByPrefix = function(prefix){
  std <- switch(prefix,
    "GCO" = data.frame(specification = "ISO/TS 19103:2005", title = "Geographic Common extensible markup language", stringsAsFactors = FALSE),
    "GFC" = data.frame(specification = "ISO/TC211 19110:2005", title = "Geographic Information - Methodology for feature cataloguing", stringsAsFactors = FALSE),
    "GMD" = data.frame(specification = "ISO/TC211 19115-1:2003", title = "Geographic Information - Metadata", stringsAsFactors = FALSE),
    "GMI" = data.frame(specification = "ISO/TC211 19115-2:2009", title = "Geographic Information - Metadata - Part 2: Extensions for imagery and gridded data", stringsAsFactors = FALSE),
    "GTS" = data.frame(specification = "ISO/TC211 19139:2007", title = "Geographic Metadata XML Schema - Geographic Temporal Schema (GTS)", stringsAsFactors = FALSE),
    "SRV" = data.frame(specification = "ISO/TC211 19119:2005", title = "Geographic Information - Service Metadata", stringsAsFactors = FALSE),
    "GMX" = data.frame(specification = "ISO/TC211 19139:2007", title = "Geographic Metadata XML Schema", stringsAsFactors = FALSE),
    "GML" = data.frame(specification = "GML 3.2.1 (ISO 19136)", title = "Geographic Markup Language", stringsAsFactors = FALSE),
    "GMLCOV" = data.frame(specification = "GML 3.2.1 Coverage (OGC GMLCOV)", title = "OGC GML Coverage Implementation Schema", stringsAsFactors = FALSE),
    "GMLRGRID" = data.frame(specification = "GML 3.3 Referenceable Grid (OGC GML)", title = "OGC GML Referenceable Grid", stringsAsFactors = FALSE),
    "SWE" = data.frame(specification = "SWE 2.0", title = "Sensor Web Enablement (SWE) Common Data Model", stringsAsFactors = FALSE)
  )
  return(std)
}

ISOAbstractObject$getISOStandard = function(clazz){
  std <- NA
  if(is.null(clazz$private_fields)) return(std)
  if(is.null(clazz$private_fields$xmlNamespacePrefix)) return(std)
  std <- ISOAbstractObject$getStandardByPrefix(clazz$private_fields$xmlNamespacePrefix)
  return(std)
}

ISOAbstractObject$getISOClasses = function(extended = FALSE, pretty = FALSE){
  list_of_classes <- unlist(sapply(search(), ls))
  list_of_classes <- list_of_classes[sapply(list_of_classes, function(x){
    clazz <- invisible(try(eval(parse(text=x)),silent=TRUE))
    r6Predicate <- class(clazz)[1]=="R6ClassGenerator"
    envPredicate <- extended
    if(r6Predicate & !extended){
      if(is.environment(clazz$parent_env)){
        envPredicate <- environmentName(clazz$parent_env)=="geometa"
      }
    }
    includePredicate <- TRUE
    if(r6Predicate){
      if(!is.null(clazz$classname)){
        includePredicate <- !(clazz$classname %in% c("geometaLogger", "INSPIREMetadataValidator",
          "ISOCodelist", "ISOCodeListValue", "ISOMetadataNamespace", "ISOTimePeriod","ISOAttributes",
          "pivot_format"))
      }
    }
    return(r6Predicate & envPredicate & includePredicate)
  })]
  list_of_classes <- as.vector(list_of_classes)
  if(pretty){
    std_info <- do.call("rbind",lapply(list_of_classes, function(x){
      clazz <- invisible(try(eval(parse(text=x)),silent=TRUE))
      std <- ISOAbstractObject$getISOStandard(clazz)
      std_info <- cbind(
        std,
        ns_prefix = clazz$private_fields$xmlNamespacePrefix,
        ns_uri = ISOMetadataNamespace[[clazz$private_fields$xmlNamespacePrefix]]$uri,
        element = clazz$private_fields$xmlElement,
        stringsAsFactors = FALSE
      )
      return(std_info)
    }))
    
    list_of_classes <- data.frame(
      geometa_class = list_of_classes,
      std_info,
      stringsAsFactors = FALSE
    )
  }
  return(list_of_classes)
}

ISOAbstractObject$getISOClassByNode = function(node){
  outClass <- NULL
  if(!is(node, "XMLInternalDocument")) node <- xmlDoc(node)
  nodeElement <- xmlRoot(node)
  nodeElementName <- xmlName(nodeElement)
  nodeElementNames <- unlist(strsplit(nodeElementName, ":"))
  if(length(nodeElementNames)>1){
    nodeElementName <- nodeElementNames[2]
  }
  
  list_of_classes <- getISOClasses()
  if(is.null(list_of_classes))
    list_of_classes <- ISOAbstractObject$getISOClasses(extended = TRUE, pretty = FALSE)
   
  for(classname in list_of_classes){
    clazz <- try(eval(parse(text=classname)))
    if(nodeElementName %in% clazz$private_fields$xmlElement){
      geometa_inherits <- FALSE
      superclazz <- clazz
      while(!geometa_inherits && !is.null(superclazz)){
        clazz_fields <- names(superclazz)
        if(!is.null(clazz_fields)) if(length(clazz_fields)>0) if("parent_env" %in% clazz_fields){
          if(environmentName(superclazz$parent_env)=="geometa"){
            geometa_inherits <- TRUE
            break
          }else{
            if("get_inherit" %in% clazz_fields){
              superclazz <- superclazz$get_inherit()
            }
          }
        }
      }
      if(!geometa_inherits) next
      if(length(clazz$private_fields)>0
         && !is.null(clazz$private_fields$xmlElement)
         && !is.null(clazz$private_fields$xmlNamespacePrefix)){
        outClass <- clazz
        break
      }
    }
  }
  return(outClass)
}

ISOAbstractObject$compare = function(metadataElement1, metadataElement2){
  text1 <- NULL
  if(is(metadataElement1, "ISOAbstractObject")){
    xml1 <-metadataElement1$encode(addNS = TRUE, validate = FALSE,
                                   resetSerialID = FALSE, setSerialID = FALSE)
    if(metadataElement1$isDocument()){
      content1 <- as(xml1, "character")
      content1 <- gsub("<!--.*?-->", "", content1)
      xml1 <- xmlParse(content1) 
    }else{
      xml1 <- XML::xmlDoc(xml1)
    }
    text1 <- as(xml1, "character")
  }else{
    text1 <- as.character(metadataElement1)
  }
  text2 <- NULL
  if(is(metadataElement2, "ISOAbstractObject")){
    xml2 <- metadataElement2$encode(addNS = TRUE, validate = FALSE, setSerialID = FALSE)
    if(metadataElement2$isDocument()){
      content2 <- as(xml2, "character")
      content2 <- gsub("<!--.*?-->", "", content2)
      xml2 <- xmlParse(content2) 
    }else{
      xml2 <- XML::xmlDoc(xml2)
    }
    text2 <- as(xml2, "character")
  }else{
    text2 <- as.character(metadataElement2)
  }
  return(text1 == text2)
}

#' @name cacheISOClasses
#' @aliases cacheISOClasses
#' @title cacheISOClasses
#' @export
#' @description \code{\link{cacheISOClasses}} allows to cache the list of
#' \pkg{geometa} classes or extended. This is especially required to fasten
#' the decoding of metadata elements from an XML file. It is called internally
#' by \pkg{geometa} the first function \code{\link{getISOClasses}} is called 
#' and each time the function \code{\link{readISO19139}} function is called to 
#' integrate eventually new classes added by user to extend \pkg{geometa} model 
#' (case of ISO profiles).
#' 
#' @usage cacheISOClasses()
#' 
#' @examples             
#'   cacheISOClasses()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
cacheISOClasses <- function(){
  .geometa.iso$classes <- ISOAbstractObject$getISOClasses(extended = TRUE, pretty = FALSE)
}

#' @name getISOClasses
#' @aliases getISOClasses
#' @title getISOClasses
#' @export
#' @description get the list of cached ISO classes
#' 
#' @usage getISOClasses()
#' 
#' @examples             
#'   getISOClasses()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getISOClasses <- function(){
  if(length(.geometa.iso$classes)==0) cacheISOClasses()
  return(.geometa.iso$classes)
}

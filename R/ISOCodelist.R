#' ISOCodelist
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO codelist
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO codelist
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used by geometa internal codelist XML decoder/encoder
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCodelist <- R6Class("ISOCodelist",
   inherit = ISOAbstractObject,
   private = list(
     metadataStandardCompliance = FALSE,
     xmlElement = "CT_Codelist",
     xmlNamespacePrefix = list(
       "19139" = "GMX",
       "19115-3" = "CAT"
     )
   ),
   public = list(
     #'@field id id
     id =NA,
     #'@field refFile ref file
     refFile = NA,
     #'@field codeSpace code space
     codeSpace = NA,
     #'@field identifier identifier
     identifier = NA,
     #'@field description description
     description = NA,
     #'@field codeEntry code entries
     codeEntry = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param refFile ref file
     #'@param id id
     initialize = function(xml = NULL, refFile = NULL, id = NULL){
       super$initialize(xml = xml)
       
       #legacy
       if(!is.null(refFile) & !is.null(id) & length(self$codeEntry)==0){
         self$refFile <- refFile
         self$parse(refFile, id)
       }
     },
     
     #'@description get code entries
     #'@param pretty prettify output as \code{data.frame}. Default is\code{FALSE}
     #'@return an object of class \link{list} or \link{data.frame}
     getCodeEntries = function(pretty = FALSE){
       entries = self$codeEntry
       if(pretty){
         entries <- do.call("rbind", lapply(entries, function(entry){
           data.frame(
             identifier = if(is(entry$identifier,"ISOScopedName")) entry$identifier$value else entry$identifier,
             description = entry$description,
             stringsAsFactors = FALSE
           )
         }))
       }
       return(entries)
     },
     
     #'@description Parse codelist
     #'@param refFile ref file
     #'@param id id
     parse = function(refFile, id){
       
       #query ISO XML Codelist file
       clFile <- refFile
       isLocalFile <- !grepl("^http", refFile) & !grepl("^https", refFile)
       # if(isLocalFile){
       #   if(getGeometaOption("internalCodelists")){
       #     clFile <- system.file("extdata/codelists", refFile, package = "geometa", mustWork = TRUE)
       #   }
       # }
       if(nchar(clFile)==0){
         stop(sprintf("Reference file '%s' missing in geometa files", refFile))
       }
       
       self$id <- id
       
       if(id == "LanguageCode" & isLocalFile & getGeometaOption("internalCodelists")){
         identifier = ISOScopedName$new(value = id)
         identifier$setCodeSpace("ISO 639-2")
         self$identifier <- identifier
         self$codeSpace <- "ISO 639-2"
         self$description <- "Language : ISO 639-2 (3 characters)"
         codeEntry <- utils::read.csv(clFile, sep="|", stringsAsFactors = FALSE)
         codeEntry <- codeEntry[,c("alpha3", "english", "english")]
         colnames(codeEntry) <- c("value","name", "description")
         self$codeEntry = lapply(1:nrow(codeEntry), function(i){
           clv = ISOCodelistValue$new()
           clv$identifier = codeEntry[i,]$value; clv$description = codeEntry[i,]$description
           return(clv)
         })
       }else{
         
         isML <- regexpr("ML", refFile) > 0
         
         #parse ISO XML codelist file
         url_regex <- '(http|https)[^([:blank:]|\\"|<|&|#\n\r)]+'
         isURL <- regexpr(url_regex, refFile) > 0
         #parse ISO XML codelist file
         if(isURL){
           clXML <- httr::GET(clFile)
           clXML <- httr::content(clXML, "text", encoding = "UTF-8")
         }else{
           clXML <- XML::xmlParse(clFile)
           clXML <- methods::as(clXML, "character")
         }
         clXML <- gsub("<!--.*?-->", "", clXML)
         clXML <- XML::xmlParse(clXML, asText = TRUE) 
         ns <- XML::xmlNamespaceDefinitions(clXML)
         nsdf <- do.call("rbind", lapply(ns, function(x){
           return(data.frame(id = x$id, uri = x$uri, stringsAsFactors = FALSE))
         }))
         clDicts <- XML::xpathApply(clXML,"//gmx:codelistItem", function(x){XML::xmlChildren(x)[[1]]},
                                    namespaces = c(gmx = nsdf[nsdf$id == "","uri"]))
         clDictXML <- NULL
         invisible(lapply(clDicts, function(x){
           clId <- XML::xmlGetAttr(x, "gml:id")
           if(is.null(clId)) clId <- XML::xmlGetAttr(x, "id")
           if(clId == id || regexpr(id, clId) != -1){
             clDictXML <<- XML::xmlDoc(x)
           }
         }))
         if(is.null(clDictXML)) return(NULL)
         
         #codelist identification
         idXML <- XML::getNodeSet(clDictXML, "//gml:identifier",
                                  namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
         if(length(idXML)>1){
           codeSpace <- XML::xmlGetAttr(idXML[[1]], "codeSpace")
           identifier = ISOScopedName$new(value = XML::xmlValue(idXML[[1]]))
           identifier$setCodeSpace(codeSpace)
           self$identifier <- identifier
           self$codeSpace = codeSpace
         }
         desXML <- XML::getNodeSet(clDictXML, "//gml:description",
                                   namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
         if(length(desXML)>1){
           self$description <- XML::xmlValue(desXML[[1]])
         }
         
         #codelist codeEntry
         entriesXML <- XML::getNodeSet(clDictXML, "//gmx:codeEntry",
                                       c(gmx = nsdf[nsdf$id=="","uri"]))
         codeEntry <- do.call("rbind",lapply(entriesXML, function(x){
           entry.df <- data.frame(identifier = NA, name = NA, description = NA)
           identifier <- getNodeSet(xmlDoc(x), "//gml:identifier", namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
           if(length(identifier)>0) entry.df$identifier <- xmlValue(identifier[[1]])
           name <- getNodeSet(xmlDoc(x), "//gml:name", namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
           if(length(name)>0) entry.df$name <- xmlValue(name[[1]])
           description <- getNodeSet(xmlDoc(x), "//gml:description", namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
           if(length(description)>0) entry.df$description <- xmlValue(description[[1]])
           return(entry.df)
         }))
         colnames(codeEntry) <- c("value", "name", "description")
         self$codeEntry = lapply(1:nrow(codeEntry), function(i){
           clv = ISOCodelistValue$new()
           clv$identifier = codeEntry[i,]$value; clv$description = codeEntry[i,]$description;
           return(clv)
         })
       }
     }
   )                      
)

#' setISOCodelists
#' @export
setISOCodelists <- function(version = "19139"){
  langCL <- system.file("extdata/codelists", "ISO-639-2_utf-8.txt", package = "geometa", mustWork = TRUE) #from http://www.loc.gov/standards/iso639-2/
  ML_gmxCL <- system.file("extdata/codelists", "ML_gmxCodelists.xml", package = "geometa", mustWork = TRUE)
  
  if(is.null(.geometa.iso$codelists)) .geometa.iso$codelists = list()
  if(is.null(.geometa.iso$codelists[[version]])){
    packageStartupMessage(sprintf("Loading ISO %s codelists...", version))
    codelists <- switch(version,
      "19139" = {
        cls = c(
          ISOCodelist$new(refFile = ML_gmxCL, id = "Country"),
          ISOCodelist$new(refFile = langCL, id = "LanguageCode"), #from http://www.loc.gov/standards/iso639-2/
          {
            cat <- ISOCodelistCatalogue$new(refFile = system.file("extdata/codelists", "gmxCodelists.xml", package = "geometa", mustWork = TRUE))
            cat$getCodelists()
          }
        )
        cls = cls[!sapply(cls, is.null)]
        names(cls) <- sapply(cls, function(cl){cl$identifier$value})
        cls
      },
      "19115-3" = {
        cls = c(
          ISOCodelist$new(refFile = ML_gmxCL, id = "Country"),
          ISOCodelist$new(refFile = langCL, id = "LanguageCode"), #from http://www.loc.gov/standards/iso639-2/
          {
            cat <- ISOCodelistCatalogue$new(refFile = system.file("extdata/schemas/19115/resources/Codelists/cat", "codelists.xml", package = "geometa", mustWork = TRUE))
            cat$getCodelists()
          }
        )
        cls = cls[!sapply(cls, is.null)]
        names(cls) <- sapply(cls, function(cl){cl$identifier$value})
        cls
      }
    )
    .geometa.iso$codelists[[version]] <- codelists
  }
}

#' @name getISOInternalCodelists
#' @aliases getISOInternalCodelists
#' @title getISOInternalCodelists
#' @export
#' @description \code{getISOInternalCodelists} allows to get the list of ISO codelists
#' registered in \pkg{geometa}
#' 
#' @usage getISOInternalCodelists()
#' 
#' @examples             
#'   getISOInternalCodelists()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getISOInternalCodelists <- function(){
  return(.geometa.iso$codelists)
}

#' @name getISOCodelists
#' @aliases getISOCodelists
#' @title getISOCodelists
#' @export
#' @description \code{getISOCodelists} allows to get the list of ISO codelists
#' registered in \pkg{geometa}, their description and XML definition. The object
#' returned is of class "data.frame"
#' 
#' @usage getISOCodelists()
#' 
#' @examples             
#'   getISOCodelists()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getISOCodelists <- function(){
  cl_classes <- list()
  classes <- ls("package:geometa")
  for(classname in classes){
    clazz <- eval(parse(text=classname))
    if(is(clazz, "R6ClassGenerator")){
      if(!is.null(clazz$inherit)){
        if(clazz$inherit == "ISOCodeListValue"){
          cl_classes <- c(cl_classes, clazz)
        }
      }
    }
  }
  cl_classes_out <- do.call("rbind", lapply(cl_classes, function(x){
    el <- x$private_fields$xmlElement
    if(el=="MD_ScopeCode") el <- "MX_ScopeCode"
    cl <- getISOCodelist(el)
    cl_ns <- getISOMetadataNamespace(x$private_fields$xmlNamespacePrefix)$uri
    out <- data.frame(
      classname = x$classname,
      codeSpace = cl$codeSpace,
      description = cl$description,
      xmlNamespace = cl_ns,
      xmlElement = cl$id
    )
    return(out)
  }))
  return(cl_classes_out)
}

#' @name getISOCodelist
#' @aliases getISOCodelist
#' @title getISOCodelist
#' @export
#' @description \code{getISOCodelist} allows to get a registered ISO codelist by id
#' registered in \pkg{geometa}
#' 
#' @usage getISOCodelist(id)
#' 
#' @param id identifier of the codelist
#' 
#' @examples             
#'   getISOCodelist(id = "LanguageCode")
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getISOCodelist <- function(id){
  codelist <- NULL
  invisible(lapply(getISOInternalCodelists()[[getMetadataStandard()]], function(cl){
    if(cl$identifier$value == id){
      codelist <<- cl
    }
  }))
  if(getMetadataStandard()=="19115-3" & is.null(codelist)){
    invisible(lapply(getISOInternalCodelists()[["19139"]], function(cl){
      if(cl$identifier$value == id){
        codelist <<- cl
      }
    }))
  }
  return(codelist)
}

#' @name registerISOCodelist
#' @aliases registerISOCodelist
#' @title registerISOCodelist
#' @export
#' @description \code{registerISOCodelist} allows to register a new codelist
#' registered in \pkg{geometa}
#' 
#' @usage registerISOCodelist(refFile, id, force)
#' 
#' @param refFile ISO XML file handling the ISO codelist
#' @param id identifier of the ISO codelist
#' @param force logical parameter indicating if registration has be to be forced
#' in case the identified codelist is already registered
#' 
#' @examples             
#'   registerISOCodelist(
#'    refFile = "http://www.isotc211.org/2005/resources/Codelist/ML_gmxCodelists.xml",
#'    id = "LanguageCode",
#'    force = TRUE
#'  )
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
registerISOCodelist <- function(refFile, id, force = FALSE){
  cl <- getISOCodelist(id)
  if(!is.null(cl)){
    if(!force) stop(sprintf("ISOcodelist with id '%s' already exists. Use force = TRUE to force registration", id))
    .geometa.iso$codelists[sapply(.geometa.iso$codelists, function(x){x$id == id})][[1]] <- ISOCodelist$new(refFile, id)
  }else{
    cl <- ISOCodelist$new(refFile, id)
    .geometa.iso$codelists <- c(.geometa.iso$codelists, cl)
  }
}

#' ISOCodelist
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO codelist
#' @return Object of \code{\link{R6Class}} for modelling an ISO codelist
#' @format \code{\link{R6Class}} object.
#'
#' @field id
#' @field refFile
#' @field codeSpace
#' @field identifier
#' @field description
#' @field entries
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(refFile, id)}}{
#'    This method is used to instantiate an ISOCodelist
#'  }
#' }
#' 
#' @note Class used by geometa internal codelist XML decoder/encoder
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCodelist <- R6Class("ISOCodelist",
  public = list(
    id =NA,
    refFile = NA,
    codeSpace = NA,
    identifier = NA,
    description = NA,
    entries = NULL,
    initialize = function(refFile, id){
      self$refFile <- refFile
      self$parse(refFile, id)
    },
     
    parse = function(refFile, id){
      
      #query ISO XML Codelist file
      clFile <- refFile
      isLocalFile <- !grepl("^http", refFile) & !grepl("^https", refFile)
      if(isLocalFile){
        if(getGeometaOption("internalCodelists")){
          clFile <- system.file("extdata/codelists", refFile, package = "geometa", mustWork = TRUE)
        }
      }
      if(nchar(clFile)==0){
        stop(sprintf("Reference file '%s' missing in geometa files", refFile))
      }
      
      self$id <- id
      
      if(id == "LanguageCode" & isLocalFile & getGeometaOption("internalCodelists")){
        self$identifier <- id
        self$codeSpace <- "ISO 639-2"
        self$description <- "Language : ISO 639-2 (3 characters)"
        self$entries <- utils::read.csv(clFile, sep="|", stringsAsFactors = FALSE)
        self$entries <- self$entries[,c("alpha3", "english", "english")]
        colnames(self$entries) <- c("value","name", "description")
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
          if(clId == id || regexpr(id, clId) != -1){
            clDictXML <<- XML::xmlDoc(x)
          }
        }))
        
        #codelist identification
        idXML <- XML::getNodeSet(clDictXML, "//gml:identifier",
                            namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
        if(length(idXML)>1){
          self$identifier <- XML::xmlValue(idXML[[1]])
          self$codeSpace <- XML::xmlGetAttr(idXML[[1]], "codeSpace")
        }
        desXML <- XML::getNodeSet(clDictXML, "//gml:description",
                             namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
        if(length(desXML)>1){
          self$description <- XML::xmlValue(desXML[[1]])
        }
        
        #codelist entries
        entriesXML <- XML::getNodeSet(clDictXML, "//gmx:codeEntry",
                                 c(gmx = nsdf[nsdf$id=="","uri"]))
        self$entries <- do.call("rbind",lapply(entriesXML, function(x){
          entry.df <- data.frame(identifier = NA, name = NA, description = NA)
          identifier <- getNodeSet(xmlDoc(x), "//gml:identifier", namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
          if(length(identifier)>0) entry.df$identifier <- xmlValue(identifier[[1]])
          name <- getNodeSet(xmlDoc(x), "//gml:name", namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
          if(length(name)>0) entry.df$name <- xmlValue(name[[1]])
          description <- getNodeSet(xmlDoc(x), "//gml:description", namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
          if(length(description)>0) entry.df$description <- xmlValue(description[[1]])
          return(entry.df)
        }))
        colnames(self$entries) <- c("value", "name", "description")
      }
    }
  )                      
)

#' setISOCodelists
#' @export
setISOCodelists <- function(){
  packageStartupMessage("Loading ISO 19115 codelists...")
  
  #parse other ISO codelists
  langCL <- "ISO-639-2_utf-8.txt" #from http://www.loc.gov/standards/iso639-2/
  ML_gmxCL <- "ML_gmxCodelists.xml"
  gmxCL <- "gmxCodelists.xml"
  codelists <- list(
    ISOCodelist$new(gmxCL, "CI_DateTypeCode"),
    ISOCodelist$new(gmxCL, "CI_PresentationFormCode"),
    ISOCodelist$new(gmxCL, "CI_RoleCode"),
    ISOCodelist$new(gmxCL, "CI_OnLineFunctionCode"),
    ISOCodelist$new(ML_gmxCL, "Country"),
    ISOCodelist$new(gmxCL, "DCPList"),
    ISOCodelist$new(gmxCL, "DQ_EvaluationMethodTypeCode"),
    ISOCodelist$new(gmxCL, "DS_AssociationTypeCode"),
    ISOCodelist$new(gmxCL, "DS_InitiativeTypeCode"),
    ISOCodelist$new(gmxCL, "FC_RoleType"),
    ISOCodelist$new(langCL, "LanguageCode"),
    ISOCodelist$new(gmxCL, "MD_CellGeometryCode"),
    ISOCodelist$new(ML_gmxCL, "MD_CharacterSetCode"),
    ISOCodelist$new(gmxCL, "MD_ClassificationCode"),
    ISOCodelist$new(gmxCL, "MD_CoverageContentTypeCode"),
    ISOCodelist$new(gmxCL, "MD_DatatypeCode"),
    ISOCodelist$new(gmxCL, "MD_DimensionNameTypeCode"),
    ISOCodelist$new(gmxCL, "MD_GeometricObjectTypeCode"),
    ISOCodelist$new(gmxCL, "MD_KeywordTypeCode"),
    ISOCodelist$new(gmxCL, "MD_ImagingConditionCode"),
    ISOCodelist$new(gmxCL, "MD_MaintenanceFrequencyCode"),
    ISOCodelist$new(gmxCL, "MD_ObligationCode"),
    ISOCodelist$new(gmxCL, "MD_ProgressCode"),
    ISOCodelist$new(gmxCL, "MD_RestrictionCode"),
    ISOCodelist$new(gmxCL, "MD_SpatialRepresentationTypeCode"),
    ISOCodelist$new(gmxCL, "MD_TopicCategoryCode"),
    ISOCodelist$new(gmxCL, "MD_TopologyLevelCode"),
    ISOCodelist$new(gmxCL, "MX_ScopeCode"),
    ISOCodelist$new(gmxCL, "SV_CouplingType"),
    ISOCodelist$new(gmxCL, "SV_ParameterDirection")
  )
  names(codelists) <- sapply(codelists, function(cl){cl$id})
  .geometa.iso$codelists <- codelists
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
  invisible(lapply(getISOInternalCodelists(), function(cl){
    if(cl$id == id){
      codelist <<- cl
    }
  }))
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
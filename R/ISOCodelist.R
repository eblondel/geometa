#' ISOCodelist
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO codelist
#' @return Object of \code{\link{R6Class}} for modelling an ISO codelist
#' @format \code{\link{R6Class}} object.
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
      if(!is.null(refFile) && !is.null(id)){
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
            identifier = entry$identifier,
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
        self$codeEntry <- utils::read.csv(clFile, sep="|", stringsAsFactors = FALSE)
        self$codeEntry <- self$codeEntry[,c("alpha3", "english", "english")]
        colnames(self$codeEntry) <- c("value","name", "description")
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
        if(is.null(clDictXML)) return(NULL)
        
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
        
        #codelist codeEntry
        entriesXML <- XML::getNodeSet(clDictXML, "//gmx:codeEntry",
                                 c(gmx = nsdf[nsdf$id=="","uri"]))
        self$codeEntry <- do.call("rbind",lapply(entriesXML, function(x){
          entry.df <- data.frame(identifier = NA, name = NA, description = NA)
          identifier <- getNodeSet(xmlDoc(x), "//gml:identifier", namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
          if(length(identifier)>0) entry.df$identifier <- xmlValue(identifier[[1]])
          name <- getNodeSet(xmlDoc(x), "//gml:name", namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
          if(length(name)>0) entry.df$name <- xmlValue(name[[1]])
          description <- getNodeSet(xmlDoc(x), "//gml:description", namespaces = c(gml = nsdf[nsdf$id == "gml","uri"]))
          if(length(description)>0) entry.df$description <- xmlValue(description[[1]])
          return(entry.df)
        }))
        colnames(self$codeEntry) <- c("value", "name", "description")
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
    #ISO 19110:2005 codelists
    ISOCodelist$new(refFile = gmxCL, id = "FC_RoleType"),
    #ISO 19115-1:2003 Codelists
    ISOCodelist$new(refFile = gmxCL, id = "CI_DateTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "CI_PresentationFormCode"),
    ISOCodelist$new(refFile = gmxCL, id = "CI_RoleCode"),
    ISOCodelist$new(refFile = gmxCL, id = "CI_OnLineFunctionCode"),
    ISOCodelist$new(refFile = ML_gmxCL, id = "Country"),
    ISOCodelist$new(refFile = gmxCL, id = "DCPList"),
    ISOCodelist$new(refFile = gmxCL, id = "DQ_EvaluationMethodTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "DS_AssociationTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "DS_InitiativeTypeCode"),
    ISOCodelist$new(refFile = langCL, id = "LanguageCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_CellGeometryCode"),
    ISOCodelist$new(refFile = ML_gmxCL, id = "MD_CharacterSetCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_ClassificationCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_CoverageContentTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_DatatypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_DimensionNameTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_DistributionUnits"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_GeometricObjectTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_KeywordTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_ImagingConditionCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_MaintenanceFrequencyCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_MediumFormatCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_MediumNameCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_ObligationCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_PixelOrientationCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_ProgressCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_RestrictionCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_SpatialRepresentationTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_TopicCategoryCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MD_TopologyLevelCode"),
    #ISO 19115-2:2009 codelists
    ISOCodelist$new(refFile = gmxCL, id = "MI_BandDefinition"),
    ISOCodelist$new(refFile = gmxCL, id = "MI_ContextCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MI_GeometryTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MI_ObjectiveTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MI_OperationTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MI_PolarisationOrientationCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MI_PriorityCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MI_SequenceCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MI_TransferFunctionTypeCode"),
    ISOCodelist$new(refFile = gmxCL, id = "MI_TriggerCode"),
    #ISO 19119:2005 codelists
    ISOCodelist$new(refFile = gmxCL, id = "SV_CouplingType"),
    ISOCodelist$new(refFile = gmxCL, id = "SV_ParameterDirection"),
    #ISO 19139:2007 codelists
    ISOCodelist$new(refFile = gmxCL, id = "MX_ScopeCode")
    #ISO/TS 19115-3:2016
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
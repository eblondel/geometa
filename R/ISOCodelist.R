#' ISOCodelist
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO codelist
#' @return Object of \code{\link{R6Class}} for modelling an ISO codelist
#' @format \code{\link{R6Class}} object.
#'
#' @field value
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
        if(getGeometaOption("codelists")=="geometa"){
          clFile <- system.file("extdata/codelists", refFile, package = "geometa", mustWork = TRUE)
        }
      }
      if(nchar(clFile)==0){
        stop(sprintf("Reference file '%s' missing in geometa files", refFile))
      }
      
      if(id == "LanguageCode" & isLocalFile & getGeometaOption("codelists")=="geometa"){
        self$identifier <- id
        self$codeSpace <- "ISO 639-2"
        self$description <- "Language : ISO 639-2 (3 characters)"
        self$entries <- utils::read.csv(clFile, sep="|", stringsAsFactors = FALSE)
        self$entries <- self$entries[,c("alpha3", "english")]
        colnames(self$entries) <- c("value","description")
      }else{
      
        isML <- regexpr("ML", refFile) > 0
        
        #parse ISO XML codelist file
        clXML <- XML::xmlParse(clFile)
        clXML <- methods::as(clXML, "character")
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
          XML::xmlToDataFrame(x, stringsAsFactors = FALSE)[,c("identifier", "description")]
        }))
        colnames(self$entries) <- c("value", "description")
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
    ISOCodelist$new(gmxCL, "DCPList"),
    ISOCodelist$new(gmxCL, "DQ_EvaluationMethodTypeCode"),
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
  names(codelists) <- sapply(codelists, function(cl){cl$identifier})
  .geometa.iso$codelists <- codelists
}

#' @name getISOCodelists
#' @aliases getISOCodelists
#' @title getISOCodelists
#' @export
#' @description \code{getISOCodelists} allows to get the list of ISO codelists
#' registered in \pkg{geometa}
#' 
#' @usage getISOCodelists()
#' 
#' @examples             
#'   getISOCodelists()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getISOCodelists <- function(){
  return(.geometa.iso$codelists)
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
  invisible(lapply(getISOCodelists(), function(cl){
    if(cl$identifier == id){
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
    .geometa.iso$codelists[sapply(.geometa.iso$codelists, function(x){x$identifier == id})][[1]] <- ISOCodelist$new(refFile, id)
  }else{
    cl <- ISOCodelist$new(refFile, id)
    .geometa.iso$codelists <- c(.geometa.iso$codelists, cl)
  }
}
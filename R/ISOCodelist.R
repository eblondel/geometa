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
#'  \item{\code{new(xml,value)}}{
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
      clFile <- system.file("extdata/codelists", refFile, package = "geometa")
      if(nchar(clFile)==0){
        stop(sprintf("Reference file '%s' missing in geometa files", refFile))
      }
      
      if(id == "LanguageCode"){
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

#' fetchISOCodelists
#' @export
fetchISOCodelists <- function(){
  cat("[geometa][INFO] Loading ISO codelists... \n")
  
  #parse other ISO codelists
  languageCL <- "ISO-639-2_utf-8.txt" #from http://www.loc.gov/standards/iso639-2/
  ML_gmxCL <- "ML_gmxCodelists.xml"
  gmxCL <- "gmxCodelists.xml"
  codelists <- list(
    ISOCodelist$new(languageCL, "LanguageCode"),
    ISOCodelist$new(ML_gmxCL, "MD_CharacterSetCode"),
    ISOCodelist$new(ML_gmxCL, "MD_ScopeCode"),
    ISOCodelist$new(gmxCL, "CI_RoleCode"),
    ISOCodelist$new(gmxCL, "MD_TopicCategoryCode"),
    ISOCodelist$new(gmxCL, "MD_RestrictionCode"),
    ISOCodelist$new(gmxCL, "MD_MaintenanceFrequencyCode"),
    ISOCodelist$new(gmxCL, "CI_DateTypeCode"),
    ISOCodelist$new(gmxCL, "CI_PresentationFormCode"),
    ISOCodelist$new(gmxCL, "MD_KeywordTypeCode"),
    ISOCodelist$new(gmxCL, "MD_TopologyLevelCode"),
    ISOCodelist$new(gmxCL, "MD_GeometricObjectTypeCode"),
    ISOCodelist$new(gmxCL, "MD_ProgressCode"),
    ISOCodelist$new(gmxCL, "MD_SpatialRepresentationTypeCode"),
    ISOCodelist$new(gmxCL, "MD_ClassificationCode"),
    ISOCodelist$new(gmxCL, "MD_CellGeometryCode"),
    ISOCodelist$new(gmxCL, "MD_DimensionNameTypeCode"),
    ISOCodelist$new(gmxCL, "MD_CoverageContentTypeCode")
  )
  names(codelists) <- sapply(codelists, function(cl){cl$identifier})
  .geometa.iso$codelists <- codelists
}

getISOCodelists <- function(){
  return(.geometa.iso$codelists)
}

getISOCodelist <- function(id){
  codelist <- NULL
  invisible(lapply(.geometa.iso$codelists, function(cl){
    if(cl$identifier == id){
      codelist <<- cl
    }
  }))
  return(codelist)
}
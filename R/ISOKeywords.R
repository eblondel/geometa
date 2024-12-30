#' ISOKeywords
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO keywords
#' @return Object of \code{\link[R6]{R6Class}} for modelling a ISO set of keywords
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #a basic keyword set
#'   md <- ISOKeywords$new()
#'   md$addKeyword("keyword1")
#'   md$addKeyword("keyword2")
#'   md$setKeywordType("theme")
#'   th <- ISOCitation$new()
#'   th$setTitle("General")
#'   md$setThesaurusName(th)
#'   xml <- md$encode()
#'   
#'   #a keyword set with anchors
#'   md <- ISOKeywords$new()
#'   kwd1 <- ISOAnchor$new(
#'     name = "keyword1",
#'     href = "http://myvocabulary.geometa/keyword1"
#'   )
#'   md$addKeyword(kwd1)
#'   kwd2 <- ISOAnchor$new(
#'     name = "keyword2",
#'     href = "http://myvocabulary.geometa/keyword2"
#'   )
#'   md$addKeyword(kwd2)
#'   md$setKeywordType("theme")
#'   xml <- md$encode()
#'   
#'   #Example for INSPIRE (GEMET Spatial Data Theme)
#'   inspire_kwd <- ISOKeywords$new()
#'   anc1 <- ISOAnchor$new(
#'     name = "Environmental monitoring facilities",
#'     href = "http://inspire.ec.europa.eu/theme/ef"
#'   )
#'   inspire_kwd$addKeyword(anc1)
#'   inspire_kwd$setKeywordType("theme")
#'   th <- ISOCitation$new()
#'   th$setTitle(
#'     ISOAnchor$new(
#'      name = "GEMET - INSPIRE themes, version 1.0",
#'      href="http://www.eionet.europa.eu/gemet/inspire_themes"
#'     )
#'   )
#'   inspire_date <- ISODate$new()
#'   inspire_date$setDate(as.Date("2008-06-01"))
#'   inspire_date$setDateType("publication")
#'   th$addDate(inspire_date)
#'   inspire_kwd$setThesaurusName(th)
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_Keywords}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_MD_Keywords}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOKeywords <- R6Class("ISOKeywords",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_Keywords",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MRI"
    )
  ),
  public = list(
    #'@field keyword keyword
    keyword = list(),
    #'@field type type
    type = NULL,
    #'@field thesaurusName thesaurus name
    thesaurusName = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Adds keyword
    #'@param keyword keyword
    #'@param locales list of localized texts. Default is \code{NULL}
    #'@return \code{TRUE} if added, \code{FALSe} otherwise
    addKeyword = function(keyword, locales = NULL){
      if(is.null(keyword)) return(FALSE);
      if(is(keyword, "character")) if(is.na(keyword)) return(FALSE);
      if(!is.null(locales)){
        keyword <- self$createLocalisedProperty(keyword, locales)
      }
      return(self$addListElement("keyword", keyword))
    },
    
    #'@description Deletes keyword
    #'@param keyword keyword
    #'@param locales list of localized texts. Default is \code{NULL}
    #'@return \code{TRUE} if deleted, \code{FALSe} otherwise
    delKeyword = function(keyword, locales = NULL){
      if(is.null(keyword)) return(FALSE);
      if(is(keyword, "character")) if(is.na(keyword)) return(FALSE);
      if(!is.null(locales)){
        keyword <- self$createLocalisedProperty(keyword, locales)
      }
      return(self$delListElement("keyword", keyword))
    },
    
    #'@description Set keyword type
    #'@param keywordType object of class \link{ISOKeywordType} or any \link{character} among
    #' values returned by \code{ISOKeywordType$values()}
    setKeywordType = function(keywordType){
      if(!is(keywordType, "ISOKeywordType")){
        keywordType <- ISOKeywordType$new(value = keywordType)
      }
      self$type <- keywordType
    },
    
    #'@description Set thesaurus name
    #'@param thesaurusName object of class \link{ISOCitation}
    setThesaurusName = function(thesaurusName){
      if(!is(thesaurusName, "ISOCitation")){
        stop("The argument should be a 'ISOCitation' object")
      }
      self$thesaurusName = thesaurusName
    }
  )                        
)

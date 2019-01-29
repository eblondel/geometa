#' ISOKeywords
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO keywords
#' @return Object of \code{\link{R6Class}} for modelling a ISO set of keywords
#' @format \code{\link{R6Class}} object.
#'
#' @field keyword
#' @field type
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOKeywords
#'  }
#'  \item{\code{addKeyword(keyword, locales)}}{
#'    Adds a keyword. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{delKeyword(keyword, locales)}}{
#'    Deletes a keyword. Locale names can be specified as \code{list}
#'    with the \code{locales} argument. Local names should match those
#'    of the keyword to be deleted, otherwise nothing will be deleted.
#'  }
#'  \item{\code{setKeywordType(keywordType)}}{
#'    Sets the keyword type
#'  }
#'  \item{\code{setThesaurusName(thesaurusName)}}{
#'    Sets the thesaurus name
#'  }
#' }
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
#'   inspire_date$setDate("2008-06-01")
#'   inspire_date$setDateType("publication")
#'   th$addDate(inspire_date)
#'   inspire_kwd$setThesaurusName(th)
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOKeywords <- R6Class("ISOKeywords",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_Keywords",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    keyword = list(),
    type = NULL,
    thesaurusName = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #addKeyword
    addKeyword = function(keyword, locales = NULL){
      if(is.null(keyword)) return(FALSE);
      if(is(keyword, "character")) if(is.na(keyword)) return(FALSE);
      if(!is.null(locales)){
        keyword <- self$createLocalisedProperty(keyword, locales)
      }
      return(self$addListElement("keyword", keyword))
    },
    
    #delKeyword
    delKeyword = function(keyword, locales = NULL){
      if(is.null(keyword)) return(FALSE);
      if(is(keyword, "character")) if(is.na(keyword)) return(FALSE);
      if(!is.null(locales)){
        keyword <- self$createLocalisedProperty(keyword, locales)
      }
      return(self$delListElement("keyword", keyword))
    },
    
    #setKeywordType
    setKeywordType = function(keywordType){
      if(!is(keywordType, "ISOKeywordType")){
        keywordType <- ISOKeywordType$new(value = keywordType)
      }
      self$type <- keywordType
    },
    
    #setThesaurusName
    setThesaurusName = function(thesaurusName){
      if(!is(thesaurusName, "ISOCitation")){
        stop("The argument should be a 'ISOCitation' object")
      }
      self$thesaurusName = thesaurusName
    }
  )                        
)

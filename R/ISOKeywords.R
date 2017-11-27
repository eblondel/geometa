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
#'  \item{\code{addKeyword(keyword)}}{
#'    Adds a keyword
#'  }
#'  \item{\code{delKeyword(keyword)}}{
#'    Deletes a keyword
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
    addKeyword = function(keyword){
      if(is.null(keyword)) return(FALSE);
      if(is(keyword, "character")) if(is.na(keyword)) return(FALSE);
      return(self$addListElement("keyword", keyword))
    },
    
    #delKeyword
    delKeyword = function(keyword){
      if(is.null(keyword)) return(FALSE);
      if(is(keyword, "character")) if(is.na(keyword)) return(FALSE);
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

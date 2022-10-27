#' ISOFormat
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO format
#' @return Object of \code{\link{R6Class}} for modelling an ISOFormat
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'  md <- ISOFormat$new()
#'  md$setName("name")
#'  md$setVersion("1.0")
#'  md$setAmendmentNumber("2")
#'  md$setSpecification("specification")
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFormat <- R6Class("ISOFormat",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "MD_Format",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      
      #'@field name name : CharacterString
      name = NULL,
      #'@field version version : CharacterString
      version = NULL,
      #'@field amendmentNumber amendmentNumber [0..1] : CharacterString
      amendmentNumber = NULL,
      #'@field specification specification [0..1] : CharacterString
      specification = NULL,
      #'@field fileDecompressionTechnique fileDecompressionTechnique [0..1] : CharacterString
      fileDecompressionTechnique = NULL,
      #'@field FormatDistributor FormatDistributor [0..*]: ISODistributor
      FormatDistributor = list(),
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
     
      #'@description Set name
      #'@param name name
      #'@param locales list of localized names. Default is \code{NULL}
      setName = function(name, locales = NULL){
        self$name <- name
        if(!is.null(locales)){
          self$name <- self$createLocalisedProperty(name, locales)
        }
      },
      
      #'@description Set version
      #'@param version version
      setVersion = function(version){
        self$version <- as.character(version)
      },
      
      #'@description Set amendment number
      #'@param amendmentNumber amendment number
      setAmendmentNumber = function(amendmentNumber){
        self$amendmentNumber <- as.character(amendmentNumber)
      },
      
      #'@description Set specification
      #'@param specification specification
      #'@param locales list of localized specifications. Default is \code{NULL}
      setSpecification = function(specification, locales = NULL){
        self$specification <- specification
        if(!is.null(locales)){
          self$specification <- self$createLocalisedProperty(specification, locales)
        }
      },
      
      #'@description Set file decompression technique
      #'@param technique technique
      setFileDecompressionTechnique = function(technique){
        self$fileDecompressionTechnique = as.character(technique)
      },
      
      #'@description Adds distributor
      #'@param distributor object of class \link{ISODistributor}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addDistributor = function(distributor){
        if(!is(distributor, "ISODistributor")){
          stop("The argument value should an object of class 'ISODistributor")
        }
        return(self$addListElement("FormatDistributor", distributor))
      },
      
      #'@description Deletes distributor
      #'@param distributor object of class \link{ISODistributor}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delDistributor = function(distributor){
        if(!is(distributor, "ISODistributor")){
          stop("The argument value should an object of class 'ISODistributor")
        }
        return(self$delListElement("FormatDistributor", distributor))
      }
    )                        
)

ISOFormat$buildFrom = function(mimetype){
  
  mimetypes = getIANAMimeTypes()
  mime = mimetypes[mimetypes$name == mimetype,]
  
  format = ISOFormat$new()
  format$setVersion(NA)
  if(!is.null(mime)){
    if(nrow(mime)>0){
      format$setName(ISOAnchor$new(name = mimetype, href = mime$uri))
      if(!is.na(mime$rfc)){
        format$setSpecification(ISOAnchor$new(name = toupper(mime$rfc), href = mime$rfc_uri))
      }
    }else{
      format$setName(mimetype)
    }
  }else{
    format$setName(mimetype)
  }
  return(format)
}
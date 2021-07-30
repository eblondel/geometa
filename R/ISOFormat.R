#' ISOFormat
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO format
#' @return Object of \code{\link{R6Class}} for modelling an ISOFormat
#' @format \code{\link{R6Class}} object.
#'
#' @field name [\code{\link{character}}] format name
#' @field version [\code{\link{character}}] format version
#' @field amendmentNumber [\code{\link{character}}] format amendment number
#' @field specification [\code{\link{character}}] format specification
#' @field fileDecompressionTechnique [\code{\link{character}}] technique for file decompression
#' @field formatDistributor [\code{\link{ISODistributor}}] format distributor
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFormat
#'  }
#'  \item{\code{setName(name, locales)}}{
#'    Sets the format name. Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setVersion(version)}}{
#'    Sets the format version
#'  }
#'  \item{\code{setAmendmentNumber(amendmentNumber)}}{
#'    Sets an admenment number
#'  }
#'  \item{\code{setSpecification(specification, locales)}}{
#'    Sets the format specification. Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setFileDecompressionTechnique(technique)}}{
#'    Sets the file decompression technique
#'  }
#'  \item{\code{addDistributor(distributor)}}{
#'    Adds a distributor, object of class \code{ISODistributor}
#'  }
#'  \item{\code{delDistributor(distributor)}}{
#'    Deletes a distributor, object of class \code{ISODistributor}
#'  }
#' }
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
      
      #+ name : CharacterString
      name = NULL,
      #+ version : CharacterString
      version = NULL,
      #+ amendmentNumber [0..1] : CharacterString
      amendmentNumber = NULL,
      #+ specification [0..1] : CharacterString
      specification = NULL,
      #+ fileDecompressionTechnique [0..1] : CharacterString
      fileDecompressionTechnique = NULL,
      #+ FormatDistributor [0..*]: ISODistributor
      FormatDistributor = list(),
      
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
     
      #setName
      setName = function(name, locales = NULL){
        self$name <- name
        if(!is.null(locales)){
          self$name <- self$createLocalisedProperty(name, locales)
        }
      },
      
      #setVersion
      setVersion = function(version){
        self$version <- as.character(version)
      },
      
      #setAmendmentNumber
      setAmendmentNumber = function(amendmentNumber){
        self$amendmentNumber <- as.character(amendmentNumber)
      },
      
      #setSpecification
      setSpecification = function(specification, locales = NULL){
        self$specification <- specification
        if(!is.null(locales)){
          self$specification <- self$createLocalisedProperty(specification, locales)
        }
      },
      
      #setFileDecompressionTechnique
      setFileDecompressionTechnique = function(technique){
        self$fileDecompressionTechnique = as.character(technique)
      },
      
      #addDistributor
      addDistributor = function(distributor){
        if(!is(distributor, "ISODistributor")){
          stop("The argument value should an object of class 'ISODistributor")
        }
        return(self$addListElement("FormatDistributor", distributor))
      },
      
      #delDistributor
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
#' ISOFormat
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO format
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOFormat
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'  md <- ISOFormat$new()
#'  md$setName("name")
#'  md$setVersion("1.0")
#'  md$setAmendmentNumber("2")
#'  md$setSpecification("specification")
#'  
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_Format}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrd/1.0/mrd/#element_MD_Format} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFormat <- R6Class("ISOFormat",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "MD_Format",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MRD"
      )
    ),
    public = list(
      
      #'@field name name : CharacterString (ISO 19139)
      name = NULL,
      #'@field formatSpecificationCitation format specification citation [1]: ISOCitation
      formatSpecificationCitation = NULL,
      #'@field version version : CharacterString (ISO 19139)
      version = NULL,
      #'@field amendmentNumber amendmentNumber [0..1] : CharacterString
      amendmentNumber = NULL,
      #'@field specification specification [0..1] : CharacterString (ISO 19139)
      specification = NULL,
      #'@field fileDecompressionTechnique fileDecompressionTechnique [0..1] : CharacterString
      fileDecompressionTechnique = NULL,
      #'@field medium medium [0..*] : ISOMedium [0..*] (ISO 19115-3)
      medium = list(),
      #'@field formatDistributor formatDistributor [0..*]: ISODistributor
      formatDistributor = list(),
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
     
      #'@description Set name
      #'@param name name
      #'@param locales list of localized names. Default is \code{NULL}
      setName = function(name, locales = NULL){
        self$stopIfMetadataStandardIsNot("19139")
        self$name <- name
        if(!is.null(locales)){
          self$name <- self$createLocalisedProperty(name, locales)
        }
      },
      
      #'@description Set format specification citation
      #'@param citation citation
      setFormatSpecificationCitation = function(citation){
        self$stopIfMetadataStandardIsNot("19115-3")
        if(!is(citation, "ISOCitation")){
          stop("The argument value should be an object of class 'ISOCitation'")
        }
        self$formatSpecificationCitation = citation
      },
      
      #'@description Set version
      #'@param version version
      setVersion = function(version){
        self$stopIfMetadataStandardIsNot("19139")
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
        self$stopIfMetadataStandardIsNot("19139")
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
        return(self$addListElement("formatDistributor", distributor))
      },
      
      #'@description Adds medium
      #'@param medium object of class \link{ISOMedium}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addMedium = function(medium){
        self$stopIfMetadataStandardIsNot("19115-3")
        if(!is(medium, "ISOMedium")){
          stop("The argument value should be an object of class 'ISOMedium'")
        }
        return(self$addListElement("medium", medium))
      },
      
      #'@description Deletes medium
      #'@param medium object of class \link{ISOMedium}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delMedium = function(medium){
        self$stopIfMetadataStandardIsNot("19115-3")
        if(!is(medium, "ISOMedium")){
          stop("The argument value should be an object of class 'ISOMedium'")
        }
        return(self$delListElement("medium", medium))
      },
      
      #'@description Deletes distributor
      #'@param distributor object of class \link{ISODistributor}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delDistributor = function(distributor){
        if(!is(distributor, "ISODistributor")){
          stop("The argument value should an object of class 'ISODistributor")
        }
        return(self$delListElement("formatDistributor", distributor))
      }
    )                        
)

ISOFormat$buildFrom = function(mimetype){
  
  mimetypes = getIANAMimeTypes()
  mime = mimetypes[mimetypes$name == mimetype,]
  
  format = ISOFormat$new()
  if(getMetadataStandard()=="19139") format$setVersion(NA)
  if(!is.null(mime)){
    if(nrow(mime)>0){
      switch(getMetadataStandard(),
        "19139" = {
          format$setName(ISOAnchor$new(name = mimetype, href = mime$uri))
          if(!is.na(mime$rfc)){
            format$setSpecification(ISOAnchor$new(name = toupper(mime$rfc), href = mime$rfc_uri))
          }
        },
        "19115-3" = {
          cit = ISOCitation$new()
          cit$setTitle(ISOAnchor$new(name = mimetype, href = mime$uri))
          if(!is.na(mime$rfc)){
            cit$addAlternateTitle(ISOAnchor$new(name = toupper(mime$rfc), href = mime$rfc_uri))
          }
          format$setFormatSpecificationCitation(cit)
        }
      )
    }else{
      switch(getMetadataStandard(),
       "19139" = {
         format$setName(name = mimetype)
       },
       "19115-3" = {
         cit = ISOCitation$new()
         cit$setTitle(mimetype)
         format$setFormatSpecificationCitation(cit)
       }
      )
    }
  }else{
    switch(getMetadataStandard(),
       "19139" = {
         format$setName(name = mimetype)
       },
       "19115-3" = {
         cit = ISOCitation$new()
         cit$setTitle(mimetype)
         format$setFormatSpecificationCitation(cit)
       }
    )
  }
  return(format)
}

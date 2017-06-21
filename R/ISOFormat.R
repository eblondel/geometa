#' ISOFormat
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO format
#' @return Object of \code{\link{R6Class}} for modelling an ISOFormat
#' @format \code{\link{R6Class}} object.
#'
#' @field name
#' @field version
#' @field amendmentNumber
#' @field specification
#' @field fileDecompressionTechnique
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFormat
#'  }
#'  \item{\code{setName(name)}}{
#'    Sets the format name
#'  }
#'  \item{\code{setVersion(version)}}{
#'    Sets the format version
#'  }
#'  \item{\code{setAmendmentNumber(amendmentNumber)}}{
#'    Sets an admenment number
#'  }
#'  \item{\code{setSpecification(specification)}}{
#'    Sets the format specification
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
      setName = function(name){
        self$name <- as.character(name)
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
      setSpecification = function(specification){
        self$specification <- as.character(specification)
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
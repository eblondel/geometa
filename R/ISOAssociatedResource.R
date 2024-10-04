#' ISOAssociatedResource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO associated resource
#' @return Object of \code{\link{R6Class}} for modelling an ISO associated resource
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_MD_AssociatedResource} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAssociatedResource <- R6Class("ISOAssociatedResource",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_AssociatedResource",
    xmlNamespacePrefix = list(
      "19115-3" = "MRI"
    )
  ),
  public = list(
    
    #'@field name name [0..1]: ISOAbstractCitation
    name = NULL,
    #'@field associationType associationType [1..1]: ISOAssociationType
    associationType = NULL,
    #'@field initiativeType initiativeType [0..1]: ISOInitiativeType
    initiativeType = NULL,
    #'@field metadataReference metadataReference [0..1]: ISOAbstractCitation
    metadataReference = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set name
    #'@param name name object of class \link{ISOAbstractCitation}
    setName = function(name){
      if(!is(name, "ISOAbstractCitation")){
        stop("The argument should be an object of class 'ISOAbstractCitation'")
      }
      self$name = name
    },
    
    #'@description Set association type
    #'@param associationType associationType object of class \link{ISOAssociationType} or
    #'any value among values listed by \code{ISOAssociationType$values()}
    setAssociationType = function(associationType){
      if(!is(associationType, "ISOAssociationType")){
        if(is(associationType, "character")){
          associationType = ISOAssociationType$new(value = associationType)
        }else{
          stop("The argument should be an object of class 'ISOAssociationType' or 'character'")
        }
      }
      self$associationType = associationType
    },
    
    #'@description Set initiative type
    #'@param initiativeType initiativeType object of class \link{ISOInitiativeType} or
    #'any value among values listed by \code{ISOInitiativeType$values()}
    setInitiativeType = function(initiativeType){
      if(!is(initiativeType, "ISOInitiativeType")){
        if(is(initiativeType, "character")){
          initiativeType = ISOInitiativeType$new(value = initiativeType)
        }else{
          stop("The argument should be an object of class 'ISOInitiativeType' or 'character'")
        }
      }
      self$initiativeType = initiativeType
    },
    
    #'@description Set metadata reference
    #'@param metadataReference metadataReference object of class \link{ISOAbstractCitation}
    setMetadatReference = function(metadataReference){
      if(!is(metadataReference, "ISOAbstractCitation")){
        stop("The argument should be an object of class 'ISOAbstractCitation")
      }
      self$metadataReference = metadataReference
    }
  )                                          
)
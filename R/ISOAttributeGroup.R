#' ISOAttributeGroup
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO attribute group
#' @return Object of \code{\link[R6]{R6Class}} for modelling a ISO attribute group
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_AttributeGroup}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAttributeGroup <- R6Class("ISOAttributeGroup",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_AttributeGroup",
     xmlNamespacePrefix = list(
       "19115-3" = "MRC"
     )
   ),
   public = list(
     
     #'@field contentType contentType [1..*] : ISOCoverageContentType
     contentType = list(),
     #'@field attribute attribute [0..*] : ISORangeDimension
     attribute = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}    
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds coverage content type
     #'@param contentType contentType object of class \link{ISOCoverageContentType} or any value among
     #'values listed in \code{ISOCoverageContentType$values()}
     #'@return \code{TRUE} if added, \code{FALSe} otherwise
     addContentType = function(contentType){
       if(!is(contentType, "ISOCoverageContentType")){
          if(is(contentType, "character")){
            contentType = ISOCoverageContentType$new(value = contentType)
          }else{
            stop("The argument should be an object of class 'ISOCoverageContentType' or 'character'")
          }
       }
       return(self$addListElement("contentType", contentType))
     },
     
     #'@description Deletes coverage content type
     #'@param contentType contentType object of class \link{ISOCoverageContentType} or any value among
     #'values listed in \code{ISOCoverageContentType$values()}
     #'@return \code{TRUE} if deleted, \code{FALSe} otherwise
     delContentType = function(contentType){
       if(!is(contentType, "ISOCoverageContentType")){
         if(is(contentType, "character")){
           contentType = ISOCoverageContentType$new(value = contentType)
         }else{
           stop("The argument should be an object of class 'ISOCoverageContentType' or 'character'")
         }
       }
       return(self$delListElement("contentType", contentType))
     },
     
     #'@description Adds attribute
     #'@param attribute object of class \link{ISORangeDimension}
     #'@return \code{TRUE} if added, \code{FALSe} otherwise
     addAttribute = function(attribute){
       if(!is(attribute, "ISORangeDimension")){
         stop("The argument should be an object of class 'ISORangeDimension'")
       }
       return(self$addListElement("attribute", attribute))
     },
     
     #'@description Deletes attribute
     #'@param attribute object of class \link{ISORangeDimension}
     #'@return \code{TRUE} if deleted, \code{FALSe} otherwise
     delAttribute = function(attribute){
       if(!is(attribute, "ISORangeDimension")){
         stop("The argument should be an object of class 'ISORangeDimension'")
       }
       return(self$delListElement("attribute", attribute))
     }
   )                        
)

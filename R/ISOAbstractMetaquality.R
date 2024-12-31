#' ISOAbstractMetaquality
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract meta quality
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract meta quality
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_AbstractDQ_Metaquality}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractMetaquality <- R6Class("ISOAbstractMetaquality",
   inherit = ISODataQualityAbstractElement,
   private = list(
     xmlElement = "AbstractDQ_Metaquality",
     xmlNamespacePrefix = list(
       "19115-3" = "MDQ"
     )
   ),
   public = list(
     
     #'@field relatedElement relatedElement [0..*]: ISODataQualityAbstractElement
     relatedElement = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds related element
     #'@param element object of class \link{ISODataQualityAbstractElement}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addRelatedElement = function(element){
       if(!is(element, "ISODataQualityAbstractElement")){
         stop("The argument value should be an object of class 'ISODataQualityAbstractElement'")
       }
       return(self$addListElement("relatedElement", element))
     },
     
     #'@description Deletes related element
     #'@param element object of class \link{ISODataQualityAbstractElement}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delRelatedElement = function(element){
       if(!is(element, "ISODataQualityAbstractElement")){
         stop("The argument value should be an object of class 'ISODataQualityAbstractElement'")
       }
       return(self$delListElement("relatedElement", element))
     }
     
   )                        
)

#' ISOConfidence
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO confidence
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO confidence
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_Confidence}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOConfidence <- R6Class("ISOConfidence",
  inherit = ISOAbstractMetaquality,
  private = list(
    xmlElement = "DQ_Confidence",
    xmlNamespacePrefix = list(
      "19115-3" = "MDQ"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )
)

#' ISOHomogeneity
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO homogeneity
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO homogeneity
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_Homogeneity}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOHomogeneity <- R6Class("ISOHomogeneity",
   inherit = ISOAbstractMetaquality,
   private = list(
     xmlElement = "DQ_Homogeneity",
     xmlNamespacePrefix = list(
       "19115-3" = "MDQ"
     )
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )
)

#' ISORepresentativity
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO representativity
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO representativity
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_Representativity}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORepresentativity <- R6Class("ISORepresentativity",
  inherit = ISOAbstractMetaquality,
  private = list(
    xmlElement = "DQ_Representativity",
    xmlNamespacePrefix = list(
      "19115-3" = "MDQ"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )
)

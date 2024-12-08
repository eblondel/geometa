#' ISOAnchor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO anchor
#' @return Object of \code{\link{R6Class}} for modelling an ISO Anchor
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   md <- ISOAnchor$new(name = "some entity name", href = "someentityuri")
#'   xml <- md$encode()
#' 
#' @references
#'  - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmx/1.0/gmx/#element_Anchor}
#'  
#'  - ISO 19115-3 \link{https://schemas.isotc211.org/19115/-3/gcx/1.0/gcx/#element_Anchor}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAnchor <- R6Class("ISOAnchor",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Anchor",
     xmlNamespacePrefix = list(
       "19139" = "GMX",
       "19115-3" = "GCX"
     )
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param name name
     #'@param ... attributes for XML encoding
     initialize = function(xml = NULL, name = NULL, ...){
       super$initialize(xml = xml)
       if(!is.null(name)){
         simpleAttrs <- list(...)
         xlinkAttrs <- c("href","role","arcrole","title","show","actuate")
         assert.xlinkAttr <- names(simpleAttrs) %in% xlinkAttrs
         if(length(which(!assert.xlinkAttr))>0){
           stop(sprintf("Attributes [%s] are not xlink attributes!",
                        paste(names(simpleAttrs)[which(!assert.xlinkAttr)], collapse=",")))
         }
         names(simpleAttrs) <- paste("xlink", names(simpleAttrs), sep=":")
         self$attrs <- simpleAttrs
         self$value <- name
       }
     }
   )                        
)
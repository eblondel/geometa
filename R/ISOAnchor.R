#' ISOAnchor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO anchor
#' @return Object of \code{\link{R6Class}} for modelling an ISO Anchor
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, name, ...)}}{
#'    This method is used to instantiate an ISOAnchor
#'  }
#' }
#' 
#' @references
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAnchor <- R6Class("ISOAnchor",
                           inherit = ISOAbstractObject,
                           private = list(
                             xmlElement = "Anchor",
                             xmlNamespacePrefix = "GMX"
                           ),
                           public = list(
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
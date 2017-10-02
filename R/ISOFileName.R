#' ISOFileName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO file name
#' @return Object of \code{\link{R6Class}} for modelling an ISO FileName
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, file, name)}}{
#'    This method is used to instantiate an ISOFileName
#'  }
#' }
#' 
#' @references
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFileName <- R6Class("ISOFileName",
                        inherit = ISOAbstractObject,
                        private = list(
                          xmlElement = "FileName",
                          xmlNamespacePrefix = "GMX"
                        ),
                        public = list(
                          attrs = list(),
                          initialize = function(xml = NULL, file = NULL, name = NULL){
                            super$initialize(xml = xml)
                            if(!is.null(file) & !is.null(name)){
                              self$attrs$src <- file
                              self$value <- name
                            }
                          }
                        )                        
)
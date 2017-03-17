#' ISOTopologyLevel
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO topology level charset
#' @return Object of \code{\link{R6Class}} for modelling an ISO TopologyLevel
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOTopologyLevel
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTopologyLevel <- R6Class("ISOTopologyLevel",
   inherit = ISOMetadataCodelistElement,
   public = list(
     initialize = function(xml = NULL, value){
       if(!is.null(xml)){
         self$decode(xml)
       }else{
         super$initialize(id = "MD_TopologyLevelCode", value = value, setValue = FALSE)
       }
     }
   )                        
)

ISOTopologyLevel$values <- function(){
  return(getISOCodelist("MD_TopologyLevelCode")$entries$value)
}
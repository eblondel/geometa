#' ISOImageryGeorectified
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO image georectified
#' @return Object of \code{\link{R6Class}} for modelling an ISO image Georectified
#' @format \code{\link{R6Class}} object.
#'
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata -- Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryGeorectified <- R6Class("ISOImageryGeorectified",
   inherit = ISOGeorectified,
   private = list(
     xmlElement = "MI_Georectified",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
    
     #'@field checkPoint checkPoint [0..*]: ISOImageryGCP
     checkPoint = list(),
     
     #'@description Initializes object 
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },

     #'@description Adds check point
     #'@param sfg simple feature object from \pkg{sf}
     #'@param m object of class \link{matrix}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addCheckPoint = function(sfg = NULL, m = NULL){
       gcp<- ISOImageryGCP$new()
       gcp$setGeographicCoordinates(sfg = sfg, m = m)
       return(self$addListElement("checkPoint", gcp))
     },
     
     #'@description Deletes check point
     #'@param sfg simple feature object from \pkg{sf}
     #'@param m object of class \link{matrix}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delCheckPoint = function(sfg = NULL, m = NULL){
       gcp<- ISOImageryGCP$new()
       gcp$setGeographicCoordinates(sfg = sfg, m = m)
       return(self$delListElement("checkPoint", gcp))
     }
     
   )                        
)
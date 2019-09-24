#' ISOImageryGeorectified
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO image georectified
#' @return Object of \code{\link{R6Class}} for modelling an ISO image Georectified
#' @format \code{\link{R6Class}} object.
#'
#' @field checkPoint [\code{list} of \code{\link{ISOImageryGCP}}]
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryGeorectified}}
#'  }
#'  \item{\code{addCheckPoint(sfg,m)}}{
#'    Adds a check point, either an object of class 'sfg' (from \pkg{sf}) or a 'matrix'
#'  }
#'  \item{\code{delCheckPoint(sfg,m)}}{
#'    Deletes a check point, either an object of class 'sfg' (from \pkg{sf}) or a 'matrix'
#'  }
#' }
#' 
#' @section Methods inherited from \code{\link{ISOGridSpatialRepresentation}}:
#' See \code{\link{ISOGridSpatialRepresentation}}
#'
#' @section Methods inherited from \code{\link{ISOGeorectified}}:
#' See \code{\link{ISOGeorectified}}
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
    
     #checkPoint [0..*]: ISOImageryGCP
     checkPoint = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },

     #addCheckPoint
     addCheckPoint = function(sfg = NULL, m = NULL){
       gcp<- ISOImageryGCP$new()
       gcp$setGeographicCoordinates(sfg = sfg, m = m)
       return(self$addListElement("checkPoint", gcp))
     },
     
     #delCheckPoint
     delCheckPoint = function(sfg = NULL, m = NULL){
       gcp<- ISOImageryGCP$new()
       gcp$setGeographicCoordinates(sfg = sfg, m = m)
       return(self$delListElement("checkPoint", gcp))
     }
     
   )                        
)
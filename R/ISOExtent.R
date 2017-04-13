#' ISOExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO Extent
#' @format \code{\link{R6Class}} object.
#'
#' @field geographicElement
#' @field temporalElement
#' @field verticalElement
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOExtent
#'  }
#'  \item{\code{addGeographicElement(extent)}}{
#'    Adds an object extending \code{ISOGeographicExtent}
#'  }
#'  \item{\code{setGeographicElement(extent)}}{
#'    Sets an object extending \code{ISOGeographicExtent}
#'  }
#'  \item{\code{delGeographicElement(extent)}}{
#'    Deletes an object extending \code{ISOGeographicExtent}
#'  }
#'  \item{\code{addTemporalElement(extent)}}{
#'    Adds an object extending \code{ISOTemporalExtent}
#'  }
#'  \item{\code{setTemporalElement(extent)}}{
#'    Sets an object extending \code{ISOTemporalExtent}
#'  }
#'  \item{\code{delTemporalElement(extent)}}{
#'    Deletes an object extending \code{ISOTemporalExtent}
#'  }
#'  \item{\code{addVerticalElement(extent)}}{
#'    Adds an object extending \code{ISOVerticalExtent}
#'  }
#'  \item{\code{setVerticalElement(extent)}}{
#'    Sets an object extending \code{ISOVerticalExtent}
#'  }
#'  \item{\code{delVerticalElement(extent)}}{
#'    Deletes an object extending \code{ISOVerticalExtent}
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOExtent <- R6Class("ISOExtent",
   inherit = ISOMetadataElement,
   private = list(
      xmlElement = "EX_Extent",
      xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #+ geographicElement [0..*]: ISOGeographicExtent
     geographicElement = list(),
     #+ temporalElement [0..*]: ISOTemporalExtent
     temporalElement = list(), #TODO
     #+ verticialElement [0..*]: ISOVerticalElement
     verticalElement = list(), #TODO
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #addGeographicElement
     addGeographicElement = function(element){
       if(!is(element, "ISOGeographicExtent")){
         stop("The argument should extend 'ISOGeographicExtent' object")
       }
       return(self$addListElement("geographicElement", element))
     },
     
     #setGeographicElement
     setGeographicElement = function(element){
       self$geographicElement = list()
       return(self$addGeographicElement(element))
     }, 
     
     #delGeographicElement
     delGeographicElement = function(element){
       if(!is(element, "ISOGeographicExtent")){
         stop("The argument should extend 'ISOGeographicExtent' object")
       }
       return(self$delListElement("geographicElement", element))
     },
     
     #addTemporalElement
     addTemporalElement = function(element){
       if(!is(element, "ISOTemporalExtent")){
         stop("The argument should extend 'ISOTemporalExtent' object")
       }
       return(self$addListElement("temporalElement", element))
     },
     
     #setTemporalElement
     setTemporalElement = function(element){
       self$temporalElement = list()
       return(self$addTemporalElement(element))
     },
     
     #delTemporalElement
     delTemporalElement = function(element){
       if(!is(element, "ISOTemporalExtent")){
         stop("The argument should extend 'ISOTemporalExtent' object")
       }
       return(self$delListElement("temporalElement", element))
     },
     
     #addVerticalElement
     addVerticalElement = function(element){
       stop("Method not yet supported by geometa!")
       if(!is(element, "ISOVerticalExtent")){
         stop("The argument should extend 'ISOVerticalExtent' object")
       }
       return(self$addListElement("verticalElement", element))
     },
     
     #setVerticalElement
     setVerticalElement = function(element){
       stop("Method not yet supported by geometa!")
       sself$verticalElement = list()
       return(self$addVerticalElement(element))
     },
     
     #delVerticalElement
     delVerticalElement = function(element){
       stop("Method not yet supported by geometa!")
       if(!is(element, "ISOVerticalExtent")){
         stop("The argument should extend 'ISOVerticalExtent' object")
       }
       return(self$delListElement("verticalElement", element))
     }
   )                        
)
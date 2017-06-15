#' GMLTimePeriod
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO time period
#' @return Object of \code{\link{R6Class}} for modelling an GMLTimePeriod
#' @format \code{\link{R6Class}} object.
#'
#' @field beginPosition
#' @field endPosition
#' @field duration
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, beginPosition, endPosition)}}{
#'    This method is used to instantiate an GMLTimePeriod
#'  }
#'  \item{\code{setBeginPosition(beginPosition)}}{
#'    Sets the begin position (beginning date or date and time of the resource 
#'    contents), as object of class "POSIXct"/"POSIXt" or "Date"
#'  }
#'  \item{\code{setEndPosition(endPosition)}}{
#'    Sets the end position (ending date or date and time of the resource 
#'    contents), as object of class "POSIXct"/"POSIXt" or "Date"
#'  }
#'  \item{\code{computeInterval()}}{
#'   Computes the ISO interval string and set as GML id
#'  }
#'  \item{\code{setId(id)}}{
#'  Sets the GML id string. 
#'  }
#'  \item{\code{setDuration(years, months, days, hours, mins, secs)}}{
#'    Set duration (Length of time between measurements)
#'  }
#' }
#' 
#' @examples 
#'   start <- ISOdate(2000, 1, 12, 12, 59, 45)
#'   end <- ISOdate(2010, 8, 22, 13, 12, 43)
#'   md <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
#'   xml <- md$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLTimePeriod <- R6Class("GMLTimePeriod",
  inherit = GMLTemporalPrimitive,
  private = list(
    xmlElement = "TimePeriod",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    attrs = list("gml:id" = NA),
    #+ beginPosition [1]: 'POSIXct','POSIXt'
    beginPosition = NULL,
    #+ endPosition [1]: 'POSIXct','POSIXt'
    endPosition = NULL,
    #+ duration [0..1]: character
    duration = NULL,
    initialize = function(xml = NULL, beginPosition, endPosition){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
      if(is.null(xml)){
        self$setBeginPosition(beginPosition)
        self$setEndPosition(endPosition)
      }else{
        gmlId <- XML::xmlGetAttr(xml, "gml:id")
        if(!is.null(gmlId)){
          self$attrs[["gml:id"]] <- gmlId
        }else{
          if(!is.null(self$beginPosition) & !is.null(self$endPosition)){
            self$computeInterval()
          }
        }
      }
    },
    
    #setBeginPosition
    setBeginPosition = function(beginPosition){
      if(!all(class(beginPosition)==c("POSIXct","POSIXt")) | is(beginPosition, "Date")){
        stop("Value should be of class ('POSIXct','POSIXt') or 'Date'")
      }
      self$beginPosition <- beginPosition
      if(!is.null(self$endPosition)) self$computeInterval()
    },
    
    #setEndPosition
    setEndPosition = function(endPosition){
      if(!all(class(endPosition)==c("POSIXct","POSIXt")) | is(endPosition, "Date")){
        stop("Value should be of class ('POSIXct','POSIXt') or 'Date'")
      }
      self$endPosition <- endPosition
      if(!is.null(self$beginPosition)) self$computeInterval()
    },
    
    #computeInterval
    computeInterval = function(){
      
      start <- self$beginPosition
      end <- self$endPosition
      years.seq <- seq(start, end, by = "years")
      years <- length(years.seq)-1
      months.start <- years.seq[length(years.seq)]
      months.seq <- seq(months.start, end, by = "months")
      months <- length(months.seq)-1
      days.start <- months.seq[length(months.seq)]
      days.seq <- seq(days.start, end, by = "DSTdays")
      days <- length(days.seq)-1
      hours.start <- days.seq[length(days.seq)]
      hours.seq <- seq(hours.start, end, by = "hours")
      hours <- length(hours.seq)-1
      mins.start <- hours.seq[length(hours.seq)]
      mins.seq <- seq(mins.start, end, by = "mins")
      mins <- length(mins.seq)-1
      secs.start <- mins.seq[length(mins.seq)]
      secs.seq <- seq(secs.start, end, by = "secs")
      secs <- length(secs.seq)-1
      isoduration <- GMLTimePeriod$computeISODuration(years, months, days, 
                                                      hours, mins, secs)           
      self$setId(isoduration)
    },
    
    #setId
    setId = function(id){
      self$attrs[["gml:id"]] <- as.character(id)
    },
    
    #setDuration
    setDuration = function(years = 0, months = 0, days = 0,
                           hours = 0, mins = 0, secs = 0){
      self$duration <- GMLTimePeriod$computeISODuration(years, months, days,
                                                        hours, mins, secs)  
    }
  )                        
)

GMLTimePeriod$computeISODuration = function(years = 0, months = 0, days = 0,
                                            hours = 0, mins = 0, secs = 0){
  duration <- "P"
  if(years>0) duration <- paste0(duration, years, "Y")
  if(months>0) duration <- paste0(duration, months, "M")
  if(days>0) duration <- paste0(duration, days, "D")
  if(hours>0 | hours>0 | mins>0) duration <- paste0(duration, "T")
  if(hours>0) duration <- paste0(duration, hours, "H")
  if(mins>0) duration <- paste0(duration, mins, "M")
  if(secs>0) duration <- paste0(duration, secs, "S")
  return(duration)
}

#' ISOTimePeriod
#' @export
#' @noRd
ISOTimePeriod <- R6Class("ISOTimePeriod",
   private = list(xmlElement = "TimePeriod", xmlNamespacePrefix = "GML"),
   public = list(initialize = function(xml = NULL, beginPosition, endPosition){
    stop("Use of 'ISOTimePeriod' is deprecated, use 'GMLTimePeriod' instead!") 
   })
)
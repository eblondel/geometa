#' GMLTimePeriod
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO time period
#' @return Object of \code{\link{R6Class}} for modelling an GMLTimePeriod
#' @format \code{\link{R6Class}} object.
#'
#' @field beginPosition [\code{\link{POSIXt}}]
#' @field endPosition [\code{\link{POSIXt}}]
#' @field duration [\code{\link{character}}]
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
  inherit = GMLAbstractTimeGeometricPrimitive,
  private = list(
    xmlElement = "TimePeriod",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    #+ beginPosition [1]: 'POSIXct','POSIXt'
    beginPosition = NULL,
    #+ endPosition [1]: 'POSIXct','POSIXt'
    endPosition = NULL,
    #+ duration [0..1]: character
    duration = NULL,
    initialize = function(xml = NULL, beginPosition = NULL, endPosition = NULL){
      super$initialize(xml = xml)
      if(is.null(xml)){
        if(!is.null(beginPosition)) self$setBeginPosition(beginPosition)
        if(!is.null(endPosition)) self$setEndPosition(endPosition)
      }
    },
    
    #setBeginPosition
    setBeginPosition = function(beginPosition){
      beginPos <- beginPosition
      if(is(beginPos,"numeric")) beginPos <- as(beginPos, "character")
      if(!(is(beginPos, "character") & nchar(beginPos) %in% c(4,7))){
        if(!all(class(beginPos)==c("POSIXct","POSIXt")) | is(beginPos, "Date")){
          stop("For a date, the value should be of class ('POSIXct','POSIXt') or 'Date'")
        }
      }
      self$beginPosition <- GMLElement$create("beginPosition", value = beginPos)
      if(!is.null(self$endPosition)) self$computeInterval()
    },
    
    #setEndPosition
    setEndPosition = function(endPosition){
      endPos <- endPosition
      if(is(endPos,"numeric")) endPos <- as(endPos, "character")
      if(!(is(endPos, "character") & nchar(endPos) %in% c(4,7))){
        if(!all(class(endPos)==c("POSIXct","POSIXt")) | is(endPos, "Date")){
          stop("Value should be of class ('POSIXct','POSIXt') or 'Date'")
        }
      }
      self$endPosition <- GMLElement$create("endPosition", value = endPos)
      if(!is.null(self$beginPosition)) self$computeInterval()
    },
    
    #computeInterval
    computeInterval = function(){
      
      if(self$beginPosition$value > self$endPosition$value){
        stop("GMLTimePeriod - 'beginPosition' should be before the 'endPosition'!")
      }
      
      start <- self$beginPosition$value
      end <- self$endPosition$value
      if(nchar(start)==4 && nchar(end)==4){
        years <- length(as.numeric(start):as.numeric(end))
        months <- 0; days <- 0; hours <- 0; mins <- 0; secs <- 0;
      }else if(nchar(start)==7 && nchar(end)==7){
        isLeapYear = function(year){ return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)) }
        startYear <- as.numeric(substr(start, 1, 4))
        startMonth <- as.numeric(substr(start,6,7))
        startDate <- as.Date(paste(start,"01",sep="-"))
        endYear <- as.numeric(substr(end, 1, 4))
        endMonth <- as.numeric(substr(end,6,7))
        endDate <- as.Date(paste(end,ifelse(isLeapYear(endYear)&&endMonth==2,"29","28"),sep="-"))
        years.seq <- seq(startDate, endDate, by = "years")
        years <- length(years.seq)-1
        months.start <- years.seq[length(years.seq)]
        months.seq <- seq(months.start, endDate, by = "months")
        months <- length(months.seq)-1
        days <- 0; hours <- 0; mins <- 0; secs <- 0;
      }else{
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
      }
      isoduration <- GMLTimePeriod$computeISODuration(years, months, days, 
                                                      hours, mins, secs)           
      self$setId(isoduration, addNS = TRUE)
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
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO time period
#' @return Object of \code{\link{R6Class}} for modelling an GMLTimePeriod
#' @format \code{\link{R6Class}} object.
#' 
#' @section Warning: Deprecated class, use \code{GMLTimePeriod} instead
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOTimePeriod <- R6Class("ISOTimePeriod",
   private = NULL,
   public = list(initialize = function(xml = NULL, beginPosition, endPosition){
    stop("Use of 'ISOTimePeriod' is deprecated, use 'GMLTimePeriod' instead!") 
   })
)
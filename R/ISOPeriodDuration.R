#' ISOPeriodDuration
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO period duration
#' @return Object of \code{\link{R6Class}} for modelling an ISO PeriodDuration
#' @format \code{\link{R6Class}} object.
#' 
#' @references
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_TM_PeriodDuration}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOPeriodDuration <- R6Class("ISOPeriodDuration",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "TM_PeriodDuration",
    xmlNamespacePrefix = list(
      "19115-3" = "GCO"
    )
  ),
  public = list(
    #'@field value value
    value = NA,
    
    #'@description Initializes a period duration
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    #'@param years years
    #'@param months months
    #'@param days days
    #'@param hours hours
    #'@param mins mins
    #'@param secs secs
    #'@param start start position
    #'@param end end position
    initialize = function(xml = NULL, value = NULL,
                          years = 0, months = 0, days = 0,
                          hours = 0, mins = 0, secs = 0,
                          start = NULL, end = NULL){
      super$initialize(xml = xml)
      if(is.null(xml)){
        if(!is.null(value)){
          self$value = value
        }else{
          self$setDuration(years = years, months = months, days = days,
                           hours = hours, mins = mins, secs = secs,
                           start = start, end = end)
        }
      }
    },
    
    #'@description Computes period duration
    #'@param years years
    #'@param months months
    #'@param days days
    #'@param hours hours
    #'@param mins mins
    #'@param secs secs
    #'@param start start position
    #'@param end end position
    setDuration = function(years = 0, months = 0, days = 0,
                           hours = 0, mins = 0, secs = 0,
                           start = NULL, end = NULL){
      
      if(!is.null(start) & !is.null(end)){
        if(is(start,"numeric")) start <- as(start, "character")
        if(!(is(start, "character") & nchar(start) %in% c(4,7))){
          if(!all(class(start)==c("POSIXct","POSIXt")) | is(start, "Date")){
            stop("For a date, the value should be of class ('POSIXct','POSIXt') or 'Date'")
          }
        }
        if(is(end,"numeric")) end <- as(end, "character")
        if(!(is(end, "character") & nchar(end) %in% c(4,7))){
          if(!all(class(end)==c("POSIXct","POSIXt")) | is(end, "Date")){
            stop("For a date, the value should be of class ('POSIXct','POSIXt') or 'Date'")
          }
        }
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
      }
      
      duration <- "P"
      if(years>0) duration <- paste0(duration, years, "Y")
      if(months>0) duration <- paste0(duration, months, "M")
      if(days>0) duration <- paste0(duration, days, "D")
      if(hours>0 | hours>0 | mins>0) duration <- paste0(duration, "T")
      if(hours>0) duration <- paste0(duration, hours, "H")
      if(mins>0) duration <- paste0(duration, mins, "M")
      if(secs>0) duration <- paste0(duration, secs, "S")
      self$value <- duration
    }
  )                        
)
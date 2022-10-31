#geometa options
#===============================================================================
#' @name getGeometaOption
#' @aliases getGeometaOption
#' @title getGeometaOption
#' @export
#' @description \code{getGeometaOption} allows to get an option from \pkg{geometa}
#' 
#' @usage getGeometaOption(option)
#' 
#' @param option the name of the option
#' @return the option
#' 
#' @examples             
#'   getGeometaOption("schemaBaseUrl")
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'    
getGeometaOption <- function(option){
  if(!(option %in% names(.geometa.options))){
    print(names(.geometa.options))
    stop(sprintf("'%s' is not a valid geometa option", option))
  }
  return(.geometa.options[[option]])
}

#'@name getGeometaOptions
#'@aliases getGeometaOptions
#'@title getGeometaOptions
#'@description \code{getGeometaOptions} allows to get options from \pkg{geometa}
#'@export
#' 
#' @usage getGeometaOptions()
#' 
#' @examples             
#'   getGeometaOptions()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'    
getGeometaOptions <- function(){
  return(.geometa.options)
}

#' @name setGeometaOption
#' @aliases setGeometaOption
#' @title setGeometaOption
#' @export
#' @description \code{setGeometaOption} allows to set an option from \pkg{geometa}
#' 
#' @usage setGeometaOption(option, value)
#' 
#' @param option the name of the option
#' @param value the value to set for the option
#' 
#' @examples             
#'   setGeometaOption("schemaBaseUrl", "http://somealternativeurl")
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
setGeometaOption <- function(option, value){
  opt <- getGeometaOption(option)
  if(missing(value)) stop(sprintf("New value not set for option '%s'", value))
  .geometa.options[[option]] <- value
  if(option == "languageUrl" & is.null(value)){
    .geometa.iso$codelists[sapply(.geometa.iso$codelists, function(x){x$identifier=="LanguageCode"})][[1]]$refFile <- "gmxCodelists.xml"
  }
}
#' @name readISO19139
#' @aliases readISO19139
#' @title readISO19139
#' @export
#' @description \code{readISO19139} is a function to read a ISO 19139 from a file
#' or url into an object in the \pkg{geometa} model.
#'
#' @usage readISO19139(file, url, raw)
#'                 
#' @param file a valid file path, as object of class \code{character}
#' @param url a valid URL, as object of class \code{character}
#' @param raw indicates if the function should return the raw XML. By
#' default this is set to \code{FALSE} and the function will try to map
#' the xml data to the \pkg{geometa} data model.
#' 
#' @return a \pkg{geometa} object inheriting \code{ISOAbstractObject}
#' 
#' @examples
#' \donttest{
#'   mdfile <- system.file("extdata/examples", "metadata.xml", package = "geometa")
#'   md <- readISO19139(mdfile)
#' }
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'    
readISO19139 <- function(file = NULL, url = NULL, raw = FALSE){
  if(is.null(file) & is.null(url)){
    stop("Please provide at least a metadata file or url")
  }
  encoding <- "UTF-8"
  raw_xml <- NULL
  if(!is.null(url)){
    req <- httr::GET(url)
    if(httr::status_code(req) != 200){
      stop("The URL resource is unavailable")
    }
    doc <- content(req, as = "text", encoding = encoding)
    raw_xml <- XML::xmlParse(doc, encoding = encoding)
  }else{
    raw_xml <- readLines(file, encoding = encoding)
    raw_xml <- paste0(raw_xml, collapse="") 
    if(Encoding(raw_xml) != "UTF-8") Encoding(raw_xml) <- "UTF-8"
    if(Encoding(raw_xml) == "unknown"){
      raw_xml <- XML::xmlParse(raw_xml, error = function (msg, ...) {})
    }else{
      raw_xml <- XML::xmlParse(raw_xml, encoding = Encoding(raw_xml), 
                               error = function (msg, ...) {})
    }
    
  }
  
  out <- NULL
  if(!is.null(raw_xml)){
    raw_xml <- as(raw_xml, "XMLInternalNode")
    iso_class <- ISOAbstractObject$getISOClassByNode(raw_xml)
    if(!is.null(iso_class)){
      out <- iso_class$new(xml = raw_xml)
    }
    if(raw){
      out <- raw_xml
    }
  }
  return(out)
}


#' @name readISO
#' @aliases readISO
#' @title readISO
#' @export
#' @description \code{readISO} is a function to read a ISO metadata from a file
#' or url into an object in the \pkg{geometa} model.
#'
#' @usage readISO(file, url, raw)
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
#'   md <- readISO(mdfile)
#' }
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'    
readISO <- function(file = NULL, url = NULL, raw = FALSE){
  
  cacheISOClasses()
  
  if(is.null(file) & is.null(url)){
    stop("Please provide at least a metadata file or url")
  }
  encoding <- "UTF-8"
  raw_xml <- NULL
  if(!is.null(url)){
    req <- httr::GET(url, httr::add_headers("Accept" = "application/xml"))
    if(httr::status_code(req) != 200){
      stop("The URL resource is unavailable")
    }
    doc <- content(req, as = "text", encoding = encoding)
    raw_xml <- XML::xmlParse(doc, encoding = encoding, addFinalizer = FALSE)
  }else{
    raw_xml <- readr::read_lines(file)
    raw_xml <- paste0(raw_xml, collapse="\n") 
    if(Encoding(raw_xml) != "UTF-8") Encoding(raw_xml) <- "UTF-8"
    if(Encoding(raw_xml) == "unknown"){
      raw_xml <- XML::xmlParse(raw_xml, error = function (msg, ...) {}, addFinalizer = FALSE)
    }else{
      raw_xml <- XML::xmlParse(raw_xml, encoding = Encoding(raw_xml), 
                               error = function (msg, ...) {}, addFinalizer = FALSE)
    }
  }
  
  out <- NULL
  if(!is.null(raw_xml)){
    raw_xml <- as(raw_xml, "XMLInternalNode")
    if(raw){
      out <- raw_xml
    }else{
      #inspect namespace
      version <- switch(XML::xmlNamespace(raw_xml),
                        "http://www.isotc211.org/2005/gmd" = "19139",
                        "http://standards.iso.org/iso/19115/-3/mdb/2.0" = "19115-3",
                        "http://www.isotc211.org/2005/gfc" = "19139",
                        "http://standards.iso.org/iso/19110/gfc/1.1" = "19115-3",
                        "19139"
      )
      setMetadataStandard(version)
      #instantiate object
      iso_class <- ISOAbstractObject$getISOClassByNode(raw_xml)
      if(!is.null(iso_class)){
        out <- iso_class$new(xml = raw_xml)
      }
    }
  }
  return(out)
}


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
  warnings("'readISO19139' is deprecated, consider using 'readISO' function")
  readISO(file = file, url = url, raw = raw)
}


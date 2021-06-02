#' setIANAMimeTypes
#' @export
setIANAMimeTypes <- function(){
  packageStartupMessage("Loading IANA mime types...")
  
  ianaUrl = "https://www.iana.org/assignments/media-types"
  ping <- try(httr::HEAD(ianaUrl), silent = TRUE)
  if(is(ping,"try-error")){
    packageStartupMessage("IANA website not reachable, skipping IANA mime types loading...")
    return(FALSE)
  }
  
  ianaNs = "http://www.iana.org/assignments"
  xml = xmlParse(httr::content(httr::GET(paste0(ianaUrl, "/media-types.xml")), "text")) 
  xml_records = getNodeSet(xml, "//ns:record", namespaces = c(ns = ianaNs))
  mimeTypes = do.call("rbind", lapply(xml_records,function(record){
      record_children = xmlChildren(record)
      
      name = xmlValue(record_children$name)
      file = xmlValue(record_children$file)
      if(!is.na(file)) name = file
      rfc = NA
      xrefs = record_children[names(record_children)=="xref"]
      rfc_xrefs = xrefs[sapply(xrefs, xmlGetAttr, "type")=="rfc"]
      if(length(rfc_xrefs)) rfc = xmlGetAttr(rfc_xrefs[[1]], "data")
      
      mimeType = data.frame(
        name = name,
        uri = sprintf("%s/%s", ianaUrl, name),
        rfc = rfc,
        rfc_uri = ifelse(!is.na(rfc), sprintf("https://tools.ietf.org/html/%s", rfc), NA),
        stringsAsFactors = FALSE
      )
      return(mimeType)
  }))
  .geometa.iana$mimetypes = mimeTypes
}

#' getIANAMimeTypes
#' @export
getIANAMimeTypes <- function(){
  return(.geometa.iana$mimetypes)
}
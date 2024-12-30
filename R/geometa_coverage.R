#overrides coercer from XML package
setAs("URI", "character",
      function(from) {
        if(from$scheme == ""){
          sprintf("%s%s%s",
                  from["path"],
                  if(from[["query"]] != "") sprintf("?%s", from[["query"]]) else "",
                  if(from[["fragment"]] != "") sprintf("#%s", from[["fragment"]]) else "" )
        }else{
          hasPort <- FALSE
          if(!is.na(from[["port"]])) if(from[["port"]] > 0) hasPort <- TRUE
          sprintf("%s://%s%s%s%s%s%s%s",
                  from[["scheme"]],
                  from[["user"]],
                  if(from[["user"]] != "") "@" else "",
                  from[["server"]],
                  if(hasPort) sprintf(":%d", as.integer(from[["port"]])) else "",
                  from["path"],
                  if(from[["query"]] != "") sprintf("?%s", from[["query"]]) else "",
                  if(from[["fragment"]] != "") sprintf("#%s", from[["fragment"]]) else ""                   
          )
        }
      })

#simplifyPath
simplifyPath <- function (path) 
{
  els = strsplit(path, "/")[[1]]
  GoOn = TRUE
  els = els[els != "."]
  while (GoOn && length(i <- which(els == ".."))) {
    i = min(i)
    if (length(i) == 1 && i == 1) 
      break
    if (all(els[seq(1, i)] == "..")) 
      break
    if (i == 2 && els[1] == "..") 
      break
    els = els[-c(i, i - 1L)]
  }
  paste(els, collapse = "/")
}

#get_schemalocation_url
get_schemalocation_url <- function(schemaLocation, baseURL){
  sep <- "/"; simplify <- TRUE;
  pu = XML::parseURI(schemaLocation)
  
  #XXX Need to strip the path in baseURL if pu$path starts with /
  if(pu$scheme == "") {
    b = XML::parseURI(baseURL)
    b$query = ""
    if(grepl("^/", pu$path)) {
      b$path = schemaLocation
      return(as(b, "character"))
    }
    
    endsWithSlash = grepl("/$", b$path)
    bdir <- if(endsWithSlash) b$path else dirname(b$path)
    sep <- if(endsWithSlash) "" else sep
    if(endsWithSlash && grepl("^\\./", schemaLocation))
      schemaLocation = substring(schemaLocation, 3)
    
    #handle ../ occurences
    parent_levels <- gregexpr("\\.\\./",schemaLocation)[[1]]
    parent_levels <- parent_levels[parent_levels!=-1]
    if(length(parent_levels)>0)
      for(i in 1:length(parent_levels)) bdir <- dirname(bdir)
    if(bdir=="/") bdir <- ""
    schemaLocation <- gsub("\\.\\./","",schemaLocation)  
    
    b$path = sprintf("%s%s%s", bdir, sep, schemaLocation)
    # handle .. in the path and try to collapse these.
    if(simplify && grepl("..", b$path, fixed = TRUE))
      b$path = simplifyPath(b$path)
    
    return(as(b, "character"))         
  } else
    schemaLocation
}

#read_schema
read_schema <- function(url, verbose = FALSE, namespaces = c(xs = "http://www.w3.org/2001/XMLSchema"), schemaCollector = new.env()){
  
  doc = xmlParse(url)
  baseURL = docName(doc)
  
  #imports
  imports = getNodeSet(doc, "//xs:schema/xs:import", namespaces)
  invisible(lapply(imports, function(import){
    #extract schema location
    schemaLocation <- xmlGetAttr(import, "schemaLocation")
    if(is.na(schemaLocation)) return(NULL)
    #get relative url
    schemaLocationUrl <- get_schemalocation_url(schemaLocation, baseURL)
    #parsing schema location
    schemaLocs <- schemaCollector$schemaLocations
    if(!is.null(schemaLocs)) if(length(schemaLocs[schemaLocs == schemaLocationUrl])>0) {
      if(verbose) cat(sprintf("Skip parsing of schemaLocationUrl '%s'. Already processed! \n", schemaLocationUrl))
    }else{
      if(verbose) cat(sprintf("Resolving '%s' schemaLocation into URL '%s' from '%s' \n", schemaLocation, schemaLocationUrl, baseURL))
      assign("schemaLocations", c(get("schemaLocations", schemaCollector), schemaLocationUrl), envir = schemaCollector)
      #read schema
      schemaDoc <- read_schema(schemaLocationUrl, verbose = verbose, namespaces = namespaces, schemaCollector)
    }
    
  }))
  
  
  #includes
  includes = getNodeSet(doc, "//xs:schema/xs:include", namespaces)
  invisible(lapply(includes, function(include){
    #extract schema location
    schemaLocation <- xmlGetAttr(include, "schemaLocation")
    if(is.na(schemaLocation)) return(NULL)
    #get relative url
    schemaLocationUrl <- get_schemalocation_url(schemaLocation, baseURL)
    #parsing schema location
    schemaLocs <- schemaCollector$schemaLocations
    if(!is.null(schemaLocs)) if(length(schemaLocs[schemaLocs == schemaLocationUrl])>0) {
      if(verbose) cat(sprintf("Skip parsing of schemaLocationUrl '%s'. Already processed! \n", schemaLocationUrl))
    }else{
      if(verbose) cat(sprintf("Resolving '%s' schemaLocation into URL '%s' from '%s' \n", schemaLocation, schemaLocationUrl, baseURL))
      schemaCollector$schemaLocations <<- c(schemaCollector$schemaLocations, schemaLocationUrl)
      assign("schemaLocations", c(get("schemaLocations", schemaCollector), schemaLocationUrl), envir = schemaCollector)
      #read schema
      schemaDoc <- read_schema(schemaLocationUrl, verbose = verbose, namespaces = namespaces, schemaCollector)
    }
    
  }))
  
  #extracting elements
  elements <- NULL
  els <- getNodeSet(doc, "//xs:element", namespaces)
  if(length(els)>0){
    elements <- do.call("rbind", lapply(els, function(el){
      out <- NULL
      name <- xmlGetAttr(el, "name")
      type <- xmlGetAttr(el, "type")
      if(is.null(type) | is.null(name)){
        return(NULL)
      }else{
        #if(endsWith(name,"Code")) name <- unlist(strsplit(name, "Code"))[1]
        ns <- unlist(strsplit(type, ":"))[1]
      }
      return(data.frame(namespace = ns, element = name, stringsAsFactors = FALSE))
    }))
    elements <- elements[!sapply(elements, is.null)]
    elements <- unique(elements)
  }
  
  
  #extracting complex types
  complexTypes <- NULL
  cts <- getNodeSet(doc, "//xs:complexType", namespaces)
  if(length(cts)>0){
    complexTypes <- do.call("rbind",lapply(cts, function(ct){
      name <- xmlGetAttr(ct,"name")
      if(is.null(name)) return(NULL)
      splitted <- FALSE
      if(endsWith(name,"_Type") & !splitted){
        name <- unlist(strsplit(name,"_Type"))[1]
        splitted <- TRUE
      }
      if(endsWith(name,"_PropertyType") & !splitted){
        name <- unlist(strsplit(name,"_PropertyType"))[1]
        splitted <- TRUE
      }
      if(endsWith(name, "Type") & !splitted){
        name <- unlist(strsplit(name,"Type"))[1]
        splitted <- TRUE
      }
      if(endsWith(name, "PropertyType") & !splitted){
        name <- unlist(strsplit(name,"PropertyType"))[1]
        splitted <- TRUE
      }
      namespace <- NA
      cts_els <- getNodeSet(xmlDoc(ct), "//xs:sequence/xs:element", namespaces)
      if(length(cts_els)>0){
        ref <- xmlGetAttr(cts_els[[1]],"ref")
        if(!is.null(ref)) namespace <- unlist(strsplit(ref,":"))[1]
      }
      return(data.frame(namespace = namespace, element = name, stringsAsFactors = FALSE))
    }))
    complexTypes <- unique(complexTypes)
  }
  
  #add to schema collectors complex elements
  if(!is.null(elements) & !is.null(complexTypes)){
    elements <- elements[elements$element %in% complexTypes$element,]
    if(nrow(elements)>0){
      elements <- do.call("rbind",lapply(1:nrow(elements), function(i){
        el <- elements[i,]
        complexType <- complexTypes[complexTypes$element == el$element,]
        complexType <- complexType[1L,]
        if(!is.na(complexType$namespace)) el$namespace <- complexType$namespace
        return(el)
      }))
      assign("elements", rbind(get("elements", schemaCollector), elements), envir = schemaCollector)
    }
  }
  
  return(doc)
  
}

#get_schema_elements
get_schema_elements <- function(url, verbose = FALSE, schemaCollector = new.env()){
  schemaCollector$schemaLocations <- list()
  schemaCollector$elements <- NULL
  read_schema(url, schemaCollector = schemaCollector, verbose = verbose)
  elements <- get("elements", schemaCollector)
  elements <- unique(elements)
  elements <- elements[with(elements, order(namespace, element)),]
  return(elements)
}

#' @name geometa_coverage
#' @aliases geometa_coverae
#' @title geometa_coverage
#' @export
#' @description \code{geometa_coverage} is a function to report coverage of ISO/OGC standard classes
#' in package \pkg{geometa}. The function will inspect all classes of the ISO/OGC standards and will
#' scan if \pkg{geometa} supports it.
#' 
#' @note This function is used as Quality Assurance indicator to assess the percentage of completeness
#' of ISO/OGC standards in \pkg{geometa}.
#'
#' @usage geometa_coverage(version)
#' 
#' @param version main metadata standard version
#' @return an object of class \code{data.frame}
#' 
#' @examples
#' \donttest{
#'   cov <- geometa_coverage(version = "19115-3")
#' }
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
geometa_coverage <- function(version = "19139"){
  setMetadataStandard(version = version)
  xsd_filepath = getISOMetadataSchemaFile(version = version)
  elements <- get_schema_elements(xsd_filepath)
  elements <- elements[elements$namespace %in% sapply(getISOMetadataNamespaces(), function(x){x$id}),]
  elements$namespace <- toupper(elements$namespace)
  classes <- ISOAbstractObject$getISOClasses(pretty = TRUE)
  colnames(classes)[colnames(classes)=="ns_prefix"] <- "namespace"
  elements <- merge(
    x = elements, y = classes,
    by = c("element", "namespace"), all.x = TRUE, all.y = FALSE
  )
  elements$in_geometa <- !sapply(elements$geometa_class, is.na)
  elements[is.na(elements$refactored),]$refactored = FALSE
  elements[is.na(elements$namespace),]$namespace <- toupper(elements[is.na(elements$namespace),"namespace"])
  elements <- elements[!(elements$namespace %in% c("XS","XLINK")),]
  std <- do.call("rbind",lapply(elements$namespace, ISOAbstractObject$getStandardByPrefix))
  elements$specification <- std$specification
  elements$schema <- std$schema
  elements$title <- std$title
  elements$ns_uri <- NULL
  elements <- elements[with(elements,order(specification, schema, namespace, element)),c("specification", "schema", "title", "namespace", "element", "geometa_class", "in_geometa", "refactored")]
  elements <- rbind(
    elements[startsWith(elements$specification, "ISO"),],
    elements[startsWith(elements$specification, "GML"),],
    elements[startsWith(elements$specification, "SWE"),]
  )
  elements <- elements[!is.na(elements$specification),]
  return(elements)
}

geometa_namespace_exports = function(){
  if(is.null(.geometa.iso$exports)){
    f <- base::system.file("NAMESPACE", package="geometa")
    objs <- readLines(f)
    exps <- objs[grepl("export", objs)]
    .geometa.iso$exports = sub("^export[^\\(]*\\(([^\\)]+)\\)", "\\1", exps)
  }
  return(.geometa.iso$exports)
}

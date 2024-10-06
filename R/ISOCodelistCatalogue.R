#' ISOCodelistCatalogue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO citation series
#' @return Object of \code{\link{R6Class}} for modelling an ISOCodelistCatalogue
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/cat/1.2.0/cat/#element_CT_CodelistCatalogue}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/cat/1.0/cat/#element_CT_CodelistCatalogue}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCodelistCatalogue <- R6Class("ISOCodelistCatalogue",
   inherit = ISOAbstractCatalogue,
   private = list(
     xmlElement = "CT_CodelistCatalogue",
     xmlNamespacePrefix = list(
       "19139" = "GMX",
       "19115-3" = "CAT"
     )
   ),
   public = list(
     #'@field codelistItem codelist items
     codelistItem = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param refFile ref file
     initialize = function(xml = NULL, refFile = NULL){
       if(!is.null(refFile)){
         xml = XML::xmlParse(refFile)
         xml <- methods::as(xml, "character")
         xml <- gsub("<!--.*?-->", "", xml)
         xml <- XML::xmlParse(xml, asText = TRUE) 
       }
       if(!is.null(xml)){
         super$initialize(xml = xml)
       }
     },
     
     #'@description Decodes and builds an \code{ISOCodelistCatalogue} from XML. This is done
     #'specifically for this class, without using the generic \code{ISOAbstractObject}
     #'decoder, to make it fully decodable on package load.
     #'@param xml object of class \link{XMLInternalNode-class}
     # decode = function(xml){
     #   message("custom decode")
     #   xml = XML::xmlRoot(xml)
     #   children = XML::xmlChildren(xml)
     #   self$codelistItem = lapply(children[names(children)=="codelistItem"], function(x){
     #     xml_dict = XML::xmlChildren(x)[[1]]
     #     the_dict = switch(xmlName(xml_dict),
     #       "CodeListDictionary" = {
     #         dict = ISOCodeListDictionary$new(xml = xml_dict)
     #         xml_dict_c = xmlChildren(xml_dict)
     #         dict$codeEntry = lapply(xml_dict_c[names(xml_dict_c)=="codeEntry"], function(y){
     #            xml_ce = XML::xmlChildren(y)[[1]]
     #            ISOCodeDefinition$new(xml = xml_ce)
     #         })
     #         dict
     #       },
     #       "CT_Codelist" = {
     #         cl = ISOCodelist$new(xml = xml_dict)
     #         xml_cl = xmlChildren(xml_dict)
     #         cl$identifier = ISOScopedName$new(xml = xmlChildren(xml_cl$name)[[1]])
     #         cl$codeEntry = lapply(xml_cl[names(xml_cl)=="codeEntry"], function(y){
     #           xml_clval = XML::xmlChildren(y)[[1]]
     #           clval = ISOCodelistValue$new(xml = xml_clval)
     #           xml_clval_c = xmlChildren(xml_clval)
     #           clval$identifier = ISOScopedName$new(xml = xmlChildren(xml_clval_c$name)[[1]])
     #         })
     #         cl
     #       }
     #     )
     #     return(the_dict)
     #   })
     # },
     
     #'@description Get codelists. The method ensure an harmonized output made of objects of
     #' class \link{ISOCodelist}. In the catalogue is built of objects of class \link{ISOCodeListDictionary}, these
     #' will be mapped as \link{ISOCodelist}, to facilitate the consumption of codelists by \pkg{geometa}
     getCodelists = function(){
        items <- lapply(self$codelistItem, function(item){
           the_item <- item
           if(is(item, "ISOCodeListDictionary")){
              the_item = item$toISOCodelist()
           }
           return(the_item)
        })
        return(items)
     },
     
     #'@description Get codelist by id.
     #'@param id id
     #'@return object of class \link{ISOCodelist}
     getCodelist = function(id){
        cls <- self$getCodelists()
        cls <- cls[sapply(cls, function(x){if(x$identifier$value == id) return(TRUE) else return(FALSE)})]
        cl <- NULL
        if(length(cls)>0) cl <- cls[[1]]
        return(cl)
     }
   )
)
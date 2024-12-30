#' GMLMultiPointCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML multipoint coverage
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML multipoint coverage
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLMultiPointCoverage <- R6Class("GMLMultiPointCoverage",
 inherit = GMLAbstractDiscreteCoverage,
 private = list(
   xmlElement = "MultiPointCoverage",
   xmlNamespacePrefix = "GML"
 ),
 public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param element element name
    #'@param attrs list of attributes
    #'@param defaults list of default values
    #'@param wrap wrap element? 
    initialize = function(xml = NULL, element = NULL, attrs = list(),
                         defaults = list(), wrap = TRUE){
     if(is.null(element)) element <- private$xmlElement
     super$initialize(xml, element = element, attrs = attrs,
                      defaults = defaults, wrap = wrap)
   }
 )
)

#' GMLMultiCurveCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML multicurve coverage
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML multicurve coverage
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLMultiCurveCoverage <- R6Class("GMLMultiCurveCoverage",
   inherit = GMLAbstractDiscreteCoverage,
   private = list(
     xmlElement = "MultiCurveCoverage",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param element element name
      #'@param attrs list of attributes
      #'@param defaults list of default values
      #'@param wrap wrap element?
      initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     }
   )
)

#' GMLMultiSurfaceCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML multisurface coverage
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML multisurface coverage
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLMultiSurfaceCoverage <- R6Class("GMLMultiSurfaceCoverage",
   inherit = GMLAbstractDiscreteCoverage,
   private = list(
     xmlElement = "MultiSurfaceCoverage",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param element element name
      #'@param attrs list of attributes
      #'@param defaults list of default values
      #'@param wrap wrap element?
      initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     }
   )
)

#' GMLMultiSolidCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML multisolid coverage
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML multisolid coverage
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLMultiSolidCoverage <- R6Class("GMLMultiSolidCoverage",
   inherit = GMLAbstractDiscreteCoverage,
   private = list(
     xmlElement = "MultiSolidCoverage",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param element element name
      #'@param attrs list of attributes
      #'@param defaults list of default values
      #'@param wrap wrap element?
      initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     }
   )
)

#' GMLGridCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML grid coverage
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML grid coverage
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLGridCoverage <- R6Class("GMLGridCoverage",
   inherit = GMLAbstractDiscreteCoverage,
   private = list(
     xmlElement = "GridCoverage",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param element element name
      #'@param attrs list of attributes
      #'@param defaults list of default values
      #'@param wrap wrap element?
      initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     }
   )
)

#' GMLRectifiedGridCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML rectified grid coverage
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML rectified grid coverage
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLRectifiedGridCoverage <- R6Class("GMLRectifiedGridCoverage",
   inherit = GMLAbstractDiscreteCoverage,
   private = list(
     xmlElement = "RectifiedGridCoverage",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param element element name
      #'@param attrs list of attributes
      #'@param defaults list of default values
      #'@param wrap wrap element?
      initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     }
   )
)
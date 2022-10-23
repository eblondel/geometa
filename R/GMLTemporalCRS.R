#' GMLTemporalCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML temporal crs
#' @return Object of \code{\link{R6Class}} for modelling an GMLTemporalCRS
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLTemporalCRS <- R6Class("GMLTemporalCRS",
    inherit = GMLAbstractSingleCRS,
    private = list(
      xmlElement = "TemporalCRS",
      xmlNamespacePrefix = "GML"
    ),
    public = list(
      #'@field timeCS time CS
      timeCS = NULL,
      #'@field temporalDatum temporal datum
      temporalDatum = NULL,
      
      #'@description Set time CS
      #'@param timeCS time CS, object of class \link{GMLTimeCS}
      setTimeCS = function(timeCS){
        if(!is(timeCS, "GMLTimeCS")){
          stop("The argument should be an object of class 'GMLTimeCS")
        }
        self$timeCS <- GMLElement$create("timeCS", timeCS)
      },
      
      #'@description Set temporal datum
      #'@param temporalDatum temporal datum
      setTemporalDatum = function(temporalDatum){
        if(!is(temporalDatum, "GMLTemporalDatum")){
          stop("The argument should be an object of class 'GMLTemporalDatum")
        }
        self$temporalDatum <- GMLElement$create("temporalDatum", temporalDatum)
      }
      
    )
)
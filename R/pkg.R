#' Mobile location algorithms and tools
#'
#' CDR and signalling data contain mobile phone events logged at specific cells (antennae). This package provides algorithms and tools to translate antennae properties to gespatial distributions. Depending on the availability of the antenna properties, such as coordinates, direction, and height, different algorithms can be used.
#'
#' @name mobloc-package
#' @aliases mobloc
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com}
#' @keywords Mobile phone data, location, Voronoi
NULL

#' Elevation data of Zuid-Limburg
#'
#' Elevation data of Zuid-Limburg, raster object of 100 x 100 meter tiles
#'
#' @usage data(ZL_elevation)
#' @name ZL_elevation
#' @docType data
NULL

#' Cellplan data for Zuid-Limburg
#'
#' Artificial cellplan data for Zuid-Limburg
#'
#' @usage data(ZL_cellplan)
#' @name ZL_cellplan
#' @docType data
NULL

#' Municipalities of Zuid-Limburg
#'
#' Municipalities of Zuid-Limburg. Source: CBS
#'
#' @usage data(ZL_muni)
#' @name ZL_muni
#' @docType data
NULL

#' Landuse of Zuid-Limburg
#'
#' Landuse groups. Source: OpenStreetMap
#'
#' @usage data(ZL_landuse)
#' @name ZL_landuse
#' @docType data
NULL


.MOBLOC_CACHE <- new.env(FALSE, parent=globalenv())

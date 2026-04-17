#' my.common.functions: Spatial Data Utilities and Wildlife Analysis Tools
#'
#' A collection of utility functions for exporting spatial data to themed KML/KMZ 
#' files and creating georeferenced PDF maps with basemaps. Supports point, line, 
#' and polygon geometries with customizable styling.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{EXPORT_SF_TO_THEMED_KML}}: Export sf objects to themed KML/KMZ files
#'   \item \code{\link{CREATE_BASEMAP_PLOT}}: Create ggplot maps with integrated basemaps
#'   \item \code{\link{EXPORT_GEOREFERENCED_PDF}}: Convert ggplot maps to georeferenced PDFs
#' }
#'
#' @section KML/KMZ Export:
#' The \code{EXPORT_SF_TO_THEMED_KML} function creates Google Earth compatible files
#' with professional styling including categorical or continuous color schemes,
#' customizable labels, and support for points, lines, and polygons.
#'
#' @section Georeferenced Maps:
#' The \code{CREATE_BASEMAP_PLOT} and \code{EXPORT_GEOREFERENCED_PDF} functions work
#' together to create publication-quality georeferenced PDF maps with basemaps from
#' ESRI, OpenStreetMap, and other providers.
#'
#' @docType package
#' @name my_common_functions-package
#' @aliases my_common_functions
#'
#' @author Bevan Ernst \email{Bevan.Ernst@@gov.bc.ca}
#'
#' @keywords spatial GIS KML maps
"_PACKAGE"

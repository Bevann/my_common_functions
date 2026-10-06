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
#' @name my.common.functions-package
#' @aliases my.common.functions
#'
#' @author Bevan Ernst
#'
#' @keywords spatial GIS KML maps
#'
#' @importFrom magrittr %>%
#' @export %>%
"_PACKAGE"

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling \code{rhs(lhs)}.
NULL

# Declare global variables used in NSE contexts to avoid R CMD check NOTEs
utils::globalVariables(c(
  ".",
  ".dates_clean",
  ".bio_year_start_date",
  "n", "d", "varN", "sdN", "N", "CI",
  "95% Lower", "95% Upper",
  "geometry", "geometry_lagged",
  "LOCAL_DATE_TIME_LAG",
  "Travel_Length_Km", "Travel_Time_H", "Travel_Speed_KmH",
  "RECNO",
  "sentence_1", "sentence_2", "sentence_3", "sentence_4", "sentence_5", "sentence_6",
  "sentence_10", "sentence", "lat_dir", "long_dir", "lat_dd", "long_dd", "rmc_date",
  "date_time_UTC", "time_zone", "DATE_TIME_LOCAL", "Date", "x", "y", "x_end", "y_end",
  "START_TIME_LOCAL", "END_TIME_LOCAL", "Duration_Minutes", "LENGTH_METERS",
  "origins"
))

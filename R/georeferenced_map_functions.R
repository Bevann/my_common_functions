# Functions for creating georeferenced PDF maps from R ggplot maps with basemaps
# Created: April 10, 2026


# Function 1: Create basemap plot with configurable extents------

#' Create Basemap Plot with Configurable Extents
#'
#' Creates a ggplot object with an integrated basemap from ESRI, OpenStreetMap, or
#' other tile providers. The function automatically handles extent expansion to ensure
#' basemap coverage and coordinate system transformation to Web Mercator (EPSG:3857).
#' Returns a ggplot object that can be enhanced with additional sf layers.
#'
#' @param largest_data An sf object defining the geographic extent of the map. The
#'   function calculates the bounding box from this object.
#' @param ext_expansion Numeric. Buffer proportion around data extent for the final
#'   map view (0.1 = 10% expansion on all sides). The basemap is fetched with 
#'   additional buffer to ensure complete coverage. Default: 0.1.
#' @param map_res Numeric. Basemap resolution/zoom level. Higher values = more detail
#'   but slower download and larger file size. Typical range: 1-10. Default: 2.
#' @param map_service Character. Basemap tile provider. Options include "esri", 
#'   "osm", "carto". See basemaps package documentation for full list. Default: "esri".
#' @param map_type Character. Basemap style from the selected service. For ESRI:
#'   "world_topo_map" (topographic), "world_imagery" (satellite), "world_street_map",
#'   "world_terrain_base" (relief/terrain). Default: "world_topo_map".
#'
#' @return A ggplot object with basemap layer and configured coordinate system. 
#'   Additional sf layers can be added with \code{geom_sf()}. The display extent
#'   is stored as an attribute accessible via \code{attr(plot, "display_extent")}.
#'
#' @details
#' The function performs these operations:
#' \itemize{
#'   \item Transforms input data to EPSG:3857 (Web Mercator) if needed
#'   \item Calculates display extent with user-specified expansion
#'   \item Fetches basemap tiles with additional buffer for complete coverage
#'   \item Creates ggplot with basemap and proper coordinate system
#'   \item Locks extent to prevent expansion when adding new layers
#' }
#'
#' The basemap extent is automatically expanded beyond the display extent to ensure
#' complete tile coverage. For ext_expansion > 0.25, basemap expansion = ext_expansion + 0.05.
#' For smaller expansions, basemap uses minimum 0.25 expansion.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(ggplot2)
#' 
#' # Create basemap with topographic tiles
#' basemap <- CREATE_BASEMAP_PLOT(
#'   largest_data = study_area,
#'   ext_expansion = 0.15,
#'   map_service = "esri",
#'   map_type = "world_topo_map",
#'   map_res = 3
#' )
#' 
#' # Add custom layers
#' map <- basemap +
#'   geom_sf(data = study_plots, aes(fill = vegetation_type), alpha = 0.6) +
#'   geom_sf(data = sample_points, color = "red", size = 2) +
#'   labs(title = "Study Area") +
#'   theme_minimal()
#' 
#' # Save as standard plot
#' ggsave("map.png", map, width = 10, height = 8)
#' 
#' # Or convert to georeferenced PDF
#' EXPORT_GEOREFERENCED_PDF(map, "map.pdf")
#' }
#'
#' @seealso 
#' \code{\link{EXPORT_GEOREFERENCED_PDF}} for exporting with embedded coordinates,
#' \code{\link[basemaps]{basemap_ggplot}} for basemap details
#'
#' @export
CREATE_BASEMAP_PLOT <- function(largest_data, 
                                ext_expansion = 0.1,
                                map_res = 2,
                                map_service = "esri",
                                map_type = "world_topo_map") {
  
  # Transform to EPSG:3857 if not already in that projection
  if (sf::st_crs(largest_data)$epsg != 3857) {
    largest_data <- sf::st_transform(largest_data, crs = 3857)
    message("Transformed input data to EPSG:3857")
  }
  
  # Calculate basemap expansion factor
  # If user-supplied expansion > 0.25, expand basemap accordingly
  if (ext_expansion > 0.25) {
    basemap_expansion <- ext_expansion + 0.05
  } else {
    basemap_expansion <- 0.25
  }
  
  # Get extent of data
  ORIGINAL_EXTENT <- sf::st_bbox(largest_data)
  
  EXT_WIDTH <- as.numeric(ORIGINAL_EXTENT["xmax"] - ORIGINAL_EXTENT["xmin"])
  EXT_HEIGHT <- as.numeric(ORIGINAL_EXTENT["ymax"] - ORIGINAL_EXTENT["ymin"])
  
  # Desired display extent (user-specified expansion for final map view)
  X_DISPLAY_BUFFER <- EXT_WIDTH * ext_expansion
  Y_DISPLAY_BUFFER <- EXT_HEIGHT * ext_expansion
  
  DISPLAY_EXTENT <- sf::st_bbox(c(
    xmin = as.numeric(ORIGINAL_EXTENT["xmin"]) - X_DISPLAY_BUFFER,
    ymin = as.numeric(ORIGINAL_EXTENT["ymin"]) - Y_DISPLAY_BUFFER,
    xmax = as.numeric(ORIGINAL_EXTENT["xmax"]) + X_DISPLAY_BUFFER,
    ymax = as.numeric(ORIGINAL_EXTENT["ymax"]) + Y_DISPLAY_BUFFER
  ), crs = sf::st_crs(largest_data))
  
  # Basemap fetch extent (larger expansion to ensure tile coverage)
  X_BASEMAP_BUFFER <- EXT_WIDTH * basemap_expansion
  Y_BASEMAP_BUFFER <- EXT_HEIGHT * basemap_expansion
  
  # Create expanded bbox for basemap tiles
  BASEMAP_BBOX <- sf::st_bbox(c(
    xmin = as.numeric(ORIGINAL_EXTENT["xmin"]) - X_BASEMAP_BUFFER,
    ymin = as.numeric(ORIGINAL_EXTENT["ymin"]) - Y_BASEMAP_BUFFER,
    xmax = as.numeric(ORIGINAL_EXTENT["xmax"]) + X_BASEMAP_BUFFER,
    ymax = as.numeric(ORIGINAL_EXTENT["ymax"]) + Y_BASEMAP_BUFFER
  ), crs = sf::st_crs(largest_data))
  
  # Debug output
  message(paste0("Original extent width: ", round(EXT_WIDTH, 2)))
  message(paste0("Display expansion: ", ext_expansion, " (", round(ext_expansion * 100, 0), "%)"))
  message(paste0("Basemap expansion: ", basemap_expansion, " (", round(basemap_expansion * 100, 0), "%)"))
  message(paste0("Display buffer: ", round(X_DISPLAY_BUFFER, 2)))
  message(paste0("Basemap buffer: ", round(X_BASEMAP_BUFFER, 2)))
  
  # Create the basemap ggplot
  MAP_PLOT <- basemaps::basemap_ggplot(
    ext = BASEMAP_BBOX,
    map_service = map_service,
    map_type = map_type,
    map_res = map_res,
    dpi = 600,
    interpolate = TRUE
  ) + 
    ggplot2::coord_sf(
      xlim = c(DISPLAY_EXTENT["xmin"], DISPLAY_EXTENT["xmax"]),
      ylim = c(DISPLAY_EXTENT["ymin"], DISPLAY_EXTENT["ymax"]), 
      expand = FALSE,
      crs = 3857,  # Explicitly set CRS to match basemap
      default_crs = NULL  # Prevents auto-expansion when adding new layers
    ) +
    ggplot2::theme_void()
  
  message(paste0("Map created with map_res: ", map_res))
  
  # Store the display extent as an attribute for potential reuse
  attr(MAP_PLOT, "display_extent") <- DISPLAY_EXTENT
  
  return(MAP_PLOT)
}


# Function 2: Convert ggplot to georeferenced PDF------

#' Export ggplot Map to Georeferenced PDF
#'
#' Converts a ggplot map object (typically created with CREATE_BASEMAP_PLOT) into
#' a georeferenced PDF file with embedded spatial metadata. The output PDF can be
#' imported into GIS software with correct coordinates. Uses GDAL for georeferencing.
#'
#' @param map_plot A ggplot object containing a map, typically created by 
#'   \code{\link{CREATE_BASEMAP_PLOT}} with additional layers.
#' @param output_filename Character. Output PDF file name. The .pdf extension is
#'   added automatically if not provided.
#' @param output_location Character. Directory for output file. Default: current
#'   working directory.
#'
#' @return Character string with the full path to the created PDF file. The function
#'   is called primarily for the side effect of creating the georeferenced PDF.
#'
#' @details
#' The function performs these operations:
#' \itemize{
#'   \item Saves ggplot as high-resolution TIFF (600 DPI)
#'   \item Converts TIFF to georeferenced raster with Web Mercator coordinates
#'   \item Reprojects to WGS84 (EPSG:4326) using Lanczos interpolation
#'   \item Converts to georeferenced PDF using GDAL with embedded spatial metadata
#'   \item Cleans up temporary files
#' }
#'
#' The resulting PDF file contains embedded geospatial information compliant with
#' ISO 32000 standard (PDF GeoPDF). The file can be opened in:
#' \itemize{
#'   \item QGIS (File > Open)
#'   \item ArcGIS Pro (Add Data)
#'   \item Avenza Maps (mobile georeferenced map viewer)
#'   \item Adobe Acrobat (displays with coordinates)
#' }
#'
#' Temporary TIFF and GeoTIFF files are created during processing but automatically
#' deleted after PDF creation.
#'
#' @examples
#' \dontrun{
#' # Create map with basemap and layers
#' basemap <- CREATE_BASEMAP_PLOT(study_area, ext_expansion = 0.1)
#' 
#' map <- basemap +
#'   geom_sf(data = boundaries, fill = NA, color = "red", size = 2) +
#'   geom_sf(data = points, color = "blue", size = 3) +
#'   labs(title = "Study Area Map")
#' 
#' # Export to georeferenced PDF
#' EXPORT_GEOREFERENCED_PDF(
#'   map_plot = map,
#'   output_filename = "study_map.pdf",
#'   output_location = "W:/Maps/Output"
#' )
#' }
#'
#' @seealso 
#' \code{\link{CREATE_BASEMAP_PLOT}} for creating the initial map,
#' \code{\link[terra]{writeRaster}} for raster export details
#'
#' @export
EXPORT_GEOREFERENCED_PDF <- function(map_plot, 
                                     output_filename,
                                     output_location = getwd()) {
  
  # Ensure output filename has .pdf extension
  if (!grepl("\\.pdf$", output_filename, ignore.case = TRUE)) {
    output_filename <- paste0(output_filename, ".pdf")
  }
  
  # Create full output path
  pdf_geo_filename <- file.path(output_location, output_filename)
  
  # Create temporary file names in output location
  tiff_temp <- file.path(output_location, 
                        paste0(tools::file_path_sans_ext(output_filename), "_temp.tiff"))
  geotiff_filename <- file.path(output_location, 
                               paste0(tools::file_path_sans_ext(output_filename), "_geo.tif"))
  
  # Step 1: Extract plot panel dimensions
  built <- ggplot2::ggplot_build(map_plot)
  x_range <- built$layout$panel_params[[1]]$x_range
  y_range <- built$layout$panel_params[[1]]$y_range
  
  # Calculate aspect ratio from the plot itself
  map_width <- diff(x_range)
  map_height <- diff(y_range)
  aspect_ratio <- map_width / map_height
  
  # Save with correct aspect ratio
  height <- 8  # or whatever you prefer
  width <- height * aspect_ratio
  
  message("Saving plot as TIFF...")
  ggplot2::ggsave(filename = tiff_temp, plot = map_plot, dpi = 600, 
         width = width, height = height, units = "in", 
         device = "tiff", compression = "lzw")
  
  # Step 2: Read TIFF as raster and flip
  message("Reading TIFF as raster...")
  map_raster <- terra::rast(tiff_temp) %>% terra::flip()
  
  # Step 3: Get bounding box from ggplot build
  raster_extent <- built$layout$panel_params[[1]][c("x_range", "y_range")]
  
  # Step 4: Set extent and projection (Web Mercator EPSG:3857)
  terra::ext(map_raster) <- c(raster_extent$x_range, raster_extent$y_range)
  terra::crs(map_raster) <- "EPSG:3857"
  
  # Store original dimensions before reprojection
  original_ncol <- ncol(map_raster)
  original_nrow <- nrow(map_raster)
  
  # Calculate target extent in 4326
  extent_3857 <- terra::ext(map_raster)
  extent_4326 <- terra::project(extent_3857, from="EPSG:3857", to="EPSG:4326")
  
  # Calculate resolution to maintain original pixel dimensions
  target_res_x <- (extent_4326[2] - extent_4326[1]) / original_ncol
  target_res_y <- (extent_4326[4] - extent_4326[3]) / original_nrow
  
  # Reproject with explicit resolution to preserve dimensions
  message("Reprojecting to EPSG:4326 with lanczos interpolation...")
  map_raster <- terra::project(map_raster, "EPSG:4326", method="lanczos", 
                       res=c(target_res_x, target_res_y), threads=TRUE)
  
  # Clamp values to valid RGB range (0-255) after reprojection
  map_raster <- terra::clamp(map_raster, lower=0, upper=255, values=TRUE)
  
  # Step 5: Write as GeoTIFF with lossless compression
  message("Writing GeoTIFF...")
  terra::writeRaster(map_raster, geotiff_filename, overwrite = TRUE, 
              gdal = c("PHOTOMETRIC=RGB", "COMPRESS=LZW", "TILED=YES"), 
              datatype = "INT1U")
  
  # Step 6: Convert GeoTIFF to georeferenced PDF with quality settings
  message("Converting to georeferenced PDF...")
  
  # Get raster dimensions to preserve exact aspect ratio
  raster_dims <- dim(map_raster)
  pixel_width <- raster_dims[2]
  pixel_height <- raster_dims[1]
  
  gdalUtilities::gdal_translate(
    src_dataset = geotiff_filename,
    dst_dataset = pdf_geo_filename,
    of = "PDF",
    ot = "Byte",
    co = c("TILED=YES", "GEO_ENCODING=ISO32000", "DPI=600"),
    outsize = c(pixel_width, pixel_height),
    r = "lanczos",
    a_srs = "EPSG:4326"
  )
  
  # Clean up temporary files
  message("Cleaning up temporary files...")
  file.remove(c(tiff_temp, geotiff_filename))
  
  message(paste0("Georeferenced PDF created successfully: ", pdf_geo_filename))
  
  return(pdf_geo_filename)
}


# Example usage------
# 
# # Load your spatial data
# OFFICE_POINT <- st_read("Office.kml") %>% st_zm(drop = TRUE)
# OFFICE_BUFFER <- OFFICE_POINT %>% st_buffer(1000)
# 
# # Create basemap plot
# MAP_PLOT <- CREATE_BASEMAP_PLOT(
#   largest_data = OFFICE_BUFFER,
#   ext_expansion = 0.1,
#   map_res = 10,
#   map_service = "esri",
#   map_type = "world_topo_map"
# )
# 
# # Add additional layers
# MAP_PLOT <- MAP_PLOT +
#   geom_sf(data = OFFICE_POINT, color = "red", size = 3) +
#   geom_sf(data = OTHER_LAYER, fill = "blue", alpha = 0.3)
# 
# # View the map
# MAP_PLOT
# 
# # Export to georeferenced PDF
# EXPORT_GEOREFERENCED_PDF(
#   map_plot = MAP_PLOT,
#   output_filename = "Office_Map",
#   output_location = "W:/Maps/Output"
# )

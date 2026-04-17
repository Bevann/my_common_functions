# Functions for exporting sf objects to themed KML/KMZ files
# Created: April 16, 2026
# Exports sf objects with thematic styling (fill/outline colors based on field values)
# Uses sf::st_write() + XML post-processing to avoid libKML dependency issues
# Preserves UTF-8 encoding for special characters


# Helper function: Convert R color to KML AABBGGRR format------
RGB_TO_KML_COLOR <- function(r_color, alpha = 1) {
  # KML uses AABBGGRR format (alpha-blue-green-red)
  # R uses RRGGBB format
  # alpha: 0-1 scale, where 0 = transparent, 1 = opaque
  
  # Convert color name or hex to RGB
  rgb_vals <- col2rgb(r_color)
  
  # Extract components
  r <- rgb_vals[1, 1]
  g <- rgb_vals[2, 1]
  b <- rgb_vals[3, 1]
  
  # Convert alpha from 0-1 to 0-255
  a <- round(alpha * 255)
  
  # Format as KML color (AABBGGRR)
  kml_color <- sprintf("%02X%02X%02X%02X", a, b, g, r)
  
  return(kml_color)
}


# Helper function: Generate color mapping for field values------
GENERATE_COLOR_MAPPING <- function(sf_object, 
                                   color_field, 
                                   color_palette = NULL,
                                   is_outline = FALSE) {
  
  # Extract field values (drop geometry for performance)
  field_values <- sf_object %>%
    st_drop_geometry() %>%
    pull(!!sym(color_field))
  
  # Determine field type
  is_numeric <- is.numeric(field_values)
  
  # Get unique values
  if (is_numeric) {
    # For continuous data, create bins
    value_range <- range(field_values, na.rm = TRUE)
    n_bins <- 100  # Number of color gradations
    
    # Default palette for continuous data
    if (is.null(color_palette)) {
      color_palette <- viridis(n_bins)
    } else if (is.function(color_palette)) {
      color_palette <- color_palette(n_bins)
    }
    
    # Create mapping function
    color_mapping <- list(
      type = "continuous",
      breaks = seq(value_range[1], value_range[2], length.out = n_bins + 1),
      colors = color_palette,
      field_values = field_values
    )
    
  } else {
    # For categorical data
    unique_values <- unique(field_values)
    unique_values <- unique_values[!is.na(unique_values)]
    n_values <- length(unique_values)
    
    # Default palette for categorical data
    if (is.null(color_palette)) {
      # Try to load pals package for better categorical colors
      if (requireNamespace("pals", quietly = TRUE)) {
        if (n_values <= 26) {
          color_palette <- pals::alphabet(n = n_values)
        } else {
          color_palette <- pals::glasbey(n = n_values)
        }
      } else {
        # Fallback to rainbow if pals not available
        message("Note: Install 'pals' package for better categorical color palettes")
        color_palette <- rainbow(n_values)
      }
    } else if (is.function(color_palette)) {
      color_palette <- color_palette(n_values)
    } else if (length(color_palette) < n_values) {
      # Recycle colors if not enough provided
      color_palette <- rep_len(color_palette, n_values)
    }
    
    # Create named vector
    color_map <- setNames(color_palette, unique_values)
    
    color_mapping <- list(
      type = "categorical",
      map = color_map,
      field_values = field_values
    )
  }
  
  return(color_mapping)
}


# Helper function: Get color for a specific value------
GET_COLOR_FOR_VALUE <- function(value, color_mapping) {
  
  if (is.na(value)) {
    return("#808080")  # Gray for NA values
  }
  
  if (color_mapping$type == "categorical") {
    # Direct lookup
    color <- color_mapping$map[as.character(value)]
    if (is.na(color)) {
      return("#808080")  # Gray for unmapped values
    }
    return(color)
    
  } else {
    # Continuous - find interval
    bin_index <- findInterval(value, color_mapping$breaks, all.inside = TRUE)
    bin_index <- max(1, min(bin_index, length(color_mapping$colors)))
    return(color_mapping$colors[bin_index])
  }
}


# Helper function: Detect geometry type------
GET_GEOMETRY_TYPE <- function(sf_object) {
  geom_types <- st_geometry_type(sf_object) %>% unique() %>% as.character()
  
  # Simplify to basic types
  if (any(grepl("POINT", geom_types, ignore.case = TRUE))) {
    return("POINT")
  } else if (any(grepl("LINE", geom_types, ignore.case = TRUE))) {
    return("LINE")
  } else if (any(grepl("POLYGON", geom_types, ignore.case = TRUE))) {
    return("POLYGON")
  } else {
    return("UNKNOWN")
  }
}


# Helper function: Create KML Style element as XML------
CREATE_KML_STYLE_ELEMENT <- function(style_id, 
                                     fill_color, 
                                     outline_color, 
                                     fill_opacity,
                                     outline_opacity,
                                     outline_width,
                                     geometry_type = "POLYGON",
                                     icon_scale = 1.0) {
  
  # Convert R colors to KML format
  kml_fill <- RGB_TO_KML_COLOR(fill_color, fill_opacity)
  kml_outline <- RGB_TO_KML_COLOR(outline_color, outline_opacity)
  
  # Create appropriate style based on geometry type
  if (geometry_type == "POINT") {
    # Points use IconStyle
    style_xml <- sprintf(
      '<Style id="%s">
        <IconStyle>
          <color>%s</color>
          <scale>%.1f</scale>
          <Icon>
            <href>http://maps.google.com/mapfiles/kml/paddle/wht-blank.png</href>
          </Icon>
        </IconStyle>
        <LabelStyle>
          <scale>0.8</scale>
        </LabelStyle>
      </Style>',
      style_id,
      kml_fill,
      icon_scale
    )
    
  } else if (geometry_type == "LINE") {
    # Lines use LineStyle only
    style_xml <- sprintf(
      '<Style id="%s">
        <LineStyle>
          <color>%s</color>
          <width>%d</width>
        </LineStyle>
      </Style>',
      style_id,
      kml_outline,
      outline_width
    )
    
  } else {
    # Polygons use both LineStyle and PolyStyle
    style_xml <- sprintf(
      '<Style id="%s">
        <LineStyle>
          <color>%s</color>
          <width>%d</width>
        </LineStyle>
        <PolyStyle>
          <color>%s</color>
          <fill>1</fill>
          <outline>1</outline>
        </PolyStyle>
      </Style>',
      style_id,
      kml_outline,
      outline_width,
      kml_fill
    )
  }
  
  return(style_xml)
}


# Helper function: Remove empty inline Style elements from Placemarks------
REMOVE_EMPTY_PLACEMARK_STYLES <- function(kml_doc) {
  # sf::st_write() creates empty <Style> elements in each Placemark
  # These override the <styleUrl> references, preventing themed colors from showing
  # This function removes those empty inline styles
  
  # Find all Placemark nodes
  placemarks <- xml_find_all(kml_doc, "//d1:Placemark", xml_ns(kml_doc))
  
  if (length(placemarks) == 0) {
    placemarks <- xml_find_all(kml_doc, "//Placemark")
  }
  
  # Remove inline Style elements from each Placemark
  for (placemark in placemarks) {
    # Find Style child (not styleUrl)
    inline_styles <- xml_find_all(placemark, "./d1:Style", xml_ns(kml_doc))
    
    if (length(inline_styles) == 0) {
      inline_styles <- xml_find_all(placemark, "./Style")
    }
    
    # Remove each inline Style element
    for (style_node in inline_styles) {
      xml_remove(style_node)
    }
  }
  
  message("Removed empty inline Style elements from Placemarks")
  return(kml_doc)
}


# Helper function: Clean up Schema and Folder names------
CLEAN_SCHEMA_NAMES <- function(kml_doc, clean_name = "features") {
  # sf::st_write() generates auto names like "file2e883f2a644f" for Schema
  # Google Earth displays this as a prefix in attribute tables
  # This function replaces with a clean name
  
  # Find Schema element
  schema <- xml_find_first(kml_doc, "//d1:Schema", xml_ns(kml_doc))
  
  if (length(schema) == 0) {
    schema <- xml_find_first(kml_doc, "//Schema")
  }
  
  if (length(schema) > 0) {
    # Get old schema name
    old_name <- xml_attr(schema, "name")
    
    if (!is.na(old_name) && old_name != clean_name) {
      # Update Schema attributes
      xml_set_attr(schema, "name", clean_name)
      xml_set_attr(schema, "id", clean_name)
      
      # Update Folder name (if it matches old schema name)
      folders <- xml_find_all(kml_doc, "//d1:Folder/d1:name", xml_ns(kml_doc))
      
      if (length(folders) == 0) {
        folders <- xml_find_all(kml_doc, "//Folder/name")
      }
      
      for (folder_name in folders) {
        if (xml_text(folder_name) == old_name) {
          xml_set_text(folder_name, clean_name)
        }
      }
      
      # Remove SchemaData schemaUrl references (prevents "features:" prefix in popup)
      schema_data <- xml_find_all(kml_doc, "//d1:SchemaData", xml_ns(kml_doc))
      
      if (length(schema_data) == 0) {
        schema_data <- xml_find_all(kml_doc, "//SchemaData")
      }
      
      for (sd in schema_data) {
        # Remove schemaUrl attribute entirely to avoid prefix in Google Earth
        xml_set_attr(sd, "schemaUrl", NULL)
      }
      
      message(paste0("Cleaned schema name: '", old_name, "' -> '", clean_name, "' (removed schemaUrl prefix)"))
    }
  }
  
  return(kml_doc)
}


# Helper function: Inject styles into KML XML document------
INJECT_STYLES_INTO_KML <- function(kml_doc,
                                   sf_object,
                                   color_field = NULL,
                                   outline_field = NULL,
                                   fill_color_mapping = NULL,
                                   outline_color_mapping = NULL,
                                   fill_opacity = 0.6,
                                   outline_opacity = 0.9,
                                   outline_width = 2,
                                   outline_color = "#000000",
                                   geometry_type = "POLYGON",
                                   icon_scale = 1.0) {
  
  # Find Document node
  doc_node <- xml_find_first(kml_doc, "//d1:Document", xml_ns(kml_doc))
  
  if (length(doc_node) == 0) {
    doc_node <- xml_find_first(kml_doc, "//Document")
  }
  
  # If no color_field specified, create single default style
  if (is.null(color_field)) {
    message("No color_field specified - using default Google Earth styling")
    return(kml_doc)  # Return unchanged
  }
  
  # Generate all unique styles needed
  styles_created <- list()
  style_map <- list()  # Map from field values to style IDs
  
  # Get unique values from color field
  field_values <- sf_object %>%
    st_drop_geometry() %>%
    pull(!!sym(color_field))
  
  unique_values <- unique(field_values)
  unique_values <- unique_values[!is.na(unique_values)]
  
  # Create style for each unique value
  for (value in unique_values) {
    # Generate safe style ID
    style_id <- paste0("style_", make.names(as.character(value)))
    
    # Get colors for this value
    fill_color <- GET_COLOR_FOR_VALUE(value, fill_color_mapping)
    
    if (!is.null(outline_color_mapping)) {
      # Use separate outline field colors
      outline_col <- GET_COLOR_FOR_VALUE(value, outline_color_mapping)
    } else {
      # Match outline to fill color (unless static outline_color specified)
      outline_col <- fill_color
    }
    
    # Create style XML
    style_xml_text <- CREATE_KML_STYLE_ELEMENT(
      style_id = style_id,
      fill_color = fill_color,
      outline_color = outline_col,
      fill_opacity = fill_opacity,
      outline_opacity = outline_opacity,
      outline_width = outline_width,
      geometry_type = geometry_type,
      icon_scale = icon_scale
    )
    
    # Parse and add to document
    style_node <- read_xml(style_xml_text)
    xml_add_child(doc_node, style_node, .where = 0)  # Add at beginning
    
    # Store mapping
    style_map[[as.character(value)]] <- style_id
  }
  
  # Now assign styleUrl to each Placemark
  placemarks <- xml_find_all(kml_doc, "//d1:Placemark", xml_ns(kml_doc))
  
  if (length(placemarks) == 0) {
    placemarks <- xml_find_all(kml_doc, "//Placemark")
  }
  
  # Match placemarks to features by index
  for (i in seq_along(placemarks)) {
    placemark <- placemarks[[i]]
    
    # Get the value for this feature
    feature_value <- field_values[i]
    
    if (!is.na(feature_value)) {
      # Get corresponding style ID
      style_id <- style_map[[as.character(feature_value)]]
      
      if (!is.null(style_id)) {
        # Create styleUrl element
        style_url_node <- read_xml(sprintf("<styleUrl>#%s</styleUrl>", style_id))
        
        # Add to placemark (before ExtendedData if it exists)
        extended_data <- xml_find_first(placemark, ".//d1:ExtendedData", xml_ns(kml_doc))
        if (length(extended_data) == 0) {
          extended_data <- xml_find_first(placemark, ".//ExtendedData")
        }
        
        if (length(extended_data) > 0) {
          xml_add_sibling(extended_data, style_url_node, .where = "before")
        } else {
          xml_add_child(placemark, style_url_node, .where = 0)
        }
      }
    }
  }
  
  message(paste0("Created ", length(style_map), " unique styles for field: ", color_field))
  
  return(kml_doc)
}


# Helper function: Set labels in KML placemarks------
SET_KML_LABELS <- function(kml_doc, sf_object, label_field = NULL) {
  
  # Auto-detect label field if not specified
  if (is.null(label_field)) {
    # Find first character field
    char_fields <- sf_object %>%
      st_drop_geometry() %>%
      select(where(is.character)) %>%
      names()
    
    if (length(char_fields) > 0) {
      label_field <- char_fields[1]
      message(paste0("Auto-detected label field: ", label_field))
    } else {
      # Use row number as fallback
      message("No character fields found - using feature index as labels")
      label_values <- paste0("Feature_", seq_len(nrow(sf_object)))
    }
  }
  
  # Get label values
  if (!is.null(label_field)) {
    label_values <- sf_object %>%
      st_drop_geometry() %>%
      pull(!!sym(label_field))
  }
  
  # Find all Placemark nodes
  placemarks <- xml_find_all(kml_doc, "//d1:Placemark", xml_ns(kml_doc))
  
  if (length(placemarks) == 0) {
    placemarks <- xml_find_all(kml_doc, "//Placemark")
  }
  
  # Update name element in each placemark
  for (i in seq_along(placemarks)) {
    placemark <- placemarks[[i]]
    
    # Find or create name element
    name_node <- xml_find_first(placemark, ".//d1:name", xml_ns(kml_doc))
    if (length(name_node) == 0) {
      name_node <- xml_find_first(placemark, ".//name")
    }
    
    # Set label value (ensure UTF-8 encoding)
    label_text <- as.character(label_values[i])
    label_text <- iconv(label_text, from = "UTF-8", to = "UTF-8")
    
    if (length(name_node) > 0) {
      xml_set_text(name_node, label_text)
    } else {
      # Create new name node
      name_xml <- read_xml(sprintf("<name>%s</name>", label_text))
      xml_add_child(placemark, name_xml, .where = 0)
    }
  }
  
  return(kml_doc)
}


# Main function: Export sf object to themed KML/KMZ------

#' Export Spatial Data to Themed KML/KMZ Files
#'
#' Exports sf spatial objects to KML or KMZ format with thematic styling based 
#' on field values. Supports point, line, and polygon geometries with customizable
#' colors, labels, and styling for viewing in Google Earth or other GIS applications.
#' Automatically handles coordinate transformation to WGS84 and preserves UTF-8 
#' encoding for special characters.
#'
#' @param sf_object An sf object to export. Must contain valid spatial geometries.
#' @param output_filename Character. Output file name. File extension (.kml or .kmz) 
#'   determines output format.
#' @param output_location Character. Directory for output file. Default: current 
#'   working directory.
#' @param color_field Character. Field name for thematic coloring. If NULL, uses
#'   default Google Earth styling. Supports both categorical and continuous data.
#' @param outline_field Character. Optional field name for outline/border colors. 
#'   If NULL, outlines match fill colors. Default: NULL.
#' @param label_field Character. Field name for feature labels. If NULL, 
#'   auto-detects first character field or uses feature index. Default: NULL.
#' @param color_palette Vector or function. Custom color palette for fill colors.
#'   Can be a vector of colors, or a function that generates colors. If NULL, 
#'   uses viridis for continuous data or pals::alphabet for categorical. Default: NULL.
#' @param outline_palette Vector or function. Custom palette for outline colors. 
#'   Default: NULL.
#' @param outline_color Character. Hex color code for static outline color when 
#'   outline_field and outline_palette are NULL. Default: "#000000" (black).
#' @param outline_width Numeric. Line/outline width in pixels. Applies to polygon
#'   outlines and line features. Default: 2.
#' @param fill_opacity Numeric. Fill transparency from 0 (transparent) to 1 (opaque).
#'   Default: 0.6.
#' @param outline_opacity Numeric. Outline transparency from 0 to 1. Default: 0.9.
#' @param icon_scale Numeric. Point marker size multiplier. Values > 1 increase 
#'   marker size. Default: 1.0.
#' @param format Character. Output format: "kml" (uncompressed) or "kmz" (compressed ZIP).
#'   If filename has .kml or .kmz extension, that overrides this parameter. Default: "kmz".
#'
#' @return Invisible NULL. Function is called for the side effect of creating a 
#'   KML/KMZ file. Success messages printed to console.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Validates sf object and parameters
#'   \item Transforms coordinates to WGS84 (EPSG:4326) required for KML
#'   \item Auto-detects geometry type (point/line/polygon)
#'   \item Generates color schemes for categorical or continuous data
#'   \item Creates KML with embedded styles and clean attribute tables
#'   \item Preserves UTF-8 encoding for special characters
#'   \item Optionally compresses to KMZ format
#' }
#'
#' For categorical data, each unique value gets a distinct color. For continuous
#' numeric data, colors are interpolated across the data range. The function
#' automatically selects appropriate color palettes but custom palettes can be
#' provided via color_palette parameter.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(bcdata)
#' 
#' # Example 1: Export polygons with categorical colors
#' wmu <- bcdc_query_geodata("wildlife-management-units") %>%
#'   filter(REGION_RESPONSIBLE_NAME == "Thompson") %>%
#'   collect()
#' 
#' EXPORT_SF_TO_THEMED_KML(
#'   sf_object = wmu,
#'   color_field = "WILDLIFE_MGMT_UNIT_ID",
#'   label_field = "WILDLIFE_MGMT_UNIT_ID",
#'   output_filename = "wmu.kmz",
#'   fill_opacity = 0.5
#' )
#' 
#' # Example 2: Export points with custom colors
#' EXPORT_SF_TO_THEMED_KML(
#'   sf_object = survey_points,
#'   color_field = "site_type",
#'   label_field = "site_name",
#'   color_palette = c("red", "blue", "green"),
#'   icon_scale = 1.5,
#'   output_filename = "sites.kml"
#' )
#' 
#' # Example 3: Continuous data with custom gradient
#' EXPORT_SF_TO_THEMED_KML(
#'   sf_object = watersheds,
#'   color_field = "mean_elevation",
#'   color_palette = viridisLite::viridis(100),
#'   output_filename = "elevation.kmz"
#' )
#' }
#'
#' @seealso 
#' \code{\link[sf]{st_write}} for standard spatial file export,
#' \code{\link[viridisLite]{viridis}} for color palettes
#'
#' @export
#' @importFrom sf st_transform st_crs st_write st_drop_geometry st_is_valid st_make_valid st_bbox st_geometry_type
#' @importFrom xml2 read_xml write_xml xml_find_all xml_find_first xml_ns xml_add_child xml_remove xml_attr xml_set_attr xml_text xml_set_text xml_add_sibling
#' @importFrom dplyr pull select where
#' @importFrom tidyr sym
#' @importFrom viridisLite viridis
#' @importFrom utils zip
EXPORT_SF_TO_THEMED_KML <- function(sf_object,
                                    output_filename,
                                    output_location = getwd(),
                                    color_field = NULL,
                                    outline_field = NULL,
                                    label_field = NULL,
                                    color_palette = NULL,
                                    outline_palette = NULL,
                                    outline_color = "#000000",
                                    outline_width = 2,
                                    fill_opacity = 0.6,
                                    outline_opacity = 0.9,
                                    icon_scale = 1.0,
                                    format = "kmz") {
  
  # Validation------
  if (!inherits(sf_object, "sf")) {
    stop("sf_object must be an sf object")
  }
  
  if (nrow(sf_object) == 0) {
    stop("sf_object has no features")
  }
  
  if (!st_is_valid(sf_object) %>% all(na.rm = TRUE)) {
    message("Warning: Some geometries are invalid. Attempting to fix with st_make_valid()...")
    sf_object <- st_make_valid(sf_object)
  }
  
  # Check if color_field exists
  if (!is.null(color_field)) {
    if (!color_field %in% names(sf_object)) {
      stop(paste0("color_field '", color_field, "' not found in sf_object"))
    }
  }
  
  # Check if outline_field exists
  if (!is.null(outline_field)) {
    if (!outline_field %in% names(sf_object)) {
      stop(paste0("outline_field '", outline_field, "' not found in sf_object"))
    }
  }
  
  # Check if label_field exists
  if (!is.null(label_field)) {
    if (!label_field %in% names(sf_object)) {
      stop(paste0("label_field '", label_field, "' not found in sf_object"))
    }
  }
  
  # Determine format from filename extension if provided------
  if (grepl("\\.kml$", output_filename, ignore.case = TRUE)) {
    format <- "kml"
    message("Detected .kml extension - will save as KML")
  } else if (grepl("\\.kmz$", output_filename, ignore.case = TRUE)) {
    format <- "kmz"
    message("Detected .kmz extension - will save as KMZ")
  } else {
    # No extension in filename, use format parameter
    format <- tolower(format)
    if (!format %in% c("kml", "kmz")) {
      stop("format must be 'kml' or 'kmz'")
    }
  }
  
  message(paste0("Exporting ", nrow(sf_object), " features to ", toupper(format), "..."))
  
  # Transform to WGS84 (EPSG:4326) - required for KML------
  original_crs <- st_crs(sf_object)
  
  if (is.na(original_crs$epsg) || original_crs$epsg != 4326) {
    message("Transforming to WGS84 (EPSG:4326) for KML export...")
    sf_object <- st_transform(sf_object, 4326)
  }
  
  # Preserve UTF-8 encoding in character fields------
  char_cols <- names(sf_object)[sapply(sf_object, is.character)]
  
  for (col in char_cols) {
    sf_object[[col]] <- iconv(sf_object[[col]], from = "UTF-8", to = "UTF-8")
  }
  
  # Detect geometry type------
  geometry_type <- GET_GEOMETRY_TYPE(sf_object)
  message(paste0("Detected geometry type: ", geometry_type))
  
  # Generate color mappings if color_field specified------
  fill_color_mapping <- NULL
  outline_color_mapping <- NULL
  
  if (!is.null(color_field)) {
    message(paste0("Generating color mapping for field: ", color_field))
    fill_color_mapping <- GENERATE_COLOR_MAPPING(
      sf_object, 
      color_field, 
      color_palette
    )
  }
  
  if (!is.null(outline_field)) {
    message(paste0("Generating outline color mapping for field: ", outline_field))
    outline_color_mapping <- GENERATE_COLOR_MAPPING(
      sf_object, 
      outline_field, 
      outline_palette
    )
  }
  
  # Export base KML using sf::st_write()------
  temp_kml <- tempfile(fileext = ".kml")
  
  # Note: KML driver has limited layer_options support
  # See: https://gdal.org/drivers/vector/kml.html
  # delete_dsn=TRUE may produce harmless warning on Windows about file access
  st_write(
    sf_object, 
    temp_kml, 
    driver = "KML",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  message("Base KML exported, processing styles...")
  
  # Read KML as XML------
  kml_doc <- read_xml(temp_kml)
  
  # Clean up KML structure------
  # Remove empty inline Style elements (they override styleUrl)
  kml_doc <- REMOVE_EMPTY_PLACEMARK_STYLES(kml_doc)
  
  # Clean up auto-generated schema names
  kml_doc <- CLEAN_SCHEMA_NAMES(kml_doc, clean_name = "features")
  
  # Inject styles------
  if (!is.null(color_field)) {
    kml_doc <- INJECT_STYLES_INTO_KML(
      kml_doc = kml_doc,
      sf_object = sf_object,
      color_field = color_field,
      outline_field = outline_field,
      fill_color_mapping = fill_color_mapping,
      outline_color_mapping = outline_color_mapping,
      fill_opacity = fill_opacity,
      outline_opacity = outline_opacity,
      outline_width = outline_width,
      outline_color = outline_color,
      geometry_type = geometry_type,
      icon_scale = icon_scale
    )
  }
  
  # Set labels------
  kml_doc <- SET_KML_LABELS(kml_doc, sf_object, label_field)
  
  # Prepare output path------
  if (format == "kmz") {
    if (!grepl("\\.kmz$", output_filename, ignore.case = TRUE)) {
      output_filename <- paste0(tools::file_path_sans_ext(output_filename), ".kmz")
    }
  } else {
    if (!grepl("\\.kml$", output_filename, ignore.case = TRUE)) {
      output_filename <- paste0(tools::file_path_sans_ext(output_filename), ".kml")
    }
  }
  
  output_path <- file.path(output_location, output_filename)
  
  # Write final KML------
  if (format == "kml") {
    write_xml(kml_doc, output_path, encoding = "UTF-8")
    message(paste0("KML saved to: ", output_path))
    
  } else {
    # Create KMZ (compressed)------
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # KMZ standard requires file to be named "doc.kml" inside archive
    doc_kml_path <- file.path(temp_dir, "doc.kml")
    write_xml(kml_doc, doc_kml_path, encoding = "UTF-8")
    
    # Create ZIP archive
    current_wd <- getwd()
    setwd(temp_dir)
    
    zip(
      zipfile = output_path,
      files = "doc.kml",
      flags = "-q -j"  # -q for quiet, -j to junk directory paths
    )
    
    setwd(current_wd)
    
    # Clean up temp directory
    unlink(temp_dir, recursive = TRUE)
    
    message(paste0("KMZ saved to: ", output_path))
  }
  
  # Clean up temp KML (suppress warnings from Windows file locking)
  suppressWarnings(unlink(temp_kml))
  
  # Report file size
  file_size <- file.info(output_path)$size
  file_size_mb <- round(file_size / 1024 / 1024, 2)
  
  if (file_size_mb < 0.01) {
    file_size_kb <- round(file_size / 1024, 1)
    message(paste0("File size: ", file_size_kb, " KB"))
  } else {
    message(paste0("File size: ", file_size_mb, " MB"))
  }
  
  message(paste0("Export complete! ", nrow(sf_object), " features exported."))
  
  # Return path invisibly
  invisible(output_path)
}

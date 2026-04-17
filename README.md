# R Spatial Utility Functions

A collection of R utility functions for spatial data processing, visualization, and export. Designed for BC Government workflows involving spatial data and field data visualization.

## Overview

This repository contains utility scripts organized into three main categories:

1. **kml_export_functions.r** - Export spatial data to themed KML/KMZ files for Google Earth
2. **georeferenced_map_functions.r** - Create georeferenced PDF maps with basemaps
3. **misc_functions.r** - Wildlife population analysis, spatial operations, and data processing utilities

---

## ðŸ“¦ Installation

### Install as R Package (Recommended)

Install directly from GitHub using devtools:

```r
# Install devtools if you don't have it
install.packages("devtools")

# Install my_common_functions package from GitHub
devtools::install_github("Bevann/my_common_functions")

# Load the package
library(my_common_functions)

# Functions are now available directly
EXPORT_SF_TO_THEMED_KML(my_data, ...)
CREATE_BASEMAP_PLOT(my_data, ...)
```

### Alternative: Source Individual Files

If you prefer not to install as a package:

```r
# Download and source the utility functions you need
source("R/kml_export_functions.R")
source("R/georeferenced_map_functions.R")
source("R/misc_functions.R")
```

### Dependencies

The package automatically installs required dependencies. If installing manually:

```r
# Core dependencies
install.packages(c("sf", "xml2", "dplyr", "tidyr", "stringr", 
                   "viridisLite", "basemaps", "terra", "gdalUtilities", 
                   "ggplot2", "lubridate", "forcats", "purrr"))

# Optional: Enhanced color palettes
install.packages(c("pals", "RColorBrewer"))
```

---

## ðŸ—ºï¸ KML Export Functions

### Overview

Export sf objects to themed KML/KMZ files with professional styling for viewing in Google Earth or other GIS applications. Automatically handles point, line, and polygon geometries with customizable colors, labels, and styling.

### Key Features

- âœ… **Multi-geometry support**: Points, lines, and polygons
- ðŸŽ¨ **Thematic coloring**: Categorical or continuous color schemes
- ðŸ·ï¸ **Smart labeling**: Automatic or field-based labels
- ðŸŒ **UTF-8 encoding**: Preserves special characters and diacritics (e.g., Ã©, Ã±, Ã¼)
- ðŸŽ­ **Custom palettes**: viridis, RColorBrewer, pals, or custom colors
- ðŸ“Š **Clean popups**: Field names without prefixes in Google Earth attribute tables
- ðŸ’¾ **Flexible output**: KML (uncompressed) or KMZ (compressed ZIP)

### Main Function: `EXPORT_SF_TO_THEMED_KML()`

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `sf_object` | sf | *required* | Spatial data object to export |
| `output_filename` | character | *required* | Output file name (.kml or .kmz) |
| `output_location` | character | `getwd()` | Directory for output file |
| `color_field` | character | `NULL` | Field name for thematic coloring |
| `outline_field` | character | `NULL` | Field name for outline colors (optional) |
| `label_field` | character | `NULL` | Field for labels (auto-detected if NULL) |
| `color_palette` | vector/function | `NULL` | Custom color palette |
| `outline_palette` | vector/function | `NULL` | Custom outline palette |
| `outline_color` | character | `"#000000"` | Static outline color if no palette |
| `outline_width` | numeric | `2` | Line/outline width in pixels |
| `fill_opacity` | numeric | `0.6` | Fill transparency (0-1) |
| `outline_opacity` | numeric | `0.9` | Outline transparency (0-1) |
| `icon_scale` | numeric | `1.0` | Point marker size multiplier |
| `format` | character | `"kmz"` | Output format ("kml" or "kmz") |

### Examples

#### Example 1: Wildlife Management Units (Categorical)

Export polygon data with each WMU colored differently:

```r
library(bcdata)
library(sf)
source("kml_export_functions.r")

# Get BC Wildlife Management Units
WMU <- bcdc_query_geodata("wildlife-management-units") %>%
  filter(REGION_RESPONSIBLE_NAME == "Thompson") %>%
  collect()

# Export with categorical colors (one color per WMU)
EXPORT_SF_TO_THEMED_KML(
  sf_object = WMU,
  color_field = "WILDLIFE_MGMT_UNIT_ID",
  label_field = "WILDLIFE_MGMT_UNIT_ID",
  output_filename = "Thompson_WMUs.kmz",
  fill_opacity = 0.5,
  outline_width = 2
)
```

#### Example 2: Point Data - Field Survey Locations

```r
# Export GPS points with colored markers
EXPORT_SF_TO_THEMED_KML(
  sf_object = survey_points,
  color_field = "site_type",
  label_field = "site_name",
  color_palette = c("red", "blue", "green", "yellow"),
  icon_scale = 1.5,  # Make markers larger
  output_filename = "field_sites.kml"  # .kml extension = uncompressed
)
```

#### Example 3: Continuous Data - Elevation or Population

```r
# Color by continuous numeric field
EXPORT_SF_TO_THEMED_KML(
  sf_object = watersheds,
  color_field = "mean_elevation",
  label_field = "watershed_name",
  color_palette = viridis(100),  # Smooth gradient
  output_filename = "watersheds_elevation.kmz"
)
```

#### Example 4: Roads/Rivers (Line Data)

```r
# Export line data with thickness and color
EXPORT_SF_TO_THEMED_KML(
  sf_object = roads,
  color_field = "road_class",
  label_field = "road_name",
  outline_width = 4,  # Thicker lines
  color_palette = c("red", "orange", "yellow"),
  output_filename = "road_network.kmz"
)
```

#### Example 5: Custom RColorBrewer Palette

```r
library(RColorBrewer)

# Use ColorBrewer pastel colors
EXPORT_SF_TO_THEMED_KML(
  sf_object = parks,
  color_field = "park_type",
  color_palette = brewer.pal(9, "Set3"),
  fill_opacity = 0.7,
  output_filename = "provincial_parks.kmz"
)
```

### Color Palette Options

#### For Categorical Data (e.g., species, zones, names)

```r
# pals package (best for many categories)
pals::alphabet()   # 26 colors
pals::kelly(22)    # 22 maximally distinct colors
pals::glasbey(32)  # 32 colors

# RColorBrewer
brewer.pal(12, "Set3")    # Pastel colors
brewer.pal(12, "Paired")  # Paired colors
brewer.pal(8, "Dark2")    # Dark colors

# Manual colors
c("red", "blue", "green", "yellow", "purple")
c("#FF0000", "#00FF00", "#0000FF")
```

#### For Continuous Data (e.g., elevation, temperature, counts)

```r
# viridis (colorblind-safe, perceptually uniform)
viridis(100)
magma(100)
plasma(100)
inferno(100)

# RColorBrewer gradients
colorRampPalette(brewer.pal(9, "YlOrRd"))(100)  # Yellow to red
colorRampPalette(brewer.pal(9, "Blues"))(100)   # Light to dark blue

# Custom gradients
colorRampPalette(c("blue", "white", "red"))(100)
```

---

## ðŸ“ Georeferenced Map Functions

### Overview

Create publication-quality georeferenced PDF maps with integrated basemaps from ESRI, OpenStreetMap, and other providers. Output PDFs include embedded spatial metadata for GIS import.

### Key Features

- ðŸ—ºï¸ **Multiple basemap sources**: ESRI, OpenStreetMap, Carto, and more
- ðŸ“ **Smart extent management**: Configurable map boundaries and basemap coverage
- ðŸŽ¯ **Projection handling**: Automatic transformation to EPSG:3857
- ðŸ“„ **Georeferenced output**: PDFs with embedded coordinate metadata
- ðŸŽ¨ **Customizable styling**: Integration with ggplot2 for overlay layers

### Main Function: `CREATE_BASEMAP_PLOT()`

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `largest_data` | sf | *required* | Spatial object defining map extent |
| `ext_expansion` | numeric | `0.1` | Buffer around data (0.1 = 10% expansion) |
| `map_res` | numeric | `2` | Basemap resolution/zoom level |
| `map_service` | character | `"esri"` | Basemap provider |
| `map_type` | character | `"world_topo_map"` | Basemap style |

#### Basemap Options

**ESRI Services:**
- `"world_topo_map"` - Topographic
- `"world_imagery"` - Satellite imagery
- `"world_street_map"` - Street map
- `"world_terrain_base"` - Terrain/relief

**OpenStreetMap:**
- Use `map_service = "osm"` with various styles

### Examples

#### Example 1: Basic Topographic Map

```r
source("georeferenced_map_functions.r")
library(sf)
library(ggplot2)

# Create basemap
basemap <- CREATE_BASEMAP_PLOT(
  largest_data = study_area,
  ext_expansion = 0.15,  # 15% buffer
  map_service = "esri",
  map_type = "world_topo_map"
)

# Add your data layers
final_map <- basemap +
  geom_sf(data = study_plots, aes(fill = vegetation_type), alpha = 0.6) +
  geom_sf(data = sample_points, color = "red", size = 2) +
  labs(title = "Study Area Vegetation",
       fill = "Vegetation Type") +
  theme_minimal()

# Save as georeferenced PDF
ggsave("study_area_map.pdf", final_map, width = 11, height = 8.5)
```

#### Example 2: Satellite Imagery Base

```r
# Use satellite imagery as basemap
basemap_satellite <- CREATE_BASEMAP_PLOT(
  largest_data = project_boundary,
  ext_expansion = 0.2,
  map_service = "esri",
  map_type = "world_imagery",
  map_res = 3  # Higher resolution
)

map <- basemap_satellite +
  geom_sf(data = project_boundary, fill = NA, color = "yellow", size = 2) +
  geom_sf_label(data = project_boundary, aes(label = project_name),
                color = "white", fill = "black", alpha = 0.7)

ggsave("project_satellite.pdf", map)
```

#### Example 3: Terrain/Relief Map

```r
# Terrain basemap for watershed analysis
basemap_terrain <- CREATE_BASEMAP_PLOT(
  largest_data = watershed_boundary,
  ext_expansion = 0.1,
  map_service = "esri",
  map_type = "world_terrain_base"
)

map <- basemap_terrain +
  geom_sf(data = streams, color = "blue", size = 1) +
  geom_sf(data = watershed_boundary, fill = NA, color = "red", size = 1.5) +
  labs(title = "Watershed Hydrology")

ggsave("watershed_map.pdf", map, width = 10, height = 8)
```

---

## ðŸ¦Œ Wildlife & Spatial Analysis Functions

### Overview

Collection of specialized functions for wildlife population analysis, spatial data processing, and data manipulation. Includes tools for mark-recapture estimates, movement analysis, geometry validation, and biological year calculations.

### Population Analysis Functions

#### `LINC_PET_PIPE()` - Lincoln-Petersen Estimate (Pipeline Version)

Calculate population estimates using the Lincoln-Petersen method for mark-recapture studies. Designed for dplyr pipelines, commonly used for caribou aerial survey analysis where collared animals serve as the "marked" population.

**Parameters:**
- `Data` - Input data frame containing survey data
- `Herd` - Column name for herd identifier (unquoted)
- `Observed_caribou` - Total number of caribou observed
- `Total_collars` - Total number of collars deployed in herd
- `Observed_collars` - Number of collars observed during survey

**Returns:** Data frame with population estimate, confidence intervals, and variance

**Example:**
```r
# Calculate population estimates for multiple herds
survey_data %>%
  LINC_PET_PIPE(
    Herd = herd_name,
    Observed_caribou = total_seen,
    Total_collars = collars_deployed,
    Observed_collars = collars_seen
  )
# Output includes: Population estimate, 95% CI (upper/lower), variance
```

#### `CALC_LINC_PET()` - Lincoln-Petersen Estimate (Vector Version)

Same calculation as `LINC_PET_PIPE()` but accepts individual numeric values rather than data frame columns. Suitable for single calculations.

**Example:**
```r
# Single population estimate
CALC_LINC_PET(
  Observed_caribou = 45,
  Total_collars = 8,
  Observed_collars = 3
)
```

#### `CALC_LAMBDA()` - Population Growth Rate

Compute the finite rate of population growth (lambda) from initial and final population sizes. Lambda > 1 indicates growth, < 1 indicates decline, = 1 indicates stable population.

**Parameters:**
- `Initial_Pop` - Initial population size
- `Final_Pop` - Final population size
- `Time_Span` - Time period between counts (typically years)

**Example:**
```r
# Population declined from 150 to 120 over 5 years
CALC_LAMBDA(Initial_Pop = 150, Final_Pop = 120, Time_Span = 5)
# Returns: 0.984 (1.6% annual decline)
```

### Temporal Analysis Functions

#### `ADD_BIO_YEAR()` - Biological Year Calculations

Calculate biological year and biological day-of-year based on a custom annual cycle start date. Useful for wildlife studies where the biological year starts on a specific date (e.g., calving season) rather than January 1.

**Parameters:**
- `data` - Data frame with date observations
- `date_col` - Column containing dates (unquoted)
- `bio_start_date` - Start date of biological year (e.g., "2023-05-15")

**Returns:** Original data with `bio_year` and `bio_day` columns

**Example:**
```r
# Caribou study where biological year starts May 15 (calving season)
caribou_data %>%
  ADD_BIO_YEAR(
    date_col = observation_date,
    bio_start_date = "2023-05-15"
  )
# Adds: bio_year (integer), bio_day (day within bio year, 1-365)
```

### Movement Analysis Functions

#### `MAKE_LINES()` - Create Movement Lines from Points

Convert sequential GPS collar point locations into line segments representing movement paths. Calculates distance, time, and speed between consecutive locations.

**Parameters:**
- `.data` - sf object with POINT geometries
- `Group_Var` - Grouping column (e.g., animal ID)
- `Date_Time_Var` - Column with date-time values (POSIXct)

**Returns:** sf object with LINESTRING geometries and movement metrics

**Example:**
```r
# Generate movement lines from GPS collar data
collar_points %>%
  MAKE_LINES(
    Group_Var = animal_id,
    Date_Time_Var = fix_time
  )
# Output includes: Travel_Length_Km, Travel_Time_H, Travel_Speed_KmH
```

### Spatial Operations Functions

#### `ARC.IDENT()` - Spatial Identity (ArcGIS Equivalent)

Performs spatial identity operation similar to ArcGIS's Identity tool. Computes intersection of two layers and preserves portions of layer_a that don't overlap with layer_b.

**Parameters:**
- `layer_a` - sf object to be split
- `layer_b` - sf object for overlay

**Example:**
```r
# Split wildlife range by land ownership
caribou_range %>%
  ARC.IDENT(land_ownership)
# Returns: Polygons split at ownership boundaries with combined attributes
```

#### `FLATTEN_POLYS()` - Resolve Overlapping Polygons

Create non-overlapping features from overlapping polygons and aggregate attributes. Useful for resolving spatial conflicts or creating union layers.

**Parameters:**
- `.data` - sf object with potentially overlapping polygons
- `LIST_COL` - Column to concatenate (semicolon-separated list)
- `MAX_COL` - Column to take maximum value
- `FIRST_COL` - Column to take first value

**Example:**
```r
# Flatten overlapping habitat polygons
habitat_layers %>%
  FLATTEN_POLYS(
    LIST_COL = "habitat_type",
    MAX_COL = "quality_score",
    FIRST_COL = "priority"
  )
# Returns: Non-overlapping polygons with aggregated attributes
```

### Geometry Validation Functions

#### `CHECK_GEOMETRY()` - Geometry Type Summary

Quick summary count of geometry types in an sf object (POINT, POLYGON, etc.)

**Example:**
```r
wildlife_habitat %>% CHECK_GEOMETRY()
# Output: POLYGON (150), MULTIPOLYGON (3)
```

#### `CHECK_VALID()` - Geometry Validity Check

Count of valid vs invalid geometries. Invalid geometries should be repaired before spatial operations.

**Example:**
```r
forest_stands %>% CHECK_VALID()
# Output: TRUE (1450), FALSE (12) - indicates 12 invalid features
```

#### `MAKE_VALID_POLYS()` - Repair Invalid Geometries

Fix invalid polygon geometries using precision setting and validation. Resolves self-intersections, bow-ties, duplicate vertices.

**Parameters:**
- `.data` - sf object with polygon geometries
- `precision` - Coordinate precision (1000 = meter, 10000 = decimeter)

**Example:**
```r
# Repair geometries with 1-meter precision
forest_layer %>%
  MAKE_VALID_POLYS(precision = 1000)
```

#### `CALC_HA()` - Calculate Hectare Areas

Add a "Hectares" column with area of each polygon feature. Ensure data is in projected CRS for accurate areas.

**Example:**
```r
# Add hectare areas to forest cutblocks
cutblocks_sf %>%
  st_transform(3005) %>%  # BC Albers
  CALC_HA()
# Adds column: Hectares (numeric)
```

---

## ðŸ”§ Workflow Tips

### Complete Workflow: Field Data to Google Earth

```r
# 1. Load and prepare spatial data
library(sf)
library(tidyverse)
source("kml_export_functions.r")

# Read shapefile or database
data <- st_read("field_data.shp")

# 2. Clean and prepare attributes
data_clean <- data %>%
  mutate(
    site_name = str_to_title(site_name),  # Capitalize names
    category = factor(category)  # Ensure categorical
  )

# 3. Export to KMZ for field viewing
EXPORT_SF_TO_THEMED_KML(
  sf_object = data_clean,
  color_field = "category",
  label_field = "site_name",
  output_filename = "field_sites_2026.kmz",
  fill_opacity = 0.6
)

# 4. Share KMZ file - others can open in Google Earth on phone/tablet
```

### Creating Print Maps for Reports

```r
source("georeferenced_map_functions.r")
library(ggplot2)

# 1. Create basemap
basemap <- CREATE_BASEMAP_PLOT(
  largest_data = project_area,
  ext_expansion = 0.2,
  map_service = "esri",
  map_type = "world_topo_map"
)

# 2. Build complete map with layers
report_map <- basemap +
  geom_sf(data = project_area, fill = "lightblue", alpha = 0.3) +
  geom_sf(data = sample_transects, color = "darkgreen", size = 1.5) +
  geom_sf(data = key_locations, color = "red", size = 3) +
  labs(
    title = "Project Survey Design",
    subtitle = "Thompson Region, 2026",
    caption = "BC Ministry of Water, Land and Air Protection"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# 3. Save as georeferenced PDF
ggsave("report_map.pdf", report_map, width = 11, height = 8.5, dpi = 300)
```

### Wildlife Analysis Workflow: GPS Collar Data Processing

```r
library(my_common_functions)
library(sf)
library(tidyverse)

# 1. Load GPS collar point data
collar_data <- st_read("caribou_gps_locations.shp")

# 2. Add biological year for calving season analysis (May 15 start)
collar_data_bio <- collar_data %>%
  ADD_BIO_YEAR(
    date_col = fix_time,
    bio_start_date = "2023-05-15"
  )

# 3. Create movement lines with speed calculations
movement_lines <- collar_data_bio %>%
  MAKE_LINES(
    Group_Var = animal_id,
    Date_Time_Var = fix_time
  )

# 4. Calculate population estimate from aerial survey
survey_results <- tibble(
  herd_name = c("Wells Gray", "Barkerville", "Groundhog"),
  total_seen = c(45, 32, 78),
  collars_deployed = c(8, 6, 12),
  collars_seen = c(3, 2, 5)
) %>%
  LINC_PET_PIPE(
    Herd = herd_name,
    Observed_caribou = total_seen,
    Total_collars = collars_deployed,
    Observed_collars = collars_seen
  )

# 5. Calculate population growth rate
lambda <- CALC_LAMBDA(
  Initial_Pop = survey_results$`Estimated Population Size`[1],
  Final_Pop = 62,  # Next year's estimate
  Time_Span = 1
)

# 6. Export movement lines to KMZ for field visualization
EXPORT_SF_TO_THEMED_KML(
  sf_object = movement_lines,
  color_field = "Travel_Speed_KmH",
  label_field = "animal_id",
  output_filename = "caribou_movements_2026.kmz",
  outline_width = 3
)
```

### Spatial Analysis Workflow: Habitat Overlay

```r
library(my_common_functions)
library(sf)

# 1. Load habitat layers
critical_habitat <- st_read("critical_habitat.shp")
land_ownership <- st_read("land_ownership.shp")

# 2. Validate and repair geometries
habitat_valid <- critical_habitat %>%
  CHECK_VALID() %>%  # Check status first
  print() %>%
  MAKE_VALID_POLYS(precision = 1000)  # Repair if needed

# 3. Perform spatial identity to split by ownership
habitat_by_owner <- habitat_valid %>%
  ARC.IDENT(land_ownership)

# 4. Calculate areas in hectares
habitat_summary <- habitat_by_owner %>%
  st_transform(3005) %>%  # BC Albers for accurate area
  CALC_HA() %>%
  st_drop_geometry() %>%
  group_by(owner_type, habitat_quality) %>%
  summarise(total_hectares = sum(Hectares, na.rm = TRUE))

# 5. Export to KMZ colored by habitat quality
EXPORT_SF_TO_THEMED_KML(
  sf_object = habitat_by_owner,
  color_field = "habitat_quality",
  label_field = "owner_type",
  output_filename = "habitat_by_ownership.kmz",
  fill_opacity = 0.5
)
```

---

## ðŸ› Troubleshooting

### KML Export Issues

**Problem**: Colors not showing in Google Earth
- **Solution**: Make sure `color_field` is specified and contains valid data

**Problem**: Field names have weird prefixes in popup tables
- **Solution**: This is fixed in the current version - field names are cleaned automatically

**Problem**: Labels not appearing
- **Solution**: Specify `label_field` parameter, or ensure your data has character fields for auto-detection

**Problem**: UTF-8 characters (Ã©, Ê”, etc.) showing as garbled text
- **Solution**: Functions automatically preserve UTF-8 encoding - ensure your source data is UTF-8 encoded

### Basemap Issues

**Problem**: Basemap not loading
- **Solution**: Check internet connection - basemaps are downloaded from tile servers

**Problem**: Basemap doesn't cover entire map area
- **Solution**: Increase `ext_expansion` parameter (e.g., `0.3` for 30% buffer)

**Problem**: Low resolution basemap
- **Solution**: Increase `map_res` parameter (higher = more detail, but slower download)

---

## ðŸ“ Notes

- **Coordinate Systems**: KML files always use WGS84 (EPSG:4326). Functions handle transformation automatically.
- **File Sizes**: KMZ files are compressed and smaller than KML. Use KMZ for sharing large datasets.
- **Performance**: For very large datasets (>10,000 features), consider simplifying geometries before export using `st_simplify()`

---

## ðŸ“„ License

Internal use - BC Government

## ðŸ‘¤ Author

Bevan Ernst  
BC Ministry of Water, Land and Air Protection  
Thompson Region

---

## ðŸ”— Related Resources

- [bcdata R package](https://github.com/bcgov/bcdata) - Access BC Government spatial data
- [sf package documentation](https://r-spatial.github.io/sf/) - Spatial data in R
- [Google Earth KML Reference](https://developers.google.com/kml/documentation/kmlreference) - KML format specification

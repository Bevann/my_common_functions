





#' Add Biological Year and Day to a Dataset
#'
#' Calculates biological year and biological day-of-year based on a custom
#' annual cycle start date. Useful for wildlife studies where the biological
#' year starts on a specific date (e.g., calving season) rather than January 1.
#'
#' @param data A data frame or tibble containing date observations
#' @param date_col The name of the column containing dates (unquoted, supports tidy evaluation)
#' @param bio_start_date The start date of the biological year (e.g., "2023-05-15" for May 15)
#'   Only the month and day are used; the year is ignored
#'
#' @return The input data frame with two new columns:
#'   \item{bio_year}{The biological year (integer) that each observation falls into}
#'   \item{bio_day}{The day number within the biological year (1 = first day of bio year)}
#'
#' @examples
#' # For a caribou study where the biological year starts on May 15 (calving season)
#' caribou_data %>%
#'   ADD_BIO_YEAR(date_col = observation_date, bio_start_date = "2023-05-15")
#'
#' @export
ADD_BIO_YEAR <- function(data, date_col, bio_start_date) {
  # --- Standardize bio_start_date ---
  bio_start_date_clean <- as.Date(bio_start_date)
  calving_month <- month(bio_start_date_clean)
  calving_day_of_month <- mday(bio_start_date_clean)
  
  # --- Main Logic using mutate() ---
  data %>%
    mutate(
      # Use {{date_col}} to access the user-specified column.
      # Create a temporary, clean date column to work with.
      .dates_clean = as.Date({{ date_col }}),
      
      # Determine the start date of the biological year for each row.
      .bio_year_start_date = make_date(
        year = if_else(
          # If the observation is before this year's calving day...
          .dates_clean < make_date(year(.dates_clean), calving_month, calving_day_of_month),
          # ...then the bio year started last calendar year.
          year(.dates_clean) - 1,
          # Otherwise, it started this calendar year.
          year(.dates_clean)
        ),
        month = calving_month,
        day = calving_day_of_month
      ),
      
      # Create the final columns.
      bio_year = year(.bio_year_start_date),
      bio_day = as.numeric(.dates_clean - .bio_year_start_date) + 1
    ) %>%
    # Remove the temporary helper columns.
    select(-.dates_clean, -.bio_year_start_date)
}



#' Calculate Population Growth Rate (Lambda)
#'
#' Computes the finite rate of population growth (lambda) from initial and final
#' population sizes over a specified time period. Lambda values > 1 indicate
#' population growth, < 1 indicate decline, and = 1 indicate stable population.
#'
#' @param Initial_Pop Initial population size (numeric)
#' @param Final_Pop Final population size (numeric)
#' @param Time_Span Time period between initial and final counts (numeric, typically in years)
#'
#' @return Numeric value representing lambda (finite rate of population change)
#'   Lambda is calculated as: (Final_Pop / Initial_Pop)^(1 / Time_Span)
#'
#' @examples
#' # Population declined from 150 to 120 over 5 years
#' CALC_LAMBDA(Initial_Pop = 150, Final_Pop = 120, Time_Span = 5)
#' # Returns lambda < 1, indicating decline
#'
#' @export
CALC_LAMBDA <- function(Initial_Pop, Final_Pop, Time_Span) {
  # Calculate the lambda value
  lambda <- (Final_Pop / Initial_Pop)^(1 / Time_Span)
  
  # Return the lambda value
  return(lambda)
  
}


#' Lincoln-Petersen Population Estimate (Pipe-Friendly Version)
#'
#' Calculates population estimates using the Lincoln-Petersen method for
#' mark-recapture studies. This version is designed for use in dplyr pipelines
#' and returns a formatted data frame with confidence intervals. Commonly used
#' for caribou aerial survey analysis where collared animals serve as the
#' "marked" population.
#'
#' @param Data Input data frame containing survey data
#' @param Herd Column name for herd identifier (unquoted)
#' @param Observed_caribou Column name for total number of caribou observed (unquoted)
#' @param Total_collars Column name for total number of collars deployed in herd (unquoted)
#' @param Observed_collars Column name for number of collars observed during survey (unquoted)
#' @param ... Additional arguments (unused, for compatibility)
#'
#' @return A data frame with columns:
#'   \item{Herd Name}{Herd identifier}
#'   \item{Total Caribou Observed}{Number of caribou seen in survey}
#'   \item{Total Collars in Herd}{Total collars deployed}
#'   \item{Observed Collars}{Collars detected in survey}
#'   \item{Estimated Population Size}{Point estimate (N)}
#'   \item{95% Upper}{Upper 95% confidence limit}
#'   \item{95% Lower}{Lower 95% confidence limit (constrained to >= observed caribou)}
#'   \item{Variance}{Variance of the estimate}
#'
#' @examples
#' survey_data %>%
#'   LINC_PET_PIPE(
#'     Herd = herd_name,
#'     Observed_caribou = total_seen,
#'     Total_collars = collars_deployed,
#'     Observed_collars = collars_seen
#'   )
#'
#' @export
LINC_PET_PIPE <- function(Data, Herd=Herd, Observed_caribou=Observed_caribou, Total_collars=Total_collars, Observed_collars=Observed_collars, ...) {
  Data %>%
    mutate(
      N = ((({{ Total_collars }} + 1) * ({{ Observed_caribou }} + 1)) / ({{ Observed_collars }} + 1)) - 1,
      n = (({{ Total_collars }} + 1) * ({{ Observed_caribou }} + 1) * ({{ Total_collars }} - {{ Observed_collars }}) * ({{ Observed_caribou }} - {{ Observed_collars }})),
      d = (({{ Observed_collars }} + 1)^2) * ({{ Observed_collars }} + 2),
      varN = n / d,
      sdN = sqrt(varN),
      CI = 1.96 * sdN,
      `95% Upper` = N + CI,
      `95% Lower` = pmax(N - CI, {{ Observed_caribou }})
    ) %>%
    select(`Herd Name` = {{ Herd }}, `Total Caribou Observed` = {{ Observed_caribou }}, `Total Collars in Herd` = {{ Total_collars }}, `Observed Collars` = {{ Observed_collars }}, `Estimated Population Size` = N, `95% Upper`, `95% Lower`, Variance=varN) %>%
    return(.)
}






#' Lincoln-Petersen Population Estimate (Vector Version)
#'
#' Calculates population estimates using the Lincoln-Petersen method for
#' mark-recapture studies. This version accepts individual numeric values
#' rather than data frame columns, making it suitable for single calculations
#' or use in non-pipeline contexts.
#'
#' @param Observed_caribou Total number of caribou observed during survey (numeric)
#' @param Total_collars Total number of radio collars deployed in the herd (numeric)
#' @param Observed_collars Number of collared animals detected during survey (numeric)
#' @param ... Additional arguments (unused, for compatibility)
#'
#' @return A data frame with one row containing:
#'   \item{Observed_caribou}{Input: total caribou observed}
#'   \item{Total_collars}{Input: total collars deployed}
#'   \item{Observed_collars}{Input: collars observed}
#'   \item{Pop_Estimate}{Estimated population size (N)}
#'   \item{CI_95_Upper}{Upper 95% confidence limit}
#'   \item{CI_95_Lower}{Lower 95% confidence limit}
#'
#' @examples
#' # Survey observed 45 caribou, 8 collars deployed, 3 collars seen
#' CALC_LINC_PET(Observed_caribou = 45, Total_collars = 8, Observed_collars = 3)
#'
#' @export
CALC_LINC_PET <-  function(Observed_caribou, Total_collars, Observed_collars,...){
  
  N = ((({{ Total_collars }} + 1) * ({{ Observed_caribou }} + 1)) / ({{ Observed_collars }} + 1)) - 1
  n = (({{ Total_collars }} + 1) * ({{ Observed_caribou }} + 1) * ({{ Total_collars }} - {{ Observed_collars }}) * ({{ Observed_caribou }} - {{ Observed_collars }}))
  d = (({{ Observed_collars }} + 1)^2) * ({{ Observed_collars }} + 2)
  varN = n / d
  sdN = sqrt(varN)
  CI = 1.96 * sdN
  `95% Upper` = N + CI
  `95% Lower` =N - CI
  
  return(data.frame( Observed_caribou=Observed_caribou,Total_collars=Total_collars, Observed_collars=Observed_collars, Pop_Estimate=N,CI_95_Upper=`95% Upper`,CI_95_Lower=`95% Lower` ))
  
  
}





#' Spatial Identity Operation (ArcGIS Identity Equivalent)
#'
#' Performs a spatial identity operation similar to ArcGIS's Identity tool.
#' Computes the intersection of two layers and preserves the portions of
#' layer_a that do not overlap with layer_b. The output contains all areas
#' from layer_a, with attributes from layer_b where they overlap.
#'
#' This is commonly used for overlay analysis where you want to split polygons
#' in one layer by boundaries in another layer while keeping all original areas.
#'
#' @param layer_a An sf object representing the input layer to be split
#' @param layer_b An sf object representing the overlay layer
#'
#' @return An sf object containing:
#'   - All intersecting areas between layer_a and layer_b (with attributes from both)
#'   - All non-intersecting areas from layer_a (with only layer_a attributes)
#'
#' @examples
#' # Split wildlife range by land ownership to get area by owner
#' caribou_range %>%
#'   ARC.IDENT(land_ownership)
#'
#' @export
ARC.IDENT <- function(layer_a, layer_b) {
  int_a_b <- st_intersection(layer_a, layer_b)
  rest_of_a <- st_difference(layer_a, st_union(layer_b))
  output <- bind_rows(int_a_b, rest_of_a)
  return(st_as_sf(output))
}


#' Check Geometry Types in an SF Object
#'
#' Provides a quick summary count of geometry types present in an sf object.
#' Useful for diagnosing mixed geometry issues or verifying expected geometry types.
#'
#' @param x An sf object to check
#'
#' @return A tibble showing the count of each geometry type (POINT, LINESTRING,
#'   POLYGON, MULTIPOLYGON, etc.)
#'
#' @examples
#' # Check what geometry types are in your spatial layer
#' wildlife_habitat %>% CHECK_GEOMETRY()
#' # Output might show: POLYGON (150), MULTIPOLYGON (3)
#'
#' @export
CHECK_GEOMETRY <- function(x) {
  st_geometry_type(x) %>% fct_count()
}

#' Check Geometry Validity in an SF Object
#'
#' Validates geometries in an sf object and returns a count of valid vs invalid
#' features. Invalid geometries can cause errors in spatial operations and should
#' be repaired with st_make_valid() or MAKE_VALID_POLYS().
#'
#' @param x An sf object to validate
#'
#' @return A tibble showing counts of TRUE (valid) and FALSE (invalid) geometries
#'
#' @examples
#' # Check if geometries are valid before performing spatial operations
#' forest_stands %>% CHECK_VALID()
#' # Output: TRUE (1450), FALSE (12) indicates 12 invalid features
#'
#' @export
CHECK_VALID <-
  function(x) {
    st_is_valid(x) %>%
      as.factor() %>%
      fct_count()
  }

#' Calculate Area in Hectares
#'
#' Adds a column containing the area of each feature in hectares. Works with
#' any sf object containing polygon or multipolygon geometries. Uses the
#' coordinate reference system of the input data, so ensure data is in an
#' appropriate projected CRS (not lat/long) for accurate area calculations.
#'
#' @param .data An sf object with polygon geometries
#'
#' @return The input sf object with an added "Hectares" column containing
#'   the area of each feature in hectares
#'
#' @examples
#' # Add hectare areas to forest cutblocks
#' cutblocks_sf %>%
#'   st_transform(3005) %>%  # BC Albers for accurate area
#'   CALC_HA()
#'
#' @export
CALC_HA <- function(.data) {
  .data %>% mutate(
    Hectares =
      as.numeric(st_area(.)) / 10000
  )
}


#' Repair and Validate Polygon Geometries
#'
#' Fixes invalid polygon geometries using a combination of precision setting,
#' validation, and geometry extraction. This function resolves common geometry
#' issues like self-intersections, bow-ties, and duplicate vertices that can
#' cause spatial operations to fail.
#'
#' The function:
#' 1. Sets coordinate precision to snap nearby vertices
#' 2. Repairs invalid geometries with st_make_valid()
#' 3. Extracts only polygon geometries (drops points/lines)
#' 4. Standardizes output to POLYGON type
#'
#' @param .data An sf object with polygon geometries (possibly invalid)
#' @param precision Numeric precision value for st_set_precision(). Typical values:
#'   1000 (meter precision), 10000 (decimeter), 100000 (centimeter). Higher
#'   values preserve more detail but may not fix precision-related issues.
#'
#' @return An sf object containing only valid POLYGON geometries
#'
#' @examples
#' # Repair geometries with 1-meter precision
#' forest_layer %>%
#'   MAKE_VALID_POLYS(precision = 1000)
#'
#' @export
MAKE_VALID_POLYS <- function(.data, precision) {.data %>%
    st_set_precision(precision) %>%
    
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON") }






#' Create Movement Lines from Point Locations
#'
#' Converts sequential point locations into line segments representing movement
#' paths. Calculates movement metrics including distance, time, and speed between
#' consecutive locations. Commonly used for GPS collar data to visualize and
#' analyze animal movement patterns.
#'
#' Points are connected in chronological order within each group (e.g., individual
#' animal). The function creates line segments between consecutive points and
#' calculates travel metrics for each segment.
#'
#' @param .data An sf object with POINT geometries and timestamp data
#' @param Group_Var Column name for grouping variable (e.g., animal ID, collar ID)
#'   Unquoted, supports tidy evaluation
#' @param Date_Time_Var Column name containing date-time values for ordering points
#'   Unquoted, supports tidy evaluation. Must be POSIXct or similar time class
#'
#' @return An sf object with LINESTRING geometries, one line per movement segment,
#'   containing:
#'   \item{Group_Var}{The grouping variable (animal ID, etc.)}
#'   \item{Date_Time_Var}{End timestamp of the movement segment}
#'   \item{Travel_Length_Km}{Distance traveled in kilometers}
#'   \item{Travel_Time_H}{Time elapsed in hours}
#'   \item{Travel_Speed_KmH}{Speed in km/h (distance/time)}
#'   \item{RECNO}{Sequential record number within each group}
#'
#' @examples
#' # Create movement lines from caribou GPS collar data
#' collar_points %>%
#'   MAKE_LINES(Group_Var = animal_id, Date_Time_Var = fix_time)
#'
#' @export
MAKE_LINES <- function (.data, Group_Var, Date_Time_Var){
  .data %>%
    
    group_by({{Group_Var}}) %>%
    arrange({{Date_Time_Var}}) %>%
    mutate(RECNO=row_number() ,
      geometry_lagged = lag(geometry, default = NA),
      LOCAL_DATE_TIME_LAG=lag({{Date_Time_Var}}, default=NA)
    ) %>%
    # drop the NA row created by lagging
    slice(-1) %>% 
    mutate(
      geometry = st_sfc(purrr::map2(
        .x = geometry, 
        .y = geometry_lagged, 
        .f = ~{st_union(c(.x, .y)) %>% st_cast("LINESTRING")}
      ))) %>%
    select(-geometry_lagged) %>%
    st_set_crs(st_crs(.data)) %>%
    ungroup() %>%
    
    mutate(Travel_Length_Km=st_length({.})/1000, 
           Travel_Time_H=as.numeric(difftime({{Date_Time_Var}},LOCAL_DATE_TIME_LAG, units = "hours")), 
           Travel_Speed_KmH=((as.numeric(Travel_Length_Km)))/(as.numeric(Travel_Time_H))) %>%
    
    select({{Group_Var}}, {{Date_Time_Var}}, Travel_Length_Km, Travel_Time_H, Travel_Speed_KmH,RECNO)
  
}

#' Flatten Overlapping Polygons with Attribute Aggregation
#'
#' Resolves overlapping polygons by creating non-overlapping features and
#' aggregating attributes from the overlapping source polygons. Useful for
#' resolving spatial conflicts or creating union layers where multiple features
#' overlap and you want to preserve information about which features contributed
#' to each output polygon.
#'
#' The function performs st_intersection() internally, which splits overlapping
#' polygons into discrete pieces, then aggregates attributes from the source
#' features that contributed to each piece.
#'
#' @param .data An sf object with polygon geometries (may have overlaps)
#' @param LIST_COL Name of column to concatenate into a semicolon-separated list
#'   for overlapping features (e.g., list all overlapping zone names)
#' @param MAX_COL Name of column to take the maximum value from when features overlap
#' @param FIRST_COL Name of column to take the first value from when features overlap
#'
#' @return An sf object with non-overlapping polygon geometries and three new columns:
#'   \item{[LIST_COL]_LIST}{Semicolon-separated list of values from overlapping features}
#'   \item{[FIRST_COL]_FIRST}{First value from overlapping features}
#'   \item{[MAX_COL]_MAX}{Maximum value from overlapping features}
#'
#' @note Column names are constructed by appending "_LIST", "_FIRST", and "_MAX"
#'   to the input column names
#'
#' @examples
#' # Flatten overlapping habitat polygons, listing all habitat types
#' habitat_layers %>%
#'   FLATTEN_POLYS(
#'     LIST_COL = "habitat_type",
#'     MAX_COL = "quality_score",
#'     FIRST_COL = "priority"
#'   )
#'
#' @export
FLATTEN_POLYS <- function (.data, LIST_COL, MAX_COL, FIRST_COL) {
  LIST_NAME <-paste0(LIST_COL,"_LIST") 
  MAX_NAME <-paste0(MAX_COL,"_MAX") 
  FIRST_NAME <-paste0(FIRST_COL,"_FIRST") 
  st_intersection(.data) %>%
    st_collection_extract("POLYGON") %>%
    
    mutate(LIST_NAME= map_chr(origins,  ~ paste0(.data$LIST_COL[.], collapse = "; ")),
           FIRST_NAME= map_chr(origins,  ~ first(.data$FIRST_COL[.])),
           MAX_NAME= map_chr(origins,  ~ max(.data$MAX_COL[.])))
  
}


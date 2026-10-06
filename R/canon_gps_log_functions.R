# Functions for importing and exporting Canon camera GPS logs (NMEA .LOG files)
# Created: October 6, 2026
# Parses $GPGGA (position/time) and $GPRMC (date) sentences into sf points,
# builds daily and per-time-step lines, and exports to files or file geodatabases.


# Helper functions: NMEA coordinate conversion------

#' Convert NMEA latitude to decimal degrees
#'
#' Converts NMEA \code{ddmm.mmmm} latitude values to decimal degrees.
#'
#' @param lat_raw Character or numeric vector of NMEA latitudes (\code{ddmm.mmmm}).
#' @param lat_dir Character vector of hemispheres (\code{"N"} or \code{"S"}).
#'   \code{"S"} returns negative values.
#'
#' @return Numeric vector of latitudes in decimal degrees.
#' @keywords internal
#' @noRd
convert_lat <- function(lat_raw, lat_dir) {
  lat_raw_num <- as.numeric(lat_raw)
  lat_deg <- floor(lat_raw_num / 100)
  lat_min <- lat_raw_num %% 100
  lat_dd <- lat_deg + lat_min / 60
  ifelse(lat_dir == "S", -lat_dd, lat_dd)
}

#' Convert NMEA longitude to decimal degrees
#'
#' Converts NMEA \code{dddmm.mmmm} longitude values to decimal degrees.
#'
#' @param long_raw Character or numeric vector of NMEA longitudes (\code{dddmm.mmmm}).
#' @param long_dir Character vector of hemispheres (\code{"E"} or \code{"W"}).
#'   \code{"W"} returns negative values.
#'
#' @return Numeric vector of longitudes in decimal degrees.
#' @keywords internal
#' @noRd
convert_long <- function(long_raw, long_dir) {
  long_raw_num <- as.numeric(long_raw)
  long_deg <- floor(long_raw_num / 100)
  long_min <- long_raw_num %% 100
  long_dd <- long_deg + long_min / 60
  ifelse(long_dir == "W", -long_dd, long_dd)
}

# Helper function: write an sf object to a file or geodatabase------
WRITE_LOG_OUTPUT <- function(OUTPUT_SF, OUTPUT_FILE, OUTPUT_DIR, OUTPUT_FORMAT) {
  if (grepl("\\.gdb/?$", OUTPUT_DIR)) {
    OUTPUT_NAME <- tools::file_path_sans_ext(basename(OUTPUT_FILE))
    sf::st_write(OUTPUT_SF, OUTPUT_DIR, layer = OUTPUT_NAME, append = FALSE)
  } else {
    sf::st_write(
      OUTPUT_SF,
      paste0(OUTPUT_DIR, "/", OUTPUT_FILE, OUTPUT_FORMAT),
      append = FALSE
    )
  }
}


#' Import a Canon camera GPS log as sf points
#'
#' Reads a Canon camera GPS log (NMEA sentences). Positions and times come from
#' \code{$GPGGA} sentences; the date comes from the matching \code{$GPRMC}
#' sentence (joined on the time stamp) because \code{$GPGGA} carries time only.
#' The local time zone is looked up for every point with
#' \code{lutz::tz_lookup_coords(method = "accurate")} and the most common time
#' zone is used to convert the whole log from UTC to local time.
#'
#' @param INPUT_FILE Path to the camera GPS \code{.LOG} file.
#'
#' @return An sf POINT object (EPSG:4326) with columns \code{date_time_UTC},
#'   \code{DATE_TIME_LOCAL}, \code{Date} (local date) and \code{Time}
#'   (local \code{HH:MM:SS}).
#'
#' @details Stops with an error if a time zone cannot be determined for any
#'   point, or if no single time zone is the most common (for example a log
#'   split evenly across two zones).
#'
#' @examples
#' \dontrun{
#' pts <- IMPORT_LOG_POINTS("26092300.LOG")
#' }
#'
#' @export
IMPORT_LOG_POINTS <- function(INPUT_FILE) {
  sentences <- data.frame(sentence = readr::read_lines(INPUT_FILE)) %>%
    tidyr::separate_wider_delim(
      sentence,
      ",",
      names_sep = "_",
      too_few = "align_start"
    )

  # GGA sentences carry only the time (hhmmss.sss); the date (ddmmyy) is in RMC
  rmc_dates <- sentences %>%
    dplyr::filter(sentence_1 == "$GPRMC") %>%
    dplyr::select(sentence_2, rmc_date = sentence_10) %>%
    dplyr::distinct(sentence_2, .keep_all = TRUE)

  sentences %>%
    dplyr::filter(sentence_1 == "$GPGGA") %>%
    dplyr::left_join(rmc_dates, by = "sentence_2") %>%
    dplyr::mutate(lat_dir = sentence_4, long_dir = sentence_6) %>%
    dplyr::mutate(
      lat_dd = convert_lat(sentence_3, lat_dir = lat_dir),
      long_dd = convert_long(sentence_5, long_dir = long_dir),
      date_time_UTC = as.POSIXct(strptime(
        paste(rmc_date, sentence_2),
        format = "%d%m%y %H%M%OS",
        tz = "UTC"
      ))
    ) %>%
    dplyr::mutate(
      time_zone = lutz::tz_lookup_coords(
        lat = lat_dd,
        lon = long_dd,
        method = "accurate"
      )
    ) %>%
    dplyr::mutate(DATE_TIME_LOCAL = {
      if (anyNA(time_zone)) {
        stop("Unable to determine the time zone for one or more GPS points.")
      }
      time_zone_counts <- table(time_zone)
      majority_time_zones <- names(time_zone_counts)[
        time_zone_counts == max(time_zone_counts)
      ]
      if (length(majority_time_zones) != 1) {
        stop("GPS points do not have a single most common time zone.")
      }
      lubridate::with_tz(date_time_UTC, tzone = majority_time_zones)
    }) %>%
    dplyr::select(lat_dd, long_dd, date_time_UTC, DATE_TIME_LOCAL) %>%
    sf::st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326) %>%
    dplyr::mutate(
      Date = as.Date(DATE_TIME_LOCAL),
      Time = format(DATE_TIME_LOCAL, "%H:%M:%S")
    )
}


#' Make one line per day from camera GPS points
#'
#' Orders points by local time and connects them into one LINESTRING per
#' local date.
#'
#' @param IMPORT_POINTS sf POINT object from \code{\link{IMPORT_LOG_POINTS}}.
#'
#' @return An sf LINESTRING object with one row per \code{Date}.
#'
#' @examples
#' \dontrun{
#' daily <- IMPORT_LOG_POINTS("26092300.LOG") %>% MAKE_DAILY_LINES()
#' }
#'
#' @export
MAKE_DAILY_LINES <- function(IMPORT_POINTS) {
  IMPORT_POINTS %>%
    dplyr::group_by(Date) %>%
    dplyr::arrange(DATE_TIME_LOCAL) %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING")
}


#' Make one line segment per time step from camera GPS points
#'
#' Creates a two-point LINESTRING between each point and the next point on the
#' same local date, so every GPS time step becomes a separate feature.
#' Segments never cross midnight. Output is transformed to BC Albers
#' (EPSG:3005) so lengths and speeds are in metric units.
#'
#' @param IMPORT_POINTS sf POINT object from \code{\link{IMPORT_LOG_POINTS}}.
#'
#' @return An sf LINESTRING object (EPSG:3005) with columns \code{Date},
#'   \code{START_TIME_LOCAL}, \code{END_TIME_LOCAL}, \code{DATE_TIME_LOCAL},
#'   \code{Duration_Minutes}, \code{LENGTH_METERS}, \code{LENGTH_KM} and
#'   \code{SPEED_KMH}.
#'
#' @examples
#' \dontrun{
#' detailed <- IMPORT_LOG_POINTS("26092300.LOG") %>% MAKE_DETAILED_LINES()
#' }
#'
#' @export
MAKE_DETAILED_LINES <- function(IMPORT_POINTS) {
  crs <- sf::st_crs(IMPORT_POINTS)

  coords <- sf::st_coordinates(IMPORT_POINTS)

  segments <- IMPORT_POINTS %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(x = coords[, 1], y = coords[, 2]) %>%
    dplyr::arrange(DATE_TIME_LOCAL) %>%
    dplyr::group_by(Date) %>%
    dplyr::mutate(
      START_TIME_LOCAL = DATE_TIME_LOCAL,
      END_TIME_LOCAL = dplyr::lead(DATE_TIME_LOCAL),
      x_end = dplyr::lead(x),
      y_end = dplyr::lead(y)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(END_TIME_LOCAL))

  lines <- purrr::pmap(
    list(segments$x, segments$y, segments$x_end, segments$y_end),
    function(x1, y1, x2, y2) {
      sf::st_linestring(matrix(c(x1, x2, y1, y2), ncol = 2))
    }
  )

  segments %>%
    dplyr::select(Date, START_TIME_LOCAL, END_TIME_LOCAL, DATE_TIME_LOCAL) %>%
    dplyr::mutate(geometry = sf::st_sfc(lines, crs = crs)) %>%
    sf::st_as_sf() %>%
    sf::st_transform(3005) %>%
    dplyr::mutate(
      Duration_Minutes = as.numeric(difftime(
        END_TIME_LOCAL,
        START_TIME_LOCAL,
        units = "mins"
      )),
      LENGTH_METERS = sf::st_length(.),
      LENGTH_KM = as.numeric(LENGTH_METERS / 1000),
      SPEED_KMH = as.numeric(LENGTH_KM / (Duration_Minutes / 60))
    )
}


#' Export camera GPS log points
#'
#' Imports a camera GPS log with \code{\link{IMPORT_LOG_POINTS}} and writes the
#' points to a file or to a file geodatabase.
#'
#' @param INPUT_FILE Path to the camera GPS \code{.LOG} file.
#' @param OUTPUT_FILE Output file name without extension, or the layer name when
#'   writing to a geodatabase (any extension is stripped).
#' @param OUTPUT_DIR Output directory, or a path ending in \code{.gdb} to write
#'   a feature class into a file geodatabase (requires the GDAL OpenFileGDB
#'   driver).
#' @param OUTPUT_FORMAT File extension including the dot (for example
#'   \code{".shp"}, \code{".kml"}, \code{".gpkg"}). Ignored for geodatabases.
#'
#' @return Invisibly returns the result of \code{sf::st_write()}; called for its
#'   side effect of writing the output.
#'
#' @examples
#' \dontrun{
#' EXPORT_LOG_POINTS("26092300.LOG", "LOG_POINTS", "C:/TEMP", ".shp")
#' EXPORT_LOG_POINTS("26092300.LOG", "LOG_POINTS", "Output.gdb", "")
#' }
#'
#' @export
EXPORT_LOG_POINTS <- function(
  INPUT_FILE,
  OUTPUT_FILE,
  OUTPUT_DIR,
  OUTPUT_FORMAT
) {
  OUTPUT_POINTS <- IMPORT_LOG_POINTS(INPUT_FILE)
  WRITE_LOG_OUTPUT(OUTPUT_POINTS, OUTPUT_FILE, OUTPUT_DIR, OUTPUT_FORMAT)
}


#' Export camera GPS log as daily lines
#'
#' Imports a camera GPS log, builds one line per local date with
#' \code{\link{MAKE_DAILY_LINES}} and writes it to a file or file geodatabase.
#'
#' @inheritParams EXPORT_LOG_POINTS
#'
#' @return Invisibly returns the result of \code{sf::st_write()}; called for its
#'   side effect of writing the output.
#'
#' @examples
#' \dontrun{
#' EXPORT_LOG_LINES_DATE("26092300.LOG", "LOG_DAILY", "C:/TEMP", ".kml")
#' }
#'
#' @export
EXPORT_LOG_LINES_DATE <- function(
  INPUT_FILE,
  OUTPUT_FILE,
  OUTPUT_DIR,
  OUTPUT_FORMAT
) {
  OUTPUT_LINES_DATE <- IMPORT_LOG_POINTS(INPUT_FILE) %>% MAKE_DAILY_LINES()
  WRITE_LOG_OUTPUT(OUTPUT_LINES_DATE, OUTPUT_FILE, OUTPUT_DIR, OUTPUT_FORMAT)
}


#' Export camera GPS log as per-time-step lines
#'
#' Imports a camera GPS log, builds one segment per time step with
#' \code{\link{MAKE_DETAILED_LINES}} (including length and speed) and writes it
#' to a file or file geodatabase.
#'
#' @inheritParams EXPORT_LOG_POINTS
#'
#' @return Invisibly returns the result of \code{sf::st_write()}; called for its
#'   side effect of writing the output.
#'
#' @examples
#' \dontrun{
#' EXPORT_LOG_LINES_DETAILED("26092300.LOG", "LOG_DETAILED", "C:/TEMP", ".shp")
#' }
#'
#' @export
EXPORT_LOG_LINES_DETAILED <- function(
  INPUT_FILE,
  OUTPUT_FILE,
  OUTPUT_DIR,
  OUTPUT_FORMAT
) {
  OUTPUT_LINES_DETAILED <- IMPORT_LOG_POINTS(INPUT_FILE) %>%
    MAKE_DETAILED_LINES()
  WRITE_LOG_OUTPUT(OUTPUT_LINES_DETAILED, OUTPUT_FILE, OUTPUT_DIR, OUTPUT_FORMAT)
}

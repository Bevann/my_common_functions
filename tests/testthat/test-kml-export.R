make_test_points <- function() {
  sf::st_as_sf(
    data.frame(
      label = c("A & <B>", "Second"),
      category = c("one", NA),
      description = c("Original", "Other"),
      value = c(1, 2),
      x = c(-120, -121), y = c(50, 51)
    ),
    coords = c("x", "y"), crs = 4326
  )
}

read_test_export <- function(x, extension = "kml", ...) {
  output <- tempfile(fileext = paste0(".", extension))
  on.exit(unlink(output), add = TRUE)
  suppressWarnings(suppressMessages(
    EXPORT_SF_TO_THEMED_KML(x, basename(output), dirname(output), ...)
  ))
  if (extension == "kmz") {
    contents <- utils::unzip(output, list = TRUE)
    expect_equal(contents$Name, "doc.kml")
    connection <- unz(output, "doc.kml", open = "r")
    on.exit(close(connection), add = TRUE)
    xml2::read_xml(paste(readLines(connection, warn = FALSE), collapse = "\n"))
  } else {
    xml2::read_xml(output)
  }
}

check_hover_styles <- function(doc, expected_colors = NULL) {
  ns <- xml2::xml_ns(doc)
  placemarks <- xml2::xml_find_all(doc, "//d1:Placemark", ns)
  for (i in seq_along(placemarks)) {
    map_id <- substring(xml2::xml_text(xml2::xml_find_first(
      placemarks[[i]], "./d1:styleUrl", ns
    )), 2)
    map <- xml2::xml_find_first(doc, sprintf("//d1:StyleMap[@id='%s']", map_id), ns)
    expect_equal(xml2::xml_attr(map, "id"), map_id)
    for (state in c("normal", "highlight")) {
      style_id <- substring(xml2::xml_text(xml2::xml_find_first(
        map, sprintf("./d1:Pair[d1:key='%s']/d1:styleUrl", state), ns
      )), 2)
      style <- xml2::xml_find_first(doc, sprintf("//d1:Style[@id='%s']", style_id), ns)
      expect_equal(xml2::xml_text(xml2::xml_find_first(style, "./d1:LabelStyle/d1:scale", ns)),
                   if (state == "normal") "0" else "1")
      if (!is.null(expected_colors) && !is.na(expected_colors[i])) {
        expect_equal(xml2::xml_text(xml2::xml_find_first(style, "./d1:IconStyle/d1:color", ns)),
                     expected_colors[i])
      }
    }
  }
}

test_that("themed labels and selected popups retain their own values", {
  doc <- read_test_export(
    make_test_points(), color_field = "category", color_palette = "red",
    label_field = "label", popup = c("value", "label")
  )
  ns <- xml2::xml_ns(doc)
  placemarks <- xml2::xml_find_all(doc, "//d1:Placemark", ns)
  expect_equal(xml2::xml_text(xml2::xml_find_all(doc, "//d1:Placemark/d1:name", ns)),
               c("A & <B>", "Second"))
  descriptions <- xml2::xml_text(xml2::xml_find_all(doc, "//d1:Placemark/d1:description", ns))
  expect_equal(descriptions[1],
               '<table border="1" cellspacing="0" cellpadding="4"><tr><th>value</th><td>1</td></tr><tr><th>label</th><td>A &amp; &lt;B&gt;</td></tr></table>')
  expect_false(grepl("Original|category", descriptions[1]))
  expect_length(placemarks, 2)
  check_hover_styles(doc, c("990000FF", NA))
})

test_that("default hover labels use the color field or the first character field", {
  x <- make_test_points()
  doc <- read_test_export(x, color_field = "category", color_palette = "red")
  expect_equal(xml2::xml_text(xml2::xml_find_all(doc, "//d1:Placemark/d1:name", xml2::xml_ns(doc))),
               c("one", ""))

  doc <- read_test_export(x, color_field = "value")
  expect_equal(xml2::xml_text(xml2::xml_find_all(doc, "//d1:Placemark/d1:name", xml2::xml_ns(doc))),
               c("1", "2"))

  doc <- read_test_export(x)
  expect_equal(xml2::xml_text(xml2::xml_find_all(doc, "//d1:Placemark/d1:name", xml2::xml_ns(doc))),
               c("A & <B>", "Second"))
})

test_that("default popups include all attributes and labels work without theming", {
  doc <- read_test_export(make_test_points(), label_field = "label")
  ns <- xml2::xml_ns(doc)
  description <- xml2::xml_text(xml2::xml_find_first(doc, "//d1:Placemark/d1:description", ns))
  expect_match(description, "<th>label</th>", fixed = TRUE)
  expect_match(description, "<th>category</th>", fixed = TRUE)
  expect_match(description, "<th>description</th><td>Original</td>", fixed = TRUE)
  expect_match(description, "<th>value</th>", fixed = TRUE)
  check_hover_styles(doc)

  empty <- read_test_export(make_test_points(), popup = character(0))
  expect_equal(xml2::xml_text(xml2::xml_find_first(empty, "//d1:Placemark/d1:description",
                                                  xml2::xml_ns(empty))),
               '<table border="1" cellspacing="0" cellpadding="4"></table>')
})

test_that("missing labels are blank and data without attributes gets feature labels", {
  x <- make_test_points()
  x$label[1] <- NA_character_
  doc <- read_test_export(x, label_field = "label")
  ns <- xml2::xml_ns(doc)
  expect_equal(xml2::xml_text(xml2::xml_find_first(doc, "//d1:Placemark/d1:name", ns)), "")

  x <- x["geometry"]
  doc <- read_test_export(x)
  ns <- xml2::xml_ns(doc)
  expect_equal(xml2::xml_text(xml2::xml_find_all(doc, "//d1:Placemark/d1:name", ns)),
               c("Feature_1", "Feature_2"))
  expect_equal(xml2::xml_text(xml2::xml_find_first(doc, "//d1:Placemark/d1:description", ns)),
               '<table border="1" cellspacing="0" cellpadding="4"></table>')
})

test_that("KMZ uses the same labels and popup selection", {
  skip_if(Sys.which("zip") == "")
  doc <- read_test_export(make_test_points(), "kmz", label_field = "label", popup = "value")
  ns <- xml2::xml_ns(doc)
  expect_equal(xml2::xml_text(xml2::xml_find_first(doc, "//d1:Placemark/d1:description", ns)),
               '<table border="1" cellspacing="0" cellpadding="4"><tr><th>value</th><td>1</td></tr></table>')
  check_hover_styles(doc)
})

test_that("line and polygon styles survive hover highlighting", {
  coords <- matrix(c(-120, 50, -121, 50, -121, 51, -120, 50), ncol = 2, byrow = TRUE)
  for (geom in list(sf::st_linestring(coords), sf::st_polygon(list(coords)))) {
    x <- sf::st_sf(category = "one", geometry = sf::st_sfc(geom, crs = 4326))
    doc <- read_test_export(x, color_field = "category", color_palette = "red")
    ns <- xml2::xml_ns(doc)
    check_hover_styles(doc)
    expect_length(xml2::xml_find_all(doc, "//d1:Placemark/d1:MultiGeometry/d1:Point", ns), 1)
    expect_length(xml2::xml_find_all(doc, "//d1:Placemark/d1:MultiGeometry/d1:LineString | //d1:Placemark/d1:MultiGeometry/d1:Polygon", ns), 1)
    coordinates <- xml2::xml_text(xml2::xml_find_first(
      doc, "//d1:Placemark/d1:MultiGeometry/d1:Point/d1:coordinates", ns
    ))
    point <- sf::st_sfc(sf::st_point(as.numeric(strsplit(coordinates, ",", fixed = TRUE)[[1]])),
                        crs = 4326)
    expect_lt(as.numeric(sf::st_distance(point, sf::st_geometry(x))), 0.01)
    for (state in c("normal", "highlight")) {
      style <- xml2::xml_find_first(
        doc, sprintf("//d1:Style[@id='hover_label_map_1_%s']", state), ns
      )
      expect_equal(xml2::xml_text(xml2::xml_find_first(style, "./d1:LineStyle/d1:width", ns)), "2")
      expect_equal(xml2::xml_text(xml2::xml_find_first(style, "./d1:LineStyle/d1:color", ns)), "E60000FF")
      expect_equal(xml2::xml_text(xml2::xml_find_first(style, "./d1:IconStyle/d1:Icon/d1:href", ns)), "")
      if (inherits(geom, "POLYGON")) {
        expect_equal(xml2::xml_text(xml2::xml_find_first(style, "./d1:PolyStyle/d1:color", ns)), "990000FF")
      }
    }
  }
})

test_that("label points do not change icons on point features", {
  polygon <- sf::st_polygon(list(rbind(c(-120, 50), c(-121, 50),
                                      c(-121, 51), c(-120, 50))))
  x <- sf::st_sf(category = c("one", "one"),
                 geometry = sf::st_sfc(sf::st_point(c(-120, 50)), polygon, crs = 4326))
  doc <- read_test_export(x, color_field = "category", color_palette = "red")
  ns <- xml2::xml_ns(doc)
  placemarks <- xml2::xml_find_all(doc, "//d1:Placemark", ns)
  ids <- substring(xml2::xml_text(xml2::xml_find_all(doc, "//d1:Placemark/d1:styleUrl", ns)), 2)
  expect_false(identical(ids[1], ids[2]))
  expect_length(xml2::xml_find_all(placemarks[[1]], "./d1:Point", ns), 1)
  expect_length(xml2::xml_find_all(placemarks[[2]], "./d1:MultiGeometry/d1:Point", ns), 1)
  for (i in seq_along(ids)) {
    href <- xml2::xml_text(xml2::xml_find_first(
      doc, sprintf("//d1:Style[@id='%s_normal']/d1:IconStyle/d1:Icon/d1:href", ids[i]), ns
    ))
    if (i == 1) expect_match(href, "wht-blank.png", fixed = TRUE)
    if (i == 2) expect_equal(href, "")
  }
})

test_that("popup fields must be existing non-geometry attributes", {
  x <- make_test_points()
  expect_error(EXPORT_SF_TO_THEMED_KML(x, "unused.kml", popup = "missing"), "popup")
  expect_error(EXPORT_SF_TO_THEMED_KML(x, "unused.kml", popup = "geometry"), "popup")
  expect_error(EXPORT_SF_TO_THEMED_KML(x, "unused.kml", popup = NA_character_), "popup")
})

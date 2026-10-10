# tests/testthat/test-dsr_map.R
#
# Tests for the DSR Calculator's maps: boundaries read from a fake ABS
# boundary service, polygons from GeoJSON, the area at a point and the map
# itself. The last test downloads real boundaries and is skipped when the ABS
# boundary service cannot be reached.

library(testthat)
library(actepir)

# ── Fixtures ────────────────────────────────────────────────────────────────

# GeoJSON features as .dsr_fetch_json() parses them
features_from <- function(json) {
  jsonlite::fromJSON(json, simplifyVector = FALSE)$features
}

square_ring <- function(x0, y0, size = 1) {
  sprintf("[[%s,%s],[%s,%s],[%s,%s],[%s,%s],[%s,%s]]",
          x0, y0, x0 + size, y0, x0 + size, y0 + size, x0, y0 + size, x0, y0)
}

area_features <- function(codes, names, code_field, name_field) {
  features_from(paste0('{"type":"FeatureCollection","features":[', paste(
    sprintf(paste0('{"type":"Feature","properties":{"%s":"%s","%s":"%s"},',
                   '"geometry":{"type":"Polygon","coordinates":[%s]}}'),
            code_field, codes, name_field, names,
            square_ring(seq_along(codes) - 1, 0)),
    collapse = ","), "]}"))
}

# A fake ArcGIS layer that answers the description, object ID and feature
# requests .dsr_boundaries() makes, and records them
fake_layer <- function(fields, features) {
  urls <- character(0)
  fetch <- function(url) {
    urls <<- c(urls, url)
    if (grepl("?f=json", url, fixed = TRUE)) {
      return(list(fields = lapply(fields, function(f) list(name = f))))
    }
    q <- strsplit(sub(".*\\?", "", url), "&", fixed = TRUE)[[1]]
    params <- stats::setNames(
      vapply(sub("^[^=]*=", "", q), utils::URLdecode, character(1)),
      sub("=.*", "", q)
    )
    if (identical(unname(params["returnIdsOnly"]), "true")) {
      return(list(objectIdFieldName = "objectid",
                  objectIds = as.list(seq_along(features))))
    }
    ids <- as.integer(strsplit(params[["objectIds"]], ",", fixed = TRUE)[[1]])
    list(type = "FeatureCollection", features = features[ids])
  }
  list(fetch = fetch, urls = function() urls)
}

# Area A is a square with a square hole, B has two parts and C no geometry
shapes <- features_from('{"features":[
  {"properties":{"code":"A","name":"Alpha"},
   "geometry":{"type":"Polygon","coordinates":[
     [[0,0],[4,0],[4,4],[0,4],[0,0]],[[1,1],[2,1],[2,2],[1,2],[1,1]]]}},
  {"properties":{"code":"B","name":null},
   "geometry":{"type":"MultiPolygon","coordinates":[
     [[[5,0],[6,0],[6,1],[5,1],[5,0]]],[[[7,0],[8,0],[8,1],[7,1],[7,0]]]]}},
  {"properties":{"code":"C","name":"Gamma"},"geometry":null}
]}')

# ── Boundaries ──────────────────────────────────────────────────────────────

test_that(".dsr_boundaries reads a layer's areas in batches and caches them", {

  codes <- as.character(80000 + 1:450)
  layer <- fake_layer(
    c("objectid", "sa3_code_2021", "sa3_name_2021", "state_code_2021", "shape"),
    area_features(codes, paste("Area", 1:450), "sa3_code_2021", "sa3_name_2021")
  )
  local_mocked_bindings(.dsr_fetch_json = layer$fetch)
  cache <- withr::local_tempdir()

  b <- .dsr_boundaries("sa3", "ACT", cache = cache)
  expect_named(b, c("Area", "Name", "x", "y", "group", "subgroup"))
  expect_equal(unique(b$Area), codes)
  expect_equal(b$Name[1], "Area 1")
  expect_equal(nrow(b), 450 * 5)

  urls <- layer$urls()
  expect_match(urls[1], "/ASGS2021/SA3/MapServer/0?f=json", fixed = TRUE)
  expect_match(urls[2], "where=state_code_2021%20%3D%20%278%27&", fixed = TRUE)
  expect_match(urls[2], "returnIdsOnly=true", fixed = TRUE)
  pages <- urls[grepl("objectIds=", urls, fixed = TRUE)]
  expect_length(pages, 3)
  expect_match(pages[1], "outFields=sa3_code_2021%2Csa3_name_2021&", fixed = TRUE)
  expect_match(pages[1], "outSR=4326&", fixed = TRUE)
  expect_match(pages[1], "maxAllowableOffset=0.0002&", fixed = TRUE)
  expect_match(pages[1], "f=geojson", fixed = TRUE)

  # The second time comes from the cache
  expect_true(file.exists(file.path(cache, "asgs2021_sa3_ACT_v1.rds")))
  expect_equal(.dsr_boundaries("sa3", "ACT", cache = cache), b)
  expect_length(layer$urls(), length(urls))

})

test_that("state boundaries take abbreviations and Australia is not filtered", {

  features <- c(
    area_features(c("1", "8"), c("New South Wales", "Australian Capital Territory"),
                  "STATE_CODE_2021", "STATE_NAME_2021"),
    features_from('{"features":[{"type":"Feature","properties":
      {"STATE_CODE_2021":"Z","STATE_NAME_2021":"Outside Australia"},
      "geometry":null}]}')
  )
  layer <- fake_layer(c("OBJECTID", "STATE_CODE_2021", "STATE_NAME_2021"),
                      features)
  local_mocked_bindings(.dsr_fetch_json = layer$fetch)

  b <- .dsr_boundaries("state", "AUS", cache = NULL)
  expect_equal(unique(b$Area), c("NSW", "ACT"))
  expect_equal(unique(b$Name), c("New South Wales", "Australian Capital Territory"))

  urls <- layer$urls()
  expect_match(urls[1], "/ASGS2021/STE/MapServer/0?f=json", fixed = TRUE)
  expect_match(urls[2], "where=1%3D1&", fixed = TRUE)
  expect_match(urls[3], "outFields=STATE_CODE_2021%2CSTATE_NAME_2021&", fixed = TRUE)
  expect_match(urls[3], "maxAllowableOffset=0.002&", fixed = TRUE)

})

test_that("field names are read from the layer", {

  layer <- fake_layer(
    c("FID", "SA2_CODE21", "SA2_NAME21", "STE_CODE21"),
    area_features("801011001", "Acton", "SA2_CODE21", "SA2_NAME21")
  )
  local_mocked_bindings(.dsr_fetch_json = layer$fetch)
  b <- .dsr_boundaries("sa2", "ACT", cache = NULL)
  expect_equal(unique(b$Area), "801011001")
  expect_match(layer$urls()[2], "where=STE_CODE21%20%3D%20%278%27", fixed = TRUE)

  layer <- fake_layer(c("objectid", "sa2_code_2021"),
                      area_features("801011001", "Acton", "sa2_code_2021", "x"))
  local_mocked_bindings(.dsr_fetch_json = layer$fetch)
  expect_error(.dsr_boundaries("sa2", "AUS", cache = NULL),
               "no field matching .* Its fields are: objectid, sa2_code_2021")

  layer <- fake_layer(c("sa2_code_2021", "sa2_name_2021"), list())
  local_mocked_bindings(.dsr_fetch_json = layer$fetch)
  expect_error(.dsr_boundaries("sa2", "AUS", cache = NULL), "returned no areas")

})

test_that("the ACT and surrounds take the ACT and the bordering SA3s", {

  surrounds <- paste0(" OR sa2_code_2021 LIKE '", c("10102", "10103", "10106",
                                                    "11302"), "%'",
                      collapse = "")
  layer <- fake_layer(
    c("objectid", "sa2_code_2021", "sa2_name_2021", "state_code_2021"),
    area_features(c("801011001", "101021007"), c("Acton", "Braidwood"),
                  "sa2_code_2021", "sa2_name_2021")
  )
  local_mocked_bindings(.dsr_fetch_json = layer$fetch)
  b <- .dsr_boundaries("sa2", "SURROUNDS", cache = NULL)
  expect_equal(unique(b$Area), c("801011001", "101021007"))
  expect_match(utils::URLdecode(layer$urls()[2]),
               paste0("where=state_code_2021 = '8'", surrounds, "&"),
               fixed = TRUE)

  # States come from the SA3s: the ACT, and the NSW surrounds as one area
  layer <- fake_layer(
    c("objectid", "sa3_code_2021", "sa3_name_2021", "state_code_2021"),
    area_features(c("80101", "80104", "10102"),
                  c("Belconnen", "Gungahlin", "Queanbeyan"),
                  "sa3_code_2021", "sa3_name_2021")
  )
  local_mocked_bindings(.dsr_fetch_json = layer$fetch)
  b <- .dsr_boundaries("state", "SURROUNDS", cache = NULL)
  expect_match(layer$urls()[1], "/ASGS2021/SA3/MapServer/0?f=json", fixed = TRUE)
  expect_equal(unique(b$Area), c("ACT", "NSW (surrounds)"))
  expect_equal(unique(b$Name), c("ACT", "NSW (surrounds)"))
  expect_length(unique(b$group), 3)
  expect_equal(.dsr_point_area(1.5, 0.5, b), "ACT")
  expect_equal(.dsr_point_area(2.5, 0.5, b), "NSW (surrounds)")

})

test_that(".dsr_fetch_json raises the service's errors and failed downloads", {

  dir <- withr::local_tempdir()
  file_url <- function(name) {
    path <- normalizePath(file.path(dir, name), winslash = "/", mustWork = FALSE)
    paste0("file://", if (!startsWith(path, "/")) "/", path)
  }

  writeLines('{"fields": [{"name": "sa2_code_2021"}]}', file.path(dir, "ok.json"))
  expect_equal(.dsr_fetch_json(file_url("ok.json"))$fields[[1]]$name,
               "sa2_code_2021")

  writeLines('{"error": {"code": 400, "message": "Invalid query parameters.",
              "details": ["Unable to perform query."]}}',
             file.path(dir, "error.json"))
  expect_error(.dsr_fetch_json(file_url("error.json")),
               "The ABS boundary service replied: Invalid query parameters. Unable to perform query.",
               fixed = TRUE)

  expect_error(.dsr_fetch_json(file_url("missing.json")))

})

test_that("query strings encode every reserved character", {

  expect_equal(.dsr_query_string(list(where = "state_code_2021 = '8'",
                                      outFields = "a,b", f = "json")),
               "where=state_code_2021%20%3D%20%278%27&outFields=a%2Cb&f=json")

})

# ── Polygons ────────────────────────────────────────────────────────────────

test_that("GeoJSON polygons keep their holes and parts", {

  p <- .dsr_geojson_polygons(shapes, "code", "name")
  expect_equal(unique(p$Area), c("A", "B"))
  expect_equal(unique(p$group), c("A 1", "B 1", "B 2"))
  expect_equal(unique(p$subgroup[p$Area == "A"]), 1:2)
  expect_equal(unique(p$Name), c("Alpha", NA))
  expect_equal(nrow(p), 4 * 5)
  expect_equal(p$x[1:5], c(0, 4, 4, 0, 0))
  expect_equal(p$y[1:5], c(0, 0, 4, 4, 0))

  empty <- .dsr_geojson_polygons(list(), "code", "name")
  expect_named(empty, names(p))
  expect_equal(nrow(empty), 0)

})

test_that(".dsr_point_area finds the area at a point, holes and parts too", {

  p <- .dsr_geojson_polygons(shapes, "code", "name")
  expect_equal(.dsr_point_area(0.5, 0.5, p), "A")
  expect_equal(.dsr_point_area(3.5, 3.5, p), "A")
  expect_true(is.na(.dsr_point_area(1.5, 1.5, p)))
  expect_equal(.dsr_point_area(5.5, 0.5, p), "B")
  expect_equal(.dsr_point_area(7.5, 0.5, p), "B")
  expect_true(is.na(.dsr_point_area(6.5, 0.5, p)))
  expect_true(is.na(.dsr_point_area(NULL, NULL, p)))

})

# ── Map ─────────────────────────────────────────────────────────────────────

test_that("the map colours areas by value and greys those without one", {

  p <- .dsr_geojson_polygons(shapes, "code", "name")
  m <- .dsr_map_plot(p, data.frame(Area = c("A", "B"), Value = c(10, NA)),
                     legend = "DSR", title = "Title", caption = "Caption")
  expect_s3_class(m, "ggplot")
  expect_equal(m$labels$title, "Title")

  fill <- ggplot2::ggplot_build(m)$data[[1]]$fill
  expect_equal(unique(fill[m$data$Area == "B"]), "#d9d9d9")
  expect_false(any(fill[m$data$Area == "A"] == "#d9d9d9"))

  # Only polygons that reach into the view are drawn
  cropped <- .dsr_map_plot(p, data.frame(Area = "A", Value = 10), legend = "DSR",
                           xlim = c(-1, 4.5), ylim = c(-1, 4.5))
  expect_equal(unique(cropped$data$Area), "A")
  expect_equal(nrow(cropped$data), 10)

  # It draws
  file <- withr::local_tempfile(fileext = ".png")
  ggplot2::ggsave(file, m, width = 4, height = 3, dpi = 50)
  expect_gt(file.size(file), 0)

})

test_that("saved maps take the shape of the area in view", {

  square <- data.frame(x = c(149, 150), y = c(-35, -34))
  size <- .dsr_map_size(square)
  expect_equal(size[["height"]], 8)
  expect_equal(size[["width"]], 6.5 * cos(34.5 * pi / 180) + 1.5)

  # Narrow and wide areas stay within 6 and 14 inches
  expect_equal(.dsr_map_size(square, xlim = c(149, 149.1))[["width"]], 6)
  expect_equal(.dsr_map_size(square, xlim = c(112.5, 154),
                             ylim = c(-36, -34))[["width"]], 14)

})

# ── Live ────────────────────────────────────────────────────────────────────

test_that("ABS boundaries cover the ACT's areas and every state", {

  skip_if_no_abs()
  cache <- withr::local_tempdir()

  for (level in c("sa3", "sa2")) {
    b <- .dsr_boundaries(level, "ACT", cache = cache)
    expect_true(all(grepl("^801", b$Area)), label = level)
    expect_false(anyNA(b$Name), label = level)
    expect_true(all(b$x > 148.7 & b$x < 149.5 & b$y > -36.0 & b$y < -35.1),
                label = level)
  }
  expect_gte(length(unique(.dsr_boundaries("sa3", "ACT", cache = cache)$Area)), 9)
  expect_gte(length(unique(.dsr_boundaries("sa2", "ACT", cache = cache)$Area)), 120)

  states <- .dsr_boundaries("state", "AUS", cache = cache)
  expect_setequal(unique(states$Area), unname(.dsr_states))

  # The ACT and surrounds: the ACT's SA2s and the 18 SA2s of the four NSW SA3s
  sa2 <- unique(.dsr_boundaries("sa2", "SURROUNDS", cache = cache)$Area)
  nsw <- sa2[!startsWith(sa2, "8")]
  expect_setequal(unique(substr(nsw, 1, 5)), names(.dsr_surrounds))
  expect_length(nsw, 18)
  expect_setequal(unique(.dsr_boundaries("state", "SURROUNDS", cache = cache)$Area),
                  c("ACT", "NSW (surrounds)"))

})

# Maps for the DSR Calculator: ASGS 2021 boundaries from the ABS and a
# ggplot2 choropleth of the calculator's results.

# ABS ArcGIS REST services holding the ASGS Edition 3 (2021) boundaries, and
# the service and field prefix for each geographic level of the calculator
.dsr_abs_service <- "https://geo.abs.gov.au/arcgis/rest/services/ASGS2021"

.dsr_boundary_layers <- list(
  state = list(service = "STE", prefix = "(state|ste)"),
  sa3   = list(service = "SA3", prefix = "sa3"),
  sa2   = list(service = "SA2", prefix = "sa2")
)


# Boundaries of the areas at a level, for a population of the calculator, as
# polygon vertices: Area (the code the calculator uses, or its label at state
# level), Name, x and y (longitude and latitude), group (one per polygon) and
# subgroup (one per ring, so holes draw as holes). Downloaded once from the
# ABS, generalised to suit the map, and cached in `cache` when it is
# writable. The states of the ACT and surrounds are drawn from their SA3s, as
# the surrounds are only part of NSW.
#' @noRd
.dsr_boundaries <- function(level, scope, cache = .dsr_cache_dir()) {

  level <- match.arg(level, names(.dsr_boundary_layers))
  scope <- match.arg(scope, names(.dsr_scopes))
  layer_level <- if (scope == "SURROUNDS" && level == "state") "sa3" else level

  file <- if (!is.null(cache)) {
    file.path(cache, sprintf("asgs2021_%s_%s_v1.rds", level, scope))
  }
  if (!is.null(file) && file.exists(file)) {
    polys <- tryCatch(readRDS(file), error = function(e) NULL)
    if (is.data.frame(polys) && nrow(polys)) return(polys)
  }

  layer <- paste0(.dsr_abs_service, "/",
                  .dsr_boundary_layers[[layer_level]]$service, "/MapServer/0")

  # Field names are read from the layer rather than assumed
  meta   <- .dsr_fetch_json(paste0(layer, "?f=json"))
  fields <- vapply(meta$fields, function(f) f$name, character(1))
  field  <- function(pattern) {
    hit <- fields[grepl(pattern, fields, ignore.case = TRUE)]
    if (!length(hit)) {
      stop("The ABS boundary layer has no field matching '", pattern,
           "'. Its fields are: ", paste(fields, collapse = ", "), ".",
           call. = FALSE)
    }
    hit[1]
  }
  prefix <- .dsr_boundary_layers[[layer_level]]$prefix
  code   <- field(paste0("^", prefix, "_code(_2021|21)?$"))
  name   <- field(paste0("^", prefix, "_name(_2021|21)?$"))
  # SA2 codes start with the code of their SA3
  where  <- if (scope == "AUS") {
    "1=1"
  } else {
    act <- paste0(field("^(state|ste)_code(_2021|21)?$"), " = '8'")
    if (scope == "ACT") {
      act
    } else {
      paste0(act, " OR ", paste0(code, " LIKE '", names(.dsr_surrounds), "%'",
                                 collapse = " OR "))
    }
  }

  # Object IDs first, then the features in batches: unlike result paging,
  # this works whatever the server's record limit and version
  ids <- .dsr_fetch_json(paste0(layer, "/query?", .dsr_query_string(list(
    where = where, returnIdsOnly = "true", f = "json"
  ))))$objectIds
  ids <- sort(unlist(ids))
  if (!length(ids)) {
    stop("The ABS boundary service returned no areas.", call. = FALSE)
  }

  features <- list()
  for (batch in split(ids, ceiling(seq_along(ids) / 200))) {
    page <- .dsr_fetch_json(paste0(layer, "/query?", .dsr_query_string(list(
      objectIds          = paste(batch, collapse = ","),
      outFields          = paste(code, name, sep = ","),
      returnGeometry     = "true",
      outSR              = "4326",
      maxAllowableOffset = if (scope == "ACT") "0.0002" else "0.002",
      geometryPrecision  = "5",
      f                  = "geojson"
    ))))
    features <- c(features, page$features)
  }

  polys <- .dsr_geojson_polygons(features, code, name)
  if (level == "state" && scope == "SURROUNDS") {
    polys$Area <- ifelse(startsWith(polys$Area, "8"), "ACT",
                         .dsr_surrounds_label)
    polys$Name <- polys$Area
  } else if (level == "state") {
    polys$Area <- unname(.dsr_states[polys$Area])
  }

  if (!is.null(file)) try(saveRDS(polys, file), silent = TRUE)
  polys

}


# Per-user cache directory for downloaded boundaries, or NULL if it cannot be
# written
#' @noRd
.dsr_cache_dir <- function() {
  dir <- tools::R_user_dir("actepir", which = "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(dir) && file.access(dir, 2) == 0) dir else NULL
}


# GET a URL and parse the JSON reply. download.file() uses the http_proxy
# and https_proxy variables set by the staff .Rprofile. It gives the reason
# for a failure in a warning, so that becomes the error. ArcGIS reports
# errors inside a successful reply, so those are raised here too.
#' @noRd
.dsr_fetch_json <- function(url) {
  file <- tempfile(fileext = ".json")
  on.exit(unlink(file))
  reason <- NULL
  tryCatch(
    withCallingHandlers(
      utils::download.file(url, file, mode = "wb", quiet = TRUE),
      warning = function(w) {
        reason <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      stop(if (is.null(reason)) conditionMessage(e) else reason, call. = FALSE)
    }
  )
  out <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  if (!is.null(out$error)) {
    stop("The ABS boundary service replied: ",
         paste(c(out$error$message, unlist(out$error$details)), collapse = " "),
         call. = FALSE)
  }
  out
}


#' @noRd
.dsr_query_string <- function(params) {
  paste0(names(params), "=",
         vapply(params, function(v) utils::URLencode(as.character(v),
                                                     reserved = TRUE),
                character(1)),
         collapse = "&")
}


# Polygon vertices from GeoJSON features (Polygon or MultiPolygon). Features
# without geometry, such as the ABS's non-spatial areas, are skipped.
#' @noRd
.dsr_geojson_polygons <- function(features, code_field, name_field) {

  cols <- list(Area = list(), Name = list(), x = list(), y = list(),
               group = list(), subgroup = list())
  k <- 0
  for (f in features) {
    g <- f$geometry
    if (is.null(g) || is.null(g$coordinates)) next
    parts <- switch(g$type,
                    Polygon      = list(g$coordinates),
                    MultiPolygon = g$coordinates,
                    NULL)
    area <- as.character(f$properties[[code_field]])
    name <- f$properties[[name_field]]
    name <- if (is.null(name)) NA_character_ else as.character(name)
    for (i in seq_along(parts)) {
      for (j in seq_along(parts[[i]])) {
        ring <- parts[[i]][[j]]
        if (!length(ring)) next
        v <- unlist(ring, use.names = FALSE)
        stride <- length(ring[[1]])
        n <- length(ring)
        k <- k + 1
        cols$x[[k]]        <- v[seq(1, length(v), by = stride)]
        cols$y[[k]]        <- v[seq(2, length(v), by = stride)]
        cols$Area[[k]]     <- rep(area, n)
        cols$Name[[k]]     <- rep(name, n)
        cols$group[[k]]    <- rep(paste(area, i), n)
        cols$subgroup[[k]] <- rep(j, n)
      }
    }
  }

  out <- as.data.frame(lapply(cols, function(v) unlist(v, use.names = FALSE)),
                       stringsAsFactors = FALSE)
  if (!nrow(out)) {
    out <- data.frame(Area = character(0), Name = character(0),
                      x = numeric(0), y = numeric(0), group = character(0),
                      subgroup = integer(0), stringsAsFactors = FALSE)
  }
  out

}


# The area whose polygons contain the point (x, y), or NA. Even-odd ray
# casting over all of an area's rings, so holes and multipart areas count
# correctly. Rings are closed, so consecutive vertices make every edge.
#' @noRd
.dsr_point_area <- function(x, y, polys) {
  n <- nrow(polys)
  if (n < 2 || is.null(x) || is.null(y)) return(NA_character_)
  i <- seq_len(n - 1)
  i <- i[polys$group[i] == polys$group[i + 1] &
         polys$subgroup[i] == polys$subgroup[i + 1]]
  x1 <- polys$x[i]
  y1 <- polys$y[i]
  x2 <- polys$x[i + 1]
  y2 <- polys$y[i + 1]
  cross <- ((y1 > y) != (y2 > y)) & (x < (x2 - x1) * (y - y1) / (y2 - y1) + x1)
  counts <- tapply(cross, polys$Area[i], sum, na.rm = TRUE)
  inside <- names(counts)[counts %% 2 == 1]
  if (length(inside)) inside[1] else NA_character_
}


# Choropleth of one value per area: polys from .dsr_boundaries() and values
# with Area and Value. Areas with no value (suppressed or not calculated) are
# grey. xlim and ylim crop the view, and only polygons that reach into it are
# drawn and set the colour scale.
#' @noRd
.dsr_map_plot <- function(polys, values, legend, title = NULL,
                          subtitle = NULL, caption = NULL,
                          xlim = NULL, ylim = NULL) {

  if (!is.null(xlim) && !is.null(ylim)) {
    g  <- polys$group
    lo <- function(v) tapply(v, g, min)
    hi <- function(v) tapply(v, g, max)
    on <- lo(polys$x) <= xlim[2] & hi(polys$x) >= xlim[1] &
          lo(polys$y) <= ylim[2] & hi(polys$y) >= ylim[1]
    polys <- polys[g %in% names(on)[on], , drop = FALSE]
  }
  polys$Value <- values$Value[match(polys$Area, values$Area)]
  comma <- function(v) format(v, big.mark = ",", scientific = FALSE, trim = TRUE)
  wrap  <- function(s, width) {
    if (!is.null(s)) paste(strwrap(s, width = width), collapse = "\n")
  }

  ggplot2::ggplot(polys, ggplot2::aes(x = .data$x, y = .data$y,
                                      group = .data$group,
                                      subgroup = .data$subgroup,
                                      fill = .data$Value)) +
    ggplot2::geom_polygon(colour = "white", linewidth = 0.25) +
    acthd_ggplot_fill("web_purples", discrete = FALSE, reverse = TRUE,
                      na.value = "#d9d9d9", name = legend, labels = comma) +
    ggplot2::coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::labs(title = title, subtitle = wrap(subtitle, 70),
                  caption = wrap(caption, 90)) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title       = ggplot2::element_text(colour = "#320557", face = "bold",
                                               size = 13),
      plot.subtitle    = ggplot2::element_text(colour = "#333740",
                                               margin = ggplot2::margin(4, 0, 8, 0)),
      plot.caption     = ggplot2::element_text(colour = "#777777", size = 8,
                                               hjust = 0),
      legend.title     = ggplot2::element_text(colour = "#333740", size = 9),
      legend.text      = ggplot2::element_text(colour = "#333740", size = 8),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin      = ggplot2::margin(10, 10, 10, 10)
    )

}


# Width and height in inches for saving a map, from the shape of the area in
# view, allowing about 1.5 inches for the legend and for the titles
#' @noRd
.dsr_map_size <- function(polys, xlim = NULL, ylim = NULL, height = 8) {
  xr <- if (is.null(xlim)) range(polys$x) else xlim
  yr <- if (is.null(ylim)) range(polys$y) else ylim
  aspect <- diff(yr) / (diff(xr) * cos(mean(yr) * pi / 180))
  c(width = min(max((height - 1.5) / aspect + 1.5, 6), 14), height = height)
}

# dev/harness/fake_abs.R
#
# Synthetic ABS boundary service for run_app.R and dryrun_live.R.
# fake_fetch_json(url) answers the layer description, object ID and GeoJSON
# feature requests that .dsr_boundaries() makes. Areas are boxes laid out
# around Canberra, with the area codes of fake_episerver.R and the ASGS 2021
# SA3 names.

local({

  box <- function(x1, x2, y1, y2) {
    list(list(x1, y1), list(x2, y1), list(x2, y2), list(x1, y2), list(x1, y1))
  }

  # SA3: ACT in a 2 x 5 grid from north to south, Queanbeyan to the east and
  # Yass to the north
  act <- c("80104", "80105", "80101", "80103", "80110", "80106",
           "80108", "80109", "80111", "80107")
  names3 <- c("80101" = "Belconnen", "80103" = "Canberra East",
              "80104" = "Gungahlin", "80105" = "North Canberra",
              "80106" = "South Canberra", "80107" = "Tuggeranong",
              "80108" = "Weston Creek", "80109" = "Woden Valley",
              "80110" = "Molonglo", "80111" = "Uriarra - Namadgi",
              "10102" = "Queanbeyan", "10103" = "Snowy Mountains",
              "10106" = "Young - Yass", "11302" = "Tumut - Tumbarumba")
  # Queanbeyan to the east, Snowy Mountains to the south, Young - Yass to the
  # north and Tumut - Tumbarumba to the west
  cells <- data.frame(
    SA3 = c(act, "10102", "10103", "10106", "11302"),
    x1 = c(rep(c(148.95, 149.10), 5), 149.25, 148.95, 148.85, 148.60),
    x2 = c(rep(c(149.10, 149.25), 5), 149.45, 149.35, 149.25, 148.95),
    y2 = c(rep(-35.15 - 0.12 * 0:4, each = 2), -35.27, -35.75, -34.90, -35.15),
    y1 = c(rep(-35.27 - 0.12 * 0:4, each = 2), -35.51, -36.00, -35.15, -35.75)
  )
  # Sizes as in fake_episerver.R: 7, 4, 4 and 3 SA2s in the NSW SA3s, 18 in
  # all as in ASGS 2021
  sizes <- c("80101" = 105, "80103" = 1, "80104" = 85, "80105" = 60,
             "80106" = 30, "80107" = 88, "80108" = 24, "80109" = 40,
             "80110" = 15, "80111" = 0.4, "10102" = 49, "10103" = 28,
             "10106" = 28, "11302" = 21)

  feature <- function(props, rings) {
    list(type = "Feature", properties = props,
         geometry = list(type = "Polygon", coordinates = rings))
  }

  sa3 <- lapply(seq_len(nrow(cells)), function(i) {
    c3 <- cells$SA3[i]
    feature(list(sa3_code_2021 = c3, sa3_name_2021 = unname(names3[c3]),
                 state_code_2021 = substr(c3, 1, 1)),
            list(box(cells$x1[i], cells$x2[i], cells$y1[i], cells$y2[i])))
  })

  # SA2: vertical strips of their SA3, coded as in fake_episerver.R
  sa2 <- do.call(c, lapply(seq_len(nrow(cells)), function(i) {
    c3 <- cells$SA3[i]
    n  <- max(1, round(sizes[[c3]] / 7))
    xs <- seq(cells$x1[i], cells$x2[i], length.out = n + 1)
    lapply(seq_len(n), function(j) {
      feature(list(sa2_code_2021 = sprintf("%s%04d", c3, 1000 + j),
                   sa2_name_2021 = paste(names3[[c3]], j),
                   state_code_2021 = substr(c3, 1, 1)),
              list(box(xs[j], xs[j + 1], cells$y1[i], cells$y2[i])))
    })
  }))

  # States: NSW with the ACT as a hole, the ACT, and no geometry for
  # Outside Australia
  ste <- list(
    feature(list(state_code_2021 = "1", state_name_2021 = "New South Wales"),
            list(box(141.0, 153.6, -37.5, -28.2),
                 box(148.95, 149.25, -35.75, -35.15))),
    feature(list(state_code_2021 = "8",
                 state_name_2021 = "Australian Capital Territory"),
            list(box(148.95, 149.25, -35.75, -35.15))),
    list(type = "Feature",
         properties = list(state_code_2021 = "Z",
                           state_name_2021 = "Outside Australia"),
         geometry = NULL)
  )

  layers <- list(SA3 = sa3, SA2 = sa2, STE = ste)
  fields <- list(
    SA3 = c("objectid", "sa3_code_2021", "sa3_name_2021", "state_code_2021"),
    SA2 = c("objectid", "sa2_code_2021", "sa2_name_2021", "state_code_2021"),
    STE = c("objectid", "state_code_2021", "state_name_2021")
  )

  fake_fetch_json <- function(url) {
    Sys.sleep(0.3)
    message("fake ABS: ", substr(url, 1, 140))
    layer <- sub(".*/ASGS2021/([A-Z0-9]+)/MapServer.*", "\\1", url)
    feats <- layers[[layer]]
    if (grepl("?f=json", url, fixed = TRUE)) {
      return(list(fields = lapply(fields[[layer]], function(f) list(name = f))))
    }
    q <- utils::URLdecode(url)
    if (grepl("returnIdsOnly=true", q, fixed = TRUE)) {
      # state_code_2021 = '8', optionally OR <code> LIKE '<prefix>%' ...
      prefixes <- sub("LIKE '", "", regmatches(q, gregexpr("LIKE '[0-9]+", q))[[1]])
      code_of <- function(f) as.character(f$properties[[grep("_code_2021$", names(f$properties), value = TRUE)[1]]])
      keep <- if (grepl("state_code_2021 = '8'", q, fixed = TRUE)) {
        which(vapply(feats, function(f) {
          f$properties$state_code_2021 == "8" ||
            any(startsWith(code_of(f), prefixes))
        }, logical(1)))
      } else {
        seq_along(feats)
      }
      return(list(objectIdFieldName = "objectid", objectIds = as.list(keep)))
    }
    ids <- as.integer(strsplit(sub(".*objectIds=([0-9,]+).*", "\\1", q), ",")[[1]])
    list(type = "FeatureCollection", features = feats[ids])
  }

  assign("fake_fetch_json", fake_fetch_json, envir = globalenv())
  assign("fake_abs_sa2", vapply(sa2, function(f) f$properties$sa2_code_2021,
                                character(1)), envir = globalenv())

})

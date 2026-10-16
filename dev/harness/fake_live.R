# dev/harness/fake_live.R
#
# Synthetic EpiServer and ABS boundary service for dry runs of the live tests
# (dryrun_live.R). It answers every query in
# tests/testthat/test-episerver_dsr_live.R. Most answers are fixed counts.
# Queries with a diagnosis condition are answered by applying the generated
# SQL condition to synthetic coded records, with SQL Server's semantics
# written here independently (outer spaces and dots removed, case ignored,
# bytewise order), so the test of the code matching is not circular.
#
# A dry run checks the test code. It cannot show that the SQL suits the real
# tables: only the maintainer's run against EpiServer does that.
#
# dryrun_live.R sources this into the test environment after the package's
# own helpers, with harness_dir set.

source(file.path(harness_dir, "fake_abs.R"), local = TRUE)

std_values <- c(1282357, 1351664, 1353177, 1352745, 1302412, 1407081, 1466615,
                1492204, 1479257, 1358594, 1300777, 1008799, 822024, 682513,
                638380, 519356, 330050, 265235)
age_names <- c(sprintf("%02d-%02d", seq(0, 80, 5), seq(4, 84, 5)), "85+")


# ── Fixed answers ───────────────────────────────────────────────────────────

areas_for <- function(sql) {
  if (grepl("geo_SA2_2021", sql) || grepl("ERP5_SA2", sql)) {
    return(c("801011001", "801011002", "101021007"))
  }
  if (grepl("geo_SA3_2021", sql) || grepl("ERP5_SA3", sql)) {
    return(c("80101", "80104", "10102"))
  }
  c("ACT", "NSW")
}

fake_fixed <- function(sql) {
  if (grepl("GROUP BY StdPopCode", sql, fixed = TRUE)) {
    return(data.frame(StdPopCode = c(101L, 300L),
                      StdPopName = c("Australia (2001)", "European (1976)"),
                      Note = c("ABS Census", "Superceded")))
  }
  if (grepl("FROM Analysis.dbo.StandardPops", sql, fixed = TRUE)) {
    return(data.frame(AgeGroup = 1:18, AgeGroupName = age_names,
                      StdPopValue = std_values))
  }
  if (grepl("SELECT DISTINCT geo_STATE", sql, fixed = TRUE)) {
    return(data.frame(geo_STATE = c("ACT", "NSW", "OT")))
  }
  if (grepl("SELECT EntityCode, SUM(ERPCount)", sql, fixed = TRUE)) {
    return(data.frame(EntityCode = 0:8,
                      Population = c(27000000, 8400000, 6900000, 5500000,
                                     1900000, 2900000, 580000, 255000,
                                     470000)))
  }
  if (grepl("MIN(ERPYear)", sql, fixed = TRUE)) {
    return(data.frame(FirstYear = 2001L, LastYear = 2024L))
  }
  if (grepl("MIN(", sql, fixed = TRUE)) {
    return(data.frame(FirstDate = as.POSIXct("2004-07-01 00:12:00", tz = "UTC"),
                      LastDate  = as.POSIXct("2025-06-30 23:50:00", tz = "UTC")))
  }
  if (grepl("AS Events", sql, fixed = TRUE)) {
    a <- areas_for(sql)
    act <- grepl("'ACT'", sql) || grepl("LIKE '8%'", sql, fixed = TRUE)
    if (act) a <- a[startsWith(a, "8") | a == "ACT"]
    e <- expand.grid(AgeGroup = c(1:18, NA), Sex = c(1L, 2L, 9L),
                     Area = c(a, NA), stringsAsFactors = FALSE)
    return(data.frame(Year = 2023L, Sex = e$Sex, AgeGroup = e$AgeGroup,
                      Area = e$Area, Events = 7000L))
  }
  if (grepl("AS Population", sql, fixed = TRUE)) {
    a <- areas_for(sql)
    if (grepl("_ACT", sql)) a <- a[startsWith(a, "8") | a == "ACT"]
    p <- expand.grid(AgeGroup = 1:18, Sex = 1:2, Area = a,
                     stringsAsFactors = FALSE)
    return(data.frame(Year = 2023L, Sex = p$Sex, AgeGroup = p$AgeGroup,
                      AgeGroupName = age_names[p$AgeGroup], Area = p$Area,
                      Population = 13000L))
  }
  stop("fake_live: unexpected SQL\n", sql)
}


# ── Coded records for diagnosis conditions ──────────────────────────────────

set.seed(1)
code_pool <- c("C12.9", "C13", "C13.0", "C14.9", "C15", "C15.4", "C15.45",
               "C15.459", "C15.46", "C15.5", "C16", "E18.2", "E18.3", "E18.5",
               "E18.78", "E18.79", "E10.9", "E14.65", "E15", "I21.4", "I21",
               "I22.0", "J45.9", "J44.9", "S06.0", "S09.9", "S10.0", "T78.3",
               "T78.4", "T78.40", "T78.5", "M8140/3", " j45.0 ", "")
records <- function(n, fields, icd) {
  d <- data.frame(Year = 2023L, Sex = sample(c(1L, 2L, 9L), n, TRUE),
                  AgeGroup = sample(c(1:18, NA), n, TRUE),
                  Area = sample(c("ACT", "NSW", NA), n, TRUE),
                  ICD = sample(icd, n, TRUE), stringsAsFactors = FALSE)
  for (k in seq_along(fields)) {
    p_na <- if (k <= 2) 0.1 else min(0.95, 0.3 + 0.1 * k)
    v <- sample(code_pool, n, TRUE)
    v[stats::runif(n) < p_na] <- NA
    d[[fields[k]]] <- v
  }
  d
}
# ICD holds ICD10AMEdition (ED) or ICDVersion (APC)
recs <- list(
  ED  = records(20000, c("EDShortListCode", paste0("Diagnosis", 1:3)),
                c(10, 11, 12, 99, NA)),
  APC = records(8000, paste0("Diagnosis", 1:100), c(9, 10, 10, 10))
)

# Whether each code meets the condition in sql, read from the SQL text
sql_match <- function(code, sql) {
  code <- toupper(gsub(".", "", trimws(code), fixed = TRUE))
  ok <- !is.na(code) & !grepl("/", code, fixed = TRUE)
  range_re <- paste0("\\(c\\.Code >= '[A-Z0-9]+' AND \\(c\\.Code <= ",
                     "'[A-Z0-9]+' OR c\\.Code LIKE '[A-Z0-9]+%'\\)\\)")
  ranges  <- regmatches(sql, gregexpr(range_re, sql))[[1]]
  rest    <- gsub(range_re, "", sql)
  singles <- regmatches(rest, gregexpr("c\\.Code LIKE '[A-Z0-9]+%'", rest))[[1]]
  hit <- rep(FALSE, length(code))
  for (t in c(ranges, singles)) {
    v <- sub("'", "", regmatches(t, gregexpr("'[A-Z0-9]+", t))[[1]])
    if (length(v) == 1) {
      hit <- hit | startsWith(code, v)
    } else {
      ge <- withr::with_collate("C", code >= v[1])
      le <- withr::with_collate("C", code <= v[2])
      hit <- hit | (ge & (le | startsWith(code, v[3])))
    }
  }
  ok & hit
}

count_by <- function(d) {
  d$k <- paste(d$Year, d$Sex, d$AgeGroup, d$Area)
  out <- d[!duplicated(d$k), c("Year", "Sex", "AgeGroup", "Area", "k")]
  out$Events <- as.integer(table(d$k)[out$k])
  out[c("Year", "Sex", "AgeGroup", "Area", "Events")]
}

# The ACT and surrounds: two ACT SA3s and their SA2s, and the four NSW SA3s
# with the SA2s of the fake ABS service
surrounds_areas <- function(sql) {
  if (grepl("NSW (surrounds)", sql, fixed = TRUE)) {
    return(c("ACT", "NSW (surrounds)"))
  }
  if (grepl("ERP5_SA2|geo_SA2_2021 AS Area", sql)) {
    return(c("801011001", "801011002",
             fake_abs_sa2[!startsWith(fake_abs_sa2, "8")]))
  }
  c("80101", "80104", "10102", "10103", "10106", "11302")
}


# ── The fake connection ─────────────────────────────────────────────────────

fake <- function(sql) {
  ds <- if (grepl("FROM Analysis.dbo.ED", sql, fixed = TRUE)) "ED" else "APC"
  d <- recs[[ds]]
  keep_icd <- function(d) {
    if (ds == "ED") d[is.na(d$ICD) | d$ICD != 99, ] else d[d$ICD == 10, ]
  }
  if (grepl("SELECT DISTINCT EntityCode", sql, fixed = TRUE)) {
    return(data.frame(EntityCode = c(10102L, 10103L, 10106L, 11302L)))
  }
  if (grepl("IN ('10102'", sql, fixed = TRUE) && !grepl("EXISTS", sql)) {
    a <- surrounds_areas(sql)
    if (grepl("AS Population", sql, fixed = TRUE)) {
      p <- expand.grid(AgeGroup = 1:18, Sex = 1:2, Area = a,
                       stringsAsFactors = FALSE)
      n <- ifelse(p$Area == "ACT", 2L,
                  ifelse(p$Area == "NSW (surrounds)", 4L, 1L))
      return(data.frame(Year = 2023L, Sex = p$Sex, AgeGroup = p$AgeGroup,
                        AgeGroupName = age_names[p$AgeGroup], Area = p$Area,
                        Population = 13000L * n))
    }
    e <- expand.grid(AgeGroup = c(1:18, NA), Sex = c(1L, 2L, 9L),
                     Area = c(a, NA), stringsAsFactors = FALSE)
    return(data.frame(Year = 2023L, Sex = e$Sex, AgeGroup = e$AgeGroup,
                      Area = e$Area, Events = 7000L))
  }
  if (grepl("AND EXISTS", sql, fixed = TRUE)) {
    stopifnot(grepl(if (ds == "ED") "COALESCE(ICD10AMEdition, 0) <> 99" else
                      "ICDVersion = 10", sql, fixed = TRUE))
    d <- keep_icd(d)
    fields <- gsub("[()]", "", unique(regmatches(
      sql, gregexpr("\\((EDShortListCode|Diagnosis[0-9]+)\\)", sql)
    )[[1]]))
    hit <- Reduce(`|`, lapply(d[fields], sql_match, sql = sql))
    out <- count_by(d[hit, ])
    # SA3 and SA2 queries give the codes of the fixed populations' areas
    if (grepl("geo_SA3_2021 AS Area", sql, fixed = TRUE)) {
      out$Area <- unname(c(ACT = "80101", NSW = "10102")[out$Area])
    } else if (grepl("geo_SA2_2021 AS Area", sql, fixed = TRUE)) {
      out$Area <- unname(c(ACT = "801011001", NSW = "101021007")[out$Area])
    }
    return(out)
  }
  if (grepl("COUNT(*) AS n", sql, fixed = TRUE)) {
    fields <- strsplit(sub("^SELECT (.*), COUNT.*$", "\\1",
                           strsplit(sql, "\n")[[1]][1]), ", ")[[1]]
    d <- keep_icd(d)
    k <- do.call(paste, c(lapply(d[fields], function(x) {
      ifelse(is.na(x), "<NA>", x)
    }), sep = "\r"))
    out <- d[!duplicated(k), fields, drop = FALSE]
    out$n <- as.integer(table(k)[k[!duplicated(k)]])
    return(out)
  }
  if (grepl("AS Events", sql, fixed = TRUE) &&
      !grepl("geo_SA3_2021 AS Area|geo_SA2_2021 AS Area|LIKE '8%'|= 'ACT'",
             sql)) {
    # All events of the state-level, Australian query: the coded records
    return(count_by(d))
  }
  fake_fixed(sql)
}


# ── Wiring ──────────────────────────────────────────────────────────────────

# The tests open connections with .epi_connection(), found here before the
# package's; the apps they run use the namespace's, replaced below
.epi_connection <- function(connect_args = list()) {
  list(query = fake, disconnect = function() invisible(NULL))
}
utils::assignInNamespace(".epi_connection", .epi_connection, ns = "actepir")
utils::assignInNamespace(".dsr_fetch_json", fake_fetch_json, ns = "actepir")

skip_if_no_episerver <- function() invisible(TRUE)
skip_if_no_abs <- function() invisible(TRUE)

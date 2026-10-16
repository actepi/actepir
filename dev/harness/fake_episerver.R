# dev/harness/fake_episerver.R
#
# Synthetic EpiServer for running the Shiny apps offline (run_app.R).
# fake_query(sql) answers every query the DSR Calculator and the EpiServer
# Browser send, by matching fragments of the SQL, and stops on any other.
# When a SQL builder changes, update the matching branch here.
#
# Every number is invented. There are 14 SA3s (the ten ACT SA3s and four NSW
# SA3s around the ACT) with SA2s in proportion to their size, 2001 to 2024.
# Event rates give realistic ED and APC volumes, and a diagnosis or procedure
# condition keeps about 1% of events, for small counts. The events also hold
# the cases the info bar reports: no age, no residence, an area missing from
# the population table and sex not stated, plus one SA2 with an empty age
# group that has events. Two small SA3s (80103 and 80111) have age groups
# under 30 people.

local({

  set.seed(42)
  age_names <- c(sprintf("%02d-%02d", seq(0, 80, 5), seq(4, 84, 5)), "85+")

  standards <- list(
    "100" = c(1271703, 1272208, 1241619, 1364074, 1396764, 1399663, 1425735, 1328387, 1294271, 1029145, 846934, 725950, 736868, 671390, 510755, 384495, 229828, 154247),
    "101" = c(1282357, 1351664, 1353177, 1352745, 1302412, 1407081, 1466615, 1492204, 1479257, 1358594, 1300777, 1008799, 822024, 682513, 638380, 519356, 330050, 265235),
    "102" = c(1458114, 1387634, 1387865, 1453459, 1611663, 1658170, 1536161, 1573910, 1587244, 1541837, 1494063, 1335993, 1226000, 954260, 727671, 558341, 444032, 403607),
    "200" = c(12000, 10000, 9000, 9000, 8000, 8000, 6000, 6000, 6000, 6000, 5000, 4000, 4000, 3000, 2000, 1000, 500, 500),
    "201" = c(8800, 8700, 8600, 8500, 8200, 7900, 7600, 7200, 6600, 6000, 5400, 4600, 3700, 3000, 2200, 1500, 900, 600),
    "300" = c(8000, 7000, 7000, 7000, 7000, 7000, 7000, 7000, 7000, 7000, 7000, 6000, 5000, 4000, 3000, 2000, 1000, 1000),
    "301" = c(5000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000, 5500, 5000, 4000, 2500, 2500)
  )
  standard_list <- data.frame(
    StdPopCode = c(100L, 101L, 102L, 200L, 201L, 300L, 301L),
    StdPopName = c("Australia (1991)", "Australia (2001)", "Australia (2011)",
                   "World SEGI (1960)", "World WHO (2002)", "European (1976)",
                   "European (2013)"),
    Note = c("ABS June estimate", "ABS Census", "ABS June estimate", NA, NA,
             "Superceded", NA)
  )

  # SA3s with relative sizes; the ACT ones plus Queanbeyan and Yass in NSW
  sa3 <- data.frame(
    SA3 = c("80101", "80103", "80104", "80105", "80106", "80107", "80108",
            "80109", "80110", "80111", "10102", "10103", "10106", "11302"),
    size = c(105, 1, 85, 60, 30, 88, 24, 40, 15, 0.4, 49, 28, 28, 21)
  )
  sa2 <- do.call(rbind, lapply(seq_len(nrow(sa3)), function(i) {
    n <- max(1, round(sa3$size[i] / 7))
    data.frame(SA2 = sprintf("%s%04d", sa3$SA3[i], 1000 + seq_len(n)),
               size = sa3$size[i] * 1000 / n * stats::runif(n, 0.5, 1.5))
  }))
  shape <- c(6.0, 6.2, 6.0, 6.3, 7.5, 8.2, 8.0, 7.6, 7.0, 6.4, 6.0, 5.6, 5.0, 4.4, 3.6, 2.5, 1.6, 1.2)
  shape <- shape / sum(shape)

  years <- 2001:2024
  pop <- expand.grid(AgeGroup = 1:18, Sex = 1:2, SA2 = sa2$SA2, Year = years,
                     stringsAsFactors = FALSE)
  pop$Population <- round(sa2$size[match(pop$SA2, sa2$SA2)] * shape[pop$AgeGroup] / 2 *
                            (1 + 0.015 * (pop$Year - 2023)) * stats::runif(nrow(pop), 0.9, 1.1))
  pop$SA3   <- substr(pop$SA2, 1, 5)
  pop$State <- ifelse(substr(pop$SA2, 1, 1) == "8", "ACT", "NSW")
  # One tiny SA2 with an empty age group that still has events
  pop$Population[pop$SA2 == sa2$SA2[sa2$SA2 > "80111"][1] & pop$AgeGroup == 18] <- 0

  rates <- list(
    ED  = c(0.45, 0.25, 0.22, 0.30, 0.35, 0.32, 0.30, 0.28, 0.27, 0.26, 0.27, 0.28, 0.30, 0.33, 0.38, 0.45, 0.60, 0.85),
    APC = c(0.15, 0.04, 0.04, 0.08, 0.12, 0.15, 0.17, 0.15, 0.13, 0.14, 0.17, 0.22, 0.30, 0.40, 0.55, 0.70, 0.90, 1.20)
  )
  events_for <- function(dataset) {
    e <- pop
    use <- ifelse(e$State == "ACT", 1, 0.12) * ifelse(e$Sex == 2, 1.05, 1)
    e$Events <- stats::rpois(nrow(e), pmax(e$Population, 2) * rates[[dataset]][e$AgeGroup] * use)
    e
  }
  events <- list(ED = events_for("ED"), APC = events_for("APC"))

  # ICD-10-AM diagnoses as .dsr_sql_icd() returns them, all invented: every
  # category A00 to Z99 and its subdivisions .0 to .9 (28,600 codes) with
  # titles of two words from a list, and five myocardial titles for the
  # search checks. X99.9 left after edition 9 and U07.1 came in edition 10.
  # Built without random numbers, so the counts above stay as recorded.
  icd_words <- c("acute", "chronic", "test", "disorder", "heart", "lung",
                 "kidney", "liver", "infection", "other", "artery", "skin",
                 "pulmonary", "renal", "cerebral", "complication", "bone")
  cats <- as.vector(outer(LETTERS, sprintf("%02d", 0:99), paste0))
  icd_code <- c(cats, as.vector(outer(cats, 0:9, paste, sep = ".")))
  icd_k <- seq_along(icd_code) - 1L
  icd_n <- length(icd_words)
  icd <- data.frame(
    Code = icd_code,
    Description = paste(icd_words[icd_k %% icd_n + 1L],
                        icd_words[icd_k %/% icd_n %% icd_n + 1L], "condition"),
    FirstEdition = 3L, LastEdition = 11L, stringsAsFactors = FALSE
  )
  myocardial <- c(
    "I21"   = "Acute myocardial test condition",
    "I21.0" = "Acute myocardial test condition of anterior wall",
    "I21.4" = "Acute subendocardial myocardial test condition",
    "I22"   = "Subsequent myocardial test condition",
    "I25.2" = "Old myocardial test condition"
  )
  icd$Description[match(names(myocardial), icd$Code)] <- myocardial
  icd$LastEdition[icd$Code == "X99.9"] <- 9L
  icd$FirstEdition[icd$Code == "U07.1"] <- 10L

  # ACHI procedures, also invented: 2,000 codes, two to each of 1,000 stems
  # from 30000, five stems to a block (blocks 1 to 200), and a block of two
  # nerve studies for the search checks
  stems <- 30000L + 7L * (0:999)
  proc_k <- seq_len(2000L) - 1L
  procedures <- data.frame(
    Code = sprintf("%05d-%02d", rep(stems, each = 2), rep(0:1, 1000)),
    Description = paste(icd_words[proc_k %% icd_n + 1L],
                        icd_words[proc_k %/% icd_n %% icd_n + 1L],
                        "procedure"),
    ProcedureBlock = rep(seq_len(200L), each = 10),
    BlockDescription = paste(icd_words[rep(seq_len(200L), each = 10) %% icd_n + 1L],
                             "procedures"),
    FirstEdition = 3L, LastEdition = 11L, stringsAsFactors = FALSE
  )
  procedures <- rbind(procedures, data.frame(
    Code = c("39001-00", "39001-01"),
    Description = c("Test nerve conduction study, one nerve",
                    "Test nerve conduction study with muscle recording"),
    ProcedureBlock = 900L, BlockDescription = "Test nerve studies",
    FirstEdition = 3L, LastEdition = 11L
  ))
  icd <- rbind(
    data.frame(VarType = "Diagnosis", icd[c("Code", "Description")],
               ProcedureBlock = NA_integer_, BlockDescription = NA_character_,
               icd[c("FirstEdition", "LastEdition")]),
    data.frame(VarType = "Procedure", procedures)
  )

  extract_years <- function(sql) {
    y <- as.integer(regmatches(sql, gregexpr("[0-9]{4}(?=0101')", sql, perl = TRUE))[[1]])
    c(y[1], y[2] - 1L)
  }

  sum_by <- function(df, cols, value) {
    out <- stats::aggregate(df[value], by = df[cols], FUN = sum)
    out[order(do.call(order, out[cols])), ]
  }

  fake_query <- function(sql) {

    Sys.sleep(0.2)

    # ── DSR Calculator ──
    if (grepl("GROUP BY StdPopCode", sql, fixed = TRUE)) return(standard_list)
    if (grepl("FROM Analysis.dbo.ICD10AM", sql, fixed = TRUE)) return(icd)
    if (grepl("FROM Analysis.dbo.StandardPops", sql, fixed = TRUE)) {
      code <- sub(".*StdPopCode = ([0-9]+).*", "\\1", sql)
      return(data.frame(AgeGroup = 1:18, AgeGroupName = age_names,
                        StdPopValue = standards[[code]]))
    }
    if (grepl("MIN(ERPYear)", sql, fixed = TRUE)) {
      last <- if (grepl("ERP5_SA3", sql)) 2023L else 2024L
      first <- if (grepl("ERP5_STE", sql)) 1971L else 2001L
      return(data.frame(FirstYear = first, LastYear = last))
    }
    if (grepl("MIN(PresentationDateTime)", sql, fixed = TRUE)) {
      return(data.frame(FirstDate = as.POSIXct("2004-07-01 00:04:00", tz = "UTC"),
                        LastDate  = as.POSIXct("2025-06-30 23:58:00", tz = "UTC")))
    }
    if (grepl("MIN(SeparationDate)", sql, fixed = TRUE)) {
      return(data.frame(FirstDate = as.Date("1991-07-01"), LastDate = as.Date("2025-06-30")))
    }
    if (grepl("AS Events", sql, fixed = TRUE)) {
      dataset <- if (grepl("dbo.ED\n", sql, fixed = TRUE)) "ED" else "APC"
      yr <- extract_years(sql)
      level <- if (grepl("geo_SA2_2021 AS Area", sql, fixed = TRUE)) "SA2" else
        if (grepl("geo_SA3_2021 AS Area", sql, fixed = TRUE)) "SA3" else "State"
      e <- events[[dataset]]
      e <- e[e$Year >= max(yr[1], 2001) & e$Year <= min(yr[2], 2024), ]
      surrounds <- grepl("IN ('10102'", sql, fixed = TRUE)
      # Every fake NSW SA3 is one of the surrounds
      if (grepl("OR geo_", sql, fixed = TRUE) && !surrounds) e <- e[e$State == "ACT", ]
      e$Area <- e[[level]]
      if (surrounds && level == "State") {
        e$Area <- ifelse(e$State == "ACT", "ACT", "NSW (surrounds)")
      }
      out <- sum_by(e, c("Year", "Sex", "AgeGroup", "Area"), "Events")
      # Residence not geocoded, an area with no population, age and sex not stated
      extra <- head(out[out$Events > 0, ], 40)
      extra$Events <- pmax(1, round(extra$Events * 0.6))
      extra$Area <- NA
      nopop <- out[1:5, ]; nopop$Area <- if (level == "State") "OT" else if (level == "SA3") "89999" else "899999499"
      noage <- out[1:3, ]; noage$AgeGroup <- NA
      nosex <- out[1:6, ]; nosex$Sex <- 9L; nosex$Events <- 2L
      out <- rbind(out, extra, nopop, noage, nosex)
      # A diagnosis or procedure condition keeps about 1% of events
      if (grepl("EXISTS (", sql, fixed = TRUE)) {
        out$Events <- stats::rbinom(nrow(out), out$Events, 0.01)
        out <- out[out$Events > 0, ]
      }
      out$Year <- as.integer(out$Year); out$Sex <- as.integer(out$Sex)
      out$Events <- as.integer(out$Events)
      rownames(out) <- NULL
      return(out)
    }
    if (grepl("AS Population", sql, fixed = TRUE)) {
      yr <- as.integer(regmatches(sql, regexec("BETWEEN ([0-9]{4}) AND ([0-9]{4})", sql))[[1]][2:3])
      level <- if (grepl("ERP5_SA2", sql)) "SA2" else if (grepl("ERP5_SA3", sql)) "SA3" else "State"
      p <- pop[pop$Year >= yr[1] & pop$Year <= yr[2], ]
      if (grepl("_ACT\n", sql, fixed = TRUE)) p <- p[p$State == "ACT", ]
      p$Area <- p[[level]]
      if (grepl("NSW (surrounds)", sql, fixed = TRUE)) {
        p$Area <- ifelse(p$State == "ACT", "ACT", "NSW (surrounds)")
      }
      out <- sum_by(p, c("Year", "Sex", "AgeGroup", "Area"), "Population")
      out$AgeGroupName <- age_names[out$AgeGroup]
      out <- out[c("Year", "Sex", "AgeGroup", "AgeGroupName", "Area", "Population")]
      out$Year <- as.integer(out$Year); out$Sex <- as.integer(out$Sex)
      out$Population <- as.integer(out$Population)
      rownames(out) <- NULL
      return(out)
    }

    # ── EpiServer Browser ──
    if (grepl("FROM sys.databases", sql, fixed = TRUE)) {
      return(data.frame(name = c("Analysis", "HospitalAPC", "master")))
    }
    if (grepl("SELECT DISTINCT TABLE_SCHEMA", sql, fixed = TRUE)) {
      return(data.frame(TABLE_SCHEMA = c("dbo", "ref")))
    }
    if (grepl("LIKE '%DataLabels%'", sql, fixed = TRUE) && grepl("COUNT(DISTINCT", sql, fixed = TRUE)) {
      return(data.frame(TABLE_SCHEMA = "ref", TABLE_NAME = "DataLabels"))
    }
    if (grepl("SELECT TABLE_NAME", sql, fixed = TRUE)) {
      return(data.frame(TABLE_NAME = c("APC", "ED", "ERP5_SA3_ACT", "StandardPops")))
    }
    if (grepl("INFORMATION_SCHEMA.COLUMNS", sql, fixed = TRUE)) {
      return(data.frame(
        ord = 1:5,
        col = c("PresentationDateTime", "Sex", "AgeYrs", "geo_STATE", "geo_SA3_2021"),
        descr = c("Presentation date and time", "Sex", "Age at admission in years",
                  "Intech geocoded State", "Intech geocoded SA3 2021"),
        dtype = c("datetime", "int", "int", "varchar", "varchar"),
        charlen = c(NA, NA, NA, 3L, 5L),
        numprec = c(NA, 10L, 10L, NA, NA),
        numscale = c(NA, 0L, 0L, NA, NA)
      ))
    }
    if (grepl("LOWER(LabelType) = 'opt'", sql, fixed = TRUE)) {
      return(data.frame(VarName = "Sex", DataCode = c("1", "2", "3", "9"),
                        LabelName = c("Male", "Female", "Other", "Not stated/inadequately described")))
    }
    stop("fake_query: unexpected SQL\n", sql)
  }

  assign("fake_query", fake_query, envir = globalenv())

})

# dev/dictionaries/convert_definitions.R
#
# Converts the linkage dictionary's definitions workbook (definitions.xlsx)
# and the datasets of its run script (dictionary_call.Rmd) into an INSERT
# script that loads the Linkage profile into the dictionary tables of
# Analysis.ref, which inst/sql/dictionary_tables.sql creates. It is run once,
# by the maintainer; the rows are then edited in SSMS and the workbook is
# retired. Neither the workbook nor the script it writes is committed: they
# hold internal comments and the names of the databases.
#
# Usage, in R, from the root of the repository, with the workbook and the
# script written kept outside it:
#   source("dev/dictionaries/convert_definitions.R")
#   # tables: run the chunk of dictionary_call.Rmd that builds it, from
#   # library(tidyverse) to filter(active==1)
#   convert_definitions("C:/dictionaries/definitions.xlsx", tables,
#                       "C:/dictionaries/dictionary_linkage.sql")
# then, in SSMS, run inst/sql/dictionary_tables.sql and the script written.
#
# It stops, writing nothing, if the workbook has a problem that would give
# wrong rows or make the INSERTs fail, and lists the problems. It reports the
# rows it leaves out (those of a Dataset that is not in `tables`) and the
# spellings it standardises. Needs openxlsx. Tests:
# dev/dictionaries/test-convert_definitions.R.

# The Linkage profile: the title lines and closing sections of the old Word
# template, template_dictionary_blank.docx
linkage_profile <- list(
  profile  = "Linkage",
  title    = "Data Linkage Dataset Metadata",
  subtitle = "Data Analytics Branch: Epidemiology Section",
  sections = data.frame(
    heading = c("General Enquiries", "Acknowledgment of Country",
                "Accessibility"),
    text = c(
      paste("Population Health Informatics",
            "Epidemiology Section",
            "Data Analytics Branch",
            "ACT Health and Community Services Directorate",
            "",
            "Data Custodian Email Contact:",
            "healthinfo@act.gov.au",
            "Linkage request submission portal:",
            "https://act-health.atlassian.net/servicedesk/customer/portal/20",
            sep = "\n"),
      paste("We acknowledge the Ngunnawal people as traditional custodians",
            "of the ACT and recognise any other people or families with",
            "connection to the lands of the ACT and region. We acknowledge",
            "and respect their continuing culture and the contribution they",
            "make to the life of this city and this region."),
      paste(paste("If you have difficulty reading a standard printed document",
                  "and would like an alternative format, please phone 13 22 81."),
            paste("If English is not your first language and you need the",
                  "Translating and Interpreting Service (TIS), please call",
                  "13 14 50."),
            paste("For further accessibility information, visit:",
                  "www.health.act.gov.au/accessibility"),
            "www.health.act.gov.au | Phone: 132281",
            "\u00a9 Australian Capital Territory, Canberra",
            sep = "\n")
    )
  )
)

# The order of themes, proposed for the maintainer to confirm: the themes
# shared by most datasets first, then each dataset's own, in the order of its
# collection, and Data management last. Themes not listed follow
# alphabetically.
linkage_theme_order <- c(
  "Record identifiers", "Personally identifying information", "Demography",
  "Geography", "Dates & times (events)", "Presentation details",
  "Admission details", "Diagnoses/Procedures", "Event summaries",
  "Death details", "Mother details", "Mother history", "Pregnancy details",
  "Delivery details", "Baby details", "Complications",
  "Child's Demographics", "Child's Health", "Asthma, Eczema, Hayfever",
  "Weight, Diet, Physical Activity",
  "Strength and Difficulties Questionnaire (SDQ)",
  "Parental Concerns - PEDS Parent Evaluation of Development Status",
  "School Assessment data", "Survey Administration",
  "Overall Measures of Health", "Physical Activity", "Food & Drink",
  "Lifestyle", "Mental Health", "Data management"
)

# The columns each sheet must have
definition_sheets <- list(
  vars   = c("Dataset", "Theme", "Column", "ColumnAlias", "DescriptionAlias",
             "ColumnAction", "ValueAction", "Limits", "Comments", "Other"),
  notes  = c("Dataset", "Aspect", "Issue", "Comment"),
  limits = c("code", "hex", "symbol", "text")
)

# The widths of the tables' text columns (inst/sql/dictionary_tables.sql)
column_widths <- list(
  datasets = c(DatasetCode = 20, FullName = 200, DatabaseName = 128,
               SchemaName = 128, TableName = 128, DOI = 100),
  columns  = c(ColumnName = 128, Theme = 100, ColumnAlias = 128,
               DescriptionAlias = 500, CaveatCode = 20, Comments = 500,
               Other = 500),
  notes    = c(Aspect = 100, Issue = 200),
  caveats  = c(CaveatCode = 20, Symbol = 4, CaveatText = 500),
  themes   = c(Theme = 100)
)

# Text with line breaks as \n and no space at either end; empty cells NA
clean_text <- function(x) {
  x <- gsub("\r\n?", "\n", as.character(x))
  x <- gsub("^[[:space:]\u00a0]+|[[:space:]\u00a0]+$", "", x)
  x[!is.na(x) & !nzchar(x)] <- NA
  x
}

# Reads the three sheets, as text, with the workbook's row numbers in `row`
read_definitions <- function(path) {
  found <- openxlsx::getSheetNames(path)
  missing <- setdiff(names(definition_sheets), found)
  if (length(missing)) {
    stop(path, " has no sheet ", paste(missing, collapse = ", "), call. = FALSE)
  }
  lapply(stats::setNames(nm = names(definition_sheets)), function(sheet) {
    d <- openxlsx::read.xlsx(path, sheet = sheet, skipEmptyRows = FALSE,
                             skipEmptyCols = FALSE)
    absent <- setdiff(definition_sheets[[sheet]], names(d))
    if (length(absent)) {
      stop("Sheet ", sheet, " has no column ", paste(absent, collapse = ", "),
           call. = FALSE)
    }
    d <- as.data.frame(lapply(d[definition_sheets[[sheet]]], clean_text),
                       stringsAsFactors = FALSE)
    d$row <- seq_len(nrow(d)) + 1
    d[rowSums(!is.na(d[definition_sheets[[sheet]]])) > 0, , drop = FALSE]
  })
}

# Problems, one row per message: its kind ("error" stops the conversion;
# "left out" and "changed" are reported), its sheet and rows, and the message
problem <- function(kind, sheet, rows, message) {
  if (!length(message)) return(NULL)
  data.frame(kind = kind, sheet = sheet,
             rows = vapply(rows, function(r) {
               if (is.numeric(r)) compress_numbers(r) else paste(r, collapse = ", ")
             }, ""),
             message = message, stringsAsFactors = FALSE)
}

# Values too wide for their column
width_problems <- function(d, widths, sheet) {
  cols <- names(widths)
  row <- if (is.null(d$row)) seq_len(nrow(d)) else d$row
  do.call(rbind, lapply(seq_along(widths), function(i) {
    x <- d[[cols[i]]]
    long <- which(!is.na(x) & nchar(x) > widths[[i]])
    problem("error", sheet, as.list(row[long]),
            sprintf("%s is longer than %d characters", rep(cols[i], length(long)),
                    widths[[i]]))
  }))
}

# The aliases: the columns sharing one must agree on everything shown, and an
# alias that ends in a range, such as COD1-9, must number its columns exactly
alias_problems <- function(vars) {
  a <- vars[!is.na(vars$ColumnAlias), , drop = FALSE]
  groups <- split(a, list(tolower(a$Dataset), tolower(a$ColumnAlias)),
                  drop = TRUE)
  do.call(rbind, lapply(groups, function(g) {
    where <- sprintf("Alias %s of %s", g$ColumnAlias[1], g$Dataset[1])
    shown <- c("DescriptionAlias", "Theme", "ColumnAction", "ValueAction",
               "Limits")
    differ <- shown[vapply(shown, function(s) {
      length(unique(tolower(g[[s]]))) > 1
    }, logical(1))]
    out <- problem("error", "vars", list(g$row),
                   if (length(differ)) {
                     paste0(where, ": its columns differ in ",
                            paste(differ, collapse = ", "))
                   })
    range <- regmatches(g$ColumnAlias[1],
                        regexec("([0-9]+)-([0-9]+)$", g$ColumnAlias[1]))[[1]]
    if (length(range)) {
      want <- seq(as.integer(range[2]), as.integer(range[3]))
      have <- suppressWarnings(
        as.integer(sub("^.*[^0-9]([0-9]+)$", "\\1", g$Column))
      )
      if (anyNA(have) || !setequal(have, want) || anyDuplicated(have)) {
        got <- if (anyNA(have)) {
          "not all of its columns end in a number"
        } else {
          sprintf("its columns are numbered %s", compress_numbers(have))
        }
        out <- rbind(out, problem(
          "error", "vars", list(g$row),
          sprintf("%s: the alias numbers %s, but %s", where,
                  compress_numbers(want), got)))
      }
    }
    out
  }))
}

# 1, 2, 3, 5 as "1 to 3, 5"
compress_numbers <- function(x) {
  x <- sort(unique(x))
  runs <- split(x, cumsum(c(1, diff(x) != 1)))
  paste(vapply(runs, function(r) {
    if (length(r) == 1) as.character(r) else paste(r[1], "to", r[length(r)])
  }, ""), collapse = ", ")
}

# The rows of the seven tables for one profile, and the problems found
definition_rows <- function(defs, tables, profile = linkage_profile,
                            theme_order = linkage_theme_order,
                            doi_placeholder = "10.9999/exam/ple999") {

  p <- profile$profile
  problems <- list()
  add <- function(x) problems[[length(problems) + 1]] <<- x

  # Datasets, from the run script's table
  need <- c("active", "fullname", "abbrname", "doi", "version", "db", "dat.sc",
            "dat.tb")
  if (length(setdiff(need, names(tables)))) {
    stop("tables has no column ", paste(setdiff(need, names(tables)),
                                        collapse = ", "), call. = FALSE)
  }
  doi <- clean_text(tables$doi)
  datasets <- data.frame(
    Profile      = p,
    DatasetCode  = clean_text(tables$abbrname),
    FullName     = clean_text(tables$fullname),
    DatabaseName = clean_text(tables$db),
    SchemaName   = clean_text(tables$dat.sc),
    TableName    = clean_text(tables$dat.tb),
    DOI          = ifelse(doi %in% doi_placeholder, NA, doi),
    Version      = as.integer(tables$version),
    Active       = as.integer(tables$active) == 1,
    stringsAsFactors = FALSE
  )
  datasets$row <- seq_len(nrow(datasets))
  for (key in c("DatasetCode", "TableName")) {
    dup <- duplicated(tolower(datasets[[key]]))
    add(problem("error", "tables", as.list(which(dup)),
                sprintf("%s %s appears twice", rep(key, sum(dup)),
                        datasets[[key]][dup])))
  }
  blank <- which(is.na(datasets$DatasetCode) | is.na(datasets$FullName) |
                   is.na(datasets$DatabaseName) | is.na(datasets$SchemaName) |
                   is.na(datasets$TableName) | is.na(datasets$Version))
  add(problem("error", "tables", as.list(blank),
              rep("a dataset needs abbrname, fullname, db, dat.sc, dat.tb and version",
                  length(blank))))
  add(width_problems(datasets, column_widths$datasets, "tables"))

  # The workbook's Dataset is the table's name, as dat.tb in the run script
  code_of <- function(dataset) {
    datasets$DatasetCode[match(tolower(dataset), tolower(datasets$TableName))]
  }
  left_out <- function(d, sheet) {
    out <- is.na(code_of(d$Dataset))
    for (ds in unique(d$Dataset[out])) {
      add(problem("left out", sheet, list(d$row[out & d$Dataset == ds]),
                  sprintf("%d rows of Dataset %s, which is not a dat.tb of tables",
                          sum(out & d$Dataset == ds), ds)))
    }
    d[!out, , drop = FALSE]
  }

  # Caveats
  lim <- defs$limits
  caveats <- data.frame(Profile = p, CaveatCode = tolower(lim$code),
                        Symbol = lim$symbol, Colour = toupper(lim$hex),
                        CaveatText = lim$text, row = lim$row,
                        stringsAsFactors = FALSE)
  bad <- which(is.na(caveats$CaveatCode) | is.na(caveats$Symbol) |
                 is.na(caveats$CaveatText) |
                 !grepl("^#[0-9A-F]{6}$", caveats$Colour))
  add(problem("error", "limits", as.list(caveats$row[bad]),
              rep("a caveat needs a code, a symbol, a text and a hex colour such as #CF1F25",
                  length(bad))))
  dup <- which(duplicated(caveats$CaveatCode))
  add(problem("error", "limits", as.list(caveats$row[dup]),
              sprintf("code %s appears twice", caveats$CaveatCode[dup])))
  add(width_problems(caveats, column_widths$caveats, "limits"))

  # Columns
  v <- left_out(defs$vars, "vars")
  missing <- which(is.na(v$Column))
  add(problem("error", "vars", as.list(v$row[missing]),
              rep("a row needs a Column", length(missing))))
  dup <- duplicated(data.frame(tolower(v$Dataset), tolower(v$Column))) &
    !is.na(v$Column)
  add(problem("error", "vars", as.list(v$row[dup]),
              sprintf("Column %s of %s appears twice", v$Column[dup],
                      v$Dataset[dup])))

  action <- toupper(v$ColumnAction)
  value  <- toupper(v$ValueAction)
  caveat <- tolower(v$Limits)
  bad <- which(!is.na(action) & !action %in% c("INCLUDE", "EXCLUDE"))
  add(problem("error", "vars", as.list(v$row[bad]),
              sprintf("ColumnAction %s is not INCLUDE or EXCLUDE",
                      v$ColumnAction[bad])))
  bad <- which(!is.na(value) & value != "SUPPRESS")
  add(problem("error", "vars", as.list(v$row[bad]),
              sprintf("ValueAction %s is not Suppress", v$ValueAction[bad])))
  bad <- which(!is.na(caveat) & !caveat %in% caveats$CaveatCode)
  add(problem("error", "vars", as.list(v$row[bad]),
              sprintf("Limits %s is not a code of the limits sheet",
                      v$Limits[bad])))
  add(alias_problems(v))

  changed <- function(before, after, what) {
    i <- which(!is.na(after) & (is.na(before) | before != after))
    for (from in unique(ifelse(is.na(before[i]), "(blank)", before[i]))) {
      j <- i[ifelse(is.na(before[i]), "(blank)", before[i]) == from]
      add(problem("changed", "vars", list(v$row[j]),
                  sprintf("%s %s written as %s in %d rows", what, from,
                          after[j][1], length(j))))
    }
  }
  action[is.na(action)] <- "INCLUDE"
  changed(v$ColumnAction, action, "ColumnAction")
  changed(v$ValueAction, value, "ValueAction")
  changed(v$Limits, caveat, "Limits")

  columns <- data.frame(
    Profile = p, DatasetCode = code_of(v$Dataset), ColumnName = v$Column,
    Theme = v$Theme, ColumnAlias = v$ColumnAlias,
    DescriptionAlias = v$DescriptionAlias, ColumnAction = action,
    ValueAction = value, CaveatCode = caveat, Comments = v$Comments,
    Other = v$Other, row = v$row, stringsAsFactors = FALSE
  )
  add(width_problems(columns, column_widths$columns, "vars"))
  for (code in setdiff(datasets$DatasetCode, columns$DatasetCode)) {
    add(problem("left out", "tables", list(which(datasets$DatasetCode == code)),
                sprintf("dataset %s (%s) has no rows in the vars sheet", code,
                        datasets$TableName[datasets$DatasetCode == code])))
  }

  # Themes, in the order given; those not listed follow alphabetically
  used <- unique(stats::na.omit(columns$Theme))
  themes <- data.frame(Profile = p, Theme = theme_order,
                       ThemeOrder = 10L * seq_along(theme_order),
                       stringsAsFactors = FALSE)
  add(width_problems(themes, column_widths$themes, "themes"))
  add(problem("left out", "themes", list(""),
              if (length(setdiff(used, theme_order))) {
                paste("themes not in the order, which follow alphabetically:",
                      paste(sort(setdiff(used, theme_order)), collapse = "; "))
              }))
  add(problem("changed", "themes", list(""),
              if (length(setdiff(theme_order, used))) {
                paste("themes in the order that no column has:",
                      paste(setdiff(theme_order, used), collapse = "; "))
              }))

  # Notes, numbered within each dataset in the order of the sheet
  n <- left_out(defs$notes, "notes")
  bad <- which(is.na(n$Aspect) | is.na(n$Comment))
  add(problem("error", "notes", as.list(n$row[bad]),
              rep("a note needs an Aspect and a Comment", length(bad))))
  notes <- data.frame(
    Profile = p, DatasetCode = code_of(n$Dataset),
    NoteOrder = stats::ave(seq_along(n$row), tolower(n$Dataset),
                           FUN = seq_along),
    Aspect = n$Aspect, Issue = n$Issue, Comment = n$Comment, row = n$row,
    stringsAsFactors = FALSE
  )
  add(width_problems(notes, column_widths$notes, "notes"))

  profiles <- data.frame(Profile = p, Title = profile$title,
                         Subtitle = profile$subtitle, stringsAsFactors = FALSE)
  sections <- data.frame(Profile = p,
                         SectionOrder = seq_len(nrow(profile$sections)),
                         Heading = profile$sections$heading,
                         SectionText = profile$sections$text,
                         stringsAsFactors = FALSE)

  drop_row <- function(d) d[setdiff(names(d), "row")]
  list(
    rows = list(
      DictionaryProfiles = profiles,
      DictionarySections = sections,
      DictionaryCaveats  = drop_row(caveats),
      DictionaryThemes   = themes,
      DictionaryDatasets = drop_row(datasets),
      DictionaryColumns  = drop_row(columns),
      DictionaryNotes    = drop_row(notes)
    ),
    problems = do.call(rbind, problems)
  )
}

# A T-SQL literal for each value. Text is written as N'...' pieces, with
# NCHAR() for line breaks and characters outside printable ASCII, so that the
# script is plain ASCII and reads the same in SSMS whatever its encoding.
sql_value <- function(x) {
  if (is.logical(x)) return(ifelse(is.na(x), "NULL", ifelse(x, "1", "0")))
  if (is.numeric(x)) return(ifelse(is.na(x), "NULL", as.character(x)))
  vapply(x, function(s) {
    if (is.na(s)) return("NULL")
    codes <- utf8ToInt(enc2utf8(s))
    if (!length(codes)) return("N''")
    if (any(codes > 65535)) {
      stop("NCHAR() cannot write a character of ", substr(s, 1, 40), call. = FALSE)
    }
    plain <- codes >= 32 & codes <= 126
    runs <- split(codes, cumsum(c(TRUE, diff(plain) != 0 | !plain[-1])))
    pieces <- vapply(runs, function(r) {
      if (r[1] >= 32 && r[1] <= 126) {
        paste0("N'", gsub("'", "''", intToUtf8(r), fixed = TRUE), "'")
      } else {
        sprintf("NCHAR(%d)", r)
      }
    }, "")
    out <- paste(pieces, collapse = " + ")
    # Joined pieces are cut at 4,000 characters unless one is nvarchar(max)
    if (length(pieces) > 1 && length(codes) > 4000) {
      out <- paste0("CAST(N'' AS nvarchar(max)) + ", out)
    }
    out
  }, "", USE.NAMES = FALSE)
}

# INSERT statements for one table, at most `chunk` rows each (SQL Server
# takes up to 1,000 rows in one VALUES list)
sql_insert <- function(table, rows, chunk = 500) {
  if (!nrow(rows)) return(character())
  values <- paste0("    (", do.call(paste, c(lapply(rows, sql_value),
                                              sep = ", ")), ")")
  groups <- split(values, ceiling(seq_along(values) / chunk))
  vapply(groups, function(v) {
    paste0("INSERT INTO ref.", table, " (", paste(names(rows), collapse = ", "),
           ")\nVALUES\n", paste(v, collapse = ",\n"), ";\n")
  }, "", USE.NAMES = FALSE)
}

# The whole script: it stops if the profile is loaded already, adds every row
# in one transaction, and then counts the rows against those expected
sql_script <- function(rows, source = "definitions.xlsx") {
  p <- rows$DictionaryProfiles$Profile[1]
  q <- sql_value(p)
  counts <- vapply(rows, nrow, integer(1))
  order_out <- rev(names(rows))
  c(
    "-- Generated by dev/dictionaries/convert_definitions.R",
    sprintf("-- from %s on %s.", basename(source), format(Sys.Date(), "%d %b %Y")),
    "--",
    sprintf("-- Loads the %s profile into the dictionary tables of Analysis.ref,", p),
    "-- which inst/sql/dictionary_tables.sql creates. Run it once in SQL Server",
    "-- Management Studio. It adds every row in one transaction, and stops,",
    "-- changing nothing, if the profile is loaded already or any row fails.",
    "-- Do not commit it: it holds internal comments and database names.",
    "--",
    "-- To load the profile again, losing any changes made in SSMS, first run:",
    sprintf("--   DELETE FROM ref.%s WHERE Profile = %s;", order_out, q),
    "",
    "USE Analysis;",
    "SET NOCOUNT ON;",
    "SET XACT_ABORT ON;",
    "",
    sprintf("IF EXISTS (SELECT 1 FROM ref.DictionaryProfiles WHERE Profile = %s)", q),
    sprintf("    THROW 50000, N'The %s profile is loaded already: see the top of this script.', 1;", p),
    "",
    "BEGIN TRANSACTION;",
    "",
    unlist(lapply(names(rows), function(t) sql_insert(t, rows[[t]]))),
    "COMMIT TRANSACTION;",
    "",
    "-- The rows loaded, against those expected",
    paste0(
      "SELECT t.TableName, t.Expected, CASE t.TableName\n",
      paste0(sprintf("    WHEN N'%s' THEN (SELECT COUNT(*) FROM ref.%s WHERE Profile = %s)",
                     names(rows), names(rows), q), collapse = "\n"),
      "\n  END AS Loaded\nFROM (VALUES\n",
      paste0(sprintf("    (N'%s', %d)", names(rows), counts), collapse = ",\n"),
      "\n) AS t (TableName, Expected);"
    ),
    sprintf("SELECT CaveatCode, Symbol, Colour FROM ref.DictionaryCaveats WHERE Profile = %s;", q)
  )
}

# Reads the workbook, checks it, and writes the INSERT script to `out`.
# Returns the rows and the problems, invisibly.
convert_definitions <- function(path, tables, out, ...) {
  converted <- definition_rows(read_definitions(path), tables, ...)
  pr <- converted$problems
  show <- function(kind, heading) {
    k <- pr[pr$kind == kind, , drop = FALSE]
    if (nrow(k)) {
      message(heading)
      where <- ifelse(grepl("[ ,]", k$rows), " (rows ", " (row ")
      message(paste0("  ", k$sheet,
                     ifelse(nzchar(k$rows), paste0(where, k$rows, ")"), ""),
                     ": ", k$message, collapse = "\n"))
    }
  }
  if (!is.null(pr)) {
    show("left out", "Left out:")
    show("changed", "Standardised:")
    if (any(pr$kind == "error")) {
      show("error", "Problems to fix in the workbook or tables:")
      stop("Nothing was written. Fix the problems listed and run it again.",
           call. = FALSE)
    }
  }
  script <- sql_script(converted$rows, path)
  writeLines(script, out, useBytes = TRUE)
  message("Wrote ", out, ": ",
          paste(sprintf("%s %d", names(converted$rows),
                        vapply(converted$rows, nrow, integer(1))),
                collapse = ", "))
  invisible(converted)
}

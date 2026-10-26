# tests/testthat/helper-browse.R
#
# A synthetic EpiServer for the Dataset Browser: databases, schemas, tables
# and views, their columns, and three label tables. It answers the browser's
# SQL by reading it, and stops on any other. The offline tests run the app
# on it, and the harness (dev/harness/fake_episerver.R) and the dry run of
# the live tests (dev/harness/fake_live.R) answer the browser's queries with
# it too.
#
# Analysis has three label tables, in the order the browser looks in them:
# - ref.DataProperties: rows for dbo.APC, dbo.Births, dbo.ACTGHSMYX and
#   stage.APC;
# - ref.DataLabels: rows for APC, Births, ED, ACTGHSMYX and the view vw_ED;
# - dbo.DataLabels: rows for Deaths.
# So dbo.APC takes its labels from ref.DataProperties, ED and vw_ED from
# ref.DataLabels, Deaths from dbo.DataLabels, and dbo.Plain has none. Births
# has rows in ref.DataProperties but no dataset label there, so it has no
# dataset label, although ref.DataLabels holds one. With properties = FALSE,
# ref.DataProperties does not exist and those tables fall back to
# ref.DataLabels. HospitalAPC has no label table. Every label is invented.
#
# `fail` holds regular expressions (Perl): a query that matches one fails.
# Returns query(sql), calls() (the SQL sent) and handles(sql), which says
# whether a query is the browser's. It uses nothing from actepir, as the
# harness sources it before the package loads.

fake_browse_episerver <- function(properties = TRUE, fail = character()) {

  p <- function(schema, table, column, type, code, text) {
    data.frame(TableSchema = schema, TableName = table, ColumnName = column,
               PropertyType = type, PropertyCode = code, PropertyText = text,
               stringsAsFactors = FALSE)
  }
  l <- function(dataset, var, type, code, text) {
    data.frame(Dataset = dataset, VarName = var, LabelType = type,
               DataCode = code, LabelName = text, DataType = "varchar",
               stringsAsFactors = FALSE)
  }

  store <- list(
    "ref.DataProperties" = rbind(
      p("dbo", "APC", NA, "DatasetLabel", NA, "Admitted patient care"),
      p("dbo", "APC", NA, "DatasetNote", "2", "Coded second."),
      p("dbo", "APC", NA, "DatasetNote", NA, "Not coded."),
      p("dbo", "APC", NA, "DatasetNote", "1", "Coded first."),
      p("dbo", "APC", "Sex", "VariableLabel", NA, "Sex of the patient"),
      p("dbo", "APC", "sex", "VariableLabel", NA, "A second label, not read"),
      p("dbo", "APC", "Sex", "ValueLabel", "2", "Female"),
      p("dbo", "APC", "Sex", "ValueLabel", "10", "Other"),
      p("dbo", "APC", "Sex", "ValueLabel", "1", "Male"),
      p("dbo", "APC", "Sex", "valuelabel", "9", "Not stated"),
      p("dbo", "APC", "Sex", "ValueLabel", "1", "A second label of 1, not read"),
      p("dbo", "APC", "AgeYrs", "VariableLabel", NA, "Age in years (<1 is 0)"),
      p("dbo", "APC", "AgeYrs", "VariableNote", "10", "Tenth note."),
      p("dbo", "APC", "AgeYrs", "VariableNote", NA, "Uncoded note."),
      p("dbo", "APC", "AgeYrs", "VariableNote", "2", "Second note."),
      p("dbo", "APC", "AgeYrs", "VariableFormat", NA, "%9.0f"),
      p("dbo", "APC", "PatientID", "Link", NA, "APC_Extra"),
      p("dbo", "Births", "Weight", "VariableLabel", NA, "Birth weight in grams"),
      p("dbo", "ACTGHSMYX", NA, "DatasetLabel", NA, "General health survey"),
      p("dbo", "ACTGHSMYX", "RSEX_Y20", "VariableLabel", NA, "Sex of respondent"),
      p("dbo", "ACTGHSMYX", "RSEX_Y20", "ValueLabel", c("1", "2"),
        c("Male", "Female")),
      p("stage", "APC", NA, "DatasetLabel", NA, "Admitted patient care, staged"),
      p("stage", "APC", "Sex", "VariableLabel", NA, "Sex as staged")
    ),
    "ref.DataLabels" = rbind(
      l("APC", NA, "DtaLabel", NA, "Old APC label"),
      l("APC", "Sex", "Var", NA, "Old sex label"),
      l("APC", "Sex", "Opt", "1", "M"),
      l("Births", NA, "DtaLabel", NA, "Births, old label"),
      l("Births", "Weight", "Var", NA, "Old weight label"),
      l("ED", NA, "DtaLabel", NA, "Emergency presentations"),
      l("ED", NA, "NoteDta", NA, "Presentations to public hospitals."),
      l("ED", "Triage", "VAR", NA, "Triage category"),
      l("ED", "Triage", "opt", c("2", "1"), c("Emergency", "Resuscitation")),
      l("ED", "Triage", "NoteVar", c("b", "a"), c("Noted second.", "Noted first.")),
      l("vw_ED", "Triage", "Var", NA, "Triage category, from the view"),
      l("ACTGHSMYX", "RSEX_Y20", "Var", NA, "Sex"),
      l("ACTGHSMYX", "RSEX_Y20", "Opt", c("1", "2"), c("Male", "Female"))
    ),
    "dbo.DataLabels" = rbind(
      l("Deaths", NA, "DtaLabel", NA, "Registered deaths"),
      l("Deaths", "Cause", "Var", NA, "Underlying cause of death")
    )
  )
  if (!properties) store[["ref.DataProperties"]] <- NULL

  # Tables and views by database and schema, label tables included, as
  # INFORMATION_SCHEMA.TABLES lists them
  objects <- function(names, views = character()) {
    data.frame(TABLE_NAME = c(names, views),
               TABLE_TYPE = rep(c("BASE TABLE", "VIEW"),
                                c(length(names), length(views))),
               stringsAsFactors = FALSE)
  }
  catalogue <- list(
    Analysis = list(
      dbo   = objects(c("ACTGHSMYX", "APC", "Births", "DataLabels", "Deaths",
                        "ED", "Plain"), "vw_ED"),
      ref   = objects(c("DataLabels", if (properties) "DataProperties",
                        "DataPropertyTables", "StandardPops")),
      stage = objects("APC")
    ),
    HospitalAPC = list(dbo = objects("APC"))
  )

  cols <- function(col, dtype, charlen = NA, numprec = NA, numscale = NA) {
    data.frame(ord = seq_along(col), col = col, dtype = dtype,
               charlen = charlen, numprec = numprec, numscale = numscale,
               stringsAsFactors = FALSE)
  }
  layout <- function(names) cols(names, "nvarchar", 128L)
  columns <- list(
    "Analysis.dbo.ACTGHSMYX" = cols(c("SVYYEAR", "RSEX_Y20"), "int", NA, 10L, 0L),
    "Analysis.dbo.APC" = cols(c("PatientID", "Sex", "AgeYrs",
                                paste0("Diagnosis", 1:40)),
                              rep(c("int", "varchar"), c(3, 40)),
                              rep(c(NA, 10L), c(3, 40)),
                              rep(c(10L, NA), c(3, 40)),
                              rep(c(0L, NA), c(3, 40))),
    "Analysis.dbo.Births" = cols("Weight", "decimal", NA, 7L, 1L),
    "Analysis.dbo.Deaths" = cols("Cause", "varchar", -1L),
    "Analysis.dbo.ED" = cols(c("PresentationDateTime", "Triage"),
                             c("datetime", "char"), c(NA, 1L)),
    "Analysis.dbo.Plain" = cols("x", "int", NA, 10L, 0L),
    "Analysis.dbo.vw_ED" = cols("Triage", "char", 1L),
    "Analysis.stage.APC" = cols("Sex", "int", NA, 10L, 0L),
    "HospitalAPC.dbo.APC" = cols("Sex", "int", NA, 10L, 0L),
    "Analysis.ref.DataProperties" = layout(c("TableSchema", "TableName",
                                             "ColumnName", "PropertyType",
                                             "PropertyCode", "PropertyText")),
    "Analysis.ref.DataLabels" = layout(c("Dataset", "VarName", "LabelType",
                                         "DataCode", "LabelName", "DataType")),
    "Analysis.dbo.DataLabels" = layout(c("Dataset", "VarName", "LabelType",
                                         "DataCode", "LabelName", "DataType"))
  )

  # Reading the SQL: the quoted values in order, the first [database], and
  # the [database].[schema].[table] after FROM
  quoted <- function(sql) {
    gsub("^N?'|'$", "", regmatches(sql, gregexpr("N?'[^']*'", sql))[[1]])
  }
  database <- function(sql) {
    sub("^\\[([^]]+)\\]$", "\\1", regmatches(sql, regexpr("\\[[^]]+\\]", sql)))
  }
  from <- function(sql) {
    m <- regmatches(sql, regexec(
      "FROM \\[[^]]+\\]\\.\\[([^]]+)\\]\\.\\[([^]]+)\\]", sql))[[1]]
    paste0(m[2], ".", m[3])
  }
  # The rows of a label table that a .lbl_where() condition selects; names
  # are matched in any case, as SQL Server matches them
  held <- function(table, where) {
    d <- store[[table]]
    v <- tolower(quoted(where))
    if (grepl("TableSchema", where, fixed = TRUE)) {
      d[tolower(d$TableSchema) %in% v[1] & tolower(d$TableName) %in% v[2], ,
        drop = FALSE]
    } else {
      d[tolower(d$Dataset) %in% v[1], , drop = FALSE]
    }
  }
  as_properties <- function(d) {
    if (!"Dataset" %in% names(d)) return(d)
    data.frame(TableSchema = NA_character_, TableName = d$Dataset,
               ColumnName = d$VarName, PropertyType = d$LabelType,
               PropertyCode = d$DataCode, PropertyText = d$LabelName,
               stringsAsFactors = FALSE)
  }

  calls <- character()

  query <- function(sql) {
    calls <<- c(calls, sql)
    for (f in fail) {
      if (grepl(f, sql, perl = TRUE)) stop("Fake failure: ", f, call. = FALSE)
    }

    if (grepl("FROM sys.databases", sql, fixed = TRUE)) {
      return(data.frame(name = c("_Archive", "Analysis", "HospitalAPC",
                                 "master", "model", "msdb", "tempdb")))
    }
    db <- database(sql)

    # Catalogue
    if (grepl("SELECT DISTINCT TABLE_SCHEMA", sql, fixed = TRUE)) {
      return(data.frame(TABLE_SCHEMA = names(catalogue[[db]])))
    }
    if (grepl("AS Layout", sql, fixed = TRUE)) {
      found <- if (db == "Analysis") names(store) else character()
      return(data.frame(
        TABLE_SCHEMA = sub("\\..*", "", found),
        TABLE_NAME   = sub(".*\\.", "", found),
        Layout = ifelse(grepl("DataProperties", found), "properties", "labels"),
        stringsAsFactors = FALSE
      ))
    }
    if (grepl("SELECT TABLE_NAME, TABLE_TYPE", sql, fixed = TRUE)) {
      t <- catalogue[[db]][[quoted(sql)[1]]]
      if (is.null(t)) t <- objects(character())
      if (grepl("TABLE_TYPE IN ('BASE TABLE', 'VIEW')", sql, fixed = TRUE)) {
        t <- t[t$TABLE_TYPE %in% c("BASE TABLE", "VIEW"), , drop = FALSE]
      }
      if (grepl("NOT LIKE '%DataLabels%'", sql, fixed = TRUE)) {
        t <- t[!grepl("DataLabels", t$TABLE_NAME, ignore.case = TRUE), ,
               drop = FALSE]
      }
      if (grepl("NOT IN ('DataProperties', 'DataPropertyTables')", sql,
                fixed = TRUE)) {
        t <- t[!t$TABLE_NAME %in% c("DataProperties", "DataPropertyTables"), ,
               drop = FALSE]
      }
      rownames(t) <- NULL
      return(t)
    }
    if (grepl("AS ord", sql, fixed = TRUE)) {
      v <- quoted(sql)
      found <- columns[[paste(db, v[1], v[2], sep = ".")]]
      return(if (is.null(found)) cols(character(), character()) else found)
    }

    # Labels: which label table to read, the dataset labels of a schema and
    # the rows of a table
    if (grepl("WHEN EXISTS", sql, fixed = TRUE)) {
      whens <- regmatches(sql, gregexpr(paste0(
        "WHEN EXISTS \\(SELECT 1 FROM \\[[^]]+\\]\\.\\[([^]]+)\\]\\.",
        "\\[([^]]+)\\] WHERE ([^)]*)\\) THEN ([0-9]+)"), sql))[[1]]
      for (w in whens) {
        m <- regmatches(w, regexec(paste0(
          "\\.\\[([^]]+)\\]\\.\\[([^]]+)\\] WHERE ([^)]*)\\) THEN ([0-9]+)"),
          w))[[1]]
        if (nrow(held(paste0(m[2], ".", m[3]), m[4]))) {
          return(data.frame(Source = as.integer(m[5])))
        }
      }
      return(data.frame(Source = 0L))
    }
    if (grepl("AS Source, ", sql, fixed = TRUE)) {
      parts <- strsplit(sql, "\nUNION ALL\n", fixed = TRUE)[[1]]
      out <- lapply(parts, function(part) {
        d <- as_properties(store[[from(part)]])
        source <- as.integer(sub("(?s)^SELECT (DISTINCT )?([0-9]+).*$", "\\2",
                                 part, perl = TRUE))
        if (grepl("TableSchema =", part, fixed = TRUE)) {
          d <- d[tolower(d$TableSchema) %in% tolower(quoted(part)[1]), ,
                 drop = FALSE]
        }
        if (grepl("^SELECT DISTINCT", part)) {
          names <- unique(d$TableName)
          return(data.frame(Source = rep(source, length(names)),
                            TableName = names, Label = NA_character_,
                            stringsAsFactors = FALSE))
        }
        d <- d[tolower(d$PropertyType) %in%
                 c("datasetlabel", "datalabel", "dtalabel"), , drop = FALSE]
        data.frame(Source = rep(source, nrow(d)), TableName = d$TableName,
                   Label = d$PropertyText, stringsAsFactors = FALSE)
      })
      return(do.call(rbind, out))
    }
    if (grepl("AS PropertyText|SELECT ColumnName, PropertyType", sql)) {
      m <- regmatches(sql, regexec(paste0(
        "WHERE (.*?)\\s+AND (PropertyType|LabelType) (NOT )?IN \\(([^)]*)\\)"),
        sql, perl = TRUE))[[1]]
      d <- as_properties(held(from(sql), m[2]))
      types <- tolower(d$PropertyType) %in% tolower(quoted(m[5]))
      d <- d[if (nzchar(m[4])) !types else types, , drop = FALSE]
      rownames(d) <- NULL
      return(d[c("ColumnName", "PropertyType", "PropertyCode", "PropertyText")])
    }

    stop("fake_browse_episerver: unexpected SQL\n", sql, call. = FALSE)
  }

  list(
    query = query,
    calls = function() calls,
    handles = function(sql) {
      grepl("FROM [", sql, fixed = TRUE) ||
        grepl("FROM sys.databases", sql, fixed = TRUE)
    }
  )
}

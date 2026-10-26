#' Write the Data Dictionaries of Tables on EpiServer
#'
#' @description
#' Builds the data dictionary of one or more tables or views on EpiServer
#' from the label tables of their databases, as the Dataset Browser shows
#' them, and writes each as a PDF and a standalone HTML file. It returns the
#' dictionaries as a data frame. Given a database in `db` in place of tables,
#' it writes the dictionary of every table and view of the database, or of
#' one schema, that has labels.
#'
#' A dictionary holds the table's description (its dataset label), notes and
#' linked tables, and its columns in the table's order, with their variable
#' labels, types, value labels and notes. The labels come from the first
#' label table of the table's database that holds rows for it, as with
#' Automatic in the Dataset Browser, or from the one label table that
#' `lbl_table`, `lbl_schema` and `lbl_db` name (see [episerver_getlabels()]),
#' for every table.
#'
#' @details
#' **Files**
#'
#' Each table gives `<table>_dictionary.pdf` and `<table>_dictionary.html` in
#' `dir`, replacing files of those names. Pages are A4 landscape, with a
#' Nightshade band at the top holding `title`, `subtitle` and the logo. The
#' heading is the table's dataset label, with its full name beneath; a table
#' without a dataset label is headed by its name. The footer gives the table,
#' the label table and the date.
#'
#' The PDF shows the first `max_values` value labels of a column and counts
#' the rest. The HTML file shows them all, folded, with a search box, and
#' needs nothing beyond itself, so it can be sent on. The PDF is made with
#' Typst, through the copy of Quarto that comes with RStudio; without it, use
#' `formats = "html"`.
#'
#' **A whole database**
#'
#' With `db`, and no `tables`, the tables and views of the database, or of
#' `schema` alone, are listed, and those that no label table holds labels
#' for are left out; a message counts them. Without `schema`, the files of
#' each schema go in a folder of its name in `dir`, as tables of the same
#' name can be in more than one schema. A PDF takes a second or two, so a
#' large database is quicker as HTML alone.
#'
#' **Logo**
#'
#' The band shows the actepir lockup, or the logo set with
#' [dictionary_logo()].
#'
#' @param tables The tables: names of tables in `Analysis.dbo`, or full
#'   names, as `"database.schema.table"`, as character; or one lazy table
#'   from EpiServer, such as one from [episerver_quickconnect()]. Leave it
#'   out to give `db`.
#' @param db A database, as character, in place of `tables`: the dictionaries
#'   of all its tables and views that have labels are written.
#' @param schema With `db`, one schema of the database, as character, whose
#'   tables and views are the only ones written.
#' @param formats The files to write: `"pdf"`, `"html"` or both.
#'   `character()` writes none, for the data frame alone.
#' @param dir The folder to write the files to.
#' @param title,subtitle The two lines of the band at the top of each page.
#' @param max_values The number of value labels of a column that the PDF
#'   shows. `Inf` shows them all.
#' @inheritParams episerver_getlabels
#' @inheritDotParams episerver_connect encrypt trust_certificate
#'
#' @return Invisibly, a tibble with a row for each value label of each
#'   column, and one row for a column without value labels: `table` (the
#'   table's full name), `position`, `column`, `description` (the variable
#'   label), `type`, `code` and `value_label` (the value label), and `notes`
#'   (the column's notes, one to a line).
#'
#' @seealso [dictionary_logo()] for the logo, [episerver_browse()], whose
#'   Download menu gives the same files for the table chosen.
#'
#' @keywords episerver
#' @export
#'
#' @examples
#' \dontrun{
#' # The PDF and HTML dictionaries of APC, in Analysis.dbo, in the working
#' # folder
#' episerver_dictionary("APC")
#'
#' # Several tables, as HTML only, in another folder
#' episerver_dictionary(c("APC", "ED", "HospitalAPC.dbo.APC_Extra"),
#'                      formats = "html", dir = "dictionaries")
#'
#' # Every table and view of Analysis.dbo that has labels
#' episerver_dictionary(db = "Analysis", schema = "dbo", dir = "dictionaries")
#'
#' # The dictionary as a data frame, without files
#' dictionary <- episerver_dictionary("APC", formats = character())
#' }
episerver_dictionary <- function(tables = NULL, db = NULL, schema = NULL,
                                 formats = c("pdf", "html"), dir = ".",
                                 title = "Data dictionary",
                                 subtitle = "Epidemiology Section, ACT Health and Community Services Directorate",
                                 max_values = 50, lbl_table = NULL,
                                 lbl_schema = NULL, lbl_db = NULL,
                                 driver = NULL, max_attempts = NULL, ...) {

  whole <- !is_invalid(db)
  if (whole == !is_invalid(tables)) {
    stop("Give the tables, or a database in db, but not both.", call. = FALSE)
  }
  if (!whole && !is_invalid(schema)) {
    stop("schema goes with db. Give a table of another schema as ",
         "\"database.schema.table\".", call. = FALSE)
  }
  formats <- unique(tolower(as.character(formats)))
  bad <- setdiff(formats, c("pdf", "html"))
  if (length(bad)) {
    stop("formats can be \"pdf\" and \"html\", not ",
         paste0("\"", bad, "\"", collapse = ", "), ".", call. = FALSE)
  }
  if (!is.numeric(max_values) || length(max_values) != 1 ||
      is.na(max_values) || max_values < 0) {
    stop("max_values must be a number of 0 or more, or Inf.", call. = FALSE)
  }
  if (length(formats) && !dir.exists(dir)) {
    stop("There is no folder ", dir, ".", call. = FALSE)
  }
  quarto <- NULL
  if ("pdf" %in% formats) {
    quarto <- .dict_quarto()
    if (is.null(quarto)) stop(.dict_no_quarto, call. = FALSE)
  }

  # The tables named: a lazy table brings its connection
  lazy <- inherits(tables, "tbl_sql")
  if (lazy) {
    targets <- list(.lbl_target(.lbl_table_path(tables), lbl_db, lbl_schema,
                                lbl_table))
    conn <- tables$src$con
  } else if (!whole) {
    if (!is.character(tables)) {
      stop("tables must be the names of tables, or a lazy table from EpiServer.",
           call. = FALSE)
    }
    parts <- strsplit(tables, ".", fixed = TRUE)
    odd <- tables[!lengths(parts) %in% c(1, 3)]
    if (length(odd)) {
      stop("Give each table as its name, in Analysis.dbo, or as ",
           "\"database.schema.table\", not ", paste(odd, collapse = ", "), ".",
           call. = FALSE)
    }
    targets <- lapply(parts, .lbl_target, lbl_db, lbl_schema, lbl_table)
    names <- vapply(targets, function(t) t$table, character(1))
    if (length(formats) && anyDuplicated(tolower(names))) {
      same <- names[tolower(names) %in% tolower(names[duplicated(tolower(names))])]
      stop("The tables ", paste(same, collapse = ", "), " have the same name, ",
           "so their files would too. Write them to different folders.",
           call. = FALSE)
    }
  }
  if (!lazy) {
    conn <- episerver_connect(driver = driver, max_attempts = max_attempts, ...)
    on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL),
            add = TRUE)
  }
  query <- function(sql) DBI::dbGetQuery(conn, sql)

  # A whole database: its tables and views that have labels
  if (whole) {
    schema <- if (is_invalid(schema)) NULL else schema
    where <- paste(c(db, schema), collapse = ".")
    label <- .lbl_target(c(db, "", ""), lbl_db, lbl_schema, lbl_table)$label
    listed <- .dict_tables(query, db, schema, label)
    if (!any(listed$labelled)) {
      message(.dict_summary(listed, where), " No dictionary was written.")
      return(invisible(.dict_flat_empty()))
    }
    targets <- .dict_targets(listed, db, label)
  }

  done <- .dict_write_all(query, targets, formats, dir,
                          by_schema = whole && is.null(schema), title, subtitle,
                          max_values, quarto, keep_going = whole,
                          say = function(path) message("Wrote ", path))
  if (whole) {
    message(.dict_summary(listed, where))
    if (length(done$failed)) {
      warning("The dictionaries of these could not be written:\n",
              paste0(names(done$failed), ": ", done$failed, collapse = "\n"),
              call. = FALSE)
    }
  }
  invisible(tibble::as_tibble(do.call(rbind, c(lapply(done$dictionaries, .dict_flat),
                                               list(.dict_flat_empty())))))

}


#' Set the Logo of the Data Dictionaries
#'
#' Sets the logo that the dictionaries of [episerver_dictionary()] and the
#' Dataset Browser show in the band at the top of each page, in place of the
#' actepir lockup. The logo is kept in actepir's folder in R's settings for
#' the user (`tools::R_user_dir("actepir", "config")`), so it stays set in
#' later sessions and projects.
#'
#' The logo sits on the Nightshade band, so a white logo on a transparent
#' background suits it. It should be a PNG at least 600 pixels wide, such as
#' 1,400 by 430, and under 500 KB, as every HTML dictionary carries it. A
#' warning says when a logo is smaller, larger or has no transparency.
#'
#' @param path The PNG file of the logo to set. Without it, the function says
#'   which logo is set.
#' @param remove If `TRUE`, removes the logo set, so the dictionaries show the
#'   actepir lockup again.
#'
#' @return Invisibly, the path of the logo the dictionaries show.
#'
#' @keywords episerver
#' @export
#'
#' @examples
#' \dontrun{
#' # Show your logo on the dictionaries
#' dictionary_logo("C:/logos/logo_white.png")
#'
#' # Which logo is set
#' dictionary_logo()
#'
#' # Back to the actepir lockup
#' dictionary_logo(remove = TRUE)
#' }
dictionary_logo <- function(path = NULL, remove = FALSE) {

  stored <- .dict_logo_file()
  if (isTRUE(remove)) {
    if (!is.null(path)) stop("Give a path or remove = TRUE, not both.", call. = FALSE)
    if (file.exists(stored)) unlink(stored)
    message("The dictionaries will show the actepir lockup.")
    return(invisible(.dict_lockup()))
  }

  if (is.null(path)) {
    if (file.exists(stored)) {
      info <- .png_info(stored)
      message("The dictionaries show your logo, ", stored, " (", info$width,
              " by ", info$height, " pixels).")
      return(invisible(stored))
    }
    message("The dictionaries show the actepir lockup. Give dictionary_logo() ",
            "a PNG to show your own logo.")
    return(invisible(.dict_lockup()))
  }

  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("There is no file ", path, ".", call. = FALSE)
  }
  info <- .png_info(path)
  if (is.null(info)) stop(path, " is not a PNG file.", call. = FALSE)
  if (info$width < 600) {
    warning("The logo is ", info$width, " pixels wide; at least 600 keeps ",
            "it sharp in print.", call. = FALSE)
  }
  if (!info$transparent) {
    warning("The logo has no transparent background, so it will show as a ",
            "box on the Nightshade band. A white logo on a transparent ",
            "background suits it.", call. = FALSE)
  }
  if (info$size > 500 * 1024) {
    warning("The logo is ", round(info$size / 1024), " KB; every HTML ",
            "dictionary carries it, so under 500 KB is better.", call. = FALSE)
  }
  dir.create(dirname(stored), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(path, stored, overwrite = TRUE)) {
    stop("The logo could not be copied to ", stored, ".", call. = FALSE)
  }
  message("The dictionaries will show ", basename(path), ", kept as ", stored,
          ".")
  invisible(stored)

}


# ── The dictionary ──────────────────────────────────────────────────────────

# The dictionary of table schema.table of database db, from its columns and
# its labels as .lbl_read() reads them (`label` names one label table).
# query(sql) runs a query.
#' @noRd
.dict_build <- function(query, db, schema, table, label = NULL) {
  tables <- query(.browse_sql_tables(db, schema))
  i <- match(tolower(table), tolower(tables$TABLE_NAME))
  if (is.na(i)) {
    stop(paste(db, schema, table, sep = "."), " is not a table or view that ",
         "you can read.", call. = FALSE)
  }
  where <- list(db = db, schema = schema, table = tables$TABLE_NAME[i],
                type = if (tables$TABLE_TYPE[i] == "VIEW") "view" else "table")
  cols <- query(.browse_sql_columns(db, schema, where$table))
  r <- .lbl_read(query, db, schema, where$table, label)
  .dict_assemble(where, cols, r$rows, r$source, r$db)
}

# The dictionary of the table `where` (db, schema, table and type) from its
# columns (`cols`, .browse_sql_columns()) and its normalised label rows,
# value labels among them. `source` is the label table read, a row as from
# .lbl_sql_sources(), in database `source_db`, or NULL. Columns are matched
# to labels in lower case, as in the browser's Columns tab, whose order and
# types they keep. Returns the table's names, type and label table (`source`,
# NA when none), its dataset label (NA when none), notes and linked tables,
# and its columns, with each column's value labels (Code and Text) and notes
# in list columns.
#' @noRd
.dict_assemble <- function(where, cols, rows, source = NULL,
                           source_db = where$db) {
  lbl <- .lbl_table(rows)
  key <- tolower(cols$col)
  label <- unname(lbl$label[key])
  columns <- data.frame(
    position    = as.integer(cols$ord),
    column      = as.character(cols$col),
    description = if (length(label)) label else character(),
    type        = .browse_type(cols$dtype, cols$charlen, cols$numprec,
                               cols$numscale),
    stringsAsFactors = FALSE
  )
  no_values <- data.frame(Code = character(), Text = character(),
                          stringsAsFactors = FALSE)
  columns$values <- lapply(key, function(k) {
    v <- lbl$values[[k]]
    if (is.null(v)) no_values else v
  })
  columns$notes <- lapply(key, function(k) {
    n <- lbl$notes[[k]]
    if (is.null(n)) character() else n
  })
  dataset_label <- lbl$dataset$label
  list(
    db     = where$db,
    schema = where$schema,
    table  = where$table,
    type   = where$type,
    name   = paste(where$db, where$schema, where$table, sep = "."),
    source = if (is.null(source)) NA_character_ else {
      paste(c(if (!identical(tolower(source_db), tolower(where$db))) source_db,
              source$TABLE_SCHEMA, source$TABLE_NAME), collapse = ".")
    },
    label   = if (length(dataset_label) && !is.na(dataset_label)) dataset_label else NA_character_,
    notes   = lbl$dataset$notes,
    links   = lbl$dataset$links,
    columns = columns
  )
}

# The dictionary as a data frame: a row for each value label of each column,
# and one for a column without value labels (episerver_dictionary()'s value)
#' @noRd
.dict_flat <- function(d) {
  cols <- d$columns
  parts <- lapply(seq_len(nrow(cols)), function(i) {
    v <- cols$values[[i]]
    n <- max(1L, nrow(v))
    notes <- cols$notes[[i]]
    data.frame(
      table       = d$name,
      position    = cols$position[i],
      column      = cols$column[i],
      description = cols$description[i],
      type        = cols$type[i],
      code        = if (nrow(v)) v$Code else NA_character_,
      value_label = if (nrow(v)) v$Text else NA_character_,
      notes       = if (length(notes)) paste(notes, collapse = "\n") else NA_character_,
      stringsAsFactors = FALSE
    )[seq_len(n), , drop = FALSE]
  })
  out <- do.call(rbind, c(parts, list(.dict_flat_empty())))
  rownames(out) <- NULL
  out
}

# The tables of `listed` (.dict_tables()) that have labels, as targets of
# .dict_write_all(), each read with label table `label` (NULL for Automatic)
#' @noRd
.dict_targets <- function(listed, db, label = NULL) {
  lapply(which(listed$labelled), function(i) {
    list(db = db, schema = listed$schema[i], table = listed$table[i],
         label = label)
  })
}

# What a database's listing (.dict_tables()) found, as a sentence; `where`
# names the database, or the database and schema
#' @noRd
.dict_summary <- function(listed, where) {
  n <- nrow(listed)
  found <- sum(listed$labelled)
  if (!n) return(paste0("There are no tables or views in ", where, " that you can read."))
  if (!found) return(paste0("No table or view of ", where, " has labels."))
  if (found == n) return(paste0("Every table and view of ", where, " has labels."))
  paste0(found, " of the ", n, " tables and views of ", where, " have labels; ",
         "the others were left out.")
}

# Builds and writes the dictionaries of `targets` (lists of db, schema, table
# and label) as `formats` in `dir`, or in a folder of each table's schema
# within it when `by_schema`. say(path) is called for each file written and
# step(i, n) after each table. With keep_going, a table that fails is
# reported in `failed` (its error, named by the table) and the others are
# written; otherwise its error stops them. Returns the dictionaries built and
# the failures.
#' @noRd
.dict_write_all <- function(query, targets, formats, dir, by_schema = FALSE,
                            title = formals(episerver_dictionary)$title,
                            subtitle = formals(episerver_dictionary)$subtitle,
                            max_values = formals(episerver_dictionary)$max_values,
                            quarto = .dict_quarto(),
                            keep_going = FALSE, say = function(path) NULL,
                            step = function(i, n) NULL) {
  dictionaries <- list()
  failed <- character()
  for (i in seq_along(targets)) {
    t <- targets[[i]]
    built <- tryCatch({
      d <- .dict_build(query, t$db, t$schema, t$table, t$label)
      folder <- if (by_schema) file.path(dir, d$schema) else dir
      if (length(formats)) dir.create(folder, showWarnings = FALSE)
      for (format in formats) {
        path <- file.path(folder, paste0(d$table, "_dictionary.", format))
        .dict_write(d, format, path, title, subtitle, max_values, quarto)
        say(path)
      }
      d
    }, error = function(e) {
      if (!keep_going) stop(e)
      failed[[paste(t$db, t$schema, t$table, sep = ".")]] <<- conditionMessage(e)
      NULL
    })
    if (!is.null(built)) dictionaries[[length(dictionaries) + 1]] <- built
    step(i, length(targets))
  }
  list(dictionaries = dictionaries, failed = failed)
}

# The dictionary as data, without rows
#' @noRd
.dict_flat_empty <- function() {
  data.frame(table = character(), position = integer(), column = character(),
             description = character(), type = character(), code = character(),
             value_label = character(), notes = character(),
             stringsAsFactors = FALSE)
}

# The tables and views of database db, or of one schema, with whether a label
# table holds labels for each: the first that holds rows for it, as
# .lbl_read() looks, or the one label table named by `label`. Label tables
# are not listed (.browse_sql_tables()).
#' @noRd
.dict_tables <- function(query, db, schema = NULL, label = NULL) {
  schemas <- if (is.null(schema)) query(.browse_sql_schemas(db))$TABLE_SCHEMA else schema
  tables <- do.call(rbind, c(lapply(schemas, function(s) {
    t <- query(.browse_sql_tables(db, s))
    data.frame(schema = rep(s, nrow(t)), table = as.character(t$TABLE_NAME),
               stringsAsFactors = FALSE)
  }), list(data.frame(schema = character(), table = character(),
                      stringsAsFactors = FALSE))))
  sources <- if (is.null(label)) query(.lbl_sql_sources(db)) else .lbl_named(query, label)
  lbl_db <- if (is.null(label)) db else label[[1]]
  tables$labelled <- vapply(seq_len(nrow(tables)), function(i) {
    if (!nrow(sources)) return(FALSE)
    pick <- query(.lbl_sql_pick(lbl_db, sources, tables$schema[i],
                                tables$table[i]))$Source[1]
    !is.na(pick) && pick > 0
  }, logical(1))
  tables
}

# The text both files show: the heading (the dataset label, or the table's
# name), the line beneath it, where the labels came from, and the date
#' @noRd
.dict_text <- function(d, date = Sys.Date()) {
  n <- nrow(d$columns)
  list(
    heading    = if (is.na(d$label)) d$table else d$label,
    subheading = sprintf("%s: a %s of %d column%s", d$name, d$type, n,
                         if (n == 1) "" else "s"),
    source     = if (is.na(d$source)) {
      "No label table holds labels for this table."
    } else {
      paste0("Labels from ", d$source, ".")
    },
    date       = paste(as.integer(format(date, "%d")), format(date, "%B %Y")),
    name       = d$name
  )
}

# The dictionary as the PDF's template and the HTML file's script read it:
# its text, the dataset's notes as lists of lines, its linked tables, and
# for each column its value labels as "code = label", the first max_values
# of them, with a line counting the rest (NA when none are left out), and
# its notes as lines. `logo` is the logo's file name beside the template.
#' @noRd
.dict_data <- function(d, title, subtitle, max_values = Inf, logo = NULL,
                       date = Sys.Date()) {
  p <- .dict_text(d, date)
  lines <- function(x) as.list(unlist(strsplit(x, "\n", fixed = TRUE)))
  cols <- d$columns
  links <- d$links
  list(
    title      = title,
    subtitle   = subtitle,
    heading    = p$heading,
    subheading = p$subheading,
    source     = p$source,
    date       = p$date,
    name       = p$name,
    logo       = logo,
    notes      = lapply(d$notes, lines),
    links      = as.list(if (nrow(links)) {
      sprintf("%s (on %s)", links$Table, links$Key)
    } else character()),
    has_notes  = any(lengths(cols$notes) > 0),
    columns    = lapply(seq_len(nrow(cols)), function(i) {
      v <- cols$values[[i]]
      shown <- seq_len(min(nrow(v), max_values))
      left <- nrow(v) - length(shown)
      list(
        position    = cols$position[i],
        column      = cols$column[i],
        description = if (is.na(cols$description[i])) "" else cols$description[i],
        type        = cols$type[i],
        values      = if (length(shown)) {
          as.list(paste0(v$Code[shown], " = ", v$Text[shown]))
        } else list(),
        more        = if (left) {
          sprintf("and %s more value label%s; see the HTML dictionary",
                  format(left, big.mark = ","), if (left == 1) "" else "s")
        } else NA_character_,
        notes       = lines(cols$notes[[i]])
      )
    })
  )
}

# Writes dictionary `d` as `format` ("pdf" or "html") to `file`, with the
# defaults of episerver_dictionary()
#' @noRd
.dict_write <- function(d, format, file,
                        title = formals(episerver_dictionary)$title,
                        subtitle = formals(episerver_dictionary)$subtitle,
                        max_values = formals(episerver_dictionary)$max_values,
                        quarto = .dict_quarto()) {
  if (format == "pdf") {
    .dict_pdf(d, file, title, subtitle, max_values, quarto)
  } else {
    .dict_html(d, file, title, subtitle)
  }
}


# ── PDF ─────────────────────────────────────────────────────────────────────

.dict_no_quarto <- paste(
  "The PDF needs Quarto, which comes with RStudio, and it was not found.",
  "Run this in RStudio, or use formats = \"html\"."
)

# Quarto: on the PATH, or the copy in RStudio, beside its pandoc
# (RSTUDIO_PANDOC is <RStudio>/resources/app/bin/quarto/bin/tools); NULL if
# neither is found
#' @noRd
.dict_quarto <- function() {
  quarto <- Sys.which("quarto")
  if (nzchar(quarto)) return(unname(quarto))
  pandoc <- Sys.getenv("RSTUDIO_PANDOC")
  if (!nzchar(pandoc)) return(NULL)
  found <- file.path(dirname(pandoc), c("quarto.exe", "quarto"))
  found <- found[file.exists(found)]
  if (length(found)) found[1] else NULL
}

# Runs `quarto typst compile`, returning its output
#' @noRd
.dict_run_quarto <- function(quarto, typ, pdf) {
  suppressWarnings(system2(quarto, c("typst", "compile", shQuote(typ),
                                     shQuote(pdf)),
                           stdout = TRUE, stderr = TRUE))
}

# The PDF: inst/dictionary/dictionary.typ compiled in a temporary folder,
# beside the dictionary as JSON (.dict_data()) and the logo
#' @noRd
.dict_pdf <- function(d, file, title, subtitle, max_values = 50,
                      quarto = .dict_quarto()) {
  if (is.null(quarto)) stop(.dict_no_quarto, call. = FALSE)
  work <- tempfile("actepir_dictionary_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  typ <- file.path(work, "dictionary.typ")
  file.copy(system.file("dictionary", "dictionary.typ", package = "actepir"), typ)
  logo <- .dict_logo()
  logo_file <- NULL
  if (!is.null(logo)) {
    logo_file <- paste0("logo.", logo$ext)
    file.copy(logo$path, file.path(work, logo_file))
  }
  jsonlite::write_json(
    .dict_data(d, title, subtitle, max_values, logo_file),
    file.path(work, "dictionary.json"),
    auto_unbox = TRUE, null = "null", na = "null", digits = NA
  )
  pdf <- file.path(work, "dictionary.pdf")
  out <- .dict_run_quarto(quarto, typ, pdf)
  if (!file.exists(pdf)) {
    stop("The PDF could not be made. Typst said:\n",
         paste(out, collapse = "\n"), call. = FALSE)
  }
  file.copy(pdf, file, overwrite = TRUE)
  invisible(file)
}


# ── HTML ────────────────────────────────────────────────────────────────────

# The HTML file: one page with nothing external. The CSS and the script
# (inst/dictionary/) and the logo are written into it, and the dictionary as
# JSON (.dict_data(), with every value label), which the search reads.
#' @noRd
.dict_html <- function(d, file, title, subtitle, date = Sys.Date()) {
  tags <- htmltools::tags
  p <- .dict_text(d, date)
  asset <- function(name) {
    paste(readLines(system.file("dictionary", name, package = "actepir"),
                    encoding = "UTF-8", warn = FALSE), collapse = "\n")
  }
  json <- jsonlite::toJSON(.dict_data(d, title, subtitle, Inf, date = date),
                           auto_unbox = TRUE, null = "null", na = "null")
  # So that no text in it is read as HTML inside the script element
  json <- gsub("<", "\\u003c", json, fixed = TRUE)
  logo <- .dict_logo()

  cols <- d$columns
  has_notes <- any(lengths(cols$notes) > 0)
  rows <- lapply(seq_len(nrow(cols)), function(i) {
    v <- cols$values[[i]]
    tags$tr(
      tags$td(cols$position[i]),
      tags$td(cols$column[i]),
      tags$td(if (is.na(cols$description[i])) "" else cols$description[i]),
      tags$td(cols$type[i]),
      tags$td(if (nrow(v)) {
        tags$details(
          tags$summary(sprintf("%s value label%s", format(nrow(v), big.mark = ","),
                               if (nrow(v) == 1) "" else "s")),
          tags$table(class = "values", tags$tbody(lapply(seq_len(nrow(v)), function(j) {
            tags$tr(tags$td(v$Code[j]), tags$td(v$Text[j]))
          })))
        )
      }),
      if (has_notes) tags$td(lapply(cols$notes[[i]], tags$p))
    )
  })
  heads <- c("#", "Column", "Description", "Type", "Value labels",
             if (has_notes) "Notes")
  # The columns' widths in percent, which the CSS keeps whatever their text
  widths <- if (has_notes) c(4, 15, 24, 9, 28, 20) else c(4, 17, 33, 10, 36)
  links <- d$links

  # htmltools moves the content of tags$head() out for Shiny to place, so the
  # head and the html element are written here
  head <- htmltools::tagList(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title(paste0(p$heading, " - ", title)),
    tags$style(htmltools::HTML(asset("dictionary.css")))
  )
  body <- tags$body(
    tags$header(
      class = "band",
      tags$div(tags$p(class = "title", title), tags$p(class = "subtitle", subtitle)),
      if (!is.null(logo)) {
        tags$img(src = paste0("data:", logo$mime, ";base64,",
                              gsub("[\r\n]", "", jsonlite::base64_enc(
                                readBin(logo$path, "raw", file.info(logo$path)$size)
                              ))),
                 alt = logo$alt)
      }
    ),
    tags$main(
      tags$h1(p$heading),
      tags$p(class = "subheading", p$subheading),
      tags$p(class = "source", p$source),
      tags$nav(class = "contents", "Contents: ",
               tags$a(href = "#information", "Dataset information"), " | ",
               tags$a(href = "#columns", "Columns")),
      tags$section(
        id = "information",
        tags$h2("Dataset information"),
        if (!length(d$notes) && !nrow(links)) {
          tags$p("This table has no notes or linked tables.")
        },
        lapply(d$notes, function(n) {
          tags$p(htmltools::HTML(paste(htmltools::htmlEscape(
            strsplit(n, "\n", fixed = TRUE)[[1]]), collapse = "<br>")))
        }),
        if (nrow(links)) {
          tags$p(paste0("Linked tables: ", paste0(links$Table, " (on ", links$Key,
                                                  ")", collapse = ", "), "."))
        }
      ),
      tags$section(
        id = "columns",
        tags$h2("Columns"),
        tags$div(
          class = "tools",
          tags$label(`for` = "search", "Search"),
          tags$input(type = "search", id = "search",
                     placeholder = "Column, label or value label"),
          tags$button(type = "button", id = "expand", "Expand value labels"),
          tags$button(type = "button", id = "collapse", "Collapse value labels"),
          tags$span(id = "count", role = "status")
        ),
        tags$table(
          class = "columns",
          tags$colgroup(lapply(widths, function(w) {
            tags$col(style = paste0("width: ", w, "%"))
          })),
          tags$thead(tags$tr(lapply(heads, tags$th))),
          tags$tbody(rows)
        )
      )
    ),
    tags$footer(paste(p$name, p$source, p$date, sep = "  |  ")),
    tags$script(type = "application/json", id = "dictionary-data",
                htmltools::HTML(json)),
    tags$script(htmltools::HTML(asset("dictionary.js")))
  )
  html <- enc2utf8(paste0(
    "<!DOCTYPE html>\n<html lang=\"en-AU\">\n<head>\n", as.character(head),
    "\n</head>\n", as.character(body), "\n</html>\n"
  ))
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(html), con)
  invisible(file)
}


# ── Logo ────────────────────────────────────────────────────────────────────

# Where dictionary_logo() keeps the logo, and the actepir lockup
#' @noRd
.dict_logo_file <- function() {
  file.path(tools::R_user_dir("actepir", "config"), "dictionary_logo.png")
}
#' @noRd
.dict_lockup <- function() {
  system.file("www", "logo-lockup.svg", package = "actepir")
}

# The logo the dictionaries show: the one set, or the actepir lockup, with
# its file's extension, its media type and its alternative text
#' @noRd
.dict_logo <- function() {
  set <- .dict_logo_file()
  if (file.exists(set)) {
    return(list(path = set, ext = "png", mime = "image/png", alt = "Logo"))
  }
  lockup <- .dict_lockup()
  if (!nzchar(lockup)) return(NULL)
  list(path = lockup, ext = "svg", mime = "image/svg+xml", alt = "actepir")
}

# The width, height, transparency and size of a PNG, from its header and
# chunks; NULL if the file is not a PNG. A PNG is transparent when its
# colour type has an alpha channel (4 or 6) or it has a tRNS chunk, which
# comes before its image data.
#' @noRd
.png_info <- function(path) {
  bytes <- readBin(path, "raw", file.info(path)$size)
  signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (length(bytes) < 33 || !identical(bytes[1:8], signature) ||
      rawToChar(bytes[13:16]) != "IHDR") {
    return(NULL)
  }
  int <- function(b) sum(as.numeric(b) * 256^(3:0))
  transparent <- as.integer(bytes[26]) %in% c(4L, 6L)
  at <- 9
  while (!transparent && at + 7 <= length(bytes)) {
    type <- rawToChar(bytes[(at + 4):(at + 7)])
    if (type %in% c("IDAT", "IEND")) break
    transparent <- type == "tRNS"
    at <- at + 12 + int(bytes[at:(at + 3)])
  }
  list(width = int(bytes[17:20]), height = int(bytes[21:24]),
       transparent = transparent, size = length(bytes))
}

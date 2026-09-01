#' Create a branded ACT Health & Community Services Quarto analysis project
#'
#' Scaffolds a working Quarto analysis in its own directory: a starter `.qmd`,
#' every bundled brand theme, an optional bibliography stub, optional data-access
#' chunks, and the standard project folders.
#'
#' For a single document dropped into a folder you already have — inside an
#' existing project, or nowhere in particular — use [use_epi_document()] instead.
#'
#' Styling is applied through Quarto's `theme` mechanism rather than a format
#' extension, so it can be changed at any time by editing one line. Because
#' *all* bundled themes are copied in, switching between them — or to any of the
#' 25 built-in Bootswatch themes — needs no further setup.
#'
#' This function is also the binding for the *ACTHCS Epidemiology Analysis* entry
#' in the RStudio **New Project** wizard.
#'
#' @param path Directory to create the analysis in. Created if it does not exist.
#' @param name Base name for the analysis document, without extension. Defaults to
#'   the directory name.
#' @param title Document title. Defaults to a tidied version of `name`.
#' @param theme Brand theme the document opens on, e.g. `"acthcsd"` or `"acthd"`.
#'   Sets the initial `theme:` line only; every bundled theme is copied in
#'   regardless, so this is not a lock-in.
#' @param conn Logical. Include the odbc connection chunk for EpiServer 121?
#' @param sel Logical. Include the dbplyr selection chunk? Requires `conn`.
#' @param sql Logical. Include a native SQL chunk run against the connection?
#'   Requires `conn`. Off by default.
#' @param bib Logical. Include a `references.yaml` bibliography stub, worked
#'   citations in the dependencies paragraph, and a References section?
#' @param dirs Logical. Create `data/`, `output/` and `images/` subdirectories?
#' @param overwrite Logical. Replace an existing analysis document of the same name?
#' @param ... Ignored. Absorbs any additional arguments passed by the RStudio wizard.
#'
#' @return Path to the created `.qmd`, invisibly.
#'
#' @keywords quarto template
#'
#' @seealso [use_epi_document()] for a single document, [use_epi_themes()] to
#'   add or refresh themes in an existing project.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' use_epi_analysis("~/analyses/rsv-severity-2026")
#' use_epi_analysis("legacy-report", theme = "acthd")
#' use_epi_analysis("scratch", conn = FALSE, bib = FALSE, dirs = FALSE)
#' }
#'
#' @author Warren Holroyd
#'
use_epi_analysis <- function(path = ".",
                             name = NULL,
                             title = NULL,
                             theme = "acthcsd",
                             conn = TRUE,
                             sel = TRUE,
                             sql = FALSE,
                             bib = TRUE,
                             dirs = TRUE,
                             overwrite = FALSE,
                             ...) {

  flags <- .resolve_data_flags(conn, sel, sql)
  bib   <- .as_flag(bib)
  dirs  <- .as_flag(dirs)

  src   <- .epi_asset_dir()
  theme <- .resolve_theme(src, theme)

  # ---- target directory ----------------------------------------------------
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (is.null(name) || !nzchar(name)) name <- basename(path)
  name <- gsub("[^A-Za-z0-9._-]+", "-", sub("\\.qmd$", "", name))
  if (is.null(title) || !nzchar(title)) {
    title <- tools::toTitleCase(gsub("[-_]+", " ", name))
  }

  qmd <- file.path(path, paste0(name, ".qmd"))
  if (file.exists(qmd) && !overwrite) {
    stop("'", basename(qmd), "' already exists. Pass overwrite = TRUE to replace it.",
         call. = FALSE)
  }

  # ---- themes --------------------------------------------------------------
  # Copy every bundled theme, not just the requested one: switching themes is
  # then a one-line edit with no further copying.
  for (f in list.files(file.path(src, "themes"), pattern = "\\.scss$",
                       full.names = TRUE)) {
    file.copy(f, file.path(path, basename(f)), overwrite = overwrite)
  }

  # ---- analysis document ---------------------------------------------------
  .write_epi_qmd(qmd, src, theme, flags, bib, title)

  # ---- bibliography --------------------------------------------------------
  if (bib) {
    file.copy(file.path(src, "skeleton", "references.yaml"),
              file.path(path, "references.yaml"), overwrite = overwrite)
  }

  # ---- project furniture ---------------------------------------------------
  if (dirs) {
    for (d in c("data", "output", "images")) {
      dir.create(file.path(path, d), showWarnings = FALSE)
      file.create(file.path(path, d, ".gitkeep"), showWarnings = FALSE)
    }
  }

  rproj <- file.path(path, paste0(basename(path), ".Rproj"))
  if (!file.exists(rproj)) {
    writeLines(
      c("Version: 1.0", "",
        "RestoreWorkspace: No", "SaveWorkspace: No", "AlwaysSaveHistory: Default", "",
        "EnableCodeIndexing: Yes", "UseSpacesForTab: Yes", "NumSpacesForTab: 2",
        "Encoding: UTF-8"),
      rproj)
  }

  message("Created ", basename(qmd), " in ", path, " (theme: ", theme, ")")
  invisible(qmd)
}


# ============================================================================
#  Internal helpers, shared with use_epi_document()
# ============================================================================

#' Locate the packaged Quarto assets
#' @return Path to the installed `quarto` asset directory.
#' @noRd
.epi_asset_dir <- function() {
  src <- system.file("quarto", package = "actepir")
  if (!nzchar(src)) {
    stop("Quarto assets not found in the installed package. Reinstall with ",
         "remotes::install_github('actepi/actepir').", call. = FALSE)
  }
  src
}

#' Names of the bundled themes (without the .scss extension)
#' @noRd
.epi_available_themes <- function(src) {
  sub("\\.scss$", "", list.files(file.path(src, "themes"), pattern = "\\.scss$"))
}

#' Validate a requested theme against those installed
#' @noRd
.resolve_theme <- function(src, theme) {
  available <- .epi_available_themes(src)
  theme <- as.character(theme)[1]
  if (!theme %in% available) {
    stop("No such theme: '", theme, "'. Available: ",
         paste(available, collapse = ", "), call. = FALSE)
  }
  theme
}

#' Coerce and reconcile the conn / sel / sql flags
#'
#' The selection and SQL chunks both reference the `con121` object created by the
#' connection chunk, so neither can survive without it. The wizard cannot express
#' that dependency, so it is enforced here for both entry points.
#' @noRd
.resolve_data_flags <- function(conn, sel, sql) {
  conn <- .as_flag(conn); sel <- .as_flag(sel); sql <- .as_flag(sql)
  if (!conn && (sel || sql)) {
    dropped <- c("dbplyr selection"[sel], "SQL"[sql])
    message("Omitting the ", paste(dropped, collapse = " and "),
            " chunk(s): both need the odbc connection chunk.")
    sel <- FALSE; sql <- FALSE
  }
  list(conn = conn, sel = sel, sql = sql)
}

#' Strip the skeleton to the requested options and write it
#' @noRd
.write_epi_qmd <- function(qmd, src, theme, flags, bib, title) {
  tmpl <- readLines(file.path(src, "skeleton", "analysis.qmd"),
                    warn = FALSE, encoding = "UTF-8")

  tmpl <- .strip_block(tmpl, "conn",  keep = flags$conn)
  tmpl <- .strip_block(tmpl, "sel",   keep = flags$sel)
  tmpl <- .strip_block(tmpl, "sql",   keep = flags$sql)
  tmpl <- .strip_block(tmpl, "bib",   keep = bib)
  tmpl <- .strip_block(tmpl, "nobib", keep = !bib)

  tmpl <- gsub("{{THEME}}", paste0(theme, ".scss"), tmpl, fixed = TRUE)
  tmpl <- gsub("{{TITLE}}", title, tmpl, fixed = TRUE)
  tmpl <- gsub("{{DATE}}",  format(Sys.Date()), tmpl, fixed = TRUE)

  writeLines(tmpl, qmd, useBytes = TRUE)
  invisible(qmd)
}

#' Coerce an RStudio wizard widget value to a logical
#' @noRd
.as_flag <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  isTRUE(as.logical(as.character(x)[1]))
}

#' Keep or drop a tagged block in a template
#'
#' Blocks are delimited by lines containing `BEGIN:<tag>` and `END:<tag>`; the
#' surrounding comment syntax is irrelevant, so the same convention works in YAML
#' front matter (`# BEGIN:bib`) and the body (`<!-- BEGIN:bib -->`).
#' @noRd
.strip_block <- function(x, tag, keep) {
  b <- grep(paste0("BEGIN:", tag, "\\b"), x)
  e <- grep(paste0("END:", tag, "\\b"), x)
  if (length(b) == 0L || length(b) != length(e) || any(e < b)) return(x)
  drop <- if (keep) c(b, e) else unlist(Map(seq, b, e))
  x[-drop]
}

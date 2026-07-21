#' Create a branded ACT Health Quarto analysis
#'
#' Scaffolds a working Quarto analysis containing a starter `.qmd` pre-wired to
#' the `acthd-html` format, the `acthd` Quarto format extension (layout, corporate
#' colours, code styling), and optionally a bibliography stub and the standard
#' project folders.
#'
#' Everything is copied out of the installed `actepir` package rather than fetched
#' from GitHub, so the function works with no network access and the styling is
#' pinned to whichever package version the analyst has installed.
#'
#' This function is also the binding for the *ACT Health Epidemiology Analysis*
#' entry in the RStudio **New Project** wizard, which supplies `path` and the
#' `conn` / `bib` / `dirs` arguments from its checkboxes.
#'
#' @param path Directory to create the analysis in. Created if it does not exist.
#'   Supplied automatically by the RStudio New Project wizard.
#' @param name Base name for the analysis document, without extension. Defaults to
#'   the directory name.
#' @param title Document title. Defaults to a tidied version of `name`.
#' @param conn Logical. Include a pre-filled EpiServer data chunk?
#' @param bib Logical. Include a `references.yaml` bibliography stub and a
#'   References section?
#' @param dirs Logical. Create `data/` and `output/` subdirectories?
#' @param overwrite Logical. Replace an existing analysis document of the same name?
#' @param ... Ignored. Absorbs any additional arguments passed by the RStudio wizard.
#'
#' @return Path to the created `.qmd`, invisibly.
#'
#' @keywords quarto template
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # From the console
#' use_epi_analysis("~/analyses/rsv-severity-2026")
#'
#' # Minimal: no database chunk, no bibliography
#' use_epi_analysis("scratch", conn = FALSE, bib = FALSE, dirs = FALSE)
#' }
#'
#' @author Warren Holroyd
#'
use_epi_analysis <- function(path = ".",
                             name = NULL,
                             title = NULL,
                             conn = TRUE,
                             bib = TRUE,
                             dirs = TRUE,
                             overwrite = FALSE,
                             ...) {

  # RStudio wizard checkboxes may arrive as character; normalise everything
  conn <- .as_flag(conn)
  bib  <- .as_flag(bib)
  dirs <- .as_flag(dirs)

  # ---- locate the packaged assets ------------------------------------------
  src <- system.file("quarto", package = "actepir")
  if (!nzchar(src)) {
    stop("Quarto assets not found in the installed package. Reinstall with ",
         "remotes::install_github('actepi/actepir').", call. = FALSE)
  }

  # ---- target directory ----------------------------------------------------
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (is.null(name) || !nzchar(name)) name <- basename(path)
  name <- gsub("[^A-Za-z0-9._-]+", "-", name)
  if (is.null(title) || !nzchar(title)) {
    title <- tools::toTitleCase(gsub("[-_]+", " ", name))
  }

  qmd <- file.path(path, paste0(name, ".qmd"))
  if (file.exists(qmd) && !overwrite) {
    stop("'", basename(qmd), "' already exists. Pass overwrite = TRUE to replace it.",
         call. = FALSE)
  }

  # ---- format extension ----------------------------------------------------
  ext_target <- file.path(path, "_extensions", "acthd")
  if (dir.exists(ext_target) && !overwrite) {
    message("Leaving existing _extensions/acthd in place (overwrite = FALSE).")
  } else {
    file.copy(file.path(src, "_extensions"), path, recursive = TRUE, overwrite = TRUE)
  }

  # ---- analysis document ---------------------------------------------------
  tmpl <- readLines(file.path(src, "skeleton", "analysis.qmd"),
                    warn = FALSE, encoding = "UTF-8")

  tmpl <- .strip_block(tmpl, "conn", keep = conn)
  tmpl <- .strip_block(tmpl, "bib",  keep = bib)

  tmpl <- gsub("{{TITLE}}", title, tmpl, fixed = TRUE)
  tmpl <- gsub("{{DATE}}", format(Sys.Date()), tmpl, fixed = TRUE)

  writeLines(tmpl, qmd, useBytes = TRUE)

  # ---- bibliography --------------------------------------------------------
  if (bib) {
    file.copy(file.path(src, "skeleton", "references.yaml"),
              file.path(path, "references.yaml"),
              overwrite = overwrite)
  }

  # ---- project furniture ---------------------------------------------------
  if (dirs) {
    for (d in c("data", "output")) {
      dir.create(file.path(path, d), showWarnings = FALSE)
      file.create(file.path(path, d, ".gitkeep"), showWarnings = FALSE)
    }
  }

  # Named to match RStudio's own convention, so the wizard simply overwrites it
  rproj <- file.path(path, paste0(basename(path), ".Rproj"))
  if (!file.exists(rproj)) {
    writeLines(
      c("Version: 1.0", "",
        "RestoreWorkspace: No",
        "SaveWorkspace: No",
        "AlwaysSaveHistory: Default", "",
        "EnableCodeIndexing: Yes",
        "UseSpacesForTab: Yes",
        "NumSpacesForTab: 2",
        "Encoding: UTF-8"),
      rproj
    )
  }

  message("Created ", basename(qmd), " in ", path)
  invisible(qmd)
}


#' Coerce an RStudio wizard widget value to a logical
#'
#' Checkbox values may arrive as logical, character or numeric depending on the
#' IDE version. Anything unparseable is treated as FALSE.
#'
#' @param x Value to coerce.
#' @return A length-one logical.
#' @noRd
.as_flag <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  isTRUE(as.logical(as.character(x)[1]))
}


#' Keep or drop a tagged block in a template
#'
#' Blocks are delimited by lines containing `BEGIN:<tag>` and `END:<tag>`. The
#' comment syntax around the tag is irrelevant, which allows the same convention
#' to be used inside YAML front matter (`# BEGIN:bib`) and in the document body
#' (`<!-- BEGIN:bib -->`). A tag may appear more than once.
#'
#' @param x Character vector of template lines.
#' @param tag Block tag, without the `BEGIN:` / `END:` prefix.
#' @param keep If TRUE, remove only the marker lines. If FALSE, remove the
#'   markers and everything between them.
#' @return The modified character vector.
#' @noRd
.strip_block <- function(x, tag, keep) {

  b <- grep(paste0("BEGIN:", tag), x, fixed = TRUE)
  e <- grep(paste0("END:", tag), x, fixed = TRUE)

  # Unbalanced or absent markers: leave the template untouched
  if (length(b) == 0L || length(b) != length(e) || any(e < b)) return(x)

  drop <- if (keep) c(b, e) else unlist(Map(seq, b, e))
  x[-drop]
}

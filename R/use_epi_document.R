#' Create a single branded Quarto analysis document
#'
#' Writes one branded `.qmd` into a folder you already have — inside an existing
#' project, or anywhere at all — and brings the chosen theme with it. This is the
#' "New File" counterpart to [use_epi_analysis()], which builds a whole project.
#' No project folders and no `.Rproj` are created.
#'
#' @details
#' A Quarto theme is a local file: the document header says `theme: acthcsd.scss`,
#' and Quarto resolves that path relative to the document. So a standalone `.qmd`
#' only renders if the `.scss` sits beside it. This function copies the chosen
#' theme next to the new document for exactly that reason; without it, a document
#' created outside a project would fail to render.
#'
#' Only the chosen theme is copied, keeping the folder tidy. Switching to a
#' built-in Bootswatch theme (`theme: cosmo`) still needs no file; to bring the
#' other brand theme in as well, call [use_epi_themes()].
#'
#' An existing `.scss` of the same name is never overwritten, so a theme you have
#' customised in the folder is left untouched.
#'
#' @param name Document name, with or without the `.qmd` extension. Required.
#' @param path Directory to create the document in. Defaults to the working
#'   directory. Created if it does not exist.
#' @param title Document title. Defaults to a tidied version of `name`.
#' @param theme Brand theme, e.g. `"acthcsd"` or `"acthd"`.
#' @param conn Logical. Include the odbc connection chunk for EpiServer 121?
#' @param sel Logical. Include the dbplyr selection chunk? Requires `conn`.
#' @param sql Logical. Include a native SQL chunk? Requires `conn`. Off by default.
#' @param bib Logical. Include the bibliography stub, worked citations and a
#'   References section? A `references.yaml` is written beside the document unless
#'   one is already present.
#' @param overwrite Logical. Replace an existing document of the same name?
#'
#' @return Path to the created `.qmd`, invisibly.
#'
#' @keywords quarto template
#'
#' @seealso [use_epi_analysis()], [use_epi_themes()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In the current folder
#' use_epi_document("subgroup-analysis")
#'
#' # Somewhere specific, on the legacy blue theme, no database chunk
#' use_epi_document("adhoc", path = "~/scratch", theme = "acthd", conn = FALSE)
#' }
#'
#' @author Warren Holroyd
#'
use_epi_document <- function(name,
                             path = ".",
                             title = NULL,
                             theme = "acthcsd",
                             conn = TRUE,
                             sel = TRUE,
                             sql = FALSE,
                             bib = TRUE,
                             overwrite = FALSE) {

  if (missing(name) || is.null(name) || !nzchar(trimws(name))) {
    stop("Please supply a document 'name'.", call. = FALSE)
  }

  flags <- .resolve_data_flags(conn, sel, sql)
  bib   <- .as_flag(bib)

  src   <- .epi_asset_dir()
  theme <- .resolve_theme(src, theme)

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  name  <- gsub("[^A-Za-z0-9._-]+", "-", sub("\\.qmd$", "", trimws(name)))
  if (is.null(title) || !nzchar(title)) {
    title <- tools::toTitleCase(gsub("[-_]+", " ", name))
  }

  qmd <- file.path(path, paste0(name, ".qmd"))
  if (file.exists(qmd) && !overwrite) {
    stop("'", basename(qmd), "' already exists. Pass overwrite = TRUE to replace it.",
         call. = FALSE)
  }

  # Theme travels with the document so it resolves outside a project. Never
  # clobber a theme already sitting in the folder.
  scss <- file.path(path, paste0(theme, ".scss"))
  if (!file.exists(scss)) {
    file.copy(file.path(src, "themes", paste0(theme, ".scss")), scss)
  }

  .write_epi_qmd(qmd, src, theme, flags, bib, title)

  if (bib) {
    refs <- file.path(path, "references.yaml")
    if (!file.exists(refs)) {
      file.copy(file.path(src, "skeleton", "references.yaml"), refs)
    }
  }

  message("Created ", basename(qmd), " in ", path, " (theme: ", theme, ")")
  invisible(qmd)
}


#' RStudio addin: create a branded Quarto document in the current location
#'
#' Prompts for a document name and creates it with [use_epi_document()] in the
#' active project (or the working directory if there is no project), then opens
#' it. Bound as an RStudio addin so it appears in the **Addins** menu and can be
#' assigned a keyboard shortcut, giving a New File-style gesture that the native
#' *New Quarto Document* dialog cannot provide for package templates.
#'
#' @return Path to the created `.qmd`, invisibly, or `NULL` if cancelled.
#'
#' @keywords internal
#'
#' @export
#'
addin_new_epi_document <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop("This addin requires RStudio.", call. = FALSE)
  }

  name <- rstudioapi::showPrompt(
    title   = "New ACTHCS Quarto Document",
    message = "Document name (without .qmd):",
    default = "analysis")
  if (is.null(name) || !nzchar(trimws(name))) return(invisible(NULL))

  proj   <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
  target <- if (!is.null(proj)) proj else getwd()

  qmd <- use_epi_document(name = name, path = target)
  rstudioapi::navigateToFile(qmd)
  invisible(qmd)
}

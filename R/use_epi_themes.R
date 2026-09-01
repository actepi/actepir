#' Add or refresh brand themes in an existing analysis
#'
#' Copies the bundled brand `.scss` theme files out of the installed `actepir`
#' package into an existing project, leaving the analysis document, bibliography
#' and everything else untouched.
#'
#' @details
#' `use_epi_analysis()` copies the themes once, when the project is created, so
#' each project holds a snapshot. Updating `actepir` — or pushing a revised
#' palette to GitHub — has no effect on projects that already exist until this is
#' run. That snapshot behaviour is deliberate: a report rendered last year keeps
#' rendering the way it did last year.
#'
#' Use this to pull in a newly added theme, or to update existing ones after a
#' palette change. To then apply a refreshed theme, no code is needed — edit the
#' `theme:` line in the document.
#'
#' @param path Project directory. Defaults to the working directory.
#' @param which Character vector of theme names (without `.scss`) to copy, or
#'   `NULL` (default) for every theme the package ships.
#' @param overwrite Logical. Replace theme files already present? Defaults to
#'   `TRUE`, since refreshing is the usual intent.
#'
#' @return The names of the themes written, invisibly.
#'
#' @keywords quarto template
#'
#' @seealso [use_epi_analysis()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Everything the installed package ships
#' use_epi_themes()
#'
#' # Just the current palette
#' use_epi_themes(which = "acthcsd")
#' }
#'
#' @author Warren Holroyd
#'
use_epi_themes <- function(path = ".", which = NULL, overwrite = TRUE) {

  src <- system.file("quarto", "themes", package = "actepir")
  if (!nzchar(src)) {
    stop("Quarto themes not found in the installed package. Reinstall with ",
         "remotes::install_github('actepi/actepir').", call. = FALSE)
  }

  available <- sub("\\.scss$", "", list.files(src, pattern = "\\.scss$"))
  if (!length(available)) {
    stop("The installed package contains no themes.", call. = FALSE)
  }

  if (is.null(which)) which <- available

  unknown <- setdiff(which, available)
  if (length(unknown)) {
    stop("No such theme: ", paste(unknown, collapse = ", "),
         ". Available: ", paste(available, collapse = ", "), call. = FALSE)
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  for (t in which) {
    dest <- file.path(path, paste0(t, ".scss"))
    if (file.exists(dest) && !overwrite) {
      message("Leaving existing ", basename(dest), " in place.")
      next
    }
    file.copy(file.path(src, paste0(t, ".scss")), dest, overwrite = TRUE)
    message("Wrote ", basename(dest))
  }

  invisible(which)
}

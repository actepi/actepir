# dev/harness/run_app.R
#
# Runs one of the package's Shiny apps on synthetic data and drives it with a
# Playwright script in headless Chromium. EpiServer and the ABS boundary
# service are replaced by fake_episerver.R and fake_abs.R, so nothing leaves
# the machine.
#
# Usage, from the repository root:
#   Rscript dev/harness/run_app.R <script.js> <output dir> [dsr | browse]
#
# The working tree is installed into a temporary library and the app started
# through its real background launcher (episerver_dsr() or
# episerver_browse()). The script is run as
#   node <script.js> <app URL> <output dir>
# Its output is printed, then the end of the app's error stream. Screenshots
# and the install log go to the output directory; everything else goes to
# temporary directories. The Dataset Browser's queries are answered by
# fake_browse_episerver() in tests/testthat/helper-browse.R.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript dev/harness/run_app.R <script.js> <output dir> ",
       "[dsr | browse]", call. = FALSE)
}
script <- normalizePath(args[1], mustWork = TRUE)
out <- args[2]
app <- if (length(args) >= 3) args[3] else "dsr"
stopifnot(app %in% c("dsr", "browse"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out)

me <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
harness <- dirname(normalizePath(me))
repo <- normalizePath(file.path(harness, "..", ".."))

# The background process loads the package from the library paths of this
# one, so the working tree goes first
lib <- tempfile("actepir_lib_")
dir.create(lib)
log <- file.path(out, "install.log")
status <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "INSTALL", "--no-test-load",
                    paste0("--library=", shQuote(lib)), shQuote(repo)),
                  stdout = log, stderr = log)
if (status != 0) stop("R CMD INSTALL failed: see ", log, call. = FALSE)
.libPaths(c(lib, .libPaths()))

# callr starts the app's process in this working directory and reads its
# .Rprofile, which swaps in the fakes when the package loads
wd <- tempfile("actepir_wd_")
dir.create(wd)
writeLines(c(
  sprintf('source("%s")', file.path(repo, "tests", "testthat", "helper-browse.R")),
  sprintf('source("%s")', file.path(harness, "fake_episerver.R")),
  sprintf('source("%s")', file.path(harness, "fake_abs.R")),
  'setHook(packageEvent("actepir", "onLoad"), function(...) {',
  '  utils::assignInNamespace(".epi_connection", function(connect_args = list()) {',
  '    list(query = fake_query, disconnect = function() invisible(NULL))',
  '  }, ns = "actepir")',
  '  utils::assignInNamespace(".dsr_fetch_json", fake_fetch_json, ns = "actepir")',
  '})'
), file.path(wd, ".Rprofile"))
setwd(wd)

# Boundaries and the compiled theme are cached; a temporary cache leaves the
# user's untouched. No browser opens: Playwright brings its own.
Sys.setenv(R_USER_CACHE_DIR = file.path(wd, "cache"))
options(browser = function(url) invisible(NULL))

suppressPackageStartupMessages(library(actepir))
proc <- switch(app,
  dsr    = episerver_dsr(),
  browse = episerver_browse()
)
url <- get(paste0(app, "_url"), envir = actepir:::.actepir_env)

res <- suppressWarnings(system2("node", c(shQuote(script), url, shQuote(out)),
                                stdout = TRUE, stderr = TRUE))
cat(res, sep = "\n")

cat("\n--- app error stream (last 40 lines) ---\n")
cat(utils::tail(strsplit(proc$read_error(), "\n")[[1]], 40), sep = "\n")

switch(app, dsr = episerver_dsr_stop(), browse = episerver_browse_stop())
quit(status = if (is.null(attr(res, "status"))) 0 else attr(res, "status"))

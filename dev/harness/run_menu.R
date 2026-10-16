# dev/harness/run_menu.R
#
# Opens the ACT Epidemiology menu (the dialog behind Addins > Open Menu) and
# drives it with menu.js. RStudio is not needed: in a background process the
# dialog is told that RStudio is available, and its gadget is served on a
# local port as an ordinary app.
#
# Usage, from the repository root:
#   Rscript dev/harness/run_menu.R <output dir>
#
# Prints the script's output and the choice the dialog returned.

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else tempfile("actepir_menu_")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out)

me <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
harness <- dirname(normalizePath(me))
repo <- normalizePath(file.path(harness, "..", ".."))

port <- httpuv::randomPort()
proc <- callr::r_bg(function(repo, serve_port) {
  pkgload::load_all(repo, quiet = TRUE)
  utils::assignInNamespace("isAvailable", function(...) TRUE, ns = "rstudioapi")
  utils::assignInNamespace(
    "runGadget",
    function(app, server = NULL, port = NULL, viewer = NULL,
             stopOnCancel = TRUE) {
      shiny::runApp(shiny::shinyApp(app, server), port = serve_port,
                    launch.browser = FALSE)
    },
    ns = "shiny"
  )
  utils::getFromNamespace("episerver_display_dialog", "actepir")()
}, args = list(repo = repo, serve_port = port))

res <- suppressWarnings(system2(
  "node", c(shQuote(file.path(harness, "menu.js")),
            sprintf("http://127.0.0.1:%d", port), shQuote(out)),
  stdout = TRUE, stderr = TRUE
))
cat(res, sep = "\n")

proc$wait(15000)
if (proc$is_alive()) {
  proc$kill()
  cat("\nThe dialog did not close.\n")
} else {
  cat("\nThe dialog returned:\n")
  utils::str(proc$get_result())
}

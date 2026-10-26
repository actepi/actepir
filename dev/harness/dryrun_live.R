# dev/harness/dryrun_live.R
#
# Dry run of the live tests (tests/testthat/test-*_live.R) against the fake
# EpiServer and ABS boundary service in fake_live.R. It shows that the test
# code runs and that its expectations hold on synthetic data, before the
# maintainer runs the tests against EpiServer.
#
# Usage, from the repository root:
#   Rscript dev/harness/dryrun_live.R [--without-dataproperties]
#
# The tests are copied to a temporary directory with the package's helpers
# and a last helper that sources fake_live.R; nothing in the repository
# changes. With --without-dataproperties, the fake Analysis database has no
# ref.DataProperties, as before the label table is created.

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
harness <- dirname(normalizePath(script))
repo <- normalizePath(file.path(harness, "..", ".."))

tests <- tempfile("actepir_live_")
dir.create(tests)
from <- file.path(repo, "tests", "testthat")
invisible(file.copy(list.files(from, "^helper-.*[.]R$", full.names = TRUE),
                    tests))
invisible(file.copy(list.files(from, "_live[.]R$", full.names = TRUE), tests))
properties <- !"--without-dataproperties" %in% commandArgs(trailingOnly = TRUE)
writeLines(c(
  sprintf('harness_dir <- "%s"', harness),
  sprintf('browse_properties <- %s', properties),
  'source(file.path(harness_dir, "fake_live.R"), local = TRUE)'
), file.path(tests, "helper-zz-fake.R"))

suppressPackageStartupMessages({
  pkgload::load_all(repo, export_all = TRUE, quiet = TRUE)
  library(testthat)
})

# Map boundaries are cached; a temporary cache leaves the user's untouched
Sys.setenv(R_USER_CACHE_DIR = file.path(tests, "cache"))

res <- as.data.frame(testthat::test_dir(tests, load_package = "none",
                                        reporter = "summary",
                                        stop_on_failure = FALSE))
cat(sprintf("\nDry run: %d tests, %d expectations, %d failed, %d errors, %d skipped\n",
            nrow(res), sum(res$nb), sum(res$failed), sum(res$error),
            sum(res$skipped)))
quit(status = if (sum(res$failed) + sum(res$error) > 0) 1 else 0)

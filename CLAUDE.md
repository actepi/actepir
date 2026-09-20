<!-- Instructions for Claude Code sessions. Every line is loaded into every session, so keep it
     short and specific (under 200 lines) and delete rules that no longer apply. -->

# actepir

R package for analysts in the Epidemiology Section, ACT Health and Community Services Directorate (ACT Government). It provides access to EpiServer (SQL Server over ODBC) with data labels applied, ACT Government branded ggplot2 and gt themes, and Quarto and R Markdown analysis templates. Public repository, GPL-3. Maintainer: Warren Holroyd.

## Where the package runs

- Analysts use it on a locked-down Windows SOE inside the ACT Government network: AppLocker, Documents redirected to UNC paths, internet access through the ACTGOV proxy, RStudio as the IDE.
- Analysts have no Rtools. Keep the package free of compiled code. Every dependency must be available from CRAN as a Windows binary.
- `pak` is blocked by group policy. User-facing install instructions use `remotes::install_github("actepi/actepir")`.
- EpiServer accepts Windows authentication from inside the ACT Government network only. Cloud sessions and CI can never connect to it.
- The repository is public. Never add credentials or data to it.

## Workflow

- Work only on the session branch. Never push to `main`. One logical change per branch and pull request.
- The maintainer pulls each branch onto a machine inside the network, tests it against EpiServer, and merges only if it passes. Write the pull request description for that step:
  1. what changed and why;
  2. what was verified offline, with the commands run and their results;
  3. what could not be verified offline, with the exact calls the maintainer should run against EpiServer to confirm it.
- Keep diffs minimal. Match the style of the file being edited and do not reformat code the change does not touch.
- Do not change `Version` in `DESCRIPTION`. The maintainer sets the version when merging.
- If a request conflicts with a rule in this file, say so and ask before proceeding.

## Commands

The cloud environment provides R with `roxygen2`, `testthat`, `rcmdcheck`, `remotes` and the package's dependencies. `devtools` is not installed, so call the underlying functions:

- Document: `Rscript -e 'roxygen2::roxygenise()'`
- Test: `Rscript -e 'testthat::test_local()'`
- Check: `Rscript -e 'rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "warning")'`

Run all three before pushing whenever R code, roxygen comments or tests change.

- If R or a package is missing, read `/var/log/r-setup.log`, install what the task needs for the session, and tell the maintainer what failed so the environment's setup script can be corrected.
- Quarto is not installed. Do not try to render the templates. The maintainer does rendering and visual checks.

## Tests

- testthat 3rd edition.
- A test that needs a live EpiServer connection calls `skip_if_no_episerver()` as its first line. The helper lives in `tests/testthat/helper-episerver.R`. It decides by probing whether the server is reachable (short timeout, result cached for the test run), never by detecting the environment. One mechanism therefore covers cloud sessions, CI and an EpiServer outage on the maintainer's machine. Do not use `skip_on_ci()` or environment flags for this.
- Every other test must pass offline. Write new code so that logic which does not need the database (validation, SQL and string building, file scaffolding) sits in its own function and has offline tests.
- Tests write only to temporary directories (`withr::local_tempdir()`) and restore any options or environment variables they change.
- When a change touches code that talks to EpiServer, add or update a live test for it even though it will be skipped offline. The maintainer runs it.

## Documentation

- roxygen2 with markdown. Never edit `man/` or `NAMESPACE` by hand.
- Generate documentation with the roxygen2 version recorded in `Config/roxygen2/version` in `DESCRIPTION`, which is the version on the maintainer's machine. If the installed version differs, install the recorded one first with `remotes::install_version()`. Never commit a change to that field.
- Australian English in documentation, messages and comments. Exported names keep their existing spelling (the `acthd_*_color()` functions follow ggplot2).
- Keep roxygen text and inline comments concise. Comments explain what the code does and any constraint that is not obvious from it. They do not narrate the history of a change.

## Code conventions

- Runtime dependencies go in `Imports`. `Suggests` is for test-only packages.
- One mechanism per job. Do not add a second way to do or configure something the package already supports. If the existing mechanism is inadequate, change it.
- Every EpiServer connection goes through `episerver_connect()`. Functions that connect pass connection arguments through `...`, documented with `@inheritDotParams episerver_connect encrypt trust_certificate`.
- Do not add R option or environment variable fallbacks for connection arguments. That approach was tried and rejected as unintuitive. The fallback still present in `episerver_connect()` is due for removal. Do not extend it or rely on it.
- `proxy_actgov_toggle()` is legacy. Leave it as it is.
- RStudio integration is limited to what `inst/rstudio/` supports: addins and project templates. RStudio has no API for adding menu bar items.

## Quarto and R Markdown templates

- Quarto styling uses the Sass theme files in `inst/quarto/themes/`, referenced from the document's `theme:` key. `use_epi_analysis()`, `use_epi_document()` and `use_epi_themes()` copy them next to the document. A Quarto format extension (`_extensions/`) was tried and abandoned because two extensions in one project collided on the title banner. Do not reintroduce it.
- Optional sections of `inst/quarto/skeleton/analysis.qmd` are wrapped in `BEGIN:<tag>` and `END:<tag>` markers and kept or dropped by `.strip_block()`. Add a new optional section the same way, with a matching argument in both scaffolding functions and a widget in `inst/rstudio/templates/project/epi_analysis.dcf`.

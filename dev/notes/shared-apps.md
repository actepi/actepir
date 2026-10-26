# Shared app infrastructure and the add-ins

The package's Shiny apps share the code in `R/epi_app.R`. There are two: the Dataset Browser (`episerver_browse()`, see `dataset-browser.md`) and the DSR Calculator (`episerver_dsr()`). Analysts open each from its own entry in the RStudio Addins menu, Open Dataset Browser and Open DSR Calculator (`inst/rstudio/addins.dcf`). Build new analytical tools the same way.

## Where the code is

| Function | Does |
|---|---|
| `.run_app()` | Runs an app factory as a gadget in the session, or by default in a background `callr` process so the console stays free. Reuses an app that is already running, and opens it in the web browser. |
| `.stop_app()` | Stops a background app. |
| `.epi_connection()` | Opens one EpiServer connection per app through `episerver_connect()`. It checks the connection before each query and reconnects once after an error. Returns `list(query, disconnect)`. |
| `.epi_send_code()`, `.epi_copy_text()`, `.epi_copy_script()` | Insert generated code at the cursor in RStudio, or copy text to the clipboard from the page. |
| `.epi_contact`, `.epi_session_report()` | Where problem reports go (healthinfo@act.gov.au), and lines on the R session for them. |
| `.epi_bug_button()`, `.epi_report_script()`, `.epi_report_server()`, `.epi_problem_report()` | The problem report: the bug button at the right of a sidebar's foot, the browser's part of the report (`bug_client`), the dialog with Copy report, and the report's text from an app's state, settings and code (see `dsr-calculator.md`). |
| `.epi_stopped_script()` | The page shown once an app stops, saying how to open it again. |
| `.epi_with_help()` | A setting's notes behind an information button beside its label. |
| `.epi_tab_settings()` | A tab's own settings in a panel down the left of its results, distinct from the app's sidebar. |
| `.epi_logo()` | The actepir lockup (`inst/www/logo-lockup.svg`: the hex sticker's pyramid and the package's name) as a data URI. |
| `.epi_theme()` | The bslib (Bootstrap 5) theme in the brand purples and fonts of `dev/reference/act-brand.md`, with Bootstrap's dialog margin. |
| `.epi_app_style()` | CSS for every app: the spacing and type tokens and the rules that apply them to controls and sections, the sidebars' tint, buttons, focus, notes, information and icon buttons, sidebar feet and button stacks, the sidebar resize handle, Help tabs, the problem report, the stopped page, notifications, DataTables and sliders. |

`test-epi_app.R` tests the theme against `dev/reference/act-brand.md`, the spacing tokens and the rules that apply them, the hover and focus rules, the notifications, the information button, the problem report and its session lines, the logo, and the add-ins.

`dev/reference/app-style.md` is the style guide that these implement: layout, spacing, type and components for every app.

## Decisions

- **Background process by default.** The console stays free while an app runs.
  - The process picks and binds its own port and reports it through a file, which avoids a race.
  - Supervision is tried first and dropped if it fails, because AppLocker can block processx's supervisor (system error 1260).
  - The worker ends itself when the server stops, because R's shutdown can hang on ODBC handles.
- **bslib, not shinydashboard,** for both apps: the DSR Calculator from pull request #6, and the Dataset Browser, which used miniUI, from pull request 1 of its plan (`dataset-browser.md`). shinydashboard is Bootstrap 3 with AdminLTE skins, which would need CSS overrides for the brand colours. bslib gives a sidebar, collapsible sections and a Sass theme, and comes with shiny. No app uses miniUI, so it is no longer in Imports, and its title bar and Done rules have gone from `.epi_app_style()`.
- **The web browser only.** Both apps open in the system web browser, and neither function has a `display` argument. The maintainer chose one place to open: the Viewer is too small for the DSR Calculator, and the RStudio window is modal over the IDE. With `background = FALSE` the gadget opens in the browser too.
- **An add-in for each app.** Each add-in is bound directly to the app's function, which needs no arguments. The ACT Epidemiology menu, a dialog in the Viewer offering each app in three displays, was removed: it added a step before each app.
- **Notifications** (Copied, the progress of a query, warnings) look like the info bar: its tint with a Nightshade left border and text, in place of Shiny's blue and grey boxes. Warnings and errors take the info bar's amber (`#8a5300`) and red (`#b00020`). The rules are scoped by `#shiny-notification-panel`, so they win over Shiny's stylesheet whatever the order.
- **Spacing and type in one place.** Tokens in `.epi_app_style()` (`--epi-font`, `--epi-gap`, `--epi-pad` and others) are applied by rules on the classes of Shiny, Bootstrap 3 and 5, bslib and shinydashboard, so a control in any app takes them without CSS of its own. The values are the DSR Calculator's sidebar, which the maintainer chose. CSS custom properties were used rather than bslib's Sass variables, which reach Bootstrap 5 only. Checked in the DSR Calculator, the Dataset Browser (then a miniUI app) and a shinydashboard page.
- **Problem reports.** `.epi_session_report()` gives what is needed to reproduce a user's session and nothing that identifies data. Proxy variables are named, never given. Reports go to `.epi_contact`; the maintainer asked that no person be named. The bug button, its script and the dialog are shared; each app gives `.epi_report_server()` a function that writes its state, settings and code with `.epi_problem_report()`.
- **Selected table rows** are Lavender (`#DCC8FA`) with Nightshade text and an Iris bar at their left, in Bootstrap 3 and 5. The info bar's tint, used at first, was too faint beside striped rows (the maintainer, 26 Oct 2026). DataTables colours them from `--dt-row-selected`, which bslib's Bootstrap 5 theme sets on `.table.dataTable` from the primary colour (Nightshade), so `.epi_app_style()` sets the variables on `:root` and on `.table.dataTable.dataTable`.
- **Hover and focus.** Buttons go to Iris (`#562C8C`) on hover and on keyboard focus. Focus rings show only for keyboard focus (`:focus-visible`), so a mouse click leaves no highlight.
- **Fonts are named, not downloaded.** Montserrat and Source Sans 3 are used where installed, otherwise the brand's alternatives, Arial and Calibri.
- **The logo is a data URI,** so it works in background apps without `addResourcePath()`.
- **Top bar.** In a bslib app the name is at the left, the tabs 48px after it, and the actepir lockup at the right. The rules are in `.epi_app_style()` (`.navbar`, `.navbar-brand`, `.epi-logo`).
- **The actepir lockup, not the ACT Government logo.** The maintainer replaced the ACT Government logo with the package's hex sticker. The whole hexagon at the bar's height was compared with a lockup of its pyramid and name, with and without a white outline: the hexagon came out about 60px tall with its name set at about 8px, while the lockup sets the name at about 26px and needs no outline, as it has no edge to set off against the Nightshade bar. The ACT Government logo's file was removed, as no app used it any more. `dev/reference/hex-sticker.md` sets out how the lockup is built.
- **No Done.** Each app runs in a browser tab, which the analyst closes. The background process keeps running until its `_stop()` function or the end of the R session, and the add-in or the app's function reopens it at once (`.run_app()` reuses a running app). In the foreground, closing the tab stops the app (`session$onSessionEnded()`), as the app holds the console. Once an app stops, `.epi_stopped_script()` says so in its tab, and how to open it again.
- **A tab's own settings** go in `.epi_tab_settings()`, a nested bslib `layout_sidebar()` with the class `epi-tab-layout`. The shared style gives it the sidebar's tint and spacing but no dividing border, a height that follows its settings, and a sticky position, so that it reads as a panel within the tab rather than a second app sidebar. `layout_sidebar()` puts named attributes on its main area, so the class is added to the layout afterwards.
- **Theme tests turn off the sass cache.** Tests that compile the theme set `withr::local_options(sass.cache = FALSE)`, or sass writes to the user's cache folder.
- **Tests of the code button's label mock `rstudioapi::isAvailable()`.** An app reads it when it is built: TRUE, as in RStudio, gives Insert Code, and FALSE, as in a background process, Copy Code. A test that does not fix it passes in a cloud session but fails when the maintainer runs the tests in RStudio. The tests use `local_mocked_bindings(isAvailable = function(...) FALSE, .package = "rstudioapi")`, and one sets TRUE to check Insert Code.

## Adding a tool

1. **Factory.** Write a factory `<tool>_app(..., query = NULL)` that returns a `shiny::shinyApp()`. Without `query`, the app connects through `.epi_connection()`. With it, the app uses that function in place of EpiServer, so tests can pass a fake, as `episerver_dsr_app()` does.
2. **Functions.** Write a user-facing function that calls `.run_app(key = "<tool>", ...)`, and a `_stop()` function that calls `.stop_app()`.
3. **Page.** Use `bslib::page_navbar()` with `.epi_theme()`, `.epi_app_style()`, `.epi_logo()`, `.epi_copy_script()`, the problem report (`.epi_bug_button()`, `.epi_report_script()`, `.epi_report_server()`) and `.epi_stopped_script()`, as in `episerver_dsr_app()` and `episerver_browse_app()`, and follow `dev/reference/app-style.md`. A shinydashboard page adds `.epi_app_style()` to its body.
4. **Add-in.** Add an entry to `inst/rstudio/addins.dcf`, named "Open <tool>" and bound to the user-facing function, and add it to the add-ins test in `test-epi_app.R`.
5. **Offline checks.** Add the tool's queries to `dev/harness/fake_episerver.R`, a browser script in `dev/harness/`, and a case in `dev/harness/run_app.R`.
6. **Notes.** Start `dev/notes/<tool>.md` from the template in `README.md`.

## Open items

- shinydashboard's colours are not themed (see `dev/reference/app-style.md`).

# Shared app infrastructure and the add-ins

The package's Shiny apps share the code in `R/epi_app.R`. There are two: the EpiServer Browser (`episerver_browse()`) and the DSR Calculator (`episerver_dsr()`). Analysts open each from its own entry in the RStudio Addins menu, Open EpiServer Browser and Open DSR Calculator (`inst/rstudio/addins.dcf`). Build new analytical tools the same way.

## Where the code is

| Function | Does |
|---|---|
| `.run_app()` | Runs an app factory as a gadget in the session, or by default in a background `callr` process so the console stays free. Reuses an app that is already running, and opens it in the web browser. |
| `.stop_app()` | Stops a background app. |
| `.epi_connection()` | Opens one EpiServer connection per app through `episerver_connect()`. It checks the connection before each query and reconnects once after an error. Returns `list(query, disconnect)`. |
| `.epi_send_code()`, `.epi_copy_text()`, `.epi_copy_script()` | Insert generated code at the cursor in RStudio, or copy text to the clipboard from the page. |
| `.epi_logo()` | The white ACT Government logo as a data URI. |
| `.epi_theme()` | The bslib (Bootstrap 5) theme in the brand purples and fonts of `dev/reference/act-brand.md`. |
| `.epi_app_style()` | CSS for what Bootstrap's variables do not reach: buttons, focus, DataTables and sliders. |

`test-epi_app.R` tests the theme against `dev/reference/act-brand.md`, the hover and focus rules, the logo, and the add-ins.

## Decisions

- **Background process by default.** The console stays free while an app runs.
  - The process picks and binds its own port and reports it through a file, which avoids a race.
  - Supervision is tried first and dropped if it fails, because AppLocker can block processx's supervisor (system error 1260).
  - The worker ends itself when the server stops, because R's shutdown can hang on ODBC handles.
- **bslib, not shinydashboard,** for the DSR Calculator's layout (pull request #6). shinydashboard is Bootstrap 3 with AdminLTE skins, which would need CSS overrides for the brand colours. bslib gives a sidebar, collapsible sections and a Sass theme, and comes with shiny. The EpiServer Browser still uses miniUI.
- **The web browser only.** Both apps open in the system web browser, and neither function has a `display` argument. The maintainer chose one place to open: the Viewer is too small for the DSR Calculator, and the RStudio window is modal over the IDE. With `background = FALSE` the gadget opens in the browser too.
- **An add-in for each app.** Each add-in is bound directly to the app's function, which needs no arguments. The ACT Epidemiology menu, a dialog in the Viewer offering each app in three displays, was removed: it added a step before each app.
- **Hover and focus.** Buttons go to Iris (`#562C8C`) on hover and on keyboard focus. Focus rings show only for keyboard focus (`:focus-visible`), so a mouse click leaves no highlight.
- **Fonts are named, not downloaded.** Montserrat and Source Sans 3 are used where installed, otherwise the brand's alternatives, Arial and Calibri.
- **The logo is a data URI,** so it works in background apps without `addResourcePath()`.
- **Theme tests turn off the sass cache.** Tests that compile the theme set `withr::local_options(sass.cache = FALSE)`, or sass writes to the user's cache folder.

## Adding a tool

1. **Factory.** Write a factory `<tool>_app(..., query = NULL)` that returns a `shiny::shinyApp()`. Without `query`, the app connects through `.epi_connection()`. With it, the app uses that function in place of EpiServer, so tests can pass a fake, as `episerver_dsr_app()` does.
2. **Functions.** Write a user-facing function that calls `.run_app(key = "<tool>", ...)`, and a `_stop()` function that calls `.stop_app()`.
3. **Page.** Use `bslib::page_navbar()` with `.epi_theme()`, `.epi_app_style()`, `.epi_logo()` and `.epi_copy_script()`, as in `episerver_dsr_app()`.
4. **Add-in.** Add an entry to `inst/rstudio/addins.dcf`, named "Open <tool>" and bound to the user-facing function, and add it to the add-ins test in `test-epi_app.R`.
5. **Offline checks.** Add the tool's queries to `dev/harness/fake_episerver.R`, a browser script in `dev/harness/`, and a case in `dev/harness/run_app.R`.
6. **Notes.** Start `dev/notes/<tool>.md` from the template in `README.md`.

## Open items

- The EpiServer Browser still uses miniUI. Moving it to the bslib layout would make the apps consistent.
- The EpiServer Browser has no offline tests. `episerver_browse_app()` has no `query` argument, so the harness runs it by replacing `.epi_connection()`.
- Done in the EpiServer Browser leaves its browser tab greyed out. The DSR Calculator says that it has stopped and how to start it again (the Done script in `episerver_dsr_app()`); the Browser, now always in the browser, could do the same.

# Infrastructure shared by the package's Shiny apps: the EpiServer Browser
# (episerver_browse()) and the DSR Calculator (episerver_dsr()).

# Package-level environment holding the background process handles
.actepir_env <- new.env(parent = emptyenv())


# Runs the app built by the internal factory function named `factory`, as a
# gadget in this session or in a background R process (see episerver_browse()
# for the technique). The factory is passed by name so that the background
# process can find it in the installed package. Each app is identified by
# `key`: its process handle, URL and port are kept in .actepir_env as
# <key>_proc, <key>_url and <key>_port. `title` names the app in windows and
# messages; `fn` is the user-facing function named in hints.
#' @noRd
.run_app <- function(key, title, fn, factory, factory_args = list(),
                     background = TRUE,
                     display = c("viewer", "window", "browser"),
                     port = NULL) {

  display <- match.arg(display)

  if (!background) {
    app <- do.call(utils::getFromNamespace(factory, "actepir"), factory_args)
    viewer_fn <- switch(display,
      viewer  = shiny::paneViewer(minHeight = 550),
      window  = shiny::dialogViewer(title, width = 1000, height = 800),
      browser = shiny::browserViewer()
    )
    return(shiny::runGadget(app, viewer = viewer_fn))
  }

  # ── Background mode ──────────────────────────────────────────────────────
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop(
      "The 'callr' package is required for background mode.\n",
      "Install it with: install.packages('callr'), ",
      "or use ", fn, "(background = FALSE).",
      call. = FALSE
    )
  }

  # Open the running app according to the display argument. Outside RStudio
  # the system browser is the only option. The RStudio dialog window is
  # opened via shiny::dialogViewer(), whose viewer functions are function(url)
  # and therefore work for a background URL as well as for runGadget().
  show_app <- function(url) {
    if (!rstudioapi::isAvailable()) {
      utils::browseURL(url)
      return(invisible(NULL))
    }
    switch(display,
      viewer  = rstudioapi::viewer(url),
      window  = shiny::dialogViewer(title, width = 1000, height = 800)(url),
      browser = utils::browseURL(url)
    )
    invisible(NULL)
  }

  # Readiness/health probe. A direct TCP connection is used deliberately:
  # libcurl-based alternatives such as url() honour the http_proxy variables
  # set by the staff .Rprofile and route 127.0.0.1 requests through the
  # corporate proxy, which breaks the check.
  port_open <- function(p) {
    con <- suppressWarnings(try(
      socketConnection("127.0.0.1", port = p, open = "r+",
                       blocking = TRUE, timeout = 1),
      silent = TRUE
    ))
    if (inherits(con, "connection")) {
      try(close(con), silent = TRUE)
      return(TRUE)
    }
    FALSE
  }

  proc_name <- paste0(key, "_proc")
  url_name  <- paste0(key, "_url")
  port_name <- paste0(key, "_port")

  # Reuse an existing live app rather than spawning a duplicate, but only
  # after verifying it is actually serving. A worker can outlive its server
  # (e.g. a hung ODBC disconnect during shutdown); such zombies are killed
  # and replaced automatically.
  existing <- .actepir_env[[proc_name]]
  if (!is.null(existing) && existing$is_alive()) {
    old_port <- .actepir_env[[port_name]]
    if (!is.null(old_port) && port_open(old_port)) {
      url <- .actepir_env[[url_name]]
      show_app(url)
      message(title, " is already running at ", url)
      return(invisible(existing))
    }
    existing$kill()
    message("A previous ", title, " process was no longer responding ",
            "and has been replaced.")
  }

  # Clear a stale handle from a process that has died
  .actepir_env[[proc_name]] <- NULL

  if (!is.null(port)) port <- as.integer(port)

  # The background process selects and binds its own port (atomically, with
  # retries on collision) and reports the chosen port through a temp file.
  # Selecting the port in the parent and binding it later in the child is a
  # race that can produce 'address already in use' failures.
  portfile <- tempfile(paste0("actepir_", key, "_port_"))

  # Spawn the app in a background process. Supervision requires processx's
  # bundled supervisor.exe, which restricted environments (e.g. AppLocker
  # policies blocking executables under AppData) may refuse to run, raising
  # system error 1260. Attempt supervision first and fall back without it.
  bg_func <- function(port, portfile, factory, factory_args) {
    app <- do.call(utils::getFromNamespace(factory, "actepir"), factory_args)
    candidates <- if (!is.null(port)) {
      as.integer(port)
    } else if (requireNamespace("httpuv", quietly = TRUE)) {
      replicate(10, httpuv::randomPort())
    } else {
      sample(20000:60000, 10)
    }
    for (p in candidates) {
      writeLines(as.character(p), portfile)
      served <- tryCatch({
        shiny::runApp(app, port = p, host = "127.0.0.1",
                      launch.browser = FALSE)
        TRUE
      }, error = function(e) {
        if (grepl("Failed to create server", conditionMessage(e),
                  fixed = TRUE)) {
          FALSE  # port collision: try the next candidate
        } else {
          stop(e)
        }
      })
      if (served) break
    }
    # Exit the worker immediately once the server has stopped. A graceful R
    # shutdown can hang on ODBC handle finalisation, leaving a zombie process
    # that blocks relaunch; process death releases all handles regardless.
    tools::pskill(Sys.getpid())
  }
  bg_args <- list(port = port, portfile = portfile,
                  factory = factory, factory_args = factory_args)

  proc <- tryCatch(
    callr::r_bg(func = bg_func, args = bg_args, supervise = TRUE),
    error = function(e) {
      message("Process supervision is unavailable on this system ",
              "(spawn blocked by policy); launching without it.")
      callr::r_bg(func = bg_func, args = bg_args, supervise = FALSE)
    }
  )

  # Wait for the app to start serving (port_open is defined above, before
  # the reuse check)
  started  <- FALSE
  deadline <- Sys.time() + 45
  while (Sys.time() < deadline) {
    cand <- NA_integer_
    if (file.exists(portfile)) {
      cand <- suppressWarnings(
        as.integer(readLines(portfile, warn = FALSE)[1])
      )
    }
    if (!is.na(cand) && port_open(cand)) {
      port    <- cand
      started <- TRUE
      break
    }
    if (!proc$is_alive()) break
    Sys.sleep(0.25)
  }
  unlink(portfile)

  if (!started) {
    err <- tryCatch(proc$read_all_error(), error = function(e) "")
    if (proc$is_alive()) proc$kill()
    stop(
      "The background ", title, " failed to start within 45 seconds.\n",
      if (nzchar(err)) paste0("Process error output:\n", err) else "",
      "\nTry ", fn, "(background = FALSE).",
      call. = FALSE
    )
  }

  url <- sprintf("http://127.0.0.1:%d", port)
  .actepir_env[[proc_name]] <- proc
  .actepir_env[[url_name]]  <- url
  .actepir_env[[port_name]] <- port

  # Safety net for unsupervised processes: kill every background app when
  # this R session exits. Registered once per session.
  if (!isTRUE(.actepir_env$finalizer_set)) {
    reg.finalizer(
      .actepir_env,
      function(e) {
        for (nm in grep("_proc$", ls(e), value = TRUE)) {
          p <- e[[nm]]
          if (!is.null(p) && p$is_alive()) p$kill()
        }
      },
      onexit = TRUE
    )
    .actepir_env$finalizer_set <- TRUE
  }

  show_app(url)

  message(
    title, " running in a background process at ", url, "\n",
    "The console remains free. Stop it with the Done button or ",
    fn, "_stop()."
  )

  invisible(proc)

}


# Stops the background process of the app identified by `key` (see .run_app)
#' @noRd
.stop_app <- function(key, title) {

  proc <- .actepir_env[[paste0(key, "_proc")]]

  if (is.null(proc) || !proc$is_alive()) {
    message("No background ", title, " is running.")
    return(invisible(FALSE))
  }

  proc$kill()
  .actepir_env[[paste0(key, "_proc")]] <- NULL
  .actepir_env[[paste0(key, "_url")]]  <- NULL
  .actepir_env[[paste0(key, "_port")]] <- NULL
  message("Background ", title, " stopped.")
  invisible(TRUE)

}


# One EpiServer connection for an app, opened through episerver_connect().
# query() validates the connection before each query and re-establishes it
# (gc-wrapped, per the Type 29 ODBC corruption workaround) if it has been
# dropped. On a query error, one reconnect-and-retry is attempted before the
# error is raised, so a corrupted connection does not fail every later query.
#' @noRd
.epi_connection <- function(connect_args = list()) {

  conn <- NULL
  connect <- function() {
    invisible(gc())
    conn <<- do.call(episerver_connect, connect_args)
    invisible(gc())
  }
  connect()

  query <- function(sql) {
    conn_bad <- tryCatch(!DBI::dbIsValid(conn), error = function(e) TRUE)
    if (conn_bad) {
      tryCatch(connect(), error = function(e) invisible(NULL))
    }
    tryCatch(
      DBI::dbGetQuery(conn, sql),
      error = function(e) {
        connect()
        DBI::dbGetQuery(conn, sql)
      }
    )
  }

  disconnect <- function() {
    tryCatch(
      if (DBI::dbIsValid(conn)) DBI::dbDisconnect(conn),
      error = function(e) invisible(NULL)
    )
  }

  list(query = query, disconnect = disconnect)

}


# Hands generated code to the analyst. In this session the code is inserted at
# the cursor; a background process has no connection to the RStudio editor,
# so there the code goes to the clipboard through .epi_copy_script().
#' @noRd
.epi_send_code <- function(session, code) {
  if (rstudioapi::isAvailable()) {
    rstudioapi::insertText(text = code)
  } else {
    .epi_copy_text(session, code, "Code copied to clipboard")
  }
}


# Copies text to the clipboard through .epi_copy_script(), from the browser
# or Viewer showing the app
#' @noRd
.epi_copy_text <- function(session, text, message = "Copied to clipboard") {
  session$sendCustomMessage("actepir_copy", text)
  shiny::showNotification(message, duration = 2, type = "message")
}


# Clipboard handler for .epi_copy_text()
#' @noRd
.epi_copy_script <- function() {
  shiny::tags$script(shiny::HTML("
    Shiny.addCustomMessageHandler('actepir_copy', function(text) {
      function fallback() {
        var ta = document.createElement('textarea');
        ta.value = text;
        document.body.appendChild(ta);
        ta.select();
        try { document.execCommand('copy'); } catch(e) {}
        document.body.removeChild(ta);
      }
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(text).then(function() {}, fallback);
      } else {
        fallback();
      }
    });
  "))
}


# Theme shared by the apps: Nightshade title bar, buttons, inputs,
# DataTables and sliders
#' @noRd
.epi_app_style <- function() {
  shiny::tags$style(shiny::HTML("
    :root {
      --epi-primary:      #320557;  /* Nightshade */
      --epi-primary-brdr: #24043f;  /* button borders */
      --epi-accent:       #562C8C;  /* Iris: accents, controls, selection focus */
      --epi-info-bg:      #eae6f1;  /* info bar */
      --epi-select-bg:    #eae6f1;  /* selected row */
    }
    .gadget-content { padding: 10px; }
    /* Title bar: Nightshade background, white title */
    .gadget-title { background-color: var(--epi-primary); }
    .gadget-title h1 { color: #fff; }
    /* Done button only (scoped by id): contrast against the dark bar */
    #done.btn-primary {
      background-color: var(--epi-primary) !important;
      border-color: #fff !important;
      color: #fff !important;
    }
    #done.btn-primary:hover, #done.btn-primary:focus,
    #done.btn-primary:active {
      background-color: var(--epi-primary) !important;
      border-color: #DCC8FA !important;
      color: #DCC8FA !important;
    }
    .selector-row { display: flex; gap: 10px; margin-bottom: 10px;
                    align-items: flex-end; }
    .selector-row > * { flex: 1; }
    .selector-row .form-group { margin-bottom: 0; }
    .info-bar {
      background: var(--epi-info-bg);
      border-left: 3px solid var(--epi-primary);
      padding: 8px 12px; margin-bottom: 10px; font-size: 12px;
      color: #555;
    }
    .options-row {
      display: flex; align-items: center; gap: 15px;
      margin-bottom: 10px; flex-wrap: wrap;
    }
    .options-row .form-group { margin-bottom: 0; }
    .radio-inline { margin-top: 0; padding-top: 0; }
    /* Recolour native radios and checkboxes */
    input[type=radio], input[type=checkbox] {
      accent-color: var(--epi-accent);
    }
    /* Themed primary buttons */
    .btn-primary {
      background-color: var(--epi-primary) !important;
      border-color: var(--epi-primary-brdr) !important;
    }
    .btn-primary:hover, .btn-primary:focus, .btn-primary:active {
      background-color: var(--epi-primary-brdr) !important;
      border-color: var(--epi-primary-brdr) !important;
    }
    /* Focus glow on inputs/selects: replace Bootstrap blue */
    .form-control:focus, select:focus, .selectize-input.focus {
      border-color: var(--epi-accent) !important;
      box-shadow: 0 0 0 2px rgba(86, 44, 140, 0.35) !important;
      outline: none !important;
    }
    /* Selected option in a native multi/again-open select list */
    select option:checked, select option:hover {
      box-shadow: 0 0 10px 100px var(--epi-primary) inset;
      color: #fff;
    }
    /* selectize dropdown (Shiny's default select widget): 'selected' is
       the current item, 'active' is hover. Theme the current item; leave
       hover as the default subtle grey. */
    .selectize-dropdown .option.selected,
    .selectize-dropdown .option.selected.active {
      background-color: var(--epi-primary) !important;
      color: #fff !important;
    }
    /* DataTables centres the filter below 768px via its own media
       query; pin it right at all widths (both core and bootstrap
       stylesheet variants) */
    .dataTables_wrapper .dataTables_filter,
    div.dataTables_wrapper div.dataTables_filter {
      float: right !important;
      text-align: right !important;
    }
    table.dataTable thead th { background: var(--epi-primary);
                               color: #fff; }
    /* Sort indicators: dataTables.bootstrap draws Unicode glyphs on
       'thead>tr>th.sorting:before/:after' with no color (so they inherit
       the white header text) but at opacity .125 inactive / .6 active,
       which is nearly invisible on the dark header. Raise the opacity;
       colour is already white by inheritance. Match the real selector
       shape (child combinators, th, and the _disabled variants). */
    table.dataTable thead > tr > th.sorting:before,
    table.dataTable thead > tr > th.sorting:after,
    table.dataTable thead > tr > th.sorting_asc:before,
    table.dataTable thead > tr > th.sorting_asc:after,
    table.dataTable thead > tr > th.sorting_desc:before,
    table.dataTable thead > tr > th.sorting_desc:after,
    table.dataTable thead > tr > th.sorting_asc_disabled:before,
    table.dataTable thead > tr > th.sorting_desc_disabled:before {
      opacity: 0.45 !important;
    }
    table.dataTable thead > tr > th.sorting_asc:before,
    table.dataTable thead > tr > th.sorting_desc:after {
      opacity: 1 !important;
    }
    /* Selected rows. Two upstream mechanisms colour these blue:
       (1) dataTables.bootstrap.extra.css targets '.table.dataTable
       tbody tr.active td' directly with white text;
       (2) dataTables.bootstrap.min.css paints 'tr.selected>*' with an
       inset box-shadow keyed on the --dt-row-selected RGB variable.
       Redefine the variable and override the .extra rule (covering the
       stripe/hover permutations); keep text dark. This build tags rows
       'active' rather than 'selected'. */
    :root {
      --dt-row-selected: 234, 230, 241;      /* #eae6f1 */
      --dt-row-selected-text: 51, 51, 51;    /* #333 */
      --dt-row-selected-link: 51, 51, 51;
    }
    .table.dataTable tbody td.active,
    .table.dataTable tbody tr.active td,
    table.dataTable tbody tr.active td,
    table.dataTable tbody td.active,
    table.dataTable.stripe tbody tr.odd.active td,
    table.dataTable.stripe tbody tr.even.active td,
    table.dataTable.display tbody tr.odd.active td,
    table.dataTable.display tbody tr.even.active td,
    table.dataTable.hover tbody tr.active:hover td,
    table.dataTable.display tbody tr.active:hover td {
      background-color: var(--epi-select-bg) !important;
      color: #333 !important;
    }
    /* DataTables pagination (Bootstrap 3 markup: ul.pagination with
       li.paginate_button): replace Bootstrap blue links and active page */
    .pagination > li > a, .pagination > li > span {
      color: var(--epi-accent);
    }
    .pagination > li > a:hover, .pagination > li > a:focus,
    .pagination > li > span:hover, .pagination > li > span:focus {
      color: var(--epi-primary);
      background-color: var(--epi-info-bg);
    }
    .pagination > .active > a, .pagination > .active > a:hover,
    .pagination > .active > a:focus, .pagination > .active > span,
    .pagination > .active > span:hover, .pagination > .active > span:focus {
      background-color: var(--epi-primary) !important;
      border-color: var(--epi-primary) !important;
      color: #fff !important;
    }
    .pagination > .disabled > a, .pagination > .disabled > a:hover,
    .pagination > .disabled > a:focus {
      color: #999;
      background-color: #fff;
    }
    /* ionRangeSlider (Shiny sliderInput) theming: default is #428bca */
    .irs--shiny .irs-bar,
    .irs--shiny .irs-to,
    .irs--shiny .irs-from,
    .irs--shiny .irs-single {
      background-color: var(--epi-accent) !important;
    }
    .irs--shiny .irs-bar {
      border-top-color: var(--epi-accent) !important;
      border-bottom-color: var(--epi-accent) !important;
    }
    .irs--shiny .irs-handle > i:first-child {
      background-color: var(--epi-accent) !important;
    }
    .irs--shiny .irs-to::before,
    .irs--shiny .irs-from::before,
    .irs--shiny .irs-single::before {
      border-top-color: var(--epi-accent) !important;
    }
  "))
}

# Infrastructure shared by the package's Shiny apps: the EpiServer Browser
# (episerver_browse()) and the DSR Calculator (episerver_dsr()).

# Package-level environment holding the background process handles
.actepir_env <- new.env(parent = emptyenv())


# Runs the app built by the internal factory function named `factory`, as a
# gadget in this session or in a background R process (see episerver_browse()
# for the technique), and opens it in the system web browser. The factory is
# passed by name so that the background process can find it in the installed
# package. Each app is identified by `key`: its process handle, URL and port
# are kept in .actepir_env as <key>_proc, <key>_url and <key>_port. `title`
# names the app in messages; `fn` is the user-facing function named in hints.
#' @noRd
.run_app <- function(key, title, fn, factory, factory_args = list(),
                     background = TRUE, port = NULL) {

  if (!background) {
    app <- do.call(utils::getFromNamespace(factory, "actepir"), factory_args)
    return(shiny::runGadget(app, viewer = shiny::browserViewer()))
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
      utils::browseURL(url)
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

  utils::browseURL(url)

  message(
    title, " running in a background process at ", url, "\n",
    "The console remains free. Closing the browser tab leaves it running, ",
    "so ", fn, "() opens it again at once; ", fn, "_stop() stops it."
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
# showing the app
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


# Where analysts send problem reports from the apps
.epi_contact <- "healthinfo@act.gov.au"


# A tab's own settings, in a panel down the left of the tab's results (...),
# as distinct from the app's sidebar of settings. `settings` is a tagList of
# inputs; the panel is 240px wide, always open and not resizable, and the
# CSS of .epi-tab-layout in .epi_app_style() gives it its look.
#' @noRd
.epi_tab_settings <- function(id, settings, ...) {
  layout <- bslib::layout_sidebar(
    sidebar = bslib::sidebar(settings, id = id, width = 240,
                             open = "always", resizable = FALSE),
    ...,
    border = FALSE, border_radius = FALSE, fillable = FALSE
  )
  # layout_sidebar() puts named attributes on its main area, not on itself
  htmltools::tagAppendAttributes(layout, class = "epi-tab-layout")
}


# A setting with notes (...) behind an information button at the right of
# its label. The button is the summary of a <details> element, so a click
# shows the notes below the label and another hides them, in Bootstrap 3 or
# 5 alike. The notes start hidden.
#' @noRd
.epi_with_help <- function(input, ...) {
  label <- htmltools::tagQuery(input)$find("label.control-label")
  tag <- label$selectedTags()[[1]]
  id <- paste0(tag$attribs$`for`, "_help")
  about <- paste("About", tolower(paste(unlist(tag$children), collapse = "")))
  shiny::div(
    class = "epi-field",
    label$after(
      shiny::tags$details(
        id = id, class = "epi-help",
        shiny::tags$summary(title = about, `aria-label` = about,
                            shiny::icon("circle-info")),
        shiny::div(class = "epi-note", ...)
      )
    )$allTags()
  )
}


# Lines on the R session for a problem report, enough to reproduce it: where
# actepir was installed from, R, the operating system, locale and time zone,
# the library paths, proxy variables, ODBC drivers and the packages loaded.
# `client` is what the browser sent: its user agent and window, the errors
# shown in the page and any script errors. Proxy variables are named but not
# given, as they can hold a password.
#' @noRd
.epi_session_report <- function(client = NULL) {
  desc <- utils::packageDescription("actepir")
  from <- if (identical(desc$RemoteType, "github")) {
    paste0(", from GitHub ", desc$RemoteUsername, "/", desc$RemoteRepo,
           if (!is.null(desc$RemoteRef)) paste0(", ", desc$RemoteRef),
           if (!is.null(desc$RemoteSha)) paste0(", commit ", desc$RemoteSha))
  } else if (!is.null(desc$RemoteType)) {
    paste0(", from ", desc$RemoteType)
  } else {
    ""
  }

  proxies <- c("http_proxy", "https_proxy", "no_proxy",
               "HTTP_PROXY", "HTTPS_PROXY", "NO_PROXY")
  proxies <- proxies[nzchar(Sys.getenv(proxies))]
  drivers <- tryCatch(unique(odbc::odbcListDrivers()$name),
                      error = function(e) character())
  pkgs <- loadedNamespaces()
  pkgs <- pkgs[order(tolower(pkgs))]
  versions <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)),
             error = function(e) "?")
  }, character(1))
  listed <- function(x) if (length(x)) paste(x, collapse = ", ") else "none"

  lines <- c(
    paste0("actepir ", desc$Version, from),
    paste0(R.version.string, ", ", R.version$platform),
    if (!is.null(utils::osVersion)) utils::osVersion,
    paste0("Locale: ", Sys.getlocale("LC_CTYPE"),
           if (isTRUE(l10n_info()[["UTF-8"]])) " (UTF-8)",
           "; time zone ", Sys.timezone()),
    paste0("Library paths: ", paste(.libPaths(), collapse = "; ")),
    paste0("Proxy variables set: ", listed(proxies)),
    paste0("ODBC drivers: ", listed(drivers)),
    paste0("Started from RStudio: ",
           if (nzchar(Sys.getenv("RSTUDIO"))) "yes" else "no")
  )
  if (!is.null(client)) {
    lines <- c(
      lines,
      paste0("Browser: ", client$agent),
      paste0("Window: ", client$width, " x ", client$height,
             " pixels, device pixel ratio ", client$ratio),
      paste0("Errors shown in the page: ", listed(unlist(client$errors))),
      paste0("Script errors: ", listed(unlist(client$script_errors)))
    )
  }
  # Package names hold no underscore, which keeps each name on the line of
  # its version while the list is wrapped
  c(lines, "Packages loaded:",
    gsub("_", " ", strwrap(paste(pkgs, versions, sep = "_", collapse = ", "),
                           width = 76, indent = 2, exdent = 2), fixed = TRUE))
}


# The reversed (white) ACT Government logo as an <img>, or NULL if the file
# is not installed. The image is embedded as a data URI, which avoids
# addResourcePath() and so works in foreground and background apps alike.
# `...` adds attributes such as class or style.
#' @noRd
.epi_logo <- function(...) {
  path <- system.file("www", "ACTGov_inline_rev.png", package = "actepir")
  if (!nzchar(path)) {
    return(NULL)
  }
  # base64_enc() breaks its output into lines, which a data URI does not need
  png <- readBin(path, "raw", file.info(path)$size)
  shiny::tags$img(
    src = paste0("data:image/png;base64,",
                 gsub("[\r\n]", "", jsonlite::base64_enc(png))),
    alt = "ACT Government", ...
  )
}


# Bootstrap 5 theme for apps built with bslib: the brand purples and fonts
# of dev/reference/act-brand.md. Fonts are named, not downloaded, so Arial
# and Calibri, the brand's alternatives, stand in where Montserrat and
# Source Sans 3 are not installed. .epi_app_style() styles the widgets that
# Bootstrap's variables do not reach.
#' @noRd
.epi_theme <- function() {
  theme <- bslib::bs_theme(
    version      = 5,
    primary      = "#320557",  # Nightshade
    base_font    = bslib::font_collection("Source Sans 3", "Source Sans Pro",
                                          "Calibri", "Arial", "sans-serif"),
    heading_font = bslib::font_collection("Montserrat", "Arial", "sans-serif"),
    "navbar-bg"                = "#320557",
    "link-color"               = "#562C8C",  # Iris
    "link-hover-color"         = "#320557",
    "component-active-bg"      = "#562C8C",
    "input-focus-border-color" = "#562C8C",
    "focus-ring-color"         = "rgba(86, 44, 140, 0.35)",
    # bslib's shiny preset sets 10%, a share of the window's width, which
    # pushes a dialog below the bottom of a wide, short window
    "modal-dialog-margin-y-sm-up" = "1.75rem"
  )
  # The app's name in the top bar is a heading
  bslib::bs_add_rules(theme,
                      ".navbar-brand { font-family: $headings-font-family; }")
}


# Theme shared by the apps: colours, the spacing and type of controls and
# sections, the title bar, buttons, notes, information buttons,
# notifications, DataTables and sliders. The rules use the classes of Shiny,
# Bootstrap 3 and 5, bslib and shinydashboard, so that controls in any app
# take them without CSS of their own. dev/reference/app-style.md sets them
# out.
#' @noRd
.epi_app_style <- function() {
  shiny::tags$style(shiny::HTML("
    :root {
      --epi-primary:      #320557;  /* Nightshade */
      --epi-primary-brdr: #24043f;  /* button borders */
      --epi-accent:       #562C8C;  /* Iris: accents, controls, selection focus */
      --epi-info-bg:      #eae6f1;  /* info bar */
      --epi-select-bg:    #eae6f1;  /* selected row */
      /* Spacing and type */
      --epi-font:      15px;   /* labels, choices and section headings */
      --epi-font-box:  14px;   /* text in boxes and their lists */
      --epi-font-note: 11px;   /* notes on settings */
      --epi-line:      1.4;    /* line height of labels and choices */
      --epi-gap:       10px;   /* between settings */
      --epi-gap-small: 6px;    /* between stacked buttons */
      --epi-pad:       16px;   /* side padding of pages, sidebars, sections */
      --epi-head-pad:  10px;   /* above and below section headings */
      --epi-body-pad:  8px var(--epi-pad) 12px;  /* inside sections */
      --epi-box-pad-y: 4px;    /* inside boxes */
      --epi-box-pad-x: 10px;
      --bslib-sidebar-padding: var(--epi-pad);
    }
    /* Controls: labels, choices and boxes */
    .shiny-input-container { margin-bottom: var(--epi-gap); }
    .shiny-input-container > .control-label {
      font-size: var(--epi-font); font-weight: 400;
      line-height: var(--epi-line); margin-bottom: 2px;
    }
    .shiny-input-container .radio, .shiny-input-container .checkbox {
      margin-top: 0; margin-bottom: 2px;
    }
    /* Shiny pulls choices up by Bootstrap 3's 10px margin above them */
    .shiny-input-radiogroup label ~ .shiny-options-group,
    .shiny-input-checkboxgroup label ~ .shiny-options-group { margin-top: 0; }
    /* shinydashboard's sidebar pads each control: the same spacing */
    section.sidebar .shiny-input-container {
      padding: var(--epi-gap) var(--epi-pad) 0; margin-bottom: 0;
    }
    .shiny-input-container .radio label,
    .shiny-input-container .checkbox label {
      font-size: var(--epi-font); line-height: var(--epi-line);
    }
    .shiny-input-container input.form-control,
    .shiny-input-container textarea.form-control,
    .shiny-input-container select.form-control,
    .shiny-input-container select.form-select,
    .shiny-input-container .selectize-input {
      padding-top: var(--epi-box-pad-y); padding-bottom: var(--epi-box-pad-y);
      padding-left: var(--epi-box-pad-x);
      font-size: var(--epi-font-box); line-height: var(--epi-line);
      min-height: 0;
    }
    .shiny-input-container input.form-control,
    .shiny-input-container select.form-control {
      height: auto; padding-right: var(--epi-box-pad-x);
    }
    .selectize-input input, .selectize-dropdown {
      font-size: var(--epi-font-box);
    }
    /* Sections: bslib accordions and shinydashboard boxes. Headings are in
       the info bar's tint, in bold Nightshade. */
    .accordion {
      --bs-accordion-bg: transparent;
      --bs-accordion-btn-bg: var(--epi-info-bg);
      --bs-accordion-active-bg: var(--epi-info-bg);
      --bs-accordion-btn-color: var(--epi-primary);
      --bs-accordion-active-color: var(--epi-primary);
      --bs-accordion-btn-padding-x: var(--epi-pad);
      --bs-accordion-btn-padding-y: var(--epi-head-pad);
      --bs-accordion-btn-icon: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='none' stroke='%23320557' stroke-linecap='round' stroke-linejoin='round'%3e%3cpath d='m2 5 6 6 6-6'/%3e%3c/svg%3e\");
      --bs-accordion-btn-active-icon: var(--bs-accordion-btn-icon);
    }
    .accordion-button { font-size: var(--epi-font); font-weight: 600; }
    .accordion-body { padding: var(--epi-body-pad); }
    .box > .box-header {
      background-color: var(--epi-info-bg); color: var(--epi-primary);
      padding: var(--epi-head-pad) var(--epi-pad);
    }
    .box > .box-header > .box-title { font-size: var(--epi-font);
                                      font-weight: 600; }
    .box > .box-body { padding: var(--epi-body-pad); }
    .accordion-body > :last-child,
    .accordion-body > :last-child .shiny-input-container,
    .box-body > :last-child,
    .box-body > :last-child .shiny-input-container { margin-bottom: 0; }
    /* Top bar of a bslib page_navbar: the app's name at the left, its tabs
       after a gap, and the logo at the right (.epi-logo), at the brand's
       minimum size, a crest 60px across, with clear space equal to the
       height of its ACT lettering, 22px */
    .navbar { --bs-navbar-padding-y: 0; }
    .navbar > .container-fluid { padding-left: 22px; padding-right: 22px; }
    .navbar-brand { margin-right: 48px; }
    .navbar .epi-logo { display: block; height: 60px; margin: 22px 0 22px 22px; }
    /* The tabs centred on the logo's height, where the bar is not collapsed:
       bslib collapses it below Bootstrap's lg breakpoint, 992px */
    @media (min-width: 992px) {
      .navbar .navbar-nav { align-items: center; }
    }
    /* Sidebars: settings are spaced by their own margins (--epi-gap), so
       bslib's gaps between them are removed */
    .bslib-sidebar-layout > .sidebar > .sidebar-content { gap: 0; }
    .bslib-sidebar-layout > .sidebar > .sidebar-content > .accordion:not(:last-child) {
      margin-bottom: var(--epi-gap);
    }
    /* Actions held at the foot of a sidebar, across its full width and
       flush with the sections above and the sidebar's bottom edge. The foot
       stays above what scrolls under it, such as a focused section heading
       (z-index 3) and slider handles, and below open dropdowns (1000). */
    .epi-actions {
      position: sticky; bottom: 0; z-index: 20;
      display: flex; align-items: center; gap: 8px;
      margin: 0 calc(-1 * var(--epi-pad));
      padding: 8px var(--epi-pad);
      background-color: var(--bslib-sidebar-bg, inherit);
      border-top: 1px solid #e6e5e5;
    }
    .accordion + .epi-actions { margin-top: calc(-1 * var(--epi-gap)); }
    .sidebar-content:has(> .epi-actions:last-child) { padding-bottom: 0; }
    /* A tab's own settings (.epi_tab_settings()): a panel at the left of
       the tab's results, without the app sidebar's dividing border, as
       tall as its settings and held in view while the results scroll */
    .epi-tab-layout { --bslib-sidebar-vert-border: none; }
    @media (min-width: 576px) {
      .epi-tab-layout > .sidebar {
        align-self: start; position: sticky; top: var(--epi-pad);
        border-radius: var(--bs-border-radius, 6px);
      }
    }
    /* Buttons stacked down a sidebar, each its full width */
    .epi-stack { display: flex; flex-direction: column;
                 gap: var(--epi-gap-small); margin-bottom: var(--epi-gap); }
    .epi-stack .btn, .epi-stack .dropdown { width: 100%; text-align: left; }
    /* Notes on a setting, and a setting's notes behind an information
       button at the right of its label (.epi_with_help()) */
    .epi-note { font-size: var(--epi-font-note); color: #777;
                margin: -4px 0 var(--epi-gap) 0; }
    .epi-note.epi-error { color: #b00020; }
    .epi-field > .shiny-input-container { position: relative; }
    .epi-field .control-label { padding-right: 22px; }
    .epi-help > summary {
      position: absolute; top: 0; right: 0; z-index: 1;
      height: calc(var(--epi-font) * var(--epi-line));
      display: flex; align-items: center; padding: 0 2px;
      list-style: none; cursor: pointer; color: var(--epi-accent);
    }
    .epi-help > summary::-webkit-details-marker { display: none; }
    .epi-help > summary:hover, .epi-help[open] > summary {
      color: var(--epi-primary);
    }
    .epi-help .epi-note { margin: 0 0 4px 0; }
    /* In shinydashboard's sidebar, inside its padding and in its colours */
    section.sidebar .epi-help > summary {
      top: var(--epi-gap); right: var(--epi-pad); color: inherit;
    }
    section.sidebar .epi-note { color: inherit; opacity: 0.75; }
    /* Buttons that are an icon alone, in Iris, Nightshade on hover */
    .epi-icon-btn { border: 0; background: none; padding: 0 2px;
                    color: var(--epi-accent); line-height: 1; }
    .epi-icon-btn:hover { color: var(--epi-primary); }
    .epi-icon-btn:focus:not(:focus-visible),
    .epi-help > summary:focus:not(:focus-visible) { outline: none; }
    .epi-icon-btn:focus-visible, .epi-help > summary:focus-visible {
      outline: 2px solid var(--epi-accent); outline-offset: 1px;
      border-radius: 2px;
    }
    /* A bslib sidebar's resize handle sits just outside the sidebar's
       edge. bslib puts the handle, and the strip that activates it, over
       the sidebar's last 12px, where they cover the scrollbar and stop the
       wheel and the scrollbar's thumb working. */
    .bslib-sidebar-layout:not(.sidebar-right) > .bslib-sidebar-resize-handle {
      left: 0;
    }
    .bslib-sidebar-layout:not(.sidebar-right) > .bslib-sidebar-resize-handle::before,
    .bslib-sidebar-layout:not(.sidebar-right) > .bslib-sidebar-resize-handle.handle-active::before {
      left: 0; right: auto; width: 100%;
    }
    .bslib-sidebar-layout:not(.sidebar-right) > .bslib-sidebar-resize-handle .resize-indicator {
      right: auto; left: 3px; transform: translateY(-50%);
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
    #done.btn-primary:hover, #done.btn-primary:focus-visible,
    #done.btn-primary:active {
      background-color: var(--epi-accent) !important;
      border-color: #fff !important;
      color: #fff !important;
    }
    .selector-row { display: flex; gap: var(--epi-gap);
                    margin-bottom: var(--epi-gap); align-items: flex-end; }
    .selector-row > * { flex: 1; }
    .selector-row .form-group { margin-bottom: 0; }
    .info-bar {
      background: var(--epi-info-bg);
      border-left: 3px solid var(--epi-primary);
      padding: 8px 12px; margin-bottom: var(--epi-gap); font-size: 12px;
      color: #555;
    }
    .info-bar p { margin: 0 0 4px 0; }
    .info-bar p:last-child { margin-bottom: 0; }
    .info-bar .epi-warn { color: #8a5300; }
    .info-bar.epi-error { border-left-color: #b00020; color: #b00020; }
    .options-row {
      display: flex; align-items: center; gap: var(--epi-gap);
      margin-bottom: var(--epi-gap); flex-wrap: wrap;
    }
    .options-row .form-group { margin-bottom: 0; }
    .radio-inline { margin-top: 0; padding-top: 0; }
    /* Notifications, such as Copied and the progress of a query, look like
       the info bar in place of Shiny's blue and grey boxes. Warnings and
       errors take the info bar's amber and red. */
    #shiny-notification-panel .shiny-notification {
      background-color: var(--epi-info-bg); color: var(--epi-primary);
      border: 0; border-left: 3px solid var(--epi-primary);
      border-radius: 0; opacity: 1;
      box-shadow: 0 2px 6px rgba(50, 5, 87, 0.25);
    }
    #shiny-notification-panel .shiny-notification-warning {
      color: #8a5300; border-left-color: #8a5300;
    }
    #shiny-notification-panel .shiny-notification-error {
      color: #b00020; border-left-color: #b00020;
    }
    #shiny-notification-panel .shiny-notification-close { color: inherit; }
    #shiny-notification-panel .progress { background-color: #fff; }
    #shiny-notification-panel .progress-bar {
      background-color: var(--epi-primary);
    }
    /* Recolour native radios and checkboxes */
    input[type=radio], input[type=checkbox] {
      accent-color: var(--epi-accent);
    }
    /* Themed primary buttons: Nightshade, Iris on hover */
    .btn-primary {
      background-color: var(--epi-primary) !important;
      border-color: var(--epi-primary-brdr) !important;
    }
    .btn-primary:hover, .btn-primary:focus-visible, .btn-primary:active {
      background-color: var(--epi-accent) !important;
      border-color: var(--epi-accent) !important;
    }
    /* A click leaves no highlight behind: focus rings and colours show for
       the keyboard (focus-visible), and a closed dropdown is plain */
    .btn:focus:not(:focus-visible),
    .accordion-button:focus:not(:focus-visible),
    .shiny-input-container .checkbox input:focus:not(:focus-visible),
    .shiny-input-container .radio input:focus:not(:focus-visible) {
      outline: none; box-shadow: none;
    }
    .accordion-button:not(.collapsed):focus:not(:focus-visible) {
      box-shadow: inset 0 calc(-1 * var(--bs-accordion-border-width))
                  0 var(--bs-accordion-border-color);
    }
    .shiny-input-container .checkbox input:not(:checked):focus:not(:focus-visible) {
      border-color: var(--bs-border-color);
    }
    .selectize-control.single .selectize-input.focus:not(.dropdown-active) {
      border-color: var(--bs-border-color, #ccc) !important;
      box-shadow: none !important;
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
    /* The arrows sit in 9px lines, one above the other, and are sized in
       em: with larger header text, or a symbol font substituted for them,
       they overlap. Fix their size and font. */
    table.dataTable thead > tr > th[class*='sorting']:before,
    table.dataTable thead > tr > th[class*='sorting']:after {
      font-family: Arial, 'Liberation Sans', sans-serif;
      font-size: 9px !important;
      line-height: 9px !important;
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

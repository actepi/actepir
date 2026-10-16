# Shiny app style reference

Layout, spacing, type and component rules for actepir's Shiny apps. It sets out how the apps look and behave, so that every tool is consistent and a new feature needs no styling of its own. Colours, fonts and logo rules come from `act-brand.md`; this file applies them to apps.

The rules were set on the DSR Calculator and are implemented once, for every app:

| Where | Holds |
|---|---|
| `.epi_theme()` in `R/epi_app.R` | The bslib (Bootstrap 5) theme: brand purples, fonts, focus colours and the dialog margin. |
| `.epi_app_style()` in `R/epi_app.R` | CSS for every app, whatever its framework: the tokens below, and rules on the classes of Shiny, Bootstrap 3 and 5, bslib and shinydashboard. |
| `.epi_with_help()`, `.epi_tab_settings()`, `.epi_copy_text()`, `.epi_session_report()`, `.epi_contact` | Shared components: a setting's notes behind an information button, a tab's own settings panel, copying to the clipboard, and the session lines and address of a problem report. |

`test-epi_app.R` checks the tokens and the rules that apply them. The browser scripts in `dev/harness/` check them in a page.

## Principles

- **One source.** Spacing and type come from the tokens. A tool does not set font sizes, padding or margins on its controls, in CSS or `style` attributes. If a token is wrong for every tool, change the token; if a tool seems to need its own, raise it with the maintainer first.
- **Standard parts.** Use Shiny's inputs and bslib's or shinydashboard's layouts. The shared rules target their classes, so they style a new control as soon as it is added.
- **Settings left, results right.** Settings are in a sidebar on the left, in sections. Results take the rest of the window.
- **Compact, not cramped.** The spacing was chosen to show a long sidebar with little scrolling (the DSR Calculator's fell from 2,105 to 1,519 pixels with every section open) while keeping 15px text.

## Tokens

CSS custom properties on `:root`, set in `.epi_app_style()`.

| Token | Value | Used for |
|---|---|---|
| `--epi-font` | 15px | Labels, choices (radio buttons, tick boxes) and section headings |
| `--epi-font-box` | 14px | Text in boxes (selects, numbers, text) and their dropdown lists |
| `--epi-font-note` | 11px | Notes on settings |
| `--epi-line` | 1.4 | Line height of labels and choices |
| `--epi-gap` | 10px | Between settings; between items in a row; below the info bar |
| `--epi-gap-small` | 6px | Between stacked buttons |
| `--epi-pad` | 16px | Side padding of the page, sidebars and sections |
| `--epi-head-pad` | 10px | Above and below a section heading |
| `--epi-body-pad` | 8px 16px 12px | Inside a section (top, sides, bottom) |
| `--epi-box-pad-y`, `--epi-box-pad-x` | 4px, 10px | Inside boxes |

`--bslib-sidebar-padding` is set to `--epi-pad`, so bslib's sidebars and main areas take it.

Other sizes in use: the info bar's text is 12px, small buttons (`btn-sm`) 14px, and the Help tab's text 13px.

## Colours

The brand purples are in `act-brand.md`. In the apps:

| Variable or value | Hex | Used for |
|---|---|---|
| `--epi-primary` | `#320557` Nightshade | Top bar, primary buttons, section heading text, table headers, left border of the info bar and notifications |
| `--epi-accent` | `#562C8C` Iris | Hover and keyboard focus of buttons, information and icon buttons, slider bars, radio and tick-box marks |
| `--epi-info-bg` | `#eae6f1` | Info bar, notifications, section headings, selected table rows |
| `--bslib-sidebar-bg` | `#f7f4fd` | Sidebars (set by each bslib app) |
| Notes | `#777` | Notes on settings |
| Warnings | `#8a5300` | Warnings in the info bar and in notifications |
| Errors | `#b00020` | Errors in notes, the info bar and notifications |

## Layout

### bslib apps (Bootstrap 5)

- **Page.** `bslib::page_navbar()` with `.epi_theme()`, `.epi_app_style()` and `.epi_logo()`. See "Adding a tool" in `dev/notes/shared-apps.md`.
- **Top bar.** The app's name (`title`) at the left; its result views as tabs, 48px after the name; then `bslib::nav_spacer()` and `bslib::nav_item(.epi_logo(class = "epi-logo"))`, which puts the logo at the right at the brand's minimum size (a crest 60px across) with 22px clear space. The bar is 104px tall.
- **No Done button.** The apps open in a browser tab, which the analyst closes. A background app keeps running, so it opens again at once, until its `_stop()` function or the end of the R session. When it stops, the tab says so and how to open it again. In the foreground (`background = FALSE`), closing the tab stops the app, which frees the console.
- **Settings sidebar.** `bslib::sidebar()`, 340px wide, resizable, open on desktop. Settings are grouped in `bslib::accordion()` sections that start open.
- **Actions at the foot.** The sidebar's actions (Calculate, Copy Code, the bug button) are in a `div(class = "epi-actions")` as its last child, held in view at the foot.
- **A tab's own settings.** Settings that belong to one tab, such as the Explore tab's, go in `.epi_tab_settings(id, settings, ...)`, with the tab's results in `...`. It is a panel down the left of the results, distinct from the app's sidebar: the same tint and spacing, 240px wide, always open and not resizable, but with no dividing border, rounded corners, as tall as its settings, and held in view while the results scroll (`.epi-tab-layout`). The results take the window's height beside it.
- **Plots that must be seen whole** fit the window below their top. The Explore tab sets `--ex-plot-height` from a short script and caps each plot's SVG at it; ggiraph keeps the plot's shape.
- **Resizing.** `.epi_app_style()` moves the resize handle of every left-hand bslib sidebar just outside its edge, as bslib puts it over the scrollbar (see the DSR Calculator's notes).

### shinydashboard apps (Bootstrap 3)

- Add the style to the body: `dashboardBody(tags$head(.epi_app_style()), ...)`.
- Controls in `dashboardSidebar()` and in `box()` take the same spacing and type. A box's heading takes the section heading's tint and weight.
- shinydashboard's own colours (the blue header, skins, `box-primary` borders) are not yet themed; see Open items.

### miniUI gadgets (Bootstrap 3)

The EpiServer Browser uses miniUI and `.epi_app_style()`, and takes the same control spacing and type.

## Controls and spacing

- **Labels** are 15px, regular weight, 2px above their box.
- **Boxes** (selects, numbers, text) have 14px text and 4px by 10px padding.
- **Choices** (radio buttons and tick boxes) are 15px, 2px apart.
- **Between settings** there are 10px, from each control's margin. bslib's flex gaps in sidebars are removed, so a wrapper (such as `.epi_with_help()` or `conditionalPanel()`) adds no space of its own.
- **Rows of settings** use `div(class = "selector-row")` for boxes side by side, or `div(class = "options-row")` for buttons and tick boxes in a line.
- **Buttons in a sidebar** go in `div(class = "epi-stack")`, which stacks them at full width, 6px apart. A bare button in a sidebar has no spacing.
- **Primary buttons** are `class = "btn-sm btn-primary"`, with an icon where one helps.

## Components

- **Section headings** (bslib accordion panels, shinydashboard boxes): the info bar's tint, bold (600) Nightshade 15px text, a Nightshade chevron, and 10px by 16px padding. They set open sections apart from their settings.
- **Notes.** Guidance that does not change with the settings goes behind an information button: `.epi_with_help(input, "text", ...)` puts an Iris (i) at the right of the input's label, and a click shows the notes under the label until a second click. It is a `<details>` element, so it works in Bootstrap 3 and 5 alike and needs no server. Feedback on what was entered (errors, codes spelt out, why a control is disabled) stays in view, in `div(class = "epi-note")` below the control.
- **Icon buttons** (`class = "epi-icon-btn"`): an icon alone, in Iris, Nightshade on hover, with a `title` and `aria-label`.
- **Info bar** (`div(class = "info-bar")`): the tint with a Nightshade left border and 12px text, above the results. Warnings in it are `p(class = "epi-warn")`; an error makes it `class = "info-bar epi-error"`.
- **Notifications** (`shiny::showNotification()`): styled like the info bar. Use `type = "message"` for confirmations, `"warning"` for a step the analyst must take first, and `"error"` for failures.
- **Dialogs** (`shiny::modalDialog()`): add `modal-dialog-scrollable` to the dialog so that it fits the window, and let one part of it scroll, with `overscroll-behavior: contain` so the wheel does not move the dialog. The DSR Calculator's problem report shows how (`.epi-report`).
- **Copying** goes through `.epi_copy_text()`, which shows a notification.
- **Problem reports** go to `.epi_contact` (healthinfo@act.gov.au). `.epi_session_report()` gives the session lines; a tool adds its own state and settings, but never data.
- **Focus.** Focus rings and colours show for the keyboard only (`:focus-visible`), so a mouse click leaves no highlight.

## Do and do not

- Do use the tokens, the shared classes and the helpers above.
- Do keep feedback in view and guidance behind information buttons.
- Do check a new screen at 1920 by 950 and 1536 by 730 pixels, a 1080-pixel screen at 100% and 125% scaling.
- Do not set `width`, `font-size`, `padding` or `margin` on controls, except `width = "100%"`.
- Do not use Bootstrap's collapse (`data-bs-toggle`) for notes; Bootstrap 3 and 5 name its attributes differently.
- Do not add a second mechanism for anything listed here; change the shared one.

## Open items

- **shinydashboard colours (waiting on the maintainer).** The header, skins, `box-primary` top border and buttons in boxes keep AdminLTE's blues. A shinydashboard tool will need them themed.
- **Note contrast.** Notes are `#777` at 11px: 4.48:1 on white and 4.1:1 on the sidebars' `#f7f4fd`, both below WCAG AA's 4.5:1 for small text. `#666` gives 5.7:1 and 5.3:1.
- **Right-hand sidebars.** The resize handle rules cover left-hand bslib sidebars only.

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acthd_gt_theme.R
\name{acthd_gt_theme}
\alias{acthd_gt_theme}
\title{Color theme for gt tables using ACT Health Directorate palettes}
\usage{
acthd_gt_theme(x, palette = "web_blues", single = FALSE, reverse = FALSE)
}
\arguments{
\item{x}{The \code{gt} object to be themed}

\item{palette}{Character name of ACT palette. See 'details' for valid palette names.}

\item{single}{Boolean indicating whether the palette should be reduced to a single colour}

\item{reverse}{Boolean indicating whether the palette should be reversed.}
}
\description{
This function allows the user to apply pre-defined theme to a \code{gt} object.  Palettes are intentionally designed from dark to light (meaning the darker colour will fill the table header), with the exception of \code{spectral}.  This function takes the minimum and maximum colours in a palette and reduces them to two values.  Applying the \code{reverse} argument may not lead to desirable results as tables can look odd with light headers and dark row lines.  Applying the \code{single} argument will reduce the palette to a single colour.  The upper end of the palette (generally darker) is used by default.  Applying the \code{reverse} argument will use the lighter end of the palette. Text is automatically inverted when \code{reverse} is applied.
}
\details{
Palettes are derived from a standard set of colours, sourced from the \href{https://actgovernment.sharepoint.com/sites/intranet-ACTHealth/SitePages/Branding,\%20Templates\%20and\%20resources.aspx}{ACT Health Directorate Brand & Graphics Standards Manual} (May 2024) or derived from the \href{https://github.com/ACTGov-Design-System/ACT-Website-Design-System}{ACT Website Design System}.
\subsection{Valid palette names}{

\strong{Official ACTHD primary colour gradients:}
\itemize{
\item \code{blue_white}
\item \code{blue_turq}
\item \code{blue_darkgrey}
}

\strong{Official ACTHD secondary colours:}
\itemize{
\item \code{spectral}
}

\strong{Official ACTGOV gradients:}
\itemize{
\item \code{darkgreys}
\item \code{navy_pink}
\item \code{red_orange}
\item \code{green_yellow}
\item \code{pink_red}
}

\strong{Semi-official (implied from web standards):}
\itemize{
\item \code{web_purples}
\item \code{web_purple_lightgrey}
\item \code{web_purple_midgrey}
\item \code{web_purple_darkgrey}
\item \code{web_greys}
\item \code{web_blues}
}

\strong{Unofficial (necessary for some visualisation/mapping):}
\itemize{
\item \code{lightgreys}
\item \code{fullgreys}
\item \code{red_yellow_green}
\item \code{red_yellow_aqua}
\item \code{blue_lightgrey}
\item \code{turq_lightgrey}
\item \code{turq_white}
}

Users should stick to the official colours where possible, using the unofficial colours only when the underlying data necessitates it.

Approach taken from \href{https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2}{Dr Simon Jackson}.
}
}
\examples{
# Normal theme application
my_gt_table |>
  acthd_gt_theme()
  
# Reversed application
my_gt_table |>
  acthd_gt_theme(palette = "web_purples", reverse = TRUE)
 
}
\keyword{acthd}
\keyword{color}
\keyword{gt}
\keyword{palette}
\keyword{table}

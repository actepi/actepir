# actepir

An R package developed for ACT Health's Epidemiology Section.

This package is intended to provide a small number of custom functions and document templates to support the work of the Epidemiology Section.  Most of this will not be relevant for anyone external to the Section.

No other content is stored on this repo.

## Installation

This package can be installed by calling:

```r
remotes::install_github("actepi/actepir")
```

Alternatively, if using `pacman`:

```r
pacman::p_load_gh(
  char   = "actepi/actepir",
  update = FALSE
  )
```

## Package Versioning

Following installation of this package, type `packageVersion("actepir")` in the R console to show the package version. If it is suffixed with a 9000 number then you are using a development version.

Released versions of this package will have version numbers consisting of three parts:

major.minor.patch

In-development versions of this package will have a fourth component, the development version number, which will increment from 9000.

## Links

- HealthStats: https://www.act.gov.au/directorates-and-agencies/act-health/data-statistics-and-surveys/healthstats-act
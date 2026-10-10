# tests/testthat/helper-dsr.R
#
# The diagnosis condition that .dsr_sql_codes() builds, written in R. The
# offline tests use it to pin down which codes a range takes in, and the live
# tests check the SQL against it on EpiServer's codes. As in the SQL, outer
# spaces and dots are removed and morphology codes (with "/") never match.
# Letters and digits compare as on SQL Server, where the collation ignores
# case, hence toupper().

dsr_code_matches <- function(x, codes) {
  x <- toupper(gsub(".", "", trimws(x), fixed = TRUE))
  vapply(x, function(code) {
    if (is.na(code) || grepl("/", code, fixed = TRUE)) return(FALSE)
    any(mapply(function(from, to) {
      if (from == to) return(startsWith(code, from))
      .dsr_code_cmp(code, from) >= 0 &&
        (.dsr_code_cmp(code, to) <= 0 || startsWith(code, to))
    }, codes$From, codes$To))
  }, logical(1), USE.NAMES = FALSE)
}

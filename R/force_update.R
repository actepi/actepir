#' Force an update to actepir
#'
#' This allows the user to force a reinstallation of the actepir local package
#' @keywords force_update
#' @export

force_update = function() {
  
  try(
    expr   = base::detach("package:actepir", unload = TRUE),
    silent = TRUE
    )
  
  try(
    expr   = utils::remove.packages("actepir"),
    silent = TRUE
    )
  
  utils::install.packages(
    pkgs   = "Q:/PHD/Health Improvement Branch/Epidemiology/Apps/R/common/localpackages/actepir", 
    repos  = NULL,
    type   = "source"
    )
  
}

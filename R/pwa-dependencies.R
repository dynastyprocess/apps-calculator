#' PWA dependencies utils
#'
#' @description This function attaches PWA manifest and icons to the given tag
#'
#' @param tag Element to attach the dependencies.
#'
#' @importFrom utils packageVersion
#' @importFrom htmltools tagList htmlDependency
#' @export
add_pwa_deps <- function(tag) {
  pwa_deps <- htmlDependency(
    name = "pwa-utils",
    version = packageVersion("DynastyProcessTradeCalculator"),
    src = c(file = "DynastyProcessTradeCalculator-3.0.0"),
    head = "<link rel=\"manifest\" href=\"manifest.webmanifest\"  />
<link rel=\"icon\" type=\"image/png\" href=\"icons/icon-144.png\" sizes=\"144x144\" />",
    package = "DynastyProcessTradeCalculator",
  )
  tagList(tag, pwa_deps)
}

add_pwacompat_deps <- function(tag) {
  pwacompat_deps <- htmlDependency(
    name = "pwacompat",
    version = "2.0.17",
    src = c(file = "pwacompat-2.0.17"),
    script = "js/pwacompat.min.js",
    package = "DynastyProcessTradeCalculator",
  )
  tagList(tag, pwacompat_deps)
}

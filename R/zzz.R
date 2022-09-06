.onLoad <- function(libname, pkgname) {
  op <- options()
  op.nexus <- list(
    nexus.autodetect = TRUE,
    nexus.verbose = TRUE
  )
  toset <- !(names(op.nexus) %in% names(op))
  if(any(toset)) options(op.nexus[toset])

  invisible()
}

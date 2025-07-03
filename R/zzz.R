.onLoad <- function(libname, pkgname) {
  theLib <- dirname(system.file(package = "mwColours"))
  pkgdesc <- packageDescription("mwColours", lib.loc = theLib)
  builddate <- gsub(';.*$', '', pkgdesc$Packaged)
  msg <- paste("mwColours (Version ", pkgdesc$Version, ")", sep = "")
  packageStartupMessage(msg)
}

.onAttach <- function(...) {
  theLib <- dirname(system.file(package = "mwColours"))
  pkgdesc <- packageDescription("mwColours", lib.loc = theLib)
  builddate <- gsub(';.*$', '', pkgdesc$Packaged)
  msg <- cat(paste("mwColours (Version ", pkgdesc$Version, ")", sep = ""),"\n",
             "Type ?show_colours to get started or execute show_colours() to take a glimpse at the colour set provided by this package.")
  packageStartupMessage(msg)
}

thispkg <- function(){
  "readthedocs"
}

#' @importFrom devtools dev_meta
inst_path <- function() {
  if (is.null(dev_meta(thispkg()))) {
    # thispkg() is probably installed
    system.file(package = thispkg())
  } else {
    # thispkg() was probably loaded with devtools
    file.path(getNamespaceInfo("thispkg()", "path"), "inst")
  }
}

# Return the thispkg() path for a package
# Could be in pkgdir/inst/thispkg()/ (for non-installed source packages)
# or in pkgdir/thispkg()/ (for installed packages)
pkg_sd_path <- function(package) {
  pathsrc <- file.path(package$path, "inst", "thispkg()")
  pathinst <- file.path(package$path, "thispkg()")

  if (dir.exists(pathsrc))
    pathsrc
  else if (dir.exists(pathinst))
    pathinst
}

file.path.ci <- function(...) {
  default <- file.path(...)
  if (file.exists(default)) return(default)

  dir <- dirname(default)
  if (!file.exists(dir)) return(default)

  pattern <- glob2rx(basename(default)) # Not perfect, but safer than raw name
  matches <- list.files(dir, pattern, ignore.case = TRUE,
    full.names = TRUE, include.dirs = TRUE, all.files = TRUE)
  if (length(matches) == 0) return(default)

  matches[[1]]
}


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

rows_list <- function(df) {
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, ]))
}

#' @importFrom markdown markdownToHTML
markdown <- function(x = NULL, path = NULL) {
  if (is.null(path)) {
    if (is.null(x) || x == "") return("")
  }

  (markdownToHTML(text = x, file = path, fragment.only = TRUE,
    options = c("safelink", "use_xhtml", "smartypants")))
}

# Given the name or vector of names, returns a named vector reporting
# whether each exists and is a directory.
dir.exists <- function(x) {
  res <- file.exists(x) & file.info(x)$isdir
  setNames(res, x)
}

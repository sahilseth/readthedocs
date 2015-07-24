#' @importFrom devtools dev_meta
inst_path <- function() {
  if (is.null(dev_meta("restr"))) {
    # restr is probably installed
    system.file(package = "restr")
  } else {
    # restr was probably loaded with devtools
    file.path(getNamespaceInfo("restr", "path"), "inst")
  }
}

# Return the restr path for a package
# Could be in pkgdir/inst/restr/ (for non-installed source packages)
# or in pkgdir/restr/ (for installed packages)
pkg_sd_path <- function(package) {
  pathsrc <- file.path(package$path, "inst", "restr")
  pathinst <- file.path(package$path, "restr")

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

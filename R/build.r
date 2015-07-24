## --- this file has been modified, using one from staticdocs as template

# Generate all topic pages for a package.
#' @importFrom staticdocs as.sd_package
#' @import stringr
#' @export
build_topics <- function(pkg = ".", topics, site_path = NULL,
                         templates_path = NULL) {
  pkg <- as.sd_package(pkg, site_path, templates_path)
  load_all(pkg)
  ## output path for all topics, vignettes, demos
  pkg$topic_path = file.path(pkg$site_path, "topics")
  if(!file.exists(pkg$topic_path))
    dir.create(pkg$topic_path, recursive = TRUE)

  # for each file, find name of one topic
  index <- pkg$rd_index
  ## use the simple name for search purposes
  index$name <- sapply(strsplit(index$name, ","), "[[", 1)
  ## select fewer topics
  if(!missing(topics)){
    index <- subset(index, name %in% topics)
    print(kable(index))
  }
  paths <- sprintf("%s/%s.rst", pkg$topic_path, index$name)

  # create columns for extra topic info
  index$title <- ""
  index$in_index <- TRUE

  for (i in 1:length(index$name)) {
    message("Generating ", basename(paths[[i]]))
    rd_fl = index$file_in[i]
    rd <- pkg$rd[[rd_fl]]
    rst <- to_rst(rd,
                  env = new.env(parent = globalenv()),
                  topic = str_replace(basename(paths[[i]]), "\\.rst$", ""),
                  pkg = pkg)
    ## use the simple name for search purposes
    rst$name <- strsplit(rst$name, ",")[[1]][1]
    rst$pagetitle <- rst$name
    rst$package <- pkg[c("package", "version")]
    render_page(pkg, "topic", rst, paths[[i]])
    graphics.off()

    if ("internal" %in% rst$keywords) {
      index$in_index[i] <- FALSE
    }
    index$title[i] <- rst$title
  }

  return(index)
}



#' @importFrom tools pkgVignettes buildVignettes
#' @importFrom knitr knit
#' @importFrom devtools load_all
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom stringr str_c str_match
#' @importFrom staticdocs as.sd_package render_page
#' @export
build_vignettes <- function(pkg = ".",
                         site_path = NULL,
                         templates_path = NULL) {
  pkg <- as.sd_package(pkg, site_path, templates_path)
  load_all(pkg)
  vigns <- pkgVignettes(dir = pkg$path)

  if (length(vigns$docs) == 0) return()

  message("Building vignettes")
  # Locate source and built versions of vignettes
  docs <- vigns$docs
  ### temporarily change wd, since knitr needs to create figures in CWD

  wd = getwd()
  mywd = file.path(pkg$site_path, "vignettes");
  if(!file.exists(mywd)) dir.create(mywd)
  setwd(mywd)
  files <- lapply(docs, function(d){
    ## follow these steps for markdown files
    outfile = sprintf("%s.rst", file_path_sans_ext(basename(d)))
    pdf = sprintf("%s.pdf", file_path_sans_ext(basename(d)))
    rst = sprintf("%s.rst", file_path_sans_ext(basename(d)))
    if(file_ext(d) %in% c("Rmd", "rmd")){
      ## md tmp
      md <- knit(d, quiet = FALSE, envir = globalenv())
      system(sprintf("pandoc -f markdown --no-wrap -t rst -o %s %s", outfile, md))
      return(list(file = rst, type = "rst"))
    }else if(file_ext(d) %in% c("Rnw", "rnw")){
      ## follow these steps for sweave files
      vig <- buildVignette(d, dir = dirname(d), quiet = TRUE)
      file.copy(file.path(dirname(d), pdf), pdf)
      return(list(file = pdf, type = "pdf"))
    }
  })
  ## switch back WD
  setwd(wd)

  # Extract titles
  titles <- vapply(vigns$docs, FUN.VALUE = character(1), function(x) {
    contents <- str_c(readLines(x), collapse = "\n")
    str_match(contents, "\\\\VignetteIndexEntry\\{(.*?)\\}")[2]
  })
  #names <- basename(vigns$outputs)
  df <- cbind(do.call(rbind, files), titles)
  return(df)
}

build_demos <- function(pkg = ".") {
  pkg <- as.sd_package(pkg)

  demo_dir <- file.path(pkg$path, "demo")
  if (!file.exists(demo_dir)) return()

  message("Rendering demos")
  demos <- readLines(file.path(demo_dir, "00Index"))

  pieces <- str_split_fixed(demos, "\\s+", 2)
  in_path <- str_c(pieces[, 1], ".r")
  filename <- str_c("demo-", pieces[,1], ".rst")
  title <- pieces[, 2]

  for(i in seq_along(title)) {
    demo_code <- readLines(file.path(demo_dir, in_path[i]))
    demo_expr <- evaluate(demo_code, new.env(parent = globalenv()),
                          new_device = FALSE)

    pkg$demo <- replay_rst(demo_expr, pkg = pkg, name = str_c(pieces[i], "-"))
    pkg$pagetitle <- title[i]
    render_page(pkg, "demo", pkg,
                file.path(pkg$site_path, filename[i]))
  }

  list(demo = unname(apply(cbind(filename, title), 1, as.list)))
}

## function from static docs
compact <- function (x) Filter(function(x) !is.null(x) & length(x), x)



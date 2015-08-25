# RTD

A wrapper around [staticdocs](https://github.com/hadley/staticdocs) providing a way to port R documentation into a 
[readthedocs](https://github.com/hadley/staticdocs) flavored website.

See here for an [example](docs.flowr.space).

## Features:

- Sphnix/readthedocs provides a convinient way to produce a beautiful, searchable and
powerful website.
- Further, Sphinx provides an excellent ( and powerful ) tree structure for organizing the docs
- this packages, makes it easy to convert all markdown vignettes and R help files to ReSt format
- Thus, one can use Markdown, but still use the power of ReadTheDocs (Sphinx).

```R
# install.packages("devtools")
devtools::install_github("sahilseth/readthedocs")
```

```R
library(staticdocs)
librar(readthedocs)
require(tools)
outwd = "flowrdocs/source/rd" ## assuming sphnix reads from the source directory
pkg = staticdocs::as.sd_package(pkg = ".",
                                site_path = outwd,
                                templates_path = system.file("templates", package = "readthedocs"))
tmp <- build_topics(pkg = pkg)
tmp <- build_vignettes(pkg = pkg)

```

Then one needs to call sphnix to build the website locally OR update this to github and point readthedocs.org to it.
Refer to [readthedocs](https://readthedocs.org) for more information regarding this setup.



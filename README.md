# RTD

This provides a convinient way to produce a beautiful, searchable and
powerful website for your R package. Simply put this provides a
extenstion to staticdocs and sits between Rd files and Sphinx website.


```R
# install.packages("devtools")
devtools::install_github("sahilseth/rtd")
```

### Features:

*I am already using Markdown, why bother with yet another language.*

- Exactly !
- I did not want to learn yet another language and keep on writing in
Markdown, but use the power of ReadTheDocs (Sphinx).
- ReSt (Sphinx) provides excellent (powerfull) doctree structure.
- Several other markups (.. seealso)


# Features

* Attractive defaults: staticdocs uses [bootstrap]
  (http://twitter.github.com/bootstrap/) to provide an attractive website.

* Customisable: you can override the default templates to provide
  alternative rendering

* Flexible ways to specify the index page so you can group related
  functions together.

Compared to `Rd2html`, staticdocs:

* Makes it easier to customise the output.

* Runs examples, so users see both input and output.

* Assumes only one package is being rendered - links to documentation in
  other packages are forwarded to [inside-R](http://www.inside-r.org/).

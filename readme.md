# mindr: an R package that converts markdown (.md) to mind maps, and vice versa

 ![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/mindr)

## Introduction

With 'mindr' you can draw a mind map in markdown syntax, or start a markdown document from a mind map.

mindr is an R package which converts markdown (.md) or markdown-based (.Rmd, .qmd) text to mind maps, and vice versa. The genarated mind maps can be directedly integrated into a document as a "mermaid" mind map, or opened by or imported to common mindmap software such as the desktop software ['FreeMind'](http://freemind.sourceforge.net/wiki/index.php/Main_Page)and ['XMind'](http://www.xmind.net), or the online webware '[mindmeister](https://www.mindmeister.com/)'. Plenty of cross-platform mindmap software suits are available (see [the list](https://en.wikipedia.org/wiki/List_of_concept-_and_mind-mapping_software)).

- If your are a user of markdown or rmarkdown or quarto, mindr can convert your .md or .Rmd or .qmd files into mind maps easily. Furthermore, you can write a new mind map with markdown syntax and use mindr to convert it into a mermaid mind map or an .mm mind map.
- If you are a mind map user, you can export your mind map into an .mm file and use mindr to convert it into a markdown file as an outline of your new document or book.

If you don't know what is markdown, here is [a demo file](https://github.com/pzhaonet/mindr/blob/master/inst/examples/md/bookdownplus1.md).

## Quick start

### Installation

```R
install.packages("mindr")
```

### Demonstration

```R
vignette('mindr', package = 'mindr')
```

## Showcase

- Mindmap of [R bookdownplus](https://github.com/pzhaonet/bookdownplus) [Textbook](https://github.com/pzhaonet/bookdownplus-textbook)

![](https://raw.githubusercontent.com/pzhaonet/mindr/master/showcase/mindr_bookdownplus.jpg)

- Mindmap of my book [*Learning R*](http://xuer.pzhao.net/) (in Chinese. to be published soon.)

![](https://raw.githubusercontent.com/pzhaonet/mindr/master/showcase/mindr_xuer.jpg)

## Updates

Version 1.4:

- New feature: generate mermaid mind maps. Hooray!
- New feature: convert all kinds of documents into mind maps only if they can be converted into markdown by pandoc.
- Simplify the exported functions. Now three functions do all the jobs.
- A vignette is available.

Version 1.3:

- Re-write the whole package.
- Rename and new arguments.
- The all-in-one wrapper function `mm()`.
- Complete individual functions `md2mm()`, `mm2md()`, `r2mm()`, `mm2r()`, `dir2mm()`, `mm2dir()`, `r2md()`, `md2r()`, `r2dir()`, `dir2r()`.
- The `markmap()` function supports more input types.
- Remove unnecessary dependencies (on pandoc, tree, jsonlite, data.tree).
- Remove redundant functions.
- Give more exmaples.

Older updates:

- 2019-01-25. **v1.2.0**. universal function `mm()`.
- 2018-12-16. **v1.1.9**. Added an option of 'pandoc' to extract headings.
- 2018-11-10. **v1.1.8**. Adapted to the roxygen style when conversion between .R scripts and .Rmd documents.
- 2018-10-26. **v1.1.7**. Conversion between .R scripts and .Rmd documents.
- 2018-10-21. **v1.1.6**. Support LaTeX equations.
- 2018-10-11. **v1.1.5**. Improve the support for bookdown projects. Bugs fixed.
- 2018-09-28. **v1.1.2**. Display .mm mind maps. Keep hyperlinks. `dir2()` for creating mindmaps from folder structure.
- 2018-04-17. **v1.1.1**. Support tibble dataframes.
- 2017-07-19. **v1.1.0**. On CRAN. See [CHANGES IN mindr VERSION 1.1](https://github.com/pzhaonet/mindr/releases/tag/v1.1).
- 2017-07-05. **v1.0.6**. [Rmarkmap](https://github.com/seifer08ms/Rmarkmap) added. Run `example(markmap)`.
- 2017-07-03. **v1.0.5**. Better backup.
- 2017-06-19. **v1.0.4**. Released on [CRAN](https://cran.r-project.org/web/packages/mindr)!
- 2017-06-02. **v1.0.0**. Backup existing files before overwritten. Submitted to CRAN.
- 2017-05-21. **v0.2.0**. Rename functions.
- 2017-05-21. **v0.1.0**. Bidirectional! Now mind maps can be converted to markdown.
- 2017-05-20. **v0.0.1**. Can Save the outline.
- 2017-05-19. **v0.0.0**. A very preliminary version.

## To do

- RStudio addin to convert text into a mindmap widget.

# License

Copyright 2021 [Peng Zhao](http://pzhao.org).

Released under the GPL-3 license.


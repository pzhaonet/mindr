# mindr: an R package that converts markdown (.md) or rmarkdown (.Rmd) files to mind maps, and vice versa

 ![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/mindr)

## Introduction

With 'mindr' you can draw a mind map in markdown syntax, or start a markdown document from a mind map!

mindr is an R package which converts markdown (.md) or rmarkdown (.Rmd) files to mind maps (.mm), and vice versa. Mind map files (.mm) can be opened by or imported to common mindmap software such as the desktop software ['FreeMind'](http://freemind.sourceforge.net/wiki/index.php/Main_Page)and ['XMind'](http://www.xmind.net), or the online webware '[mindmeister](https://www.mindmeister.com/)'. Plenty of cross-platform mindmap software suits are available (see [the list](https://en.wikipedia.org/wiki/List_of_concept-_and_mind-mapping_software)).

- If your are a  user of markdown or  rmarkdown or bookdown or blogdown, mindr can convert your .md or .Rmd files into mind maps easily. Furthermore, you can write a new mind map with markdown syntax and use mindr to convert it into an .mm mind map.
- If you are a mind map user, you can export your mind map into an .mm file and use mindr to convert it into a markdown file as an outline of your new document or book.

If you don't know what is markdown, here is [a demo file](https://github.com/pzhaonet/mindr/blob/master/inst/examples/md/bookdownplus1.md).

## Quick start

### Installation

```R
# stable version:
install.packages("mindr")
# or development version:
devtools::install_github("pzhaonet/mindr")
```

### Convert between mind maps, markdown files, texts and directories 

From v.1.2.0, the old individual functions such as `md2mm()` or `mm2md()` are to be deprecated. A more powerful, universal function `mm()` takes the place as the main function of mindr.

```
library('mindr')
### text -> widget
input <- c("# Chapter 1", "## Section 1.1", "## Section 1.2", "# Chapter 2")
mm(from = input, type = "text", root = "mindr")

### directory -> widget
input <- paste0(.libPaths(), "/mindr")[1]
mm(from = input, type = "dir")
mm(from = input, type = "dir", widget_name = "mindrtest.html")
### directory -> mm
mm(from = input, type = "dir", to = "test.mm")
### directory -> md
mm(from = input, type = "dir", to = "test.md")
### directory -> txt
mm(from = input, type = "dir", to = "test.txt")

### Rmd -> widget
input <- system.file("examples/r/rmd2r.Rmd", package = "mindr")
mm(from = input, type = "file", root = "mindr")
### Rmd -> r
mm(from = input, type = "file", root = "mindr", to = "test.r")
### Rmd -> mm
mm(from = input, type = "file", root = "mindr", to = "test.mm")

### mm -> widget
input <- system.file("examples/mm/bookdownplus.mm", package = "mindr")
mm(from = input, type = "file", root = "mindr")
### mm -> Rmd
mm(from = input, type = "file", root = "mindr", to = "test.Rmd")
### mm -> r
mm(from = input, type = "file", root = "mindr", to = "test.r")

### r -> widget
input <- system.file("examples/r/r2rmd.R", package = "mindr")
mm(from = input, type = "file", root = "mindr")
### r -> Rmd
mm(from = input, type = "file", root = "mindr", to = "test.Rmd")
### r -> mm
mm(from = input, type = "file", root = "mindr", to = "test.mm")

### The outline of the book Learning R
input <- system.file("examples/xuer/xuer.md", package = "mindr")
mm(from = input, type = "file", root = "Learning R", to = "learningr.mm")
```

### Create Interactive Web MindMap with the JavaScript 'markmap' Library

Run: 

```R
example(markmap)
```

then you will see a demo interactive mind map in the viewer of your R session. 

More themes can be seen if you run:

```R
example(markmapOption)
```

To create your own interactive mind map, create a folder named `mm` in the working directory (`getwd()`), and drop some .mm files into `mm/`. Run:

```R
markmap()
```

### Extract the outline from (a) markdown files

Run: 

```R
library('mindr')
example(outline)
```
then you will get a demo outline file `outline.md` in the working directory (`getwd()`). 

To extract the outline from your own markdown files, create a folder named `mm` in the working directory (`getwd()`), and drop some markdown or rmarkdown files into `mm/`. Run:

```R
outline()
```


Have fun!

Still being developed. Feel free to give your feedback to me!

## Showcase

- Mindmap of [R bookdownplus](https://github.com/pzhaonet/bookdownplus) [Textbook](https://github.com/pzhaonet/bookdownplus-textbook)

![](https://raw.githubusercontent.com/pzhaonet/mindr/master/showcase/mindr_bookdownplus.jpg)

- Mindmap of my book [*Learning R*](http://xuer.pzhao.net/) (in Chinese. to be published soon.)

![](https://raw.githubusercontent.com/pzhaonet/mindr/master/showcase/mindr_xuer.jpg)

## Updates

New updates after 2019-01-25 are not logged here. Please see the commits.

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

- convert md/Rmd/mm into slides.
- RStudio addin to convert selected text into a mindmap widget.
- support numbered/unnumbered lists.
- show the complete path of the output files.

# License

Copyright 2019 [Peng Zhao](http://pzhao.org).

Released under the MIT license.


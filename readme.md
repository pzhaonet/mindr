# mindr: an R package that converts markdown (.md) or rmarkdown (.Rmd) files to mind maps, and vice versa

 ![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/mindr)

## Introduction

With 'mindr' you can draw a mind map in markdown syntax, or start a markdown document from a mind map!

mindr is an R package which converts markdown (.md) or rmarkdown (.Rmd) files to mind maps (.mm), and vice versa. Mind map files (.mm) can be opened by or imported to common mindmap software such as the desktop software ['FreeMind'](http://freemind.sourceforge.net/wiki/index.php/Main_Page)and ['XMind'](http://www.xmind.net), or the online webware '[mindmeister](https://www.mindmeister.com/)'.

- If your are a  user of markdown or  rmarkdown or bookdown or blogdown, mindr can convert your .md or .Rmd files into mind maps easily. Furthermore, you can write a new mind map with markdown syntax and use mindr to convert it into an .mm mind map.
- If you are a mind map user, you can export your mind map into an .mm file and use mindr to convert it into a markdown file as an outline of your new document or book.

## Quick start

### Installation

```R
# stable version:
install.packages("mindr")
# or development version:
devtools::install_github("pzhaonet/mindr")
```

### Convert (a) markdown file(s) into a mindmap 

Run: 

```R
library('mindr')
example(md2mm)
```
then you will get a demo mind map file `mindr.mm` in the working directory (`getwd()`). Open it with any mind-map (brainstorm) software, and you will get a mind map.

To create a mind map from your own markdown files, create a folder named `mm` in the working directory (`getwd()`), and drop some markdown or rmarkdown files into `mm/`. Run:

```R
md2mm()
```

Plenty of cross-platform mindmap software suits are available (see [the list](https://en.wikipedia.org/wiki/List_of_concept-_and_mind-mapping_software)), among which [freemind](http://freemind.sourceforge.net/wiki/index.php/Download) and [Xmind](http://www.xmind.net/download/win/) are highly recommended. If you would not like to install any of them, you could open the `.mm` file with the online webware '[mindmeister](https://www.mindmeister.com/)' in your web browser.

If you don't know what is markdown, here is [a demo file](https://github.com/pzhaonet/mindr/blob/master/inst/examples/md/bookdownplus1.md).

Actually this is a new way to draw a mind map!

### Convert (a) mind map(s) into a markdown file 

Run: 

```R
library('mindr')
example(mm2md)
```

then you will get a demo markdown file `mindr.md`in the working directory (`getwd()`). 

To create a markdown file from your own mind map files, create a folder named `mm` in the working directory (`getwd()`), and drop some .mm files into `mm/`. Run:

```R
mm2md()
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

### Convert a tibble dataframe column into amindmap

Run: 

```R
library('mindr')
example(tree2mm)
```
then you will get a demo outline file `mindr.md` in the working directory (`getwd()`). 

Have fun!

Still being developed. Feel free to give your feedback to me!

## Showcase

- Mindmap of [R bookdownplus](https://github.com/pzhaonet/bookdownplus) [Textbook](https://github.com/pzhaonet/bookdownplus-textbook)

![](https://raw.githubusercontent.com/pzhaonet/mindr/master/showcase/mindr_bookdownplus.jpg)

- Mindmap of my book [*Learning R*](http://xuer.pzhao.net/) (in Chinese. to be published soon.)

![](https://raw.githubusercontent.com/pzhaonet/mindr/master/showcase/mindr_xuer.jpg)

## Updates

- 2018-09-28. **v1.1.2**. Display .mm mind maps.
- 2018-04-17. **v1.1.1**. Support tribble dataframes.
- 2017-07-19. **v1.1.0**. On CRAN. See [CHANGES IN mindr VERSION 1.1](https://github.com/pzhaonet/mindr/releases/tag/v1.1).
- 2017-07-05. **v1.0.6**. [Rmarkmap](https://github.com/seifer08ms/Rmarkmap) added. Run `example(markmap)`.
- 2017-07-03. **v1.0.5**. Better backup.
- 2017-06-19. **v1.0.4**. Released on [CRAN](https://cran.r-project.org/web/packages/mindr)!
- 2017-06-02. **v1.0.0**. Backup existing files before overwritten. Submitted to CRAN.
- 2017-05-21. **v0.2.0**. Rename functions.
- 2017-05-21. **v0.1.0**. Bidirectional! Now mind maps can be converted to markdown.
- 2017-05-20. **v0.0.1**. Can Save the outline.
- 2017-05-19. **v0.0.0**. A very preliminary version.

# License

Copyright 2017 [Peng Zhao](http://pzhao.org).

Released under the GPL license.


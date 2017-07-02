# mindr: an R package which convert markdown (.md) or rmarkdown (.Rmd) files to mind maps, and vice versa


## Introduction

mindr is an R package which converts markdown (.md) or rmarkdown (.Rmd) files to mind maps (.mm), and vice versa. Mind map files (.mm) can be opened by or imported to common mindmap software such as ['FreeMind'](http://freemind.sourceforge.net/wiki/index.php/Main_Page)and ['XMind'](http://www.xmind.net).

If your are a markdown or  rmarkdown or bookdown or blogdown user, mindr can convert your .md or .Rmd files into mind maps easily. You can write a new mind map with markdown syntax and use mindr to convert it into an .mm mind map.

If you are a mind map user, you can export your mind map into an .mm file and use mindr to convert it into a markdown file as an outline of your document or book.

## Quick start

### Installation

```R
# stable version:
install.packages("mindr")
# or development version:
devtools::install_github("pzhaonet/mindr")
```

### Convert (a) markdown file(s) into a mindmap 

Create a folder named `mm` in the working directory (`getwd()`), and drop some markdown or rmarkdown files into `mm/`. Run: 

```R
mindr::md2mm()
```

then you will get a `mm.mm` file. Open it with any mind-map (brainstorm) software, and you will get a mind map.

Plenty of cross-platform and online mindmap software suits are available (see [the list](https://en.wikipedia.org/wiki/List_of_concept-_and_mind-mapping_software)), among which [freemind](http://freemind.sourceforge.net/wiki/index.php/Download) and [Xmind](http://www.xmind.net/download/win/) are highly recommended.

### Convcert (a) mind map(s) into a markdown file 

Drop some mindmap files (.mm) into `mm/`. Run: 

```R
mindr::mm2md()
```

then you will get a `mm.md` markdown file.

### Extract the outline from (a) markdown files

Drop some markdown or rmarkdown files into `mm/`. Run: 

```R
mindr::outline()
```

then you will get an `outline.md` file with headers in it as an outline.


Have fun!

Still being developed. Feel free to give your feedback to me!

## Showcase

- Mindmap of [bookdown manual](https://github.com/rstudio/bookdown/tree/master/inst/examples)

![](https://raw.githubusercontent.com/pzhaonet/mindr/master/showcase/mindr_bookdown.jpeg)

- Mindmap of [blogdown manual](https://github.com/rstudio/blogdown/tree/master/docs)

![](https://raw.githubusercontent.com/pzhaonet/mindr/master/showcase/mindr_blogdown.jpg)

- Mindmap of my book *Learning R*

![](https://raw.githubusercontent.com/pzhaonet/mindr/master/showcase/mindr_xuer.jpg)

## Updates

- 2017-06-19: **v1.0.4**. Released on [CRAN](https://cran.r-project.org/web/packages/mindr)!
- 2017-06-02. **V1.0.0**. Backup existing files before overwritten. Submitted to CRAN.
- 2017-05-21. **V0.2.0**. Rename functions.
- 2017-05-21. **V0.1.0**. Bidirectional! Now mind maps can be converted to markdown.
- 2017-05-20. **V0.0.1**. Can Save the outline.
- 2017-05-19. **V0.0.0**. A very preliminary version.

# License

Copyright 2017 [Peng Zhao](http://pzhao.org).

Released under the [MIT](https://github.com/pzhaonet/bookdown-plus/blob/master/LICENSE.md) license.


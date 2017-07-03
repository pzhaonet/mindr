# mindr: an R package which converts markdown (.md) or rmarkdown (.Rmd) files 

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

Create a folder named `mm` in the working directory (`getwd()`), and drop some markdown or rmarkdown files into `mm/`. Run: 

```R
mindr::md2mm()
```

then you will get a `mm.mm` file. Open it with any mind-map (brainstorm) software, and you will get a mind map.

Plenty of cross-platform mindmap software suits are available (see [the list](https://en.wikipedia.org/wiki/List_of_concept-_and_mind-mapping_software)), among which [freemind](http://freemind.sourceforge.net/wiki/index.php/Download) and [Xmind](http://www.xmind.net/download/win/) are highly recommended. If you would not like to install any of them, you could open the `.mm` file with the online webware '[mindmeister](https://www.mindmeister.com/)' in your web browser.

If you don't know what is markdown, just copy the following texts and paste them into a text file, and follow the steps described previously.

```markdown
# Introduction 
## What is 'bookdown' 
## What is 'bookdownplus' 
## Why 'bookdownplus' 
## Giants' Shoulders 
# Quick Start 
## Preparation 
## Installation of 'bookdownplus' 
## How to use 
## More output formats
## More templates
## A magic trick
## Recommendations
# Basic 
## Markdown Syntax 
### What is Markdown 
### Basic syntax 
### Chapters 
### Figures and tables 
### References 
### Theorems, lemma, definitions, etc. 
### Export Word document 
### Equations numbering 
```

Actually this is a new way to draw a mind map!

### Convert (a) mind map(s) into a markdown file 

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

- Mindmap of [R bookdownplus Textbook](https://github.com/pzhaonet/bookdownplus-textbook)

![](https://raw.githubusercontent.com/pzhaonet/mindr/master/showcase/mindr_bookdownplus.jpg)

- Mindmap of the book *Learning R* (in Chinese)

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


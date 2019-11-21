#' Convert markdown or rmarkdown files to mindmap files.
#'
#' @param title character. The title of the output file.
#' @param remove_curly_bracket logical. Whether to remove {#ID} in the headers of the markdown file (usually in a 'bookdown' <https://github.com/rstudio/bookdown> project).
#' @param path character. The path of the folder which contains the input file(s).
#' @param pattern an optional regular expression for filtering the input files. See `help(dir)`.
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backup.
#' @param bookdown_style logical. whether the markdown files are in bookdown style, i.e. index.Rmd at the beginning, `# (PART)`, `# (APPENDIX)` and `# References` as an upper level of normal `#` title
#' @param keep_eq logical. whether to keep LaTeX equations.
#' @param method "regexpr" uses regular expressions, 'pandoc' uses pandoc to find the headings.
#' @param savefile logical. Whether to save the output as a file.
#'
#' @return a mindmap file, which can be viewed by common mindmap software, such as 'FreeMind' (<http://freemind.sourceforge.net/wiki/index.php/Main_Page>) and 'XMind' (<http://www.xmind.net>).
#' @export
#' @examples
#' path <- system.file('examples/md', package = 'mindr')
#' md2mm(path = path)
#' md2mm(path = path, remove_curly_bracket = TRUE)
md2mm <- function(pattern = '*.[R]*md$',
                  title = NA,
                  path = '.',
                  remove_curly_bracket = FALSE,
                  savefile = TRUE,
                  savefilename = NA,
                  backup = TRUE,
                  bookdown_style = TRUE,
                  keep_eq = FALSE,
                  method = c('regexpr', 'pandoc')) {
  if (dir.exists(path)) {
    method <- match.arg(method)
    header <- outline(
      path = path,
      pattern = pattern,
      remove_curly_bracket = remove_curly_bracket,
      savefile = FALSE,
      bookdown_style = bookdown_style,
      keep_eq = keep_eq,
      method = method
    )
    foldername <- basename(path)
    if (is.na(title))
      title <- foldername
    if (is.na(savefilename))
      savefilename <- paste0(foldername, '.mm')
    mm <- mdtxt2mmtxt(title = title, mdtxt = header)
    if (savefile)
      writeLines2(text = mm,
                  filename = savefilename,
                  backup = backup)
    return(mm)
  } else {
    message(paste('The directory', path, 'does not exist!'))
  }
}


#' Convert a mind map (.mm) into markdown headers.
#'
#' @param path character. The path of the folder which contains the input file(s).
#' @param savefile logical. Whether to save the output as a markdown file.
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backups.
#' @param pattern an optional regular expression for filtering the input files. See `help(dir)`.
#'
#' @return a vector of strings showing outline of a markdown document or book.
#' @export
#' @examples
#' path <- system.file('examples/mm', package = 'mindr')
#' mm2md(path = path)
mm2md <- function(pattern = '*.mm$',
                  path = '.',
                  savefile = TRUE,
                  savefilename = 'mindr.md',
                  backup = TRUE) {
  if (dir.exists(path)) {
    mm <- NULL
    for (filename in dir(path, pattern = pattern, full.names = TRUE))
      mm <- c(mm, readLines(filename, encoding = 'UTF-8'))
    # compitable for both versions: node ends with '/>' or '</node>'
    mm <- gsub(pattern = '/>', '</node>', mm)
    # keep links
    loc_link <- grep('LINK="([^\"]*)"', mm)
    links <-
      gsub('LINK="([^\"]*)"', '\\1', regmatches(mm, gregexpr('LINK="[^\"]*"', mm)))
    for (i in loc_link)
      mm[i] <-
      gsub('TEXT="([^"]*)"',
           paste0('TEXT=\"[\\1](', links[i], ')\"'),
           mm[i])

    mm <- paste0(mm, collapse = '')
    node_begin <- unlist(gregexpr('<node', mm))
    node_end <- unlist(gregexpr('</node', mm))
    node_sign <-
      c(rep(1, length(node_begin)), rep(-1, length(node_end)))
    node_loc <- c(node_begin, node_end)
    node_sign <- node_sign[order(node_loc)]
    node_level <- cumsum(node_sign)[node_sign == 1]
    headers <-
      gsub('TEXT="([^"]*)"', '\\1', regmatches(mm, gregexpr('TEXT="[^"]*"', mm))[[1]])
    md <-
      paste(sapply(node_level - 1, function(x)
        paste0(rep('#', x), collapse = '')), headers)
    md[1] <- paste('Title:', md[1])
    md <- gsub('&amp;', '&', md)
    md <- gsub('&quot;', '"', md)
    if (savefile) {
      writeLines2(text = md,
                  filename = savefilename,
                  backup = backup)
    }
    return(md)
  } else {
    print(paste('The directory', path, 'does not exist!'))
  }
}

#' Extract headers of markdown or rmarkdown files as an outline.
#'
#' @param path character. The path of the folder which contains the input file(s).
#' @param remove_curly_bracket logical. Whether to remove {#ID} in the headers of the markdown file (usually in a 'bookdown' <https://github.com/rstudio/bookdown> project).
#' @param savefile logical. Whether to save the output as a markdown file.
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backups.
#' @param pattern an optional regular expression for filtering the input files. See `help(dir)`.
#' @param bookdown_style logical. whether the markdown files are in bookdown style, i.e. index.Rmd at the beginning, `# (PART)`, `# (APPENDIX)` and `# References` as an upper level of normal `#` title
#' @param keep_eq logical. whether to keep LaTeX equations.
#' @param method "regexpr" uses regular expressions, 'pandoc' uses pandoc to find the headings.
#'
#' @return a vector of strings showing outline of a markdown document or book.
#' @import jsonlite
#' @export
#' @examples
#' path <- system.file('examples/md', package = 'mindr')
#' outline(path = path)
#' outline(path = path, remove_curly_bracket = TRUE)
outline <- function(pattern = '*.[R]*md',
                    path = '.',
                    remove_curly_bracket = FALSE,
                    savefile = TRUE,
                    savefilename = 'outline.md',
                    backup = TRUE,
                    bookdown_style = TRUE,
                    keep_eq = FALSE,
                    method = c('regexpr', 'pandoc')) {
  if (dir.exists(path)) {
    method <- match.arg(method)
    # read data
    md <- NULL
    files <- dir(path, pattern = pattern, full.names = TRUE)
    ## bookdown style: if index.Rmd exists, read it first
    if (bookdown_style) {
      first_file <- paste0(path, '/index.Rmd')
      if (first_file %in% files)
        files <- c(first_file, files[files != first_file])
    }
    for (filename in files)
      md <- c(md, readLines(filename, encoding = 'UTF-8'))
    mdlength <- length(md)

    if (method == 'pandoc') {
      temptxt <- 'mimdrtemp.tmp'
      writeLines(md, temptxt, useBytes = TRUE)
      md <-
        system2('pandoc',
                c('-f', 'markdown', '-t', 'json', temptxt),
                stdout = TRUE)
      file.remove(temptxt)
      md <- jsonlite::fromJSON(txt = md)
      md <- md$blocks$c
      header <- paste(sapply(sapply(md, function(x)
        x[[1]][1]), function(x)
          paste(rep('#', x), collapse = '')),
        sapply(md, function(x)
          x[[2]][[1]][1]))
    } else {
      # exclude the code blocks
      codeloc2 <- grep('^````', md)
      if (length(codeloc2) > 0)
        md <-
          md[!sapply(1:mdlength, function(x)
            rmvcode(index = x, loc = codeloc2))]
      codeloc <- grep('^```', md)
      if (length(codeloc) > 0)
        md <-
        md[!sapply(1:mdlength, function(x)
          rmvcode(index = x, loc = codeloc))]

      # get the outline
      headerloc <- get_heading(text = md)

      # remove the curly brackets
      if (remove_curly_bracket)
        md[headerloc] <-
        gsub(pattern = '\\{.*\\}', '', md[headerloc])

      # remove the heading marker, which is eight '-' at the end of a heading
      md[headerloc] <-
        gsub(pattern = ' --------$', '', md[headerloc])

      # extract equations
      if (keep_eq) {
        eq_begin <- grep('^\\$\\$', md)
        eq_end <- grep('\\$\\$$', md)
        eq_loc <- get_eqloc(eq_begin, eq_end)
        headerloc <- c(headerloc, eq_loc)
        headerloc <- headerloc[order(headerloc)]
      }
      header <- md[headerloc]

      # lower the levels after `# (PART)` and `# (APPENDIX)`
      if (bookdown_style) {
        part_loc <-
          c(
            grep('^# \\(PART\\)', header),
            grep('^# \\(APPENDIX\\)', header),
            grep('^# References', header)
          )
        if (length(part_loc) > 0) {
          header[part_loc] <- gsub(' \\(PART\\)', '', header[part_loc])
          header[part_loc] <-
            gsub(' \\(APPENDIX\\)', '', header[part_loc])
          lower_loc <- (part_loc[1] + 1):length(header)
          lower_loc <- lower_loc[!lower_loc %in% part_loc]
          header[lower_loc] <- paste0('#', header[lower_loc])
        }
      }
    }
    # save file
    if (savefile) {
      writeLines2(text = header,
                  savefilename,
                  backup = backup)
    }

    return(header)
  } else {
    print(paste('The directory', path, 'does not exist!'))
  }
}



#' Create a markmap widget
#'
#' This function, modified from <https://github.com/seifer08ms/Rmarkmap>, creates a markmap widget using htmlwidgets. The widget can be rendered on HTML pages generated from R Markdown, Shiny,or other applications.
#'
#' @param path character. The path of the folder which contains the input file(s).
#' @param remove_curly_bracket logical. Whether to remove {#ID} in the headers of the markdown file (usually in a 'bookdown' <https://github.com/rstudio/bookdown> project).
#' @param width the width of the markmap
#' @param height the height of the markmap
#' @param elementId character.
#' @param options the markmap options
#' @param input character, The format of theinput files
#' @param root character. a string displayed as the root of the mind map
#' @param bookdown_style logical. whether the markdown files are in bookdown style, i.e. index.Rmd at the beginning, `# (PART)`, `# (APPENDIX)` and `# References` as an upper level of normal `#` title
#' @param method "regexpr" uses regular expressions, 'pandoc' uses pandoc to find the headings.
#'
#' @import htmlwidgets
#' @return A HTML widget object rendered from a given document.
#' @export
#' @examples
#' path <- system.file('examples/md', package = 'mindr')
#' markmap(path = path)
#' markmap(path = path, remove_curly_bracket = TRUE)
markmap <- function(root = NA,
                    input = c('.md', '.Rmd', '.mm'),
                    path = '.',
                    remove_curly_bracket = FALSE,
                    width = NULL,
                    height = NULL,
                    elementId = NULL,
                    options = markmapOption(preset = 'colorful'),
                    bookdown_style = TRUE,
                    method = c('regexpr', 'pandoc')) {
  input <- match.arg(input)
  if (!is.na(path) & dir.exists(path)) {
    if (input == '.md') {
      method <- match.arg(method)
      header <- outline(
        path = path,
        remove_curly_bracket = remove_curly_bracket,
        savefile = FALSE,
        bookdown_style = bookdown_style,
        method = method
      )
      header <- paste0('#', header)
      header <-
        c(paste('#', ifelse(is.na(root), path, root)), header)
    } else if (input == '.mm') {
      header <- mm2md(path = path, savefile = FALSE)
    } else {
      message('Please give a valid input.')
    }
    data <- paste(header, collapse = '\n')
    # forward options using x
    x = list(data = data,
             options = options)

    # create widget
    htmlwidgets::createWidget(
      name = 'markmap',
      x,
      width = width,
      height = height,
      sizingPolicy = htmlwidgets::sizingPolicy(
        defaultWidth = '100%',
        defaultHeight = 400,
        padding = 0,
        browser.fill = TRUE
      ),
      package = 'mindr',
      elementId = elementId
    )
  } else {
    message(paste('Please give a valid path to the directory.'))
  }
}

#' Options for markmap creation
#'
#' This function is taken from <https://github.com/seifer08ms/Rmarkmap>.

#' @param preset the name of built-in theme for markmap. If present, any other parameters will be ignored.
#' @param nodeHeight the height of nodes in the markmap.
#' @param nodeWidth the width of nodes in the markmap.
#' @param spacingVertical space of vertical.
#' @param spacingHorizontal space of horizontal.
#' @param duration duration time for animation.
#' @param layout layout mode of makrmap. Currently, only 'tree' is accepted.
#' @param color color of markmap. A character color value ,either 'gray' or
#' a categorical colors including 'category10','category20','category20b' and 'category20c'.
#' @param linkShape link shape of markmap. A character value, either 'diagonal' or 'bracket'.
#' @param renderer rendered shaped of markmap. A character value ,either 'basic' or 'boxed'.
#' @param ... other options.
#' @describeIn theme Options for markmap creation
#' @details Currently,markmap have 'default' and 'colorful' themes.
#' 'colorful' themes have three different parameters from default themes:
#'  {nodeHeight: 10, renderer: 'basic',color: 'category20'}
#' @seealso \url{https://github.com/dundalek/markmap/blob/master/lib/view.mindmap.js} for details.
#' @export
#' @examples
#' path <- system.file('examples/md', package = 'mindr')
#' markmap(path = path, remove_curly_bracket = TRUE,
#'   options = markmapOption(preset = 'colorful')) # 'colorful' theme
#' markmap(path = path, remove_curly_bracket = TRUE,
#'   options = markmapOption(color = 'category20b',
#'     linkShape = 'bracket')) # 'colorful' theme
#' markmap(path = path, remove_curly_bracket = TRUE,
#'   options = markmapOption(color = 'category20b',
#'     linkShape = 'diagonal',
#'     renderer = 'basic')) # 'colorful' theme
markmapOption <- function(preset = NULL,
                          nodeHeight = 20,
                          nodeWidth = 180,
                          spacingVertical = 10,
                          spacingHorizontal = 120,
                          duration = 750,
                          layout = 'tree',
                          color = 'gray',
                          linkShape = 'diagonal',
                          renderer = 'boxed',
                          ...) {
  filterNULL <- function (x) {
    if (length(x) == 0 || !is.list(x))
      return(x)
    x[!unlist(lapply(x, is.null))]
  }
  if (!is.null(preset) &&
      (preset == 'default' | preset == 'colorful')) {
    filterNULL(list(preset = preset, autoFit = TRUE))
  } else{
    if (is.null(layout) || (layout != 'tree')) {
      warning('Currenly, only tree layout is supported. Changing to tree layout...')
      layout = 'tree'
    }
    filterNULL(
      list(
        nodeHeight = nodeHeight,
        nodeWidth = nodeWidth,
        spacingVertical = spacingVertical,
        spacingHorizontal = spacingHorizontal,
        duration = duration,
        layout = 'tree',
        color = color,
        linkShape = linkShape,
        renderer = renderer,
        autoFit = TRUE,
        ...
      )
    )
  }
}
#' Shiny bindings for markmap
#'
#' Output function for using markmap within Shiny applications and interactive Rmd documents. This function is taken from <https://github.com/seifer08ms/Rmarkmap>.
#'
#' @param width Must be a valid CSS unit (like \code{'100\%'}, \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a string and have \code{'px'} appended.
#' @param height See 'width'.
#' @param outputId output variable to read from
#'
# @rdname markmap-shiny
#' @export
#'
markmapOutput <-
  function(outputId,
           width = '100%',
           height = '400px') {
    htmlwidgets::shinyWidgetOutput(outputId, 'markmap', width, height, package = 'mindr')
  }
#' Shiny bindings for markmap
#'
#' Render function for using markmap within Shiny applications and interactive Rmd documents. This function is taken from <https://github.com/seifer08ms/Rmarkmap>.
#'
#' @param expr An expression that generates a markmap
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This is useful if you want to save an expression in a variable.
#'
# @rdname markmap-shiny
#' @export
#'
renderMarkmap <-
  function(expr,
           env = parent.frame(),
           quoted = FALSE) {
    if (!quoted) {
      expr <- substitute(expr)
    } # force quoted
    htmlwidgets::shinyRenderWidget(expr, markmapOutput, env, quoted = TRUE)
  }

#' Convert markdown text to mindmap text.
#'
#' @param title character. The title of the output file.
#' @param mdtxt character. The markdown text to convert.
#' @param keep_eq logical. whether to keep LaTeX equations.
#'
#' @return a mindmap text.
#' @export
#' @examples
#' mdtxt2mmtxt(mdtxt = c('# Chapter 1', '## Section 1.1', '## Section 1.2'))
mdtxt2mmtxt <-
  function(title = 'my title',
           mdtxt = '',
           keep_eq = FALSE) {
    j <- 1
    mmtxt2 <- mdtxt[j]
    for (i in 2:length(mdtxt)) {
      if (grepl('^#+', mdtxt[i])) {
        j <- j + 1
        mmtxt2[j] <- mdtxt[i]
      } else {
        mmtxt2[j] <- paste(mmtxt2[j], mdtxt[i])
      }
    }

    mmtxt <- gsub(pattern = '&', '&amp;', mmtxt2)
    mmtxt <- gsub(pattern = '"', '&quot;', mmtxt)
    ncc <-
      sapply(mmtxt, function(x)
        nchar(strsplit(x, split = ' ')[[1]][1])) # level of the headings
    mmtext <- substr(mmtxt, ncc + 2, nchar(mmtxt)) # heading text

    # get the hyperlinks
    which_link <- grep(pattern = '\\[.*](.*)', mmtext)
    mmtext[which_link] <-
      gsub('\\((.*)\\)', '" LINK="\\1', mmtext[which_link])
    mmtext[which_link] <-
      gsub(pattern = '[][]*', replacement = '', mmtext[which_link])
    mm <- rep('', length(mmtxt) + 3)
    mm[1] <- '<map version="1.0.1">'

    mm[2] <-
      paste0('<node TEXT="', title, '">', paste0(rep('<node TEXT="">', ncc[1] - 1), collapse = ''))
    diffncc <- diff(ncc)
    for (i in 1:(length(mmtxt) - 1)) {
      if (diffncc[i] == 1)
        mm[i + 2] <- paste0('<node TEXT="', mmtext[i], '">')
      if (diffncc[i] == 0)
        mm[i + 2] <- paste0('<node TEXT="', mmtext[i], '"></node>')
      if (diffncc[i] < 0)
        mm[i + 2] <-
          paste0('<node TEXT="', mmtext[i], '">', paste0(rep('</node>',-diffncc[i] + 1), collapse = ''))
      mm[i + 2] <-
        gsub('(\\$\\$[^$]+\\$\\$)(">)(</node>)$', '\\2\\1\\3', mm[i + 2])
      mm[i + 2] <-
        gsub(
          '\\$\\$([^$]+)\\$\\$',
          '<hook EQUATION="\\1" NAME="plugins/latex/LatexNodeHook.properties"/>',
          mm[i + 2]
        )
    }
    mm[length(mmtxt) + 2] <-
      paste0('<node TEXT="', mmtext[length(mmtxt)], '">', paste0(rep('</node>', ncc[length(mmtxt)]), collapse = ''))
    mm[length(mmtxt) + 3] <- '</node></map>'
    return(mm)
  }

#' Convert a directory tree to a mindmap file.
#'
#' @param tree character. The directory tree.
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backup.
#' @param n_root numeric. Which element is the root of the tree.
#' @param savefile logical. Whether to save the output as a file.
#'
#' @return a mindmap file, which can be viewed by common mindmap software, such as 'FreeMind' (<http://freemind.sourceforge.net/wiki/index.php/Main_Page>) and 'XMind' (<http://www.xmind.net>).
#' @export
#' @examples
#' et2 <- c('/Root name',
#' '/Path A',
#' '/Path A/Product A',
#' '/Path A/Product A/Process A',
#' '/Path A/Product A/Process A/Step A',
#' '/Path A/Product A/Process A/Step A/Record 1',
#' '/Path A/Product A/Process A/Step A/Record 1/Analyses',
#' '/Path A/Product A/Process A/Step A/Record 1/Analyses/Object 1',
#' '/Path A/Product A/Process A/Step A/Record 1/Analyses/Object 1/Type: data source',
#' '/Path A/Product A/Process A/Step A/Record 1/Analyses/Object 1/Version: 3',
#' '/Path A/Product A/Process A/Step A/Record 1/Analyses/Object 2',
#' '/Path A/Product A/Process A/Step A/Record 1/Analyses/Object 3',
#' '/Path A/Product A/Process A/Step A/Record 1/Setup Parts',
#' '/Path A/Product A/Process A/Step A/Record 1/Setup Parts/Par 1',
#' '/Path A/Product A/Process A/Step A/Record 1/Setup Parts/Par 2',
#' '/Path A/Product A/Process A/Step A/Record 1/Setup Parts/Par 3',
#' '/Path B',
#' '/Path C')
#' tree2mm(et2)
tree2mm <- function(tree,
                    savefile = TRUE,
                    savefilename = 'mindr',
                    backup = TRUE,
                    n_root = 1) {
  tree_title <- gsub('/', '', tree[n_root])
  tree_node <-
    sapply(strsplit(tree[-n_root], '/'), function(x)
      x[length(x)])
  tree_pre <-
    strrep('#', sapply(gregexpr('/', tree[-n_root]), length))
  tree_new <- paste(tree_pre, tree_node)
  mm <- mdtxt2mmtxt(title = tree_title, mdtxt = tree_new)
  if (savefile) {
    savefilename <-
      paste0(savefilename,
             ifelse(backup &
                      file.exists(paste0(
                        savefilename, '.mm'
                      )), paste0(
                        '-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S')
                      ), ''),
             '.mm')
    writeLines(text = mm, savefilename, useBytes = TRUE)
  }
  return(mm)
}


#' Convert .R scripts into a .md/.Rmd file
#'
#' @param path the path of the folder which contains the .R scripts
#' @param filepattern the pattern of the script file names
#' @param savefilename the destinated file name
#' @param backup logical. whether backup the existent file
#' @param body the indicator of the body text
#'
#' @return a markdown file
#' @export
#' @examples r2md()
r2md <- function(filepattern = '*.R$',
                 path = '.',
                 savefilename = NA,
                 backup = TRUE,
                 body = "#' ") {
  if (dir.exists(path)) {
    # read data
    rtext <- NULL
    files <- dir(path, pattern = filepattern, full.names = TRUE)
    for (filename in files)
      rtext <- c(rtext, readLines(filename, encoding = 'UTF-8'))

    # find the indeces of the headings, the body texts, and the code blocks
    headerloc <- get_heading(text = rtext)
    if (body != '^#[^[:blank:]#]+')
      body <- paste0('^', body)
    bodyloc <- get_body(pattern = body, text = rtext)
    codeloc <- grep(pattern = '^[^#]', x = rtext)

    # process the code blocks
    codeloc_diff <- diff(codeloc)
    codeloc_begin <- codeloc[c(1, which(codeloc_diff > 1) + 1)]
    codeloc_end <-
      codeloc[c(which(codeloc_diff > 1), length(codeloc))]
    rtext[codeloc_begin] <-
      paste('```{r}', rtext[codeloc_begin], sep = '\n')
    rtext[codeloc_end] <-
      paste(rtext[codeloc_end], '```', sep = '\n')

    # process the headings
    ## remove the heading marker
    rtext[headerloc] <-
      gsub(pattern = '[#-]{4,}$', '', rtext[headerloc])

    # process the body text
    bodypattern <- ifelse(body == '^#[^[:blank:]#]+',
                          '^#',
                          paste0('^', body))
    rtext[bodyloc] <-
      gsub(pattern = bodypattern, '', rtext[bodyloc])

    # write
    foldername <- basename(path)
    if (is.na(savefilename))
      savefilename <- paste0(foldername, '.md')

    writeLines2(text = rtext, savefilename, backup = backup)
  } else {
    message(paste('The directory', path, 'does not exist!'))
  }
}

#' Convert .md or .Rmd files into a .R script
#'
#' @param path the path of the folder which contains the .Rmd or .md files
#' @param filepattern the pattern of the file names
#' @param savefilename the destinated file name
#' @param backup logical. whether backup the existent file
#' @param heading the indicator of the headings
#' @param body the indicator of the body text
#'
#' @return a .R script
#' @export
#' @examples
#' path <- system.file('examples/md', package = 'mindr')
#' md2r(path = path)
md2r <- function(filepattern = '*.[R]*md$',
                 path = '.',
                 savefilename = NA,
                 backup = TRUE,
                 heading = ' --------',
                 body = '#') {
  if (dir.exists(path)) {
    # read data
    rtext <- NULL
    files <- dir(path, pattern = filepattern, full.names = TRUE)
    for (filename in files)
      rtext <- c(rtext, readLines(filename, encoding = 'UTF-8'))

    # find the indeces of the headings, the body texts, and the code blocks
    headerloc <- get_heading(text = rtext)
    codemarkloc <- grep('^```', rtext)

    codeloc <- NULL
    if (length(codemarkloc) > 0)
      codeloc <-
      get_eqloc(eq_begin = codemarkloc[seq(1, length(codemarkloc), by = 2)],
                eq_end = codemarkloc[seq(2, length(codemarkloc), by = 2)])
    allloc <- 1:length(rtext)
    bodyloc <- allloc[-c(headerloc, codemarkloc, codeloc)]

    # process the headings
    rtext[headerloc] <- paste(rtext[headerloc], heading)

    # process the body text
    rtext[bodyloc] <- paste0(body, rtext[bodyloc])

    # process the codes
    rtext <- rtext[-codemarkloc]

    # write
    foldername <- basename(path)
    if (is.na(savefilename))
      savefilename <- paste0(foldername, '.R')

    writeLines2(text = rtext, savefilename, backup = backup)
  } else {
    message(paste('The directory', path, 'does not exist!'))
  }
}

#' Convert .R scripts into a .Rmd file
#'
#' @param path the path of the folder which contains the .R scripts
#' @param filepattern the pattern of the script file names
#' @param savefilename the destinated file name
#' @param savefile logical. Whether to save the output as a file.
#'
#' @return an R markdown file
#' @export
#' @examples
#' path <- system.file('examples/r', package = 'mindr')
#' r2rmd(path = path)
r2rmd <- function(filepattern = '*.R$',
                  savefile = TRUE,
                  path = '.',
                  savefilename = NA) {
  if (dir.exists(path)) {
    # read data
    rtext <- NULL
    files <- dir(path, pattern = filepattern, full.names = TRUE)
    for (filename in files)
      rtext <- c(rtext, readLines(filename, encoding = 'UTF-8'))

    # find the indeces of the headings, the body texts, and the code blocks
    headerloc <- get_heading2(text = rtext)

    # process the headings
    ## remove the heading marker
    rtext[headerloc] <-
      gsub(pattern = '[#-]{4,}$', '', rtext[headerloc])
    ## consistency with the roxygen style
    rtext[headerloc] <-
      gsub(pattern = '^#=', "#'", rtext[headerloc])

    # write
    if (is.na(savefilename)) {
      foldername <- basename(path)
      savefilename <- paste0(foldername, '.Rmd')
    }
    tempfile <- 'mindr-r2rmd-temp.R'
    writeLines(text = rtext, tempfile)
    knitr::spin(hair = tempfile, knit = FALSE)
    file.rename(paste0(tempfile, 'md'), savefilename)
    file.remove(tempfile)
    md <- readLines(savefilename, encoding = 'UTF-8')
    if (!savefile)
      file.remove(savefilename)
    return(md)
  } else {
    return(message(paste(
      'The directory', path, 'does not exist!'
    )))
  }
}

#' Convert .md or .Rmd files into a .R script
#'
#' @param path the path of the folder which contains the .Rmd or .md files
#' @param filepattern the pattern of the file names
#' @param savefilename the destinated file name
#' @param backup logical. whether backup the existent file
#' @param heading the indicator of the headings
#' @param chunkheading logical. whether treat chunk options as headings (ending with ----)
#' @param savefile logical. Whether to save the output as a file.
#'
#' @return a .R script
#' @export
#' @examples
#' path <- system.file('examples/r', package = 'mindr')
#' rmd2r(path = path)
rmd2r <- function(filepattern = '*.[R]*md$',
                  path = '.',
                  savefile = TRUE,
                  savefilename = NA,
                  backup = TRUE,
                  heading = ' --------',
                  chunkheading = FALSE) {
  if (dir.exists(path)) {
    # read data
    rmdtext <- NULL
    files <- dir(path, pattern = filepattern, full.names = TRUE)
    for (filename in files)
      rmdtext <- c(rmdtext, readLines(filename, encoding = 'UTF-8'))

    # convert the .R scripts into .Rmd with knitr::purl()
    tempfile <- 'mindr-rmd2r-temp.R'
    writeLines(text = rmdtext, tempfile, useBytes = TRUE)
    knitr::purl(tempfile, documentation = 2)
    rtext <- readLines(tempfile, encoding = 'UTF-8')
    file.remove(tempfile)

    # processing the headings
    rtext <-
      gsub(pattern = "^#'( #+ .*)$",
           replacement = paste0("#=\\1", heading),
           rtext)

    # process the inline codes
    rtext <-
      gsub(pattern = "^(#' .*)`r ([^`]*)`", replacement = "\\1{{\\2}}", rtext)

    # process the chunk options
    rtext <-
      gsub(
        pattern = "^## ----(.*[^-]+)([-]+)$",
        replacement = ifelse(chunkheading, "#+ \\1\\2", "#+ \\1"),
        rtext
      )
    rtext <- gsub(pattern = "^## -*$", replacement = "", rtext)
    rtext <- rtext[rtext != '']
    rtext[rtext == "#' "] <- ''

    # write
    foldername <- basename(path)
    if (is.na(savefilename))
      savefilename <- paste0(foldername, '.R')
    if (savefile)
      writeLines2(text = rtext, savefilename, backup = backup)
    return(rtext)
  } else {
    message(paste('The directory', path, 'does not exist!'))
  }
}

#' Convert .R scripts into a .mm file
#'
#' @param path the path of the folder which contains the .R scripts
#' @param filepattern the pattern of the script file names
#' @param savefilename the destinated file name
#' @param title title of the mindmap
#' @param savefile logical. Whether to save the output as a file.
#'
#' @return an mindmap file
#' @export
#' @examples
#' path <- system.file('examples/r', package = 'mindr')
#' r2mm(path = path)
r2mm <- function(filepattern = '*.R$',
                 path = '.',
                 title = NA,
                 savefile = TRUE,
                 savefilename = NA) {
  if (dir.exists(path)) {
    foldername <- basename(path)
    savefilename_rmd <- ifelse(is.na(savefilename),
                               paste0(foldername, '.Rmd'),
                               paste0(savefilename, '.Rmd'))
    r2rmd(filepattern = filepattern,
          path = path,
          savefilename = savefilename_rmd)
    md2mm(
      title = title,
      path = '.',
      pattern = savefilename_rmd,
      remove_curly_bracket = TRUE,
      savefile = savefile,
      savefilename = paste0(savefilename, '.mm'),
      backup = TRUE,
      bookdown_style = TRUE,
      keep_eq = FALSE
    )
    file.remove(savefilename_rmd)
  } else {
    message(paste('The directory', path, 'does not exist!'))
  }
}

#' Convert .mm into a .R script
#'
#' @param path the path of the folder which contains the .Rmd or .md files
#' @param filepattern the pattern of the file names
#' @param savefilename the destinated file name
#' @param backup logical. whether backup the existent file
#' @param heading the indicator of the headings
#' @param savefile logical. Whether to save the output as a file.
#'
#' @return a .R script
#' @export
#' @examples
#' path <- system.file('examples/mm', package = 'mindr')
#' mm2r(path = path)
mm2r <- function(filepattern = '*.mm$',
                 path = '.',
                 savefile = TRUE,
                 savefilename = NA,
                 backup = TRUE,
                 heading = ' --------') {
  if (dir.exists(path)) {
    foldername <- basename(path)
    savefilename_md <- ifelse(is.na(savefilename),
                              paste0(foldername, '.md'),
                              paste0(savefilename, '.md'))
    mm2md(
      pattern = filepattern,
      path = path,
      savefile = TRUE,
      savefilename = savefilename_md,
      backup = TRUE
    )
    newmd <- readLines(savefilename_md, encoding = 'UTF-8')
    newmd <- c('```{r}\n```', newmd)
    writeLines(newmd, savefilename_md, useBytes = TRUE)
    rmd2r(
      filepattern = savefilename_md,
      path = '.',
      savefile = savefile,
      savefilename = paste0(savefilename, '.R'),
      backup = TRUE,
      heading = ' --------'
    )
  } else {
    message(paste('The directory', path, 'does not exist!'))
  }
}

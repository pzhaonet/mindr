#' Convert markdown or rmarkdown files to mindmap files.
#'
#' @param title character. The title of the output file.
#' @param remove_curly_bracket logical. Whether to remove {#ID} in the headers of the markdown file (usually in a 'bookdown' <https://github.com/rstudio/bookdown> project).
#' @param folder character. The folder which contains the input file(s).
#' @param pattern an optional regular expression for filtering the input files. See `help(dir)`.
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backup.
#' @param bookdown_style logical. whether the markdown files are in bookdown style, i.e. index.Rmd at the beginning, `# (PART)`, `# (APPENDIX)` and `# References` as an upper level of normal `#` title
#' @param keep_eq logical. whether to keep LaTeX equations.
#'
#' @return a mindmap file, which can be viewed by common mindmap software, such as 'FreeMind' (<http://freemind.sourceforge.net/wiki/index.php/Main_Page>) and 'XMind' (<http://www.xmind.net>).
#' @export
#' @examples
#' folder <- system.file('examples/md', package = 'mindr')
#' md2mm(folder = folder)
#' md2mm(folder = folder, remove_curly_bracket = TRUE)
md2mm <- function(title = NA,
                  folder = 'md',
                  pattern = '*.[R]*md$',
                  remove_curly_bracket = FALSE,
                  savefilename = NA,
                  backup = TRUE,
                  bookdown_style = TRUE,
                  keep_eq = FALSE) {
  if (dir.exists(folder)) {
    header <- outline(folder = folder,
                      pattern = pattern,
                      remove_curly_bracket = remove_curly_bracket,
                      savefile = FALSE,
                      bookdown_style = bookdown_style,
                      keep_eq = keep_eq)
    foldername <- get_foldername(folder)
    if(is.na(title)) title <- foldername
    if(is.na(savefilename)) savefilename <- paste0(foldername, '.mm')
    mm <- mdtxt2mmtxt(title = title, mmtxt = header)
    writeLines2(text = mm, filename = savefilename, backup = backup)
  } else {message(paste('The directory', folder, 'does not exist!'))}
}


#' Convert a mind map (.mm) into markdown headers.
#'
#' @param folder character. The folder which contains the input file(s).
#' @param savefile logical. Whether to save the output as a markdown file.
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backups.
#' @return a vector of strings showing outline of a markdown document or book.
#' @export
#' @examples
#' folder <- system.file('examples/mm', package = 'mindr')
#' mm2md(folder = folder)
mm2md <- function(folder = 'mm',
                  savefile = TRUE,
                  savefilename = 'mindr.md',
                  backup = TRUE) {
  if (dir.exists(folder)) {
    mm <- NULL
    for (filename in dir(folder, full.names = TRUE)) mm <- c(mm, readLines(filename, encoding = 'UTF-8'))
    # compitable for both versions: node ends with '/>' or '</node>'
    mm <- gsub(pattern = '/>', '</node>', mm)
    # keep links
    loc_link <- grep('LINK="([^\"]*)"', mm)
    links <- gsub('LINK="([^\"]*)"', '\\1', regmatches(mm, gregexpr('LINK="[^\"]*"', mm)))
    for(i in loc_link) mm[i] <- gsub('TEXT="([^"]*)"', paste0('TEXT=\"[\\1](', links[i], ')\"'), mm[i])

    mm <- paste0(mm, collapse = '')
    node_begin <- unlist(gregexpr('<node', mm))
    node_end <- unlist(gregexpr('</node', mm))
    node_sign <- c(rep(1, length(node_begin)), rep(-1, length(node_end)))
    node_loc <- c(node_begin, node_end)
    node_sign <- node_sign[order(node_loc)]
    node_level <- cumsum(node_sign)[node_sign == 1]
    headers <- gsub('TEXT="([^"]*)"', '\\1', regmatches(mm, gregexpr('TEXT="[^"]*"', mm))[[1]])
    md <- paste(sapply(node_level - 1, function(x) paste0(rep('#', x), collapse = '')), headers)
    md[1] <- paste('Title:', md[1])
    if (savefile) {
      writeLines2(text = md, filename = savefilename, backup = backup)
    }
    return(md)
  } else {print(paste('The directory', folder, 'does not exist!'))}
}

#' Extract headers of markdown or rmarkdown files as an outline.
#'
#' @param folder character. The folder which contains the input file(s).
#' @param remove_curly_bracket logical. Whether to remove {#ID} in the headers of the markdown file (usually in a 'bookdown' <https://github.com/rstudio/bookdown> project).
#' @param savefile logical. Whether to save the output as a markdown file.
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backups.
#' @param pattern an optional regular expression for filtering the input files. See `help(dir)`.
#' @param bookdown_style logical. whether the markdown files are in bookdown style, i.e. index.Rmd at the beginning, `# (PART)`, `# (APPENDIX)` and `# References` as an upper level of normal `#` title
#' @param keep_eq logical. whether to keep LaTeX equations.
#'
#' @return a vector of strings showing outline of a markdown document or book.
#' @export
#' @examples
#' folder <- system.file('examples/md', package = 'mindr')
#' outline(folder = folder)
#' outline(folder = folder, remove_curly_bracket = TRUE)
outline <- function(folder = 'md',
                    pattern = '*.[R]*md',
                    remove_curly_bracket = FALSE,
                    savefile = TRUE,
                    savefilename = 'outline.md',
                    backup = TRUE,
                    bookdown_style = TRUE,
                    keep_eq = FALSE) {
  if (dir.exists(folder)) {

    # read data
    md <- NULL
    files <- dir(folder, pattern = pattern, full.names = TRUE)
    ## bookdown style: if index.Rmd exists, read it first
    if(bookdown_style){
      first_file <- paste0(folder, '/index.Rmd')
      if(first_file %in% files) files <- c(first_file, files[files != first_file])
    }
    for (filename in files) md <- c(md, readLines(filename, encoding = 'UTF-8'))
    mdlength <- length(md)

    # exclude the code blocks
    codeloc2 <- grep('^````', md)
    if (length(codeloc2) > 0) md <- md[!sapply(1:mdlength, rmvcode, loc = codeloc2)]
    codeloc <- grep('^```', md)
    if (length(codeloc) > 0) md <- md[!sapply(1:mdlength, rmvcode, loc = codeloc)]

    # get the outline
    headerloc <- get_heading(text = md)

    # remove the curly brackets
    if (remove_curly_bracket) md[headerloc] <- gsub(pattern = '\\{.*\\}', '', md[headerloc])

    # remove the heading marker, which is eight '-' at the end of a heading
    md[headerloc] <- gsub(pattern = ' --------$', '', md[headerloc])

    # extract equations
    if(keep_eq){
      eq_begin <- grep('^\\$\\$', md)
      eq_end <- grep('\\$\\$$', md)
      eq_loc <- get_eqloc(eq_begin, eq_end)
      headerloc <- c(headerloc, eq_loc)
      headerloc <- headerloc[order(headerloc)]
    }
    header <- md[headerloc]

    # lower the levels after `# (PART)` and `# (APPENDIX)`
    if(bookdown_style){
      part_loc <- c(grep('^# \\(PART\\)', header), grep('^# \\(APPENDIX\\)', header), grep('^# References', header))
      if(length(part_loc) > 0) {
        header[part_loc] <- gsub(' \\(PART\\)', '', header[part_loc])
        header[part_loc] <- gsub(' \\(APPENDIX\\)', '', header[part_loc])
        lower_loc <- (part_loc[1] + 1) : length(header)
        lower_loc <- lower_loc[!lower_loc %in% part_loc]
        header[lower_loc] <- paste0('#', header[lower_loc])
      }
    }

    # save file
    if (savefile) {
      writeLines2(text = header, savefilename, backup = backup, useBytes = TRUE)
    }

    return(header)
  } else {print(paste('The directory', folder, 'does not exist!'))}
}

#' Create a markmap widget
#'
#' This function, modified from <https://github.com/seifer08ms/Rmarkmap>, creates a markmap widget using htmlwidgets. The widget can be rendered on HTML pages generated from R Markdown, Shiny,or other applications.
#'
#' @param folder character. The folder which contains the input file(s).
#' @param remove_curly_bracket logical. Whether to remove {#ID} in the headers of the markdown file (usually in a 'bookdown' <https://github.com/rstudio/bookdown> project).
#' @param width the width of the markmap
#' @param height the height of the markmap
#' @param elementId character.
#' @param options the markmap options
#' @param input character, The format of theinput files
#' @param root character. a string displayed as the root of the mind map
#' @param bookdown_style logical. whether the markdown files are in bookdown style, i.e. index.Rmd at the beginning, `# (PART)`, `# (APPENDIX)` and `# References` as an upper level of normal `#` title
#'
#' @import htmlwidgets
#' @return A HTML widget object rendered from a given document.
#' @export
#' @examples
#' folder <- system.file('examples/md', package = 'mindr')
#' markmap(folder = folder)
#' markmap(folder = folder, remove_curly_bracket = TRUE)
markmap <- function(root = NA,
                    input = c('.md', '.mm'),
                    folder = NA,
                    remove_curly_bracket = FALSE,
                    width = NULL,
                    height = NULL,
                    elementId = NULL,
                    options = markmapOption(),
                    bookdown_style = TRUE) {
  input <- match.arg(input)
  if(!is.na(folder) & dir.exists(folder)) {
    if(input == '.md'){
      header <- outline(folder = folder,
                        remove_curly_bracket = remove_curly_bracket,
                        savefile = FALSE,
                        bookdown_style = bookdown_style)
      header <- paste0('#', header)
      header <- c(paste('#', ifelse(is.na(root), folder, root)), header)
    } else if(input == '.mm'){
      header <- mm2md(folder = folder, savefile = FALSE)
    } else {
      message('Please give a valid input.')
    }
    data <- paste(header, collapse = '\n')
    # forward options using x
    x = list(
      data = data,
      options = options
    )

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
#' folder <- system.file('examples/md', package = 'mindr')
#' markmap(folder = folder, remove_curly_bracket = TRUE,
#'   options = markmapOption(preset = 'colorful')) # 'colorful' theme
#' markmap(folder = folder, remove_curly_bracket = TRUE,
#'   options = markmapOption(color = 'category20b',
#'     linkShape = 'bracket')) # 'colorful' theme
#' markmap(folder = folder, remove_curly_bracket = TRUE,
#'   options = markmapOption(color = 'category20b',
#'     linkShape = 'diagonal',
#'     renderer = 'basic')) # 'colorful' theme
markmapOption <- function(preset = NULL, nodeHeight = 20,
                          nodeWidth = 180,
                          spacingVertical = 10,
                          spacingHorizontal = 120,
                          duration = 750,
                          layout = 'tree',
                          color = 'gray',#
                          linkShape = 'diagonal',
                          renderer = 'boxed',...){
  filterNULL <- function (x) {
    if (length(x) == 0 || !is.list(x))
      return(x)
    x[!unlist(lapply(x, is.null))]
  }
  if(!is.null(preset) && (preset == 'default' | preset == 'colorful')){
    filterNULL(list(preset = preset, autoFit = TRUE))
  }else{
    if (is.null(layout) || (layout!='tree')){
      warning('Currenly, only tree layout is supported. Changing to tree layout...')
      layout = 'tree'
    }
    filterNULL(list(nodeHeight = nodeHeight,
                    nodeWidth = nodeWidth,
                    spacingVertical = spacingVertical,
                    spacingHorizontal = spacingHorizontal,
                    duration = duration,
                    layout = 'tree',
                    color = color,
                    linkShape = linkShape,
                    renderer = renderer,
                    autoFit = TRUE,
                    ...))
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
markmapOutput <- function(outputId, width = '100%', height = '400px'){
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
renderMarkmap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
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
mdtxt2mmtxt <- function(title = 'my title', mdtxt = '', keep_eq = FALSE) {
  j <- 1
  mmtxt2 <- mdtxt[j]
  for(i in 2:length(mdtxt)) {
    if(grepl('^#+', mdtxt[i])) {
      j <- j + 1
      mmtxt2[j] <- mdtxt[i]
      } else {
        mmtxt2[j] <- paste(mmtxt2[j], mdtxt[i])
      }
  }

  mmtxt <- gsub(pattern = '&', '&amp;', mmtxt2)
  ncc <- sapply(mmtxt, function(x) nchar(strsplit(x, split = ' ')[[1]][1])) # level of the headings
  mmtext <- substr(mmtxt, ncc + 2, nchar(mmtxt)) # heading text

  # get the hyperlinks
  which_link <- grep(pattern = '\\[.*](.*)', mmtext)
  mmtext[which_link] <- gsub('\\((.*)\\)', '" LINK="\\1', mmtext[which_link])
  mmtext[which_link] <- gsub(pattern = '[][]*', replacement = '', mmtext[which_link])
  mm <- rep('', length(mmtxt) + 3)
  mm[1] <- '<map version="1.0.1">'

  mm[2] <- paste0('<node TEXT="', title, '">', paste0(rep('<node TEXT="">', ncc[1] - 1), collapse = ''))
  diffncc <- diff(ncc)
  for (i in 1:(length(mmtxt) - 1)) {
    if (diffncc[i] == 1) mm[i+2] <- paste0('<node TEXT="', mmtext[i], '">')
    if (diffncc[i] == 0) mm[i+2] <- paste0('<node TEXT="', mmtext[i], '"></node>')
    if (diffncc[i] < 0) mm[i+2] <- paste0('<node TEXT="', mmtext[i], '">', paste0(rep('</node>', -diffncc[i] + 1), collapse = ''))
    mm[i+2] <- gsub('(\\$\\$[^$]+\\$\\$)(">)(</node>)$', '\\2\\1\\3', mm[i + 2])
    mm[i+2] <- gsub('\\$\\$([^$]+)\\$\\$', '<hook EQUATION="\\1" NAME="plugins/latex/LatexNodeHook.properties"/>', mm[i + 2])
  }
  mm[length(mmtxt) + 2] <- paste0('<node TEXT="', mmtext[length(mmtxt)], '">', paste0(rep('</node>', ncc[length(mmtxt)]), collapse = ''))
  mm[length(mmtxt) + 3] <- '</node></map>'
  return(mm)
}

#' Convert a directory tree to a mindmap file.
#'
#' @param tree character. The directory tree.
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backup.
#' @param n_root numeric. Which element is the root of the tree.
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
                    savefilename = 'mindr',
                    backup = TRUE,
                    n_root = 1) {
  tree_title <- gsub('/', '', tree[n_root])
  tree_node <- sapply(strsplit(tree[-n_root], '/'), function(x) x[length(x)])
  tree_pre <- strrep('#', sapply(gregexpr('/', tree[-n_root]), length))
  tree_new <- paste(tree_pre, tree_node)
  mm <- mdtxt2mmtxt(title = tree_title, mmtxt = tree_new)
  savefilename <- paste0(savefilename, ifelse(backup & file.exists(paste0(savefilename, '.mm')), paste0('-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S')), ''), '.mm')
  writeLines(text = mm, savefilename, useBytes = TRUE)
}


#' Convert a folder structure into a mindmap.
#'
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backup.
#' @param path character. the path of the folder.
#' @param output a file with the folder structure.
#'
#' @return a mindmap file, which can be viewed by common mindmap software, such as 'FreeMind' (<http://freemind.sourceforge.net/wiki/index.php/Main_Page>) and 'XMind' (<http://www.xmind.net>).
#' @export
#' @examples
#' dir2(NA)
dir2 <- function(path = getwd(),
                 savefilename = 'mindr',
                 output = c('mm', 'txt', 'md'),
                 backup = TRUE) {
  output <- match.arg(output)
  if(is.na(path)) return(message('The path cannot be NA!'))
  if (dir.exists(path)) {
    if_files = FALSE
    tree <- paste0('tree "', path, '" /A', ifelse(if_files, ' /f', ''))
    mytree <- system(tree, intern = T)
    if('txt' %in% output) {
      if (backup & file.exists(paste0(savefilename, '.txt'))){
        message(paste0(savefilename, '.txt already exits.'))
        savefilename <- paste0(savefilename, '-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S'))
      }
      writeLines(mytree, paste0(savefilename, '.txt'), useBytes = TRUE)
      message(paste(savefilename), '.txt is generated!')
    }
    md <- mytree[-(1:3)]
    # headers <- sapply(mytree, function(x) strsplit(x, ifelse(grepl('+---', x), '+---', '\\---'))[[1]][2])
    # attributes(headers) <- NULL
    md <- gsub(pattern = '\\+---', '# ', md)
    md <- gsub(pattern = '\\\\---', '# ', md)
    md <- gsub(pattern = '\\|   ', '#', md)
    md <- gsub(pattern = '    ', '#', md)
    mm <- mdtxt2mmtxt(title = path, mmtxt = md)
    if('md' %in% output){
      writeLines2(text = md,
                  filename = paste0(savefilename, 'md'),
                  backup = backup,
                  useBytes = TRUE)
    }
    if('mm' %in% output){
      writeLines2(text = md,
                  filename = paste0(savefilename, 'mm'),
                  backup = backup,
                  useBytes = TRUE)
    }
  } else {message(paste('The directory', path, 'does not exist!'))}
}

#' Convert .R scripts into a .md/.Rmd file
#'
#' @param path the path of the folder which contains the .R scripts
#' @param filepattern the pattern of the script file names
#' @param savefilename the destinated file name
#' @param backup logical. whether backup the existent file
#' @param heading the indicator of the headings
#' @param body the indicator of the body text
#'
#' @return a markdown file
#' @export
#' @examples r2md()
r2md <- function(path = '.',
                 filepattern = '*.R$',
                 savefilename = NA,
                 backup = TRUE,
                 heading = ' --------$',
                 body = '^#[^[:blank:]#]+'
) {
  if (dir.exists(path)) {
    # read data
    rtext <- NULL
    files <- dir(path, pattern = filepattern, full.names = TRUE)
    for (filename in files) rtext <- c(rtext, readLines(filename, encoding = 'UTF-8'))

    # find the indeces of the headings, the body texts, and the code blocks
    headerloc <- get_heading(text = rtext)
    bodyloc <- get_body(pattern = body, text = rtext)
    codeloc <- grep(pattern = '^[^#]', x = rtext)

    # process the code blocks
    codeloc_diff <- diff(codeloc)
    codeloc_begin <- codeloc[c(1, which(codeloc_diff > 1) + 1)]
    codeloc_end <- codeloc[c(which(codeloc_diff > 1), length(codeloc))]
    rtext[codeloc_begin] <- paste('```{r}', rtext[codeloc_begin], sep = '\n')
    rtext[codeloc_end] <- paste(rtext[codeloc_end], '```', sep = '\n')

    # process the headings
    ## remove the heading marker
    rtext[headerloc] <- gsub(pattern = heading, '', rtext[headerloc])

    # process the body text
    rtext[bodyloc] <- gsub(pattern = '^#', '', rtext[bodyloc])

    # write
    foldername <- get_foldername(path)
    if(is.na(savefilename)) savefilename <- paste0(foldername, '.md')

    writeLines2(text = rtext, savefilename, backup = backup)
  } else {message(paste('The directory', folder, 'does not exist!'))}
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
#' @examples md2r()
md2r <- function(path = '.',
                 filepattern = '*.[R]*md$',
                 savefilename = NA,
                 backup = TRUE,
                 heading = ' --------',
                 body = '#'
) {
  if (dir.exists(path)) {
    # read data
    rtext <- NULL
    files <- dir(path, pattern = filepattern, full.names = TRUE)
    for (filename in files) rtext <- c(rtext, readLines(filename, encoding = 'UTF-8'))

    # find the indeces of the headings, the body texts, and the code blocks
    headerloc <- get_heading(text = rtext)
    codemarkloc <- grep('^```', rtext)
    codeloc <- get_eqloc(eq_begin = codemarkloc[seq(1, length(codemarkloc), by = 2)],
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
    foldername <- get_foldername(path)
    if(is.na(savefilename)) savefilename <- paste0(foldername, '.R')

    writeLines2(text = rtext, savefilename, backup = backup)
  } else {message(paste('The directory', folder, 'does not exist!'))}
}

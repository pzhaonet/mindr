#' Extract headings of (R) Markdown-syntax text as an outline
#' @inheritParams mm
#' @return Character, showing the outline.
#' @export
#' @examples
#' input <- system.file('examples/mindr-md.Rmd', package = 'mindr')
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' outline(input_txt)
#' outline(input_txt, md_list = TRUE, md_bookdown = TRUE)
#' outline(input_txt, md_list = TRUE, md_bookdown = TRUE, md_maxlevel = 2)
outline <- function(from,
                    md_list = FALSE,
                    md_eq = FALSE,
                    md_braces = FALSE,
                    md_bookdown = FALSE,
                    md_maxlevel = '') {
  mdlength <- length(from)

  # exclude the code blocks
  codeloc4backsticks <- grep('^````', from)
  if (length(codeloc4backsticks) > 0){
    from <- from[!sapply(1:mdlength, function(x) rmvcode(index = x, loc = codeloc4backsticks))]
  }
  codeloc3backsticks <- grep('^```', from)
  if (length(codeloc3backsticks) > 0){
    from <- from[!sapply(1:mdlength, function(x) rmvcode(index = x, loc = codeloc3backsticks))]
  }

  # convert list to heading
  if (md_list){
    from <- list2heading(from)
  }

  # get the heading locations
  headingloc <- grep(paste0('^#{1,', md_maxlevel, '} '), from)

  # remove the curly brackets
  if (!md_braces){
    from[headingloc] <- gsub(pattern = '\\{.*\\}', '', from[headingloc])
  }

  # remove the heading marker, which are multiple '-' or '#' at the end of a heading
  from[headingloc] <- gsub(pattern = '[-#]{2,}[:blank:]*$', '', from[headingloc])

  # include between-line equations
  eq_loc <- NA
  if (md_eq && any(grepl('^\\$\\$', from))) {
    eq_begin <- grep('^\\$\\$', from)
    eq_end <- grep('\\$\\$$', from)
    eq_loc <- get_eqloc(eq_begin, eq_end)
  }

  heading <- from[headingloc]

  # bookdown style: lower the levels after `# (PART)` and `# (APPENDIX)`
  if (md_bookdown) {
    part_loc <- c(grep('^# \\(PART\\)', heading),
                  grep('^# \\(APPENDIX\\)', heading),
                  grep('^# References', heading)
    )
    if (length(part_loc) > 0) {
      heading[part_loc] <- gsub(' \\(PART\\)', '', heading[part_loc])
      heading[part_loc] <- gsub(' \\(APPENDIX\\)', '', heading[part_loc])
      lower_loc <- (part_loc[1] + 1):length(heading)
      lower_loc <- lower_loc[!lower_loc %in% part_loc]
      heading[lower_loc] <- paste0('#', heading[lower_loc])
    }
  }
  if (!is.na(eq_loc[1])){
    heading <- c(heading, from[eq_loc])[order(c(headingloc, eq_loc))]
  }
  return(heading)
}

#' Convert (R) Markdown-syntax text to FreeMind mind map code
#' @inheritParams mm
#' @return FreeMind mind map code, which can be saved as a .mm file and viewed by common mind map software, such as \href{http://freemind.sourceforge.net/wiki/index.php/Main_Page}{FreeMind} and \href{https://www.xmind.net}{XMind}.
#' @export
#' @examples
#' input <- system.file('examples/mindr-md.Rmd', package = 'mindr')
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' output_txt <- md2mm(input_txt)
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".mm")
#' writeLines(output_txt, output, useBytes = TRUE)
#' # file.show(input) # Open the input file
#' # file.show(output) # Open the output file
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
md2mm <- function(from = NA,
                  root = 'mindr',
                  md_list = FALSE,
                  md_braces = FALSE,
                  md_bookdown = FALSE,
                  md_eq = FALSE,
                  md_maxlevel = '') {
  md <- outline(from = from,
                md_list = md_list,
                md_eq = md_eq,
                md_braces = md_braces,
                md_bookdown = md_bookdown,
                md_maxlevel = md_maxlevel)
  mm <- mdtxt2mmtxt(from = md, root = root, md_eq = md_eq)
  return(mm)
}

#' Convert FreeMind mind map code into Markdown headings
#' @inheritParams mm
#' @return Character, showing outline in Markdown syntax.
#' @export
#' @examples
#' input <- system.file('examples/mindr-mm.mm', package = 'mindr')
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' output_txt <- mm2md(input_txt)
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".md")
#' writeLines(output_txt, output, useBytes = TRUE)
#' # file.show(input) # Open the input file
#' # file.show(output) # Open the output file
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
mm2md <- function(from = NA) {
  # compatible for both versions: node ends with '/>' or '</node>'
  mm <- gsub('/>', '</node>', from)

  # preserve links
  loc_link <- grep('LINK="([^\"]*)"', mm)
  links <- gsub('LINK="([^\"]*)"', '\\1', regmatches(mm, gregexpr('LINK="[^\"]*"', mm)))
  for (i in loc_link) mm[i] <- gsub('TEXT="([^"]*)"', paste0('TEXT=\"[\\1](', links[i], ')\"'), mm[i])

  # build the code for md file
  mm <- paste0(mm, collapse = '')
  node_begin <- unlist(gregexpr('<node', mm))
  node_end <- unlist(gregexpr('</node', mm))

  node_sign <- c(rep(1, length(node_begin)), rep(-1, length(node_end)))
  node_loc <- c(node_begin, node_end)

  node_loc <- node_loc[-c(1, length(node_loc))]
  node_sign <- node_sign[-c(1, length(node_sign))]

  node_sign <- node_sign[order(node_loc)]
  node_level <- cumsum(node_sign)[node_sign == 1]

  headers <- gsub('TEXT="([^"]*)"', '\\1', regmatches(mm, gregexpr('TEXT="[^"]*"', mm))[[1]])
  md <- paste(sapply(node_level, function(x) paste0(rep('#', x), collapse = '')), headers[-1])
  md <- c(paste('Title:', headers[1]), md)
  md <- gsub('&amp;', '&', md)
  md <- gsub('&quot;', '"', md)
  return(md)
}

#' Convert R code into (R) Markdown-syntax text
#' @inheritParams mm
#' @return R markdown-syntax text.
#' @importFrom knitr spin
#' @export
#' @examples
#' input <- system.file('examples/mindr-r.R', package = 'mindr')
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' output_txt <- r2md(from = input_txt)
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".Rmd")
#' writeLines(output_txt, output, useBytes = TRUE)
#' # file.show(input) # Open the input file
#' # file.show(output) # Open the output file
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
r2md <- function(from = NA) {
  # process the headings
  ## find the location of the headings
  headerloc <- grep('^#= #+ ', from)
  ## remove the heading ending "####" "----"
  from[headerloc] <- gsub(pattern = '[#-]{4,}$', '', from[headerloc])
  ## consistency with the roxygen style
  from[headerloc] <- gsub('^#=', "#'", from[headerloc])

  # write to a file for knitr::spin()
  rfile_temp <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".R")
  writeLines(text = from, rfile_temp, useBytes = TRUE)

  # convert
  knitr::spin(hair = rfile_temp, knit = FALSE)
  file.remove(rfile_temp)
  md <- readLines(paste0(rfile_temp, 'md'), encoding = 'UTF-8')

  return(md)
}

#' Convert (R) Markdown-syntax text into R code
#' @inheritParams mm
#' @return Character, R code.
#' @importFrom knitr purl
#' @export
#' @examples
#' input <- system.file('examples/mindr-md.Rmd', package = 'mindr')
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' output_txt <- md2r(from = input_txt)
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".R")
#' writeLines(output_txt, output, useBytes = TRUE)
#' # file.show(input) # Open the input file
#' # file.show(output) # Open the output file
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
md2r <- function(from = NA, r_seclabel = ' --------', r_chunkheading = FALSE) {
  # prepare a temp file for knitr::purl()
  rmdfile_temp <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".Rmd")
  writeLines(from, rmdfile_temp, useBytes = TRUE)

  # convert .Rmd into .R with knitr::purl()
  rfile_temp <- gsub('.Rmd$', '.R', rmdfile_temp)
  knitr::purl(rmdfile_temp, output = rfile_temp, documentation = 2, quiet = TRUE)

  # process .R
  rtext <- readLines(rfile_temp, encoding = 'UTF-8')
  file.remove(c(rfile_temp, rmdfile_temp))

  # rtext <- knitr::purl(text = from, documentation = 2) # not working if there is no R code in markdown text

  ## processing the headings
  rtext <- gsub(pattern = "^#'( #+ .*)$", replacement = paste0("#=\\1", r_seclabel),rtext)

  # process the inline codes
  rtext <- gsub("^(#' .*)`r ([^`]*)`", "\\1{{\\2}}", rtext)

  # process the chunk options
  rtext <- gsub("^## ----(.*[^-]+)([-]+)$", ifelse(r_chunkheading, "#+ \\1\\2", "#+ \\1"), rtext)
  rtext <- gsub(pattern = "^## -*$", replacement = "", rtext)
  rtext <- rtext[rtext != '']
  rtext[rtext == "#' "] <- ''

  return(rtext)
}

#' Display a directory hierarchical structure in Markdown syntax
#' @inheritParams mm
#' @return Character, in Markdown syntax.
#' @export
#' @examples
#' input <- system.file(package = 'mindr')
#' dir2md(input)
#' dir2md(input, dir_files = FALSE, dir_all = TRUE, dir_excluded = 'Meta')
#' output_txt <- dir2md(input)
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".Rmd")
#' writeLines(output_txt, output, useBytes = TRUE)
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.show(output) # Open the output file
#' # file.remove(output) # remove the output file
dir2md <- function(from = '.', dir_files = TRUE, dir_all = TRUE, dir_excluded = NA) {
  tree <- if (dir_files) {
    list.files(from, all.files = dir_all, recursive = TRUE, include.dirs = TRUE)
  } else {
    list.dirs(from, full.names = FALSE)[-1]
  }

  if (!is.na(dir_excluded[1])) tree <- tree[!grepl(paste(paste0('^', dir_excluded), collapse = '|'), tree)]

  tree_node <- basename(tree)
  tree_level <- sapply(tree, function(x) length(gregexpr('/', x)[[1]])) + 1
  tree_level[!grepl('/', tree)] <- 1
  tree_pre <- strrep('#', tree_level)
  md <- paste(tree_pre, tree_node)

  return(md)
}

#' Create hierarchical directories according to (R) Markdown-syntax text
#' @inheritParams mm
#' @return Directories generated.
#' @export
#' @examples
#' output <- file.path(tempdir(), 'mindr')
#' md2dir(c('# a', '## a1', '## a2'), output)
#' message('Output: ', output)
#' # unlink(output, recursive = TRUE) # remove the output file
#'
#' input <- system.file('examples/mindr-md.Rmd', package = 'mindr')
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' output <- file.path(tempdir(), 'mindr')
#' md2dir(from = input_txt, output)
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.show(input) # Open the input file
#' # unlink(output, recursive = TRUE) # remove the output file
#'
#' md2dir(from = input_txt, output, md_list = TRUE)
md2dir <- function(from = NA, dir_to = 'mindr', md_list = FALSE, md_bookdown = TRUE, dir_quiet = FALSE){
  if (is.na(dir_to)) dir_to <- 'mindr'
  if (dir.exists(dir_to)) return(warning(dir_to, ' already exists! Please use another folder name for "dir_to =" and try again.'))
  md <- outline(from = from,
                md_list = md_list,
                md_eq = FALSE,
                md_braces = FALSE,
                md_bookdown = md_bookdown)
  md <- gsub('\\[(.+)\\]\\(.+\\)', '\\1', md)
  titles <- gsub('^#+ (.*[^ ]+) *$', '\\1', x = md)
  oldheadings <- gsub('^(#+) .+', '\\1', x = md)
  oldheadings[!grepl('^(#+) .+', x = md)] <- NA
  heading_level <- nchar(oldheadings)
  level_change <- diff(heading_level)
  n <- length(md)
  dirs <- character(length = n)
  dirs[1] <- titles[1]
  for (i in 2:n) {
    if (level_change[i - 1] == 0) dirs[i] <- file.path(dirname(dirs[i-1]), titles[i])
    if (level_change[i - 1] == 1) dirs[i] <- file.path(dirs[i-1], titles[i])
    if (level_change[i - 1] < 0) {
      dirs[i] <- dirname(dirs[i-1])
      for (j in 1:abs(level_change[i - 1])) dirs[i] <- dirname(dirs[i])
      dirs[i] <- ifelse(dirs[i] == '.', titles[i], file.path(dirs[i], titles[i]))
    }
  }
  dirs <- c(dir_to, file.path(dir_to, dirs))
  for (i in dirs) {
    dir.create(i)
    if (!dir_quiet) message('Generated directory: ', i)
    }
}

#' Create a mind map in HTML widget
#' @inheritParams mm
#' @return HTML widget object.
#' @importFrom htmlwidgets createWidget sizingPolicy
#' @export
#' @details This function, adapted from the \href{https://github.com/seifer08ms/Rmarkmap}{Rmarkup} package, creates a markmap widget using htmlwidgets. The widget can be rendered on HTML pages generated from R Markdown, Shiny,or other applications.
#' @examples
#' # Display Markdown:
#' input <- system.file('examples/mindr-md.Rmd', package = 'mindr')
#' # file.show(input)
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' markmap(input_txt)
#' markmap(input_txt, root = basename(input),
#'         md_list = TRUE, md_eq = FALSE, md_braces = FALSE, md_bookdown = TRUE)
#'
#' # Display Mind Map:
#' input <- system.file('examples/mindr-mm.mm', package = 'mindr')
#' # file.show(input)
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' markmap(input_txt)
#' markmap(input_txt, root = basename(input))
#'
#' # Display R script:
#' input <- system.file('examples/mindr-r.R', package = 'mindr')
#' # file.show(input)
#' from <- input_txt <- readLines(input, encoding = 'UTF-8')
#' markmap(input_txt)
#' markmap(input_txt, root = basename(input),
#'         md_list = TRUE, md_eq = FALSE, md_braces = FALSE, md_bookdown = TRUE)
#'
#' # Display directory:
#' input <- system.file(package = 'mindr')
#' markmap(input)
#' markmap(input, root = 'The mindr package',
#'         dir_files = FALSE, dir_excluded = c('Meta','htmlwidgets/lib'),
#'         widget_elementId = "mindr-dir")
markmap <- function(from = '.', root = NA, input_type = c('auto', 'markdown', 'mindmap', 'R', 'dir'),
                    md_list = FALSE, md_eq = FALSE, md_braces = FALSE, md_bookdown = FALSE, md_maxlevel = '', # markdown options
                    dir_files = TRUE, dir_all = TRUE, dir_excluded = NA, # dir options
                    widget_name = NA, widget_width = NULL, widget_height = NULL, widget_elementId = NULL, widget_options = markmapOption(preset = 'colorful') # widget options
                    ) {
  input_type <- match.arg(input_type, c('auto', 'markdown', 'mindmap', 'R', 'dir'))
  if (input_type == 'auto') input_type <- guess_type(from)
  if (input_type == 'mindmap') {
    md <- mm2md(from = from)
  } else{
    if (input_type == 'markdown') md <- outline(from = from, md_list = md_list, md_eq = md_eq, md_braces = md_braces, md_bookdown =  md_bookdown, md_maxlevel = md_maxlevel)
    if (input_type == 'R') md <- outline(r2md(from = from),   md_list = md_list, md_eq = md_eq, md_braces = md_braces, md_bookdown =  md_bookdown, md_maxlevel = md_maxlevel)
    if (input_type == 'dir') {
      md <- outline(dir2md(from = from, dir_files = dir_files, dir_all = dir_all, dir_excluded = dir_excluded),
                    md_maxlevel = md_maxlevel)
      if (is.na(root)) root <- basename(from)
      }
    }

  md <- c(paste('#', ifelse(is.na(root), 'root', root)), paste0('#', md))
  data <- paste(md, collapse = '\n')
  # forward options using x
  x <- list(data = data, options = widget_options)

  # create widget
  htmlwidgets::createWidget(
    name = 'markmap',
    x,
    width = widget_width,
    height = widget_height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = '100%',
      defaultHeight = 400,
      padding = 0,
      browser.fill = TRUE
    ),
    package = 'mindr',
    elementId = widget_elementId
  )
}

#' Theme options for markmap creation
#'
#' @details This function is adapted from the \href{https://github.com/seifer08ms/Rmarkmap}{Rmarkup} package.
#'
#' Currently, markmap have 'default' and 'colorful' themes. colorful' themes have three different parameters from default themes: {nodeHeight: 10, renderer: 'basic',color: 'category20'}
#' @param preset the name of built-in theme for markmap. If present, any other parameters will be ignored.
#' @param nodeHeight the height of nodes in the markmap.
#' @param nodeWidth the width of nodes in the markmap.
#' @param spacingVertical space of vertical.
#' @param spacingHorizontal space of horizontal.
#' @param duration duration time for animation.
#' @param layout layout mode of makrmap. Currently, only 'tree' is accepted.
#' @param color color of markmap. A character color value ,either 'gray' or a categorical colors including 'category10','category20','category20b' and 'category20c'.
#' @param linkShape link shape of markmap. A character value, either 'diagonal' or 'bracket'.
#' @param renderer rendered shaped of markmap. A character value ,either 'basic' or 'boxed'.
#' @param ... other options.
#' @seealso \url{https://github.com/seifer08ms/Rmarkmap} and \url{https://github.com/dundalek/markmap/blob/master/lib/view.mindmap.js} for details.
#' @export
#' @examples
#' input <- system.file('examples/mindr-md.Rmd', package = 'mindr')
#' # file.show(input)
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' markmap(input_txt)
#' markmap(input_txt, widget_options = markmapOption(preset = 'default'))
#' markmap(input_txt, widget_options = markmapOption(color = 'category20b', linkShape = 'bracket'))
#' markmap(input_txt, widget_options = markmapOption(color = 'category10', linkShape = 'diagonal', renderer = 'basic'))
#' markmap(input_txt, widget_options = markmapOption(nodeHeight = 30, nodeWidth = 100, spacingHorizontal = 60))
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

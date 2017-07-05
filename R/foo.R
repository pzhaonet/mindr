#' Convert markdown or rmarkdown files to mindmap files.
#'
#' @param title character. The title of the output file.
#' @param remove_curly_bracket logical. Whether to remove {#ID} in the headers of the markdown file (usually in a 'bookdown' <https://github.com/rstudio/bookdown> project).
#' @param folder character. The folder which contains the input file(s).
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backup.
#'
#' @return a mindmap file, which can be viewed by common mindmap software, such as 'FreeMind' (<http://freemind.sourceforge.net/wiki/index.php/Main_Page>) and 'XMind' (<http://www.xmind.net>).
#' @export
#' @examples
#' folder <- system.file('examples/md', package = 'mindr')
#' md2mm(folder = folder)
#' md2mm(folder = folder, remove_curly_bracket = TRUE)
md2mm <- function(title = 'my title',
                  folder = 'mm',
                  remove_curly_bracket = FALSE,
                  savefilename = 'mindr',
                  backup = TRUE) {
  if (dir.exists(folder)) {
    header <- outline(folder, remove_curly_bracket, savefile = FALSE)
    ncc <- sapply(header, function(x) nchar(strsplit(x, split = ' ')[[1]][1]))
    mmtext <- substr(header, ncc + 2, nchar(header))
    mm <- '<map version="1.0.1">'
    mm[2] <- paste0('<node TEXT="', title, '">', paste0(rep('<node TEXT="">', ncc[1] - 1), collapse = ''))
    diffncc <- diff(ncc)
    for (i in 1:length(diffncc)) {
      if (diffncc[i] == 1) mm[i+2] <- paste0('<node TEXT="', mmtext[i], '">')
      if (diffncc[i] == 0) mm[i+2] <- paste0('<node TEXT="', mmtext[i], '"></node>')
      if (diffncc[i] < 0) mm[i+2] <- paste0('<node TEXT="', mmtext[i], '">', paste0(rep('</node>', -diffncc[i] + 1), collapse = ''))
    }
    mm[length(ncc) + 2] <- paste0('<node TEXT="', mmtext[length(ncc)], '">', paste0(rep('</node>', ncc[length(ncc)]), collapse = ''))
    mm[length(ncc) + 3] <- '</node></map>'
    savefilename <- paste0(savefilename, ifelse(backup & file.exists(paste0(savefilename, '.mm')), paste0('-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S')), ''), '.mm')
    # if (backup & file.exists(paste0(savefilename, '.mm'))){ #file.copy(savefilename, to = paste0(savefilename, 'backup'))
    #   savefilename <- paste0(savefilename, '-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S'))
    # }
    writeLines(text = mm, savefilename, useBytes = TRUE)
  } else {print(paste('The directory', folder, 'does not exist!'))}
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
                  savefilename = 'mindr',
                  backup = TRUE) {
  if (dir.exists(folder)) {
    mm <- NULL
    for (filename in dir(folder, full.names = TRUE)) mm <- c(mm, readLines(filename, encoding = 'UTF-8'))
    mm <- paste0(mm, collapse = '')
    node_begin <- unlist(gregexpr('<node', mm))
    node_end <- unlist(gregexpr('</node', mm))
    node_sign <- c(rep(1, length(node_begin)), rep(-1, length(node_end)))
    node_loc <- c(node_begin, node_end)
    node_sign <- node_sign[order(node_loc)]
    node_level <- cumsum(node_sign)[node_sign == 1]
    headers <- gsub('TEXT="([^>]*)">', '\\1', regmatches(mm, gregexpr('TEXT="[^>]*">', mm))[[1]])
    md <- paste(sapply(node_level - 1, function(x) paste0(rep('#', x), collapse = '')), headers)
    md[1] <- paste('Title:', md[1])
    if (savefile) {
      savefilename <- paste0(savefilename, ifelse(backup & file.exists(paste0(savefilename, '.md')), paste0('-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S')), ''), '.md')
      # if (backup & file.exists(paste0(savefilename, '.md'))){ #file.copy(savefilename, to = paste0(savefilename, 'backup'))
      #   savefilename <- paste0(savefilename, '-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S'))
      # }
      # if (backup & file.exists(savefilename)) file.copy(savefilename, to = paste0(savefilename, 'backup'))
      writeLines(text = md, savefilename, useBytes = TRUE)
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
#'
#' @return a vector of strings showing outline of a markdown document or book.
#' @export
#' @examples
#' folder <- system.file('examples/md', package = 'mindr')
#' outline(folder = folder)
#' outline(folder = folder, remove_curly_bracket = TRUE)
outline <- function(folder = 'mm',
                    remove_curly_bracket = FALSE,
                    savefile = TRUE,
                    savefilename = 'outline',
                    backup = TRUE) {
  if (dir.exists(folder)) {
    md <- NULL
    for (filename in dir(folder, full.names = TRUE)) md <- c(md, readLines(filename, encoding = 'UTF-8'))
    headerloc <- grep('^#+', md)
    codeloc <- grep('^```', md)
    rmvcode <- function(x) sum(x > codeloc[seq(1, length(codeloc), by = 2)] & x < codeloc[seq(2, length(codeloc), by = 2)])
    # exclude the lines begining with # but in code
    if (length(codeloc) > 0) headerloc <- headerloc[!sapply(headerloc, rmvcode) == 1]
    header <- md[headerloc]
    if (remove_curly_bracket) header <- gsub(pattern = '\\{.*\\}', '', header)
    if (savefile) {
      savefilename <- paste0(savefilename, ifelse(backup & file.exists(paste0(savefilename, '.md')), paste0('-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S')), ''), '.md')
      # if (backup & file.exists(paste0(savefilename, '.md'))){ #file.copy(savefilename, to = paste0(savefilename, 'backup'))
      #   savefilename <- paste0(savefilename, '-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S'))
      # }
      # if (backup & file.exists(savefilename)) file.copy(savefilename, to = paste0(savefilename, 'backup'))
      writeLines(text = header, savefilename, useBytes = TRUE)
    }
    return(header)
  } else {print(paste('The directory', folder, 'does not exist!'))}
}

####################################################

#' Create a markmap widget
#'
#' This function creates a markmap widget using htmlwidgets.
#' The widget can be rendered on HTML pages generated from
#' R Markdown,Shiny,or other applications.
#' @param path the path of markdown file
#' @param width the width of the markmap
#' @param height the height of the markmap
#' @param options the markmap options
#' @import htmlwidgets
#' @return A HTML widget object rendered from a given document.
#' @examples
#' folder <- system.file('examples/md', package = 'mindr')
#' markmap(folder = folder)
#' markmap(folder = folder, remove_curly_bracket = TRUE)
#' @export
markmap <- function(folder = 'mm',
                    remove_curly_bracket = FALSE,
                    width = NULL, height = NULL, elementId = NULL, options = markmapOption()) {
  if (dir.exists(folder)) {
    header <- outline(folder, remove_curly_bracket, savefile = FALSE)
    data <-paste(header, collapse = '\n')
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
  } else {print(paste('The directory', folder, 'does not exist!'))}

}
#' Options for markmap creation
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
#' @seealso \url{https://github.com/dundalek/markmap/blob/master/view.mindmap.js} for details.
#' @examples
#' md<-system.file('examples/test.md',package = 'Rmarkmap')
#' markmap(md,options = markmapOption(preset = 'colorful')) # 'colorful' theme
#'
#' # more options for self-defined markmap
#' markmap(md,options = markmapOption(color='category20b',linkShape='bracket'))
#'
#' markmap(md,options = markmapOption(color='category20b',linkShape='diagonal',renderer = 'basic'))
#' @export
markmapOption <- function(preset=NULL,nodeHeight=20,
                          nodeWidth=180,
                          spacingVertical=10,
                          spacingHorizontal=120,
                          duration=750,
                          layout='tree',
                          color='gray',#
                          linkShape='diagonal',
                          renderer='boxed',...){
  filterNULL<-function (x) {
    if (length(x) == 0 || !is.list(x))
      return(x)
    x[!unlist(lapply(x, is.null))]
  }
  if(!is.null(preset)&&(preset=='default'|preset=='colorful')){
    filterNULL(list(preset=preset,autoFit=TRUE))
  }else{
    if (is.null(layout)||(layout!='tree')){
      warning('Currenly, only tree layout is supported. Changing to tree layout...')
      layout = 'tree'
    }
    filterNULL(list(nodeHeight=nodeHeight,
                    nodeWidth=nodeWidth,
                    spacingVertical=spacingVertical,
                    spacingHorizontal=spacingHorizontal,
                    duration=duration,
                    layout='tree',
                    color=color,
                    linkShape=linkShape,
                    renderer=renderer,
                    autoFit=TRUE,
                    ...))
  }

}
#'
#'
#' Shiny bindings for markmap
#'
#' Output and render functions for using markmap within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a markmap
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' #'
#' @rdname markmap-shiny
#'
#' @export
markmapOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'markmap', width, height, package = 'mindr')
}
#' @rdname markmap-shiny
#' @export
renderMarkmap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, markmapOutput, env, quoted = TRUE)
}

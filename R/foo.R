#' Convert markdown or rmarkdown files to mindmap files.
#'
#' @param title character. The title of the output file.
#' @param remove_curly_bracket logical.
#' @param folder character. The folder which contains the input file(s).
#'
#' @return a mindmap file, which can be viewed by common mindmap software, such as FreeMind or Xmind.
#' @export
#'
#' @examples
#' mindr()
mindr <- function(title = 'my title',
                  folder = 'mm',
                  remove_curly_bracket = FALSE) {
  header <- outline(folder, remove_curly_bracket, inputformat)
  ncc <- sapply(header, function(x) nchar(strsplit(x, split = ' ')[[1]][1]))
  mmtext <- substr(header, ncc + 2, nchar(header))
  mm <- '<map version="1.0.1">'
  mm[2] <- paste0('<node TEXT="', title, '">')
  diffncc <- diff(ncc)
  for (i in 1:length(diffncc)) {
    if (diffncc[i] == 1) mm[i+2] <- paste0('<node TEXT="', mmtext[i], '">')
    if (diffncc[i] == 0) mm[i+2] <- paste0('<node TEXT="', mmtext[i], '"></node>')
    if (diffncc[i] < 0) mm[i+2] <- paste0('<node TEXT="', mmtext[i], '">', paste0(rep('</node>', -diffncc[i] + 1), collapse = ''))
  }
  mm[length(ncc) + 2] <- paste0('<node TEXT="', mmtext[length(ncc)], '">', paste0(rep('</node>', ncc[length(ncc)]), collapse = ''))
  mm[length(ncc) + 3] <- '</node></map>'
  writeLines(text = mm, 'mm.mm', useBytes = TRUE)
}

#' Extract headers of markdown or rmarkdown files as an outline, or convert a mind map (.mm) into a markdown outline.
#'
#' @param remove_curly_bracket logical.
#' @param folder character. The folder which contains the input file(s).
#' @param inputformat character. Its value can be:
#' - 'markdown' (default)
#' - 'mindmap'
#' @param savefile logical
#' @param savefilename character. Valid when savefile == TRUE
#'
#' @return outline of a markdown document or book.
#' @export
#'
#' @examples
#' outline()
outline <- function(folder = 'mm',
                    remove_curly_bracket = FALSE,
                    inputformat = 'markdown',
                    savefile = FALSE,
                    savefilename = 'outline.md') {
  if (inputformat == 'markdown') {
    md <- NULL
    for (filename in dir(folder, full.names = TRUE)) md <- c(md, readLines(filename, encoding = 'UTF-8'))
    headerloc <- grep('^#+', md)
    codeloc <- grep('^```', md)
    # exclude the lines begining with # but in code
    header <- md[headerloc[!sapply(headerloc, function(x) sum(x > codeloc[seq(1, length(codeloc), by = 2)] & x < codeloc[seq(2, length(codeloc), by = 2)])) == 1]]
    if (remove_curly_bracket) header <- gsub(pattern = '\\{.*\\}', '', header)
    if (savefile) writeLines(text = header, savefilename, useBytes = TRUE)
    return(header)
  } else if (inputformat == 'mindmap') {
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
    if (savefile) writeLines(text = md, savefilename, useBytes = TRUE)
    return(md)
  } else {
    message('"inputformat" must be either "markdown" or "mindmap"!')
  }
}

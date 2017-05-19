#' mindr()
#'
#' @param title character. The title of the output file.
#' @param folder character. The folder which contains the input file(s).
#' @param headerfile logical. Whether to save the headers into a markdown file.
#' @param task character. Its value can be:
#' - 'markdown to mindmap' (default)
#' - 'mindmap to markdown'
#' @return a mindmap file,which can be viewed by mindmap software, such as FreeMind or Xmind.
#' @export
#'
#' @examples
#' mindr()
mindr <- function(title = 'my title', folder = 'mm', headerfile = FALSE, task = 'markdown to mindmap') {
  if (task == 'markdown to mindmap') {
  md <- NULL
  for (filename in dir(folder, full.names = TRUE)) md <- c(md, readLines(filename, encoding = 'UTF-8'))
  headerloc <- grep('^#+', md)
  codeloc <- grep('^```', md)
  # exclude the lines begining with # but in code
  header <- md[headerloc[!sapply(headerloc, function(x) sum(x > codeloc[seq(1, length(codeloc), by = 2)] & x < codeloc[seq(2, length(codeloc), by = 2)])) == 1]]
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
  if (headerfile) writeLines(text = header, 'header.md', useBytes = TRUE)
  } else if (task == 'mindmap to markdown') {
    message('This function is still under construction. Please come later.')
  } else {
    message('"task" must be either "markdown to mindmap" or "mindmap to markdown"!')
  }
}

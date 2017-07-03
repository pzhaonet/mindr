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
#' @examples md2mm()
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
    if (backup & file.exists(paste0(savefilename, '.mm'))){ #file.copy(savefilename, to = paste0(savefilename, 'backup'))
      savefilename <- paste0(savefilename, '-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S'))
    }
    writeLines(text = mm, paste0(savefilename, '.mm'), useBytes = TRUE)
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
#' mm2md()
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
      if (backup & file.exists(paste0(savefilename, '.md'))){ #file.copy(savefilename, to = paste0(savefilename, 'backup'))
        savefilename <- paste0(savefilename, '-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S'))
      }
      # if (backup & file.exists(savefilename)) file.copy(savefilename, to = paste0(savefilename, 'backup'))
      writeLines(text = md, paste0(savefilename, '.md'), useBytes = TRUE)
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
#' outline()
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
      if (backup & file.exists(paste0(savefilename, '.md'))){ #file.copy(savefilename, to = paste0(savefilename, 'backup'))
        savefilename <- paste0(savefilename, '-', format(Sys.time(), '%Y-%m-%d-%H-%M-%S'))
      }
      # if (backup & file.exists(savefilename)) file.copy(savefilename, to = paste0(savefilename, 'backup'))
      writeLines(text = header, savefilename, useBytes = TRUE)
    }
    return(header)
  } else {print(paste('The directory', folder, 'does not exist!'))}
}


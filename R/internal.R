#' Get the index of equations in a string vector
#'
#' @param eq_begin the beginning index of an equation
#' @param eq_end the end index of an equation
#'
#' @return a index vector
get_eqloc <- function(eq_begin, eq_end){
  eq <- eq_begin[1]:eq_end[1]
  if(length(eq_begin) > 1) {
    for(i in 2:length(eq_begin)) eq <- c(eq, eq_begin[i]:eq_end[i])
  }
  return(eq)
}

#' #' Get the folder name of a given complete path
#' #'
#' #' @param path The complete path
#' #'
#' #' @return The folder name
#' #'
#' get_foldername <- function(path){
#'   foldername <- strsplit(path, '[/\\]')[[1]]
#'   return(foldername[length(foldername)])
#' }

#' get the file name extension
#'
#' @param filename character, the file name
#'
#' @return character, the file name extension
get_filename_ext <- function(filename){
  filename_sep <- strsplit(filename, '\\.')[[1]]
  nfilename <- length(filename_sep)
  if(nfilename == 1) {
    filename_ext <- ''
  } else {
    filename_ext <- filename_sep[nfilename]
  }
  return(filename_ext)
}

#' Rename a file automatically with a time stamp
#'
#' @param filename character.
#' @param connect the connecting character in the time stamp
#'
#' @return a new file name
#'
rename2 <- function(filename, connect = '-'){
  filename_sep <- strsplit(filename, '\\.')[[1]]
  nfilename <- length(filename_sep)
  if(nfilename == 1) {
    filename2 <- c(filename_sep, '')
  } else {
    filename2 <- c(paste(filename_sep[-nfilename], collapse = '.'), paste0('.', filename_sep[nfilename]))
  }
  newname <- paste0(filename2[1], connect, format(Sys.time(), paste('%Y', '%m','%d', '%H', '%M', '%S', sep = connect)), filename2[2])
  return(newname)
}

#' Write txt files avoiding overwriting existent files.
#'
#' @param text The text to write.
#' @param filename The destinated file name
#' @param backup Logical.
#'
#' @return a txt file
writeLines2 <- function(text, filename, backup = TRUE){
  newname <- filename
  if (backup & file.exists(filename)){
    newname <- rename2(filename)
  }
  writeLines(text = text, newname, useBytes = TRUE)
  message(newname, ' was generated!')
}

#' check whether a digital number is within a given range
#'
#' @param index integer. a row number in a markdown file
#' @param loc integer vector. the row numbers of the code block indicator, e.g.  triple backticks
#'
#' @return logical.
rmvcode <- function(index, loc) {
  sum(index > loc[seq(1, length(loc), by = 2)] &
        index < loc[seq(2, length(loc), by = 2)])
}

#' get the headings out of given strings
#'
#' @param pattern The definition of the headings
#' @param text the given strings
#'
#' @return integer. the index of the headings in the given strings.
get_heading <- function(pattern = '^#+ ', text){
  return(grep(pattern = pattern, x = text))
}

#' get the headings out of given strings
#'
#' @param pattern The definition of the headings
#' @param text the given strings
#'
#' @return integer. the index of the headings in the given strings.
get_heading2 <- function(pattern = '^#= #+ ', text){
  return(grep(pattern = pattern, x = text))
}

#' get the headings out of given strings
#'
#' @param pattern The definition of the headings
#' @param text the given strings
#'
#' @return integer. the index of the headings in the given strings.
get_heading3 <- function(pattern = "^#' #+ ", text){
  return(grep(pattern = pattern, x = text))
}

#' get the body out of given strings
#'
#' @param pattern The definition of the body text
#' @param text the given strings
#'
#' @return integer. the index of the body text in the given strings.
get_body <- function(pattern = '^#[^ ]*', text){
  return(grep(pattern = pattern, x = text))
}

#' Count the spaces between two given strings
#'
#' @param sep character for separation.
#' @param mychar The character to check.
#'
#' @return character as title with '#' inserted.
count_space <- function(mychar, sep){
  mychar_new <- gsub(sep, '    ', mychar)
  spaces <- gsub('^( +).*', '\\1', mychar_new)
  title <- gsub('^( +)(.*)', '\\2', mychar_new)
  if(title == '') return('NULLLL')
  paste(paste(rep('#', nchar(spaces)/4), collapse = ''), title)
}

#' Convert a folder structure into a mindmap by using the 'tree' command.
#' @details
#' For LinUx OS and mac OS, the 'tree' command must be pre-installed.
#' - Linux: `sudo apt-get install tree`
#' - mac: install [Homebrew](https://brew.sh/) first. Then in the terminal: `brew install tree`.
#'
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backup.
#' @param path character. the path of the folder.
#' @param output a file with the folder structure.
#' @param savefile logical. Whether to save the output as a file.
#' @param dir_files logical. Whether to display files besides folders.
#'
#' @return a mindmap file, which can be viewed by common mindmap software, such as 'FreeMind' (<http://freemind.sourceforge.net/wiki/index.php/Main_Page>) and 'XMind' (<http://www.xmind.net>).
dir2 <- function(path = getwd(),
                 savefile = TRUE,
                 savefilename = 'mindr.mm',
                 output = c('mm', 'txt', 'md', 'Rmd'),
                 backup = TRUE,
                 dir_files = FALSE) {
  output <- match.arg(output)
  if (is.na(path))
    return(message('The path cannot be NA!'))
  if (dir.exists(path)) {
    os <- Sys.info()['sysname']
    if(os == 'Windows') {
      non_ascii <- readLines(system.file('resource/non-ascii-windows.txt', package = 'mindr'), encoding = 'UTF-8')
      tree <- paste0('tree "', path, '"', ifelse(dir_files, ' /F', ''))
    } else{
      non_ascii <- readLines(system.file('resource/non-ascii-linux.txt', package = 'mindr'), encoding = 'UTF-8')
      tree <- paste0('tree "', path, '"', ifelse(dir_files, '', ' -d'))
    }
    mytree <- system(tree, intern = T)
    for(i in c('\033\\[[[:digit:]]{2};[[:digit:]]{2}m', '\033\\[00m')) mytree <- gsub(i, '', mytree)
    if ('txt' %in% output) {
      if (backup & file.exists(paste0(savefilename, '.txt'))) {
        message(paste0(savefilename, '.txt already exits.'))
        savefilename <-
          paste0(savefilename,
                 '-',
                 format(Sys.time(), '%Y-%m-%d-%H-%M-%S'))
      }
      writeLines(mytree, savefilename, useBytes = TRUE)
      message(paste(savefilename), ' was generated!')
    }
    if(os == 'Windows') {
      md <- mytree[-(1:3)]
    } else {
      md <- mytree[-1]
      md_length <- length(md)
      md <- md[-((md_length - 1):md_length)]
    }

    ## dir_files
    if(dir_files & os == 'Windows'){
      loc_files <- !(grepl(non_ascii[1], md) | grepl(non_ascii[3], md))
      md[loc_files] <- unlist(sapply(md[loc_files], function(x) count_space(x, sep = non_ascii[2])))
    }

    md <- md[md != 'NULLLL']
    md <- gsub(pattern = non_ascii[1], '# ', md)
    md <- gsub(pattern = non_ascii[3], '# ', md)
    md <- gsub(pattern = non_ascii[2], '#', md)
    md <- gsub(pattern = '    ', '#', md)

    mm <- mdtxt2mmtxt(title = path, mdtxt = md)
    if ('md' %in% output) {
      if(savefile) writeLines2(
        text = md,
        filename = savefilename,
        backup = backup
      )
      return(md)
    }
    if ('Rmd' %in% output) {
      if(savefile) writeLines2(
        text = md,
        filename = savefilename,
        backup = backup
      )
      return(md)
    }
    if ('mm' %in% output) {
      if(savefile) writeLines2(
        text = mm,
        filename = savefilename,
        backup = backup
      )
      return(mm)
    }
  } else {
    return(message(paste('The directory', path, 'does not exist!')))
  }
}

#' Convert a folder structure into a mindmap.
#'
#' @param savefilename character. Valid when savefile == TRUE.
#' @param backup logical. Whether the existing target file, if any, should be saved as backup.
#' @param path character. the path of the folder.
#' @param output a file with the folder structure.
#' @param savefile logical. Whether to save the output as a file.
#' @param dir_files logical. Whether to display files besides folders.
#'
#' @return a mindmap file, which can be viewed by common mindmap software, such as 'FreeMind' (<http://freemind.sourceforge.net/wiki/index.php/Main_Page>) and 'XMind' (<http://www.xmind.net>).
dir4 <- function(path = getwd(),
                 savefile = TRUE,
                 savefilename = 'mindr.mm',
                 output = c('mm', 'txt', 'md', 'Rmd'),
                 backup = TRUE,
                 dir_files = FALSE) {
  output <- match.arg(output)
  if (is.na(path))
    return(message('The path cannot be NA!'))
  if (dir.exists(path)) {
    os <- Sys.info()['sysname']
    # windows ----
    if(os == 'Windows') {
      oldlocale <- Sys.getlocale('LC_CTYPE')
      on.exit(Sys.setlocale('LC_CTYPE', oldlocale))
      Sys.setlocale('LC_CTYPE', 'Chinese')
      non_ascii <- readLines(system.file('resource/non-ascii-windows.txt', package = 'mindr'), encoding = 'UTF-8')
      tree <- paste0('tree "', path, '"', ifelse(dir_files, ' /F', ''))
      mytree <- system(tree, intern = T, show.output.on.console = TRUE)
      md <- mytree[-(1:3)]
      ## dir_files
      if(dir_files){
        loc_files <- !(grepl(non_ascii[1], md) | grepl(non_ascii[3], md))
        md[loc_files] <- unlist(sapply(md[loc_files], function(x) count_space(x, sep = non_ascii[2])))
      }
    } else {
      ## non windows ----
      # data.tree method ----
      non_ascii <- readLines(system.file('resource/non-ascii-datatree.txt', package = 'mindr'), encoding = 'UTF-8')
      if(path == '.') path <- getwd()
      if(path == '..') path <- dirname(getwd())
      if(dir_files) mydir <- list.files(path, full.names = TRUE, recursive = TRUE) else mydir <- list.dirs(path, full.names = TRUE, recursive = TRUE)
      rootname <- path #dirname(mydir[1])
      root <- dirname(rootname)
      mydir <- gsub(paste0('^', root, '/'), '', mydir)
      mytree <- data.tree::as.Node(data.frame(pathString = mydir))
      md <- print(mytree)[, 1]
      md[1] <- rootname
    }

    # Both ----
    if ('txt' %in% output) {
      if (backup & file.exists(paste0(savefilename, '.txt'))) {
        message(paste0(savefilename, '.txt already exits.'))
        savefilename <-
          paste0(savefilename,
                 '-',
                 format(Sys.time(), '%Y-%m-%d-%H-%M-%S'))
      }
      writeLines(mytree, savefilename, useBytes = TRUE)
      message(paste(savefilename), ' was generated!')
    }

    md <- md[md != 'NULLLL']
    md <- gsub('[ ]*$', '', md)
    md <- gsub(pattern = non_ascii[1], '# ', md)
    md <- gsub(pattern = non_ascii[3], '# ', md)
    md <- gsub(pattern = non_ascii[2], '#', md)
    md <- gsub(pattern = '    ', '#', md)

    mm <- mdtxt2mmtxt(title = path, mdtxt = md)
    if ('md' %in% output) {
      if(savefile) writeLines2(
        text = md,
        filename = savefilename,
        backup = backup
      )
      return(md)
    }
    if ('Rmd' %in% output) {
      if(savefile) writeLines2(
        text = md,
        filename = savefilename,
        backup = backup
      )
      return(md)
    }
    if ('mm' %in% output) {
      if(savefile) writeLines2(
        text = mm,
        filename = savefilename,
        backup = backup
      )
      return(mm)
    }
  } else {
    return(message(paste('The directory', path, 'does not exist!')))
  }
}

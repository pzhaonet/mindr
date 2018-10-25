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

#' Get the folder name of a given complete path
#'
#' @param path The complete path
#'
#' @return The folder name
#'
#' @examples get_foldername('c:/dropbox/temp')
get_foldername <- function(path){
  foldername <- strsplit(path, '[/\\]')[[1]]
  return(foldername[length(foldername)])
}

#' Rename a file automatically with a time stamp
#'
#' @param filename character.
#' @param connect the connecting character in the time stamp
#'
#' @return a new file name
#'
#' @examples rename2('md.mm.m3', connect = '')
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
    newname <- rename2(filename2)
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

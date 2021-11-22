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

#' Get the file name extension
#'
#' @param filename character, the file name
#'
#' @return character, the file name extension
get_filename_ext <- function(filename){
  gsub('^.*\\.([^.]+)$', '\\1', basename(filename))
}

#' Check whether a digital number is within a given range
#'
#' @param index integer. a row number in a markdown file
#' @param loc integer vector. the row numbers of the code block indicator, e.g.  triple backsticks
#'
#' @return logical.
rmvcode <- function(index, loc) {
  sum(index > loc[seq(1, length(loc), by = 2)] & index < loc[seq(2, length(loc), by = 2)])
}

#' convert lists to headings in a text
#'
#' @importFrom stats na.omit
#' @param text the given strings
#'
#' @return integer. the index of the headings in the given strings.
list2heading <- function(text){
  ## remove blank lines
  text <- text[text != '']

  ## get the heading level by counting # number
  oldheadings <- gsub('^(#+) .+', '\\1', x = text)
  oldheadings[!grepl('^(#+) .+', x = text)] <- NA
  heading_level <- nchar(oldheadings)
  heading_level <- na.omit(heading_level)[cumsum(!is.na(heading_level))]

  ## unnumbered list
  text_unnumbered <- text
  list_loc_unnumbered <- grepl('^ *[-*\\+] .+', text_unnumbered)
  text_unnumbered[!list_loc_unnumbered] <- NA
  list_marker_unnumbered <- gsub('^( *[-*\\+]) .+', '\\1', text_unnumbered)
  list_level_unnumbered <- nchar(gsub('^( *)[-*+] .+', '\\1', text_unnumbered))/2 + 1 + heading_level
  list_title_unnumbered <- gsub('^( *[-*+]) (.+)', '\\2', text_unnumbered)

  ## numbered list
  text_numbered <- text
  list_loc_numbered <- grepl('^ *[0-9]+\\. .+', text_numbered)
  text_numbered[!list_loc_numbered] <- NA
  list_marker_numbered <- gsub('^( *[0-9]+\\.) .+$', '\\1', text_numbered)
  list_level_numbered <- nchar(gsub('^( *)[0-9]+\\. .+', '\\1', text_numbered))/2 + 1 + heading_level
  list_title_numbered <- gsub('^( *[0-9]+\\.) (.+)', '\\2', text_numbered)

  ## merge
  list_title <- list_title_numbered
  list_title[is.na(list_title)] <- list_title_unnumbered[is.na(list_title)]
  list_title <- na.omit(list_title)
  list_level <- list_level_numbered
  list_level[is.na(list_level)] <- list_level_unnumbered[is.na(list_level)]
  list_level <- na.omit(list_level)
  list_loc <- list_loc_numbered + list_loc_unnumbered

  ## output
  new_list_prefix <- sapply(list_level, function(x) paste(rep('#', x), collapse = ''))
  text[as.logical(list_loc)] <- paste(new_list_prefix, list_title)
  return(text)
}

#' Convert Markdown text to FreeMind mind map text.
#' @inheritParams mm
#' @return a mindmap text.
mdtxt2mmtxt <-  function(from = '', root = 'root', md_eq = FALSE) {
  # merge non-headings (e.g. multi-line headings, equations, if any) into headings
  j <- 1
  md_heading <- from[j]
  for (i in 2:length(from)) {
    if (grepl('^#+ ', from[i])) {
      j <- j + 1
      md_heading[j] <- from[i]
    } else {
      md_heading[j] <- paste(md_heading[j], from[i])
    }
  }

  # replace un-supported characters
  md_heading <- gsub(pattern = '&', '&amp;', md_heading)
  md_heading <- gsub(pattern = '"', '&quot;', md_heading)

  # get heading titles

  ncc <- nchar(gsub('^(#+) .*$', '\\1', md_heading)) # level of the headings
  mmtext <- substr(md_heading, ncc + 2, nchar(md_heading)) # heading text

  # get the hyperlinks
  which_link <- grep(pattern = '\\[.*](.*)', mmtext)
  mmtext[which_link] <- gsub('\\((.*)\\)', '" LINK="\\1', mmtext[which_link])
  mmtext[which_link] <- gsub(pattern = '[][]*', replacement = '', mmtext[which_link])

  # build the code for .mm file
  mm <- rep('', length(md_heading) + 3)
  mm[1] <- '<map version="1.0.1">'
  mm[2] <- paste0('<node TEXT="', root, '">', paste0(rep('<node TEXT="">', ncc[1] - 1), collapse = ''))
  diffncc <- diff(ncc)
  for (i in 1:(length(md_heading) - 1)) {
    if (diffncc[i] == 1) mm[i + 2] <- paste0('<node TEXT="', mmtext[i], '">')
    if (diffncc[i] == 0) mm[i + 2] <- paste0('<node TEXT="', mmtext[i], '"></node>')
    if (diffncc[i] < 0)  mm[i + 2] <- paste0('<node TEXT="', mmtext[i], '">', paste0(rep('</node>',-diffncc[i] + 1), collapse = ''))
    if (md_eq){
      # mm[i + 3] <- gsub('(\\$\\$[^$]+\\$\\$)(">)(</node>)$', '\\2\\1\\3', mm[i + 3])
      mm[i + 2] <- gsub('(\\$\\$[^$]+\\$\\$)(">)', '\\2\\1', mm[i + 2])
      mm[i + 2] <- gsub('\\$\\$([^$]+)\\$\\$', '<hook EQUATION="\\1" NAME="plugins/latex/LatexNodeHook.properties"/>', mm[i + 2])
    }
  }
  mm[length(md_heading) + 2] <- paste0(rep('</node>', ncc[length(md_heading)] - 1), collapse = '')
  mm[length(md_heading) + 3] <- '</node></map>'
  return(mm)
}

#' Guess the type of input or output
#'
#' @importFrom utils head
#' @param from The source text
#'
#' @return the type, including 'dir', 'mindmap', 'R', 'markdown'.
guess_type <- function(from){
  if (length(from) == 1 && dir.exists(from)) return('dir')
  if (any(grepl('<node TEXT=', head(from)))) return('mindmap')
  if (any(grepl("^#' |^#= ", outline(from)))) return('R')
  if (any(grepl("^# ", from))) return('markdown')
  return(warning('Sorry, I do not know what type it is. The mission as aborted.'))
}

#' A function for markmap
#'
#' @param x something
#'
#' @return something else
filterNULL <- function (x) {
  if (length(x) == 0 || !is.list(x))
    return(x)
  x[!unlist(lapply(x, is.null))]
}

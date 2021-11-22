#' Convert R code into FreeMind mind map code
#' @inheritParams mm
#' @export
#' @return Character, FreeMind mind map code.
#' @examples
#' input <- system.file('examples/mindr-r.R', package = 'mindr')
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' output_txt <- r2mm(from = input_txt)
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".mm")
#' writeLines(output_txt, output, useBytes = TRUE)
#' # file.show(input) # Open the input file
#' # file.show(output) # Open the output file
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
r2mm <- function(from = NA, root = NA,
                 md_list = FALSE,
                 md_braces = FALSE,
                 md_bookdown = FALSE,
                 md_eq = FALSE,
                 md_maxlevel = '') {
  if (is.na(root)) root <- 'root'
  md <- r2md(from)
  mm <- md2mm(from = md, root = root,
              md_list = md_list,
              md_braces = md_braces,
              md_bookdown = md_bookdown,
              md_eq = md_eq,
              md_maxlevel = md_maxlevel)
  return(mm)
}

#' Convert FreeMind mind map code into .R code
#' @inheritParams mm
#' @export
#' @return Character, R code.
#' @examples
#' input <- system.file('examples/mindr-mm.mm', package = 'mindr')
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' output_txt <- mm2r(input_txt)
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".R")
#' writeLines(output_txt, output, useBytes = TRUE)
#' # file.show(input) # Open the input file
#' # file.show(output) # Open the output file
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
mm2r <- function(from = NA, r_seclabel = ' --------', r_chunkheading = FALSE) {
  md <- mm2md(from = from)
  rtext <- md2r(from = md, r_seclabel = r_seclabel, r_chunkheading = r_chunkheading)
  return(rtext)
}

#' Display hierarchical structure of a directory in FreeMind mind map
#' @inheritParams mm
#' @export
#' @return FreeMind mind map code.
#' @examples
#' input <- system.file(package = 'mindr')
#' dir2mm(input)
#' dir2mm(input, dir_files = FALSE, dir_all = TRUE, dir_excluded = 'Meta')
#'
#' output_txt <- dir2mm(input)
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".mm")
#' writeLines(output_txt, output, useBytes = TRUE)
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.show(output) # Open the output file
#' # file.remove(output) # remove the output file
dir2mm <- function(from = '.', root = NA, dir_files = TRUE, dir_all = TRUE, dir_excluded = NA, md_maxlevel = ''){
  if (is.na(root)) root <- basename(from)
  md <- dir2md(from = from, dir_files = dir_files, dir_all = dir_all, dir_excluded = dir_excluded)
  mm <- md2mm(from = md, root = root,
              md_list = FALSE,
              md_braces = TRUE,
              md_bookdown = FALSE,
              md_eq = FALSE,
              md_maxlevel = md_maxlevel)
  return(mm)
}

#' Generate hierarchical directories according to a FreeMind mind map
#' @inheritParams mm
#' @export
#' @return Directory generated.
#' @examples
#' input <- system.file('examples/mindr-mm.mm', package = 'mindr')
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' output <- file.path(tempdir(), 'mindr')
#' mm2dir(input_txt, output)
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.show(input) # Open the input file
#' # system2("open", output) # Open the output dir in explorer.
#' # unlink(output, recursive = TRUE) # remove the output file
#'
mm2dir <- function(from = NA, dir_to = NA, dir_quiet = FALSE){
  md <- mm2md(from)
  md2dir(from = md, dir_to = dir_to, md_list = FALSE, md_bookdown = FALSE, dir_quiet = dir_quiet)
}

#' Convert a hierarchical directory into R code
#' @inheritParams mm
#' @export
#' @return Character, R code
#' @examples
#' input <- system.file(package = 'mindr')
#' dir2r(input)
#' dir2r(input, dir_files = FALSE, dir_all = TRUE, dir_excluded = 'Meta')
#'
#' output_txt <- dir2r(input)
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".R")
#' writeLines(output_txt, output, useBytes = TRUE)
#' message('Input:  ', input, '\nOutput: ', output)
#' # system2("open", input) # Open the input dir in explorer.
#' # file.show(output) # Open the output file
#' # file.remove(output) # remove the output file
#'
dir2r <- function(from = '.', dir_files = TRUE, dir_all = TRUE, dir_excluded = NA, r_seclabel = ' --------', r_chunkheading = FALSE){
  md <- dir2md(from = from, dir_files = dir_files, dir_all = dir_all, dir_excluded = dir_excluded)
  rtext <- md2r(from = md, r_seclabel = r_seclabel, r_chunkheading = r_chunkheading)
  return(rtext)
}

#' Generate hierarchical directories according to the outline of R code
#' @inheritParams mm
#' @export
#' @return Directory generated.
#' @examples
#' input <- system.file('examples/mindr-r.R', package = 'mindr')
#' input_txt <- readLines(input, encoding = 'UTF-8')
#' output <- file.path(tempdir(), 'mindr')
#' r2dir(input_txt, output)
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.show(input) # Open the input file
#' # system2("open", output) # Open the output dir in explorer.
#' # unlink(output, recursive = TRUE) # remove the output file
#'
r2dir <- function(from = NA, dir_to = NA, md_list = FALSE, md_bookdown = TRUE, dir_quiet = FALSE){
  md <- r2md(from = from)
  md2dir(from = md, dir_to = dir_to, md_list = md_list, md_bookdown = md_bookdown, dir_quiet = dir_quiet)
}

#' All-in-one wrapper for the conversion between (R) Markdown, FreeMind mind map, R code, directory structure, and HTML widget.
#' @details \code{mm()} converts between (R) Markdown syntax text, R code, FreeMind mind map code, and directory, and display them in a HTML widget. It is a wrapper for other conversion functions in this package.
#'
#' @param from Character. The source text of the (R) Markdown syntax text, the R code, the FreeMind mind map code, or the path to the directory.
#' @param input_type Character. The type of the input text. It can be \code{'auto', 'markdown', 'mindmap', 'R', 'dir'}. The default value is \code{'auto'}, which means the type will be automatically assgined according to the features of the input text.
#' @param output_type Character. The type of the output. It can be \code{'widget', 'mindmap', 'markdown', 'R', 'dir'}. The default value is \code{'widget'}.
#' @param root Character. The string displayed as the root (center) of the mind map.
#' @param md_list Logical. whether to process lists like headings in the Markdown input.
#' @param md_eq Logical. Whether to include LaTeX equations in the Markdown input when converted to other formats.
#' @param md_braces Logical. Whether to remove \href{https://bookdown.org/yihui/bookdown/cross-references.html}{{#ID}} in the headings of the markdown file (usually in a \href{https://github.com/rstudio/bookdown}{bookdown}> project.
#' @param md_bookdown Logical. Whether the R Markdown syntax text is in bookdown style, i.e. \code{# (PART), # (APPENDIX)}, and \code{# References} as an upper level of the Level 1 heading.
#' @param md_maxlevel Integer or ''. The maximum level of the markdown headings that are displayed in the mind map.
#' @param r_seclabel Character. The ending characters indicating sections in R Markdown.
#' @param r_chunkheading Logical. Whether process the chunk label as headings.
#' @param dir_files Logical. Whether to include files. If \code{FALSE}, only folders are included. If \code{TRUE}, folders and files are included.
#' @param dir_all Logical. Whether to include all files in a directory. If \code{FALSE}, only the names of visible files are included (following Unix-style visibility, that is files whose name does not start with a dot). If \code{TRUE}, all file names will be included.
#' @param dir_excluded Character. The directories which are not included in the output.
#' @param dir_to Character. The path of the output directory.
#' @param dir_quiet Logical. Whether to display the results of generated directories.
#' @param widget_name Character. The name of the html widget.
#' @param widget_width Numeric. The width of the widget.
#' @param widget_height Numeric. The height of the widget.
#' @param widget_elementId Character. The ID of teh Widget.
#' @param widget_options List. Options for the markmap widget. It should be a list passed from the \code{markmapOption()} function.
#'
#' @return Desired output.
#' @export
#' @examples
#' ################################################
#' # Example 1: From Markdown to other outputs ####
#' ################################################
#'
#' ## Source document ####
#' input <- system.file('examples/mindr-md.Rmd', package = 'mindr')
#'
#' ## file.show(input) # Open the input file with the default program, if any
#' input_txt <- readLines(input, encoding = 'UTF-8')
#'
#' ## Convert to mind map text, markdown outline, R script, and HTML widget ####
#' mm_output <- mm(input_txt, output_type = c('mindmap', 'markdown', 'R', 'widget'))
#' mm_output
#'
#' ## Save the output texts as files ####
#'
#' ### mind map ####
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".mm")
#' writeLines(mm_output$mindmap, output, useBytes = TRUE)
#' # file.show(output) # Open the output file with the default program, if any
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ### markdown outline ####
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".md")
#' writeLines(mm_output$markdown, output, useBytes = TRUE)
#' # file.show(output) # Open the output file with the default program, if any
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ### R script ####
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".R")
#' writeLines(mm_output$r, output, useBytes = TRUE)
#' # file.show(output) # Open the output file with the default program, if any
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ### Widget ####
#' # output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
#' # htmlwidgets::saveWidget(mm_output$widget, file = output)
#' # file.show(output) # Open the output file with the default program, if any
#' # message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ## Generate directory according to the source document ####
#' temp_dir <- file.path(tempdir(), 'mindr')
#' mm_output <- mm(input_txt, output_type = 'dir', root = 'mindr',
#'                 md_list = TRUE, md_braces = TRUE, md_bookdown = TRUE,
#'                 dir_to = temp_dir)
#' # system2('open', temp_dir) # Open the generated directory
#' # unlink(temp_dir, recursive = TRUE) # remove the generated directory
#'
#' ## More arguments ####
#' mm_output <- mm(input_txt, output_type = c('mindmap', 'markdown', 'R', 'widget'), root = 'mindr',
#'                 md_list = TRUE, md_braces = TRUE, md_bookdown = TRUE)
#' mm_output
#'
#' ################################################
#' # Example 2: From mind map to other outputs ####
#' ################################################
#'
#' ## Source document ####
#' input <- system.file('examples/mindr-mm.mm', package = 'mindr')
#'
#' ## file.show(input) # Open the input file with the default program, if any
#' input_txt <- readLines(input, encoding = 'UTF-8')
#'
#' ## Convert markdown outline, R script, and HTML widget ####
#' mm_output <- mm(input_txt, output_type = c('markdown', 'R', 'widget'))
#' mm_output
#'
#' ## Save the output texts as files ####
#'
#' ### markdown outline ####
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".md")
#' writeLines(mm_output$markdown, output, useBytes = TRUE)
#' # file.show(output) # Open the output file with the default program
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ### R script ####
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".R")
#' writeLines(mm_output$r, output, useBytes = TRUE)
#' # file.show(output) # Open the output file with the default program
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ### Widget ####
#' # output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
#' # htmlwidgets::saveWidget(mm_output$widget, file = output)
#' # file.show(output) # Open the output file with the default program, if any
#' # message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ## Generate directory according to the source document ####
#' temp_dir <- file.path(tempdir(), 'mindr')
#' mm_output <- mm(input_txt, output_type = 'dir', root = 'mindr',
#'                 dir_to = temp_dir)
#' # system2('open', temp_dir) # Open the generatecd directory
#' # unlink(temp_dir, recursive = TRUE) # remove the generated directory
#'
#' ################################################
#' # Example 3: From R script to other outputs ####
#' ################################################
#'
#' ## Source document ####
#' input <- system.file('examples/mindr-r.R', package = 'mindr')
#'
#' ## file.show(input) # Open the input file with the default program, if any
#' input_txt <- readLines(input, encoding = 'UTF-8')
#'
#' ## Convert to mind map text, markdown text,  and HTML widget ####
#' mm_output <- mm(input_txt, output_type = c('mindmap', 'markdown',  'widget'))
#' mm_output
#'
#' ## Save the output texts as files ####
#'
#' ### mind map ####
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".mm")
#' writeLines(mm_output$mindmap, output, useBytes = TRUE)
#' # file.show(output) # Open the output file with the default program, if any
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ### R markdown ####
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".Rmd")
#' writeLines(mm_output$markdown, output, useBytes = TRUE)
#' # file.show(output) # Open the output file with the default program, if any
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ### Widget ####
#' # output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
#' # htmlwidgets::saveWidget(mm_output$widget, file = output)
#' # file.show(output) # Open the output file with the default program, if any
#' # message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ## Generate directory according to the source document ####
#' temp_dir <- file.path(tempdir(), 'mindr')
#' mm_output <- mm(input_txt, output_type = 'dir', root = 'mindr',
#'                 dir_to = temp_dir)
#' # system2('open', temp_dir) # Open the generated directory
#' # unlink(temp_dir, recursive = TRUE) # remove the generated directory
#'
#' #################################################
#' # Example 4: From directory to other outputs ####
#' #################################################
#'
#' ## Source directory ####
#' input <- system.file(package = 'mindr')
#'
#' ## Convert to mind map text, markdown outline, R script, and HTML widget ####
#' mm_output <- mm(input, output_type = c('mindmap', 'markdown', 'R', 'widget'))
#' mm_output
#'
#' ## Save the output texts as files ####
#'
#' ### mind map ####
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".mm")
#' writeLines(mm_output$mindmap, output, useBytes = TRUE)
#' # file.show(output) # Open the output file with the default program, if any
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ### markdown outline ####
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".md")
#' writeLines(mm_output$markdown, output, useBytes = TRUE)
#' # file.show(output) # Open the output file with the default program, if any
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ### R script ####
#' output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".R")
#' writeLines(mm_output$r, output, useBytes = TRUE)
#' # file.show(output) # Open the output file with the default program, if any
#' message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ### Widget ####
#' # output <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
#' # htmlwidgets::saveWidget(mm_output$widget, file = output)
#' # file.show(output) # Open the output file with the default program, if any
#' # message('Input:  ', input, '\nOutput: ', output)
#' # file.remove(output) # remove the output file
#'
#' ## Clone the source directory ####
#' temp_dir <- file.path(tempdir(), 'mindr')
#' mm_output <- mm(input, output_type = 'dir',
#'                 dir_to = temp_dir)
#' # system2('open', temp_dir) # Open the generated directory
#' # unlink(temp_dir, recursive = TRUE) # remove the generated directory
#'
#' ################################################
#' # Example 5: From any format to mind map    ####
#' ################################################
#'
#' # With the help of pandoc, you can display the outline of any documents that pandoc can convert to Markdown.
#'
#' # # HTML: here we use the R-FQA webpage
#' # myurl <- 'https://cran.r-project.org/doc/FAQ/R-FAQ.html'
#' # input <- tempfile(pattern = "file", tmpdir = tempdir())
#' # markdown_temp <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".md")
#' # download.file(myurl, destfile = input, method = 'curl')
#' # rmarkdown::pandoc_convert(input, to = "markdown", output = markdown_temp)
#' # input_txt <- readLines(markdown_temp, encoding = 'UTF-8')
#' # mindr::mm(input_txt)
#'
#' # # MS Word: here we use a .docx document shipped by the 'officer' package
#' # input <- system.file('doc_examples/example.docx', package = 'officer')
#' # markdown_temp <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".md")
#' # rmarkdown::pandoc_convert(input, to = "markdown", output = markdown_temp)
#' # input_txt <- readLines(markdown_temp, encoding = 'UTF-8')
#' # mindr::mm(input_txt, md_list = TRUE)
mm <- function(from = NA,
               input_type = c('auto', 'markdown', 'mindmap', 'R', 'dir'),
               output_type = c('widget', 'mindmap', 'markdown', 'R', 'dir'),
               root = NA,
               md_list = FALSE, md_eq = FALSE, md_braces = FALSE, md_bookdown = FALSE, md_maxlevel = '', # markdown options
               r_seclabel = ' --------', r_chunkheading = FALSE, # R script options
               dir_files = TRUE, dir_all = TRUE, dir_excluded = NA, dir_to = NA, dir_quiet = FALSE, # dir options
               widget_name = NA, widget_width = NULL, widget_height = NULL, widget_elementId = NULL, widget_options = markmapOption(preset = 'colorful') # widget options
               ){
  input_type <- match.arg(input_type, c('auto', 'markdown', 'mindmap', 'R', 'dir'))
  if (input_type == 'auto') input_type <- guess_type(from)
  output_type <- match.arg(output_type, c('widget', 'markdown', 'mindmap', 'R', 'dir'), several.ok = TRUE)

  output <- list()

  if ('mindmap' %in% output_type){
    if (input_type == "markdown") mindmap <- md2mm(from = from, root = root, md_list = md_list, md_braces = md_braces, md_bookdown = md_bookdown, md_eq = md_eq, md_maxlevel = md_maxlevel)
    if (input_type == "mindmap") mindmap <- from
    if (input_type == "R") mindmap <- r2mm(from = from, root = root, md_list = md_list, md_braces = md_braces, md_bookdown = md_bookdown, md_eq = md_eq, md_maxlevel = md_maxlevel)
    if (input_type == "dir") mindmap <- dir2mm(from = from, dir_files = dir_files, dir_all = dir_all, dir_excluded = dir_excluded, md_maxlevel = md_maxlevel)
    output <- c(output, list(mindmap = mindmap))
  }

  if ('markdown' %in% output_type){
    if (input_type == "markdown") markdown <-  outline(from = from, md_list = md_list, md_eq = md_eq, md_braces = md_braces, md_bookdown = md_bookdown)
    if (input_type == "mindmap") markdown <- mm2md(from = from)
    if (input_type == "R") markdown <- r2md(from = from)
    if (input_type == "dir") markdown <- dir2md(from = from, dir_files = dir_files, dir_all = dir_all, dir_excluded = dir_excluded)
    output <- c(output, list(markdown = markdown))
  }

  if ('R' %in% output_type){
    if (input_type == "markdown") r <- md2r(from = from, r_seclabel = r_seclabel, r_chunkheading = r_chunkheading)
    if (input_type == "mindmap") r <- mm2r(from = from, r_seclabel = r_seclabel, r_chunkheading = r_chunkheading)
    if (input_type == "R") r <- from
    if (input_type == "dir") r <- dir2r(from = from, dir_files = dir_files, dir_all = dir_all, dir_excluded = dir_excluded, r_seclabel = r_seclabel, r_chunkheading = r_chunkheading)
    output <- c(output, list(r = r))
  }

  if ('dir' %in% output_type){
    if (input_type == "markdown") md2dir(from = from, dir_to = dir_to, dir_quiet = dir_quiet, md_list = md_list, md_bookdown = md_bookdown)
    if (input_type == "mindmap") mm2dir(from = from, dir_to = dir_to, dir_quiet = dir_quiet)
    if (input_type == "R") r2dir(from = from, dir_to = dir_to, dir_quiet = dir_quiet, md_list = md_list, md_bookdown = md_bookdown)
    if (input_type == "dir") md2dir(from = dir2md(from = from, dir_files = FALSE, dir_all = dir_all, dir_excluded = dir_excluded),
                                    dir_to = dir_to, dir_quiet = dir_quiet, md_list = FALSE, md_bookdown = FALSE)
  }

  if ('widget' %in% output_type){
    widget <- markmap(from = from, root = root, input_type = input_type,
                      md_list = md_list, md_eq = md_eq, md_braces = md_braces, md_bookdown = md_bookdown, md_maxlevel = md_maxlevel, # markdown options
                      dir_files = dir_files, dir_all = dir_all, dir_excluded = dir_excluded, # dir options
                      widget_name = widget_name, widget_width = widget_width, widget_height = widget_height, widget_elementId = widget_elementId, widget_options = widget_options) # widget options
    output <- c(output, list(widget = widget))
  }

  return(output)
}

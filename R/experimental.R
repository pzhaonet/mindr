#' Extract the outline from pdf toc, and output as Markdown
#'
#' @param input_toc Character. The table of contents (TOC) of a pdf file, extracted by \code{pdftools::pdf_toc()}.
#' @return Character, showing the TOC in Markdown.
#' @export
#' @examples
#' input <- file.path(R.home(), 'doc/manual/R-intro.pdf')
#' input_toc <- pdftools::pdf_toc(input)
#' outline_pdf(input_toc)
outline_pdf <- function(input_toc){
  input_toc <- unlist(input_toc)
  toc_name <- names(input_toc)
  names(input_toc) <- NULL

  heading_level <- sapply(toc_name, function(x){
    x1 <- gregexpr('\\.', x)
    if(x1[[1]][1] == -1) 0 else length(x1[[1]])
  }
  )
  names(heading_level) <- NULL
  # heading_level <- heading_level + 1
  input_txt <- paste(sapply(heading_level, function(x) paste(rep('#', x), collapse = '')), input_toc)
  input_txt
}

#' Convert almost any file into mind map.
#' @details The input file type could be .md, .Rmd, .R, .mm, .pdf, .docx, .html, .rtf, .odt, .epub, .tex, and any other types which pandoc can convert from. See \href{https://pandoc.org/}{pandoc} for more details.
#' @param input_file Character. The path to the file for input.
#' @inheritParams mm
#' @importFrom pdftools pdf_toc
#' @importFrom rmarkdown pandoc_convert
#' @return Desired output.
#' @export
#' @examples
#' # pdf: here we use the pdf files in the R installation
#' input <- file.path(R.home(), 'doc/manual/R-intro.pdf')
#' mmm(input)
#' input <- file.path(R.home(), 'doc/manual/R-data.pdf')
#' mmm(input, root = 'R-data', md_maxlevel = 2)
#'
#' # MS Word: here we use a .docx document shipped by the 'officer' package
#' # input <- system.file('doc_examples/example.docx', package = 'officer')
#' # mmm(input)
#'
#' # HTML: here we use the R-FQA webpage
#' input <- file.path(R.home(), 'doc/html/rw-FAQ.html')
#' mmm(input)
mmm <- function(input_file = NA,
                output_type = c('widget', 'mindmap', 'markdown'),
                root = NA,
                md_list = FALSE, md_eq = FALSE, md_braces = FALSE, md_bookdown = FALSE, md_maxlevel = '', # markdown options
                r_seclabel = ' --------', r_chunkheading = FALSE, # R script options
                widget_name = NA, widget_width = NULL, widget_height = NULL, widget_elementId = NULL, widget_options = markmapOption(preset = 'colorful') # widget options
){
  output_type <- match.arg(output_type, c('widget', 'markdown', 'mindmap', 'R'), several.ok = TRUE)
  if (length(input_file) > 1) return(message('Processing multiple files is not supported. Please use lapply() or loop for processing multiple files. Mission aborted.'))
  if (!file.exists(input_file)) return(message(input_file, ' does not exist. Mission aborted.'))

  file_ext <- get_filename_ext(input_file)
  markdown_temp <- NULL
  if (!file_ext %in% c('R', 'r', 'md', 'Rmd', 'mm', 'pdf')){
    markdown_temp <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".md")
    rmarkdown::pandoc_convert(input_file, output = markdown_temp)
    input_type <- 'markdown'
    input_file <- markdown_temp
  }

  if (file_ext == 'pdf'){
    input_toc <- pdftools::pdf_toc(input_file)
    from <- outline_pdf(input_toc)
    input_type <- 'markdown'
  } else {
    from <- readLines(input_file, encoding = 'UTF-8')
  }

  if (file_ext %in% c('r', 'R')) input_type <- 'R'
  if (file_ext %in% c('md', 'Rmd')) input_type <- 'markdown'
  if (file_ext %in% c('mm')) input_type <- 'mindmap'

  output <- mm(from = from, input_type = input_type,
               output_type = output_type,
               root = root,
               md_list = md_list, md_eq = md_eq, md_braces = md_braces, md_bookdown = md_bookdown, md_maxlevel = md_maxlevel, # markdown options
               r_seclabel = r_seclabel, r_chunkheading = r_chunkheading, # R script options
               widget_name = widget_name, widget_width = widget_width, widget_height = widget_height, widget_elementId = widget_elementId, widget_options = widget_options # widget options
  )

  if (!is.null(markdown_temp)) file.remove(markdown_temp)
  return(output)
}

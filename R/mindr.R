#' Convert between .R, .Rmd, .mm according to the given file names, and create a markmap widget
#' @details
#' For LinUx OS and mac OS, the 'tree' command must be pre-installed before using 'show_files = FALSE'.
#' - Linux: `sudo apt-get install tree`
#' - mac: install [Homebrew](https://brew.sh/) first. Then in the terminal: `brew install tree`.
#' @param remove_curly_bracket logical. Whether to remove {#ID} in the headers of the markdown file (usually in a 'bookdown' <https://github.com/rstudio/bookdown> project).
#' @param width the width of the markmap
#' @param height the height of the markmap
#' @param elementId character.
#' @param options the markmap options
#' @param root character. a string displayed as the root of the mind map
#' @param bookdown_style logical. whether the markdown files are in bookdown style, i.e. index.Rmd at the beginning, `# (PART)`, `# (APPENDIX)` and `# References` as an upper level of normal `#` title
#' @param method "regexpr" uses regular expressions, 'pandoc' uses pandoc to find the headings.
#' @param from character. The path of the input file, or the input markdown text, or the path to the directory. Dependent on 'type'.
#' @param to character. The path of the output file.
#' @param type character. The type of the input. If type == 'dir' and the OS is LinUx, the 'tree' command must be pre-installed: `sudo apt-get install tree`.
#' @param show_files logical. Whether to show files in a directory. Only valid when type == 'dir'.
#' @param widget_name The file name of the html widget to save.
#' @param include_list logical. whether to convert unnumbered lists into headings.
#'
#' @import htmlwidgets
#' @return A HTML widget object rendered from a given document.
#' @export
#' @examples
#' \dontrun{
#' ### text -> widget
#' input <- c('# Chapter 1', '## Section 1.1', '## Section 1.2', '# Chapter 2')
#' mm(from = input, type = 'text', root = 'mindr')
#'
#' ### directory -> widget
#' input <- paste0(.libPaths(), '/mindr')[1]
#' mm(from = input, type = 'dir')
#' mm(from = input, type = 'dir', widget_name = 'mindrtest.html')

#' ### directory -> mm
#' mm(from = input, type = 'dir', to = 'test.mm')
#' ### directory -> md
#' mm(from = input, type = 'dir', to = 'test.md')
#' ### directory -> txt
#' mm(from = input, type = 'dir', to = 'test.txt')
#'
#' ### Rmd -> widget
#' input <- system.file('examples/r/rmd2r.Rmd', package = 'mindr')
#' mm(from = input, type = 'file', root = 'mindr')
#' ### Rmd -> r
#' mm(from = input, type = 'file', root = 'mindr', to = 'test.r')
#' ### Rmd -> mm
#' mm(from = input, type = 'file', root = 'mindr', to = 'test.mm')
#'
#' ### mm -> widget
#' input <- system.file('examples/mm/bookdownplus.mm', package = 'mindr')
#' mm(from = input, type = 'file', root = 'mindr')
#' ### mm -> Rmd
#' mm(from = input, type = 'file', root = 'mindr', to = 'test.Rmd')
#' ### mm -> r
#' mm(from = input, type = 'file', root = 'mindr', to = 'test.r')
#'
#' ### r -> widget
#' input <- system.file('examples/r/r2rmd.R', package = 'mindr')
#' mm(from = input, type = 'file', root = 'mindr')
#' ### r -> Rmd
#' mm(from = input, type = 'file', root = 'mindr', to = 'test.Rmd')
#' ### r -> mm
#' mm(from = input, type = 'file', root = 'mindr', to = 'test.mm')
#'
#' ### The outline of the book Learning R
#' input <- system.file('examples/xuer/xuer.md', package = 'mindr')
#' mm(from = input, type = 'file', root = 'Learning R', to = 'learningr.mm')
#' }
#'
mm <- function(from = NULL,
               to = NULL,
               type = c('file', 'text', 'dir'),
               root = NA,
               show_files = TRUE,
               remove_curly_bracket = TRUE,
               bookdown_style = TRUE,
               widget_name = NA,
               width = NULL,
               height = NULL,
               elementId = NULL,
               options = markmapOption(preset = 'colorful'),
               method = c('regexpr', 'pandoc'),
               include_list = FALSE) {
  type <- match.arg(type)
  method <- match.arg(method)

  if (!is.null(to)) {
    if(dirname(to) == '.') to <- file.path(getwd(), to)
    to_name <- basename(to)
    to_dir <- dirname(to)
    to_ext <- get_filename_ext(to_name)
  }

  # 1. get clean headers for widgets, and convert files  ----
  if (type == 'text') {
    # 1.1 input is text -------------------------------
    header <- from
  } else if (type == 'dir') {
    # 1.2 input is dir ---------------------------------
    return(
      tree(
        from = from,
        to = to,
        root = root,
        show_files = show_files,
        widget_name = widget_name,
        width = width,
        height = height,
        elementId = elementId,
        options = options
      )
    )
    # if(!is.null(to)) {
    #   to_name <- basename(to)
    #   to_dir <- dirname(to)
    #   to_ext <- get_filename_ext(to_name)
    #   header <- dir2(path = from, output = to_ext, savefile = TRUE, savefilename = to)
    # }
    # header <- dir2(path = from, output = 'md', savefile = FALSE)
  } else if (type == 'file') {
    # 1.3 input is a file --------------------------------------
    # if(!file.exists(from)) return(message('The file', from, ' does not exist. Please give a valid path.'))
    from_name <- basename(from)
    from_dir <- dirname(from)
    from_ext <- get_filename_ext(from_name)
    # 1.3.1 from md ----------------------------------------------
    if (from_ext == 'md' | from_ext == 'Rmd') {
      header <- outline(
        pattern = from_name,
        path = from_dir,
        remove_curly_bracket = remove_curly_bracket,
        savefile = FALSE,
        bookdown_style = bookdown_style,
        method = method,
        include_list = include_list
      )
      # to mm ------------------------------------------------
      if (!is.null(to)) {
        # if(dirname(to) == '.') to <- file.path(getwd(), to)
        # to_name <- basename(to)
        # to_dir <- dirname(to)
        # to_ext <- get_filename_ext(to_name)
        if (to_ext == 'mm') {
          md2mm(
            pattern = from_name,
            title = root,
            path = from_dir,
            remove_curly_bracket = remove_curly_bracket,
            savefile = TRUE,
            savefilename = to,
            bookdown_style = bookdown_style,
            method = method,
            include_list = include_list
          )
        }
        # to r -------------------------------------------------
        if (to_ext == 'R' | to_ext == 'r') {
          rmd2r(
            filepattern = from_name,
            path = from_dir,
            savefile = TRUE,
            savefilename = to
          )
        }
      }
    }

    # 1.3.2 from mm ----------------------------------------------
    if (from_ext == 'mm') {
      if (is.null(to)) {
        header <-
          mm2md(pattern = from_name,
                path = from_dir,
                savefile = FALSE)
      } else{
        # to_name <- basename(to)
        # to_dir <- dirname(to)
        # to_ext <- get_filename_ext(to_name)
        # to md ------------------------------------------------
        if (to_ext == 'Rmd' | to_ext == 'md') {
          header <-
            mm2md(
              pattern = from_name,
              path = from_dir,
              savefile = TRUE,
              savefilename = to
            )
        }
        # to r -------------------------------------------------
        if (to_ext == 'R' | to_ext == 'r') {
          mmtemp <- rename2(from_name)
          header <-
            mm2md(
              pattern = from_name,
              path = from_dir,
              savefile = TRUE,
              savefilename = mmtemp
            )
          rmd2r(filepattern = mmtemp, savefilename = to)
          file.remove(mmtemp)
        }
      }
    }

    # 1.3.3 from r -----------------------------------------------
    if (from_ext == 'r' | from_ext == 'R') {
      if (is.null(to)) {
        mmtemp <- rename2(from_name)
        header <-
          r2rmd(
            filepattern = from_name,
            path = from_dir,
            savefile = TRUE,
            savefilename = mmtemp
          )
        header <- outline(
          pattern = mmtemp,
          remove_curly_bracket = remove_curly_bracket,
          savefile = FALSE,
          bookdown_style = bookdown_style,
          method = method,
          include_list = include_list
        )
        file.remove(mmtemp)
      } else{
        # to_name <- basename(to)
        # to_dir <- dirname(to)
        # to_ext <- get_filename_ext(to_name)
        # to rmd -----------------------------------------------
        if (to_ext == 'Rmd' | to_ext == 'md') {
          header <-
            r2rmd(
              filepattern = from_name,
              path = from_dir,
              savefilename = to
            )
          header <- outline(
            pattern = to_name,
            path = to_dir,
            remove_curly_bracket = remove_curly_bracket,
            savefile = FALSE,
            bookdown_style = bookdown_style,
            method = method,
            include_list = include_list
          )
        }
        # to mm ------------------------------------------------
        if (to_ext == 'mm') {
          mmtemp <- rename2(from_name)
          header <-
            r2rmd(
              filepattern = from_name,
              path = from_dir,
              savefile = TRUE,
              savefilename = mmtemp
            )
          header <- outline(
            pattern = mmtemp,
            remove_curly_bracket = remove_curly_bracket,
            savefile = FALSE,
            bookdown_style = bookdown_style,
            method = method,
            include_list = include_list
          )
          file.remove(mmtemp)
          r2mm(
            filepattern = from_name,
            path = from_dir,
            title = root,
            savefilename = gsub('.mm$', '', to)
          )
        }
      }
    }
  }

  # 2 format the headers ----
  header <- paste0('#', header)
  if (is.na(root)) {
    header <- c('# root', header)
  } else {
    header <- c(paste('#', root), header)
    }

  # 3 create widget ----
  data <- paste(header, collapse = '\n')
  x = list(data = data, options = options)
  tree_widget <- htmlwidgets::createWidget(
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
  if (!is.na(widget_name)) {
    filetemp <- paste0('mindr-tree-', Sys.Date(), '.html')
    htmlwidgets::saveWidget(tree_widget, filetemp)
    file.copy(filetemp, widget_name)
    file.remove(filetemp)
  }
  return(tree_widget)
}


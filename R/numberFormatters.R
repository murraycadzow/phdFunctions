# number formatters

#' @title
#' round_dp
#'
#' @description
#' create a string formatted to n decimal places
#'
#' @param num a number
#' @param dp a whole number >= 0
#' @return character string version of the float to specified decimal places
#' @export
round_dp <- function(num,dp){
  if (!is.numeric(num) | !is.numeric(dp)) {
    stop("num and dp must be numeric")
  }
  if (dp < 0) {
    stop("dp must be positive")
  }
  if (!(dp == floor(dp) & dp == ceiling(dp))) {
    stop("dp must be a whole number")
  }
  sprintf(paste0('%.',dp,'f'), num)
}

#' @title
#' three_dp
#'
#' @description
#' rounds a number to 3 decimal places and returns a string
#' convience function
#'
#' @param num a number to round
#' @return number rounded to 3 decimal places
#' @export
#' @examples
#' three_dp(0.123456)
three_dp <- function(num){
  if (!is.numeric(num)) {
    stop("num must be numeric")
  }
  round_dp(num, 3)
}

#' @title
#' format_p_md
#' @details
#' if format = "latex', use knitr::kable( format = 'latex', escape = FALSE)
#' @param p numeric atom
#' @param sci use scientific x10 format
#' @param digits number of decimal places to display
#' @param sci_thres threshold below which to apply scientific format [0,1]
#' @return character atom
#' @export
format_p_md <- function(p, sci = TRUE, digits = 3, sci_thres = 1e-3, format = 'html'){
  replace1 <- " x 10^"
  replace2 <- "^"
  if(format == 'latex'){
    replace1 <- " x 10$^{"
    replace2 <- "}$"
  }
  if(p < sci_thres && sci){
    sprintf(paste0("%.",digits,"e"), p) %>%
      stringr::str_replace(., "e-0", "e-") %>%
      stringr::str_replace(.,"e0", "e") %>%
      stringr::str_replace(., pattern = "e", replacement = stringr::fixed(replace1)) %>%
      paste0(.,replace2)
  } else{
    sprintf(paste0("%.",digits,"f"), p)
  }
}



#' @title
#' format_p_ke
#'
#' @description
#' a wrapper for kableExtra::cell_spec() around format_p_md
#'
#' @param p numeric atom
#' @param sci use scientific x10 format
#' @param digits number of decimal places to display
#' @param sci_thres threshold below which to apply scientific format [0,1]
#' @param ... parameters for kableExtra::cell_spec(). e.g. bold = TRUE
#' @return character atom
#' @export
#' @examples
#' format_p_ke(p = 0.1234, sci = TRUE, digits = 3, sci_thres = 1e-3, bold = TRUE)
format_p_ke  <- function(p, sci = TRUE, digits = 3, sci_thres = 1e-3, format = "html", ...){
  #ifelse(as.numeric(p)<0.05, cell_spec(p, "html", bold = T), cell_spec(p, "html",  bold = F))

  kableExtra::cell_spec(format_p_md(p, sci, digits, sci_thres, format), format, ...)
}


#' @title
#' num_to_words
#'
#' @param x a number to convert to a string
#' @importFrom dplyr case_when
#' @return a string
#' @export
#'
#' @examples num_to_word(3)
num_to_word <- function(x){
  dplyr::case_when(
    0 == x ~ 'zero',
    1 == x ~ 'one',
    2 == x ~ 'two',
    3 == x ~ 'three',
    4 == x ~ 'four',
    5 == x ~ 'five',
    6 == x ~ 'six',
    7 == x ~ 'seven',
    8 == x ~ 'eight',
    9 == x ~ 'nine',
    10 == x ~ 'ten',
    TRUE ~ as.character(x)
  )
}

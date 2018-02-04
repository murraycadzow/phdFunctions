# number formatters

#' create a string formatted to n decimal places
#' @param num a number
#' @param dp a whole number >= 0
#' @return a string version of the float to specified decimal places
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

#' rounds a number to 3 decimal places and returns a string
#' convience function
three_dp <- function(num){
  if (!is.numeric(num)) {
    stop("num must be numeric")
  }
  round_dp(num, 3)
}


#' numbers 0:10 are converted into words
#' @param x a number
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

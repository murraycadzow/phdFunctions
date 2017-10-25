# number formatters

#create a string formatted to n decimal places
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

# rounds a number to 3 decimal places and returns a string
# convience function
three_dp <- function(num){
  if (!is.numeric(num)) {
    stop("num must be numeric")
  }
  round_dp(num, 3)
}


# numbers 0:10 are converted into words
# @param x
num_to_word <- function(x){

  switch(x,
         'zero' = 0,
         'one' = 1,
         'two' = 2,
         'three' = 3,
         'four' = 4,
         'five' = 5,
         'six' = 6,
         'seven' = 7,
         'eight' = 8,
         'nine' = 9,
         'ten' = 10,
         x
  )
}

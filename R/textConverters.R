# replaces the long statname with the abbrevation that can then be fed into short_to_latex
# which creates the latex glossary command for the stat
stat_to_short <- function(x){
  switch(x,
         Tajima.D = "td",
         Fay.Wu.H = "fwh",
         Fu.Li.D = "fld",
         Fu.Li.F = "flf",
         Zeng.E = "ze",
         x
  )
}

# often used in conjunction with stat_to_short()
short_to_latex <- function(x){
  switch(x,
         td = "\\gls{td}",
         fwh = "\\gls{fwh}",
         fld = "\\gls{fld}",
         flf = "\\gls{flf}",
         ze = "\\gls{ze}",
         x
  )
}


# initial cap
# refer to https://stackoverflow.com/questions/24956546/capitalizing-letters-r-equivalent-of-excel-proper-function
proper <- function(x){
  paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
}

## check if a given year is a leap year
is.leapyear <- function(year) {
  if (year<1582) return(FALSE)
  if (((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
    return(TRUE)
  return(FALSE)
}

## day of year to date in the form of MonthDay conversion
doy2mmdd <- function(doy, leap=FALSE) {
  dom <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dom[3] = 29
  day <- 0
  for (i in 1:length(dom)) {
    if (doy - sum(dom[1:i]) <= 0) {
      i <- i-1
      day <- doy - sum(dom[1:i])
      break
    }
  }
  return(sprintf("%02d%02d",i,day))
}

## date in the form of MonthDay to day of year conversion
mmdd2doy <- function(mmdd, leap=FALSE) {
  mmdd <- as.numeric(mmdd)
  stopifnot(mmdd>100 && mmdd<1232)
  dom <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dom[3] = 29
  return(sum(dom[1:floor(mmdd / 100)]) + mmdd - (floor(mmdd / 100)) * 100)
}

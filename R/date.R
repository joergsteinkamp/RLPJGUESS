## days per month
dpm <- c(31,28,31,30,31,30,31,31,30,31,30,31)

## check if a given year is a leap year
is.leapyear <- function(year, always=FALSE) {
  if (!always && year<1582) return(FALSE)
  if (((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
    return(TRUE)
  return(FALSE)
}

## day of year to date in the form of MonthDay conversion
doy2mmdd <- function(doy, leap=FALSE) {
  dpm <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dpm[3] = 29
  day <- 0
  for (i in 1:length(dpm)) {
    if (doy - sum(dpm[1:i]) <= 0) {
      i <- i-1
      day <- doy - sum(dpm[1:i])
      break
    }
  }
  return(sprintf("%02d%02d",i,day))
}

## date in the form of MonthDay to day of year conversion
mmdd2doy <- function(mmdd, leap=FALSE) {
  mmdd <- as.numeric(mmdd)
  stopifnot(mmdd>100 && mmdd<1232)
  dpm <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dpm[3] = 29
  return(sum(dpm[1:floor(mmdd / 100)]) + mmdd - (floor(mmdd / 100)) * 100)
}

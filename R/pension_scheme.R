# Return pension scheme with target OADR
# start gives starting values (a df with Year and Age columns)
# sim gives simulated populations
# column is which column from OADR estimate to use
# No changes up to 2023.
pension_scheme <- function(sim, start, target, column = "OADR") {
  scheme <- start
  startindex <- which(scheme$Year == 2024)
  startyear <- scheme$Year[startindex]
  for (i in startindex:NROW(scheme)) {
    year <- scheme$Year[i]
    scheme$Age[i] <- scheme$Age[i - 1]
    current <- oadr_sim(
      sim %>% filter(Year == year),
      scheme %>% filter(Year == year)
    )[[column]][1]
    while ((current > target) & diff(scheme$Age[(i - 1):i]) <= 11/12) {
      scheme$Age[i] <- scheme$Age[i] + 1/12
      current <- oadr_sim(
        sim %>% filter(Year == year),
        scheme %>% filter(Year == year)
      )[[column]][1]
    }
  }
  return(scheme)
}

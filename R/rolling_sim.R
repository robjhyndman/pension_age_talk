# t = 2014 - 1950 + 1
# H = forecast horizon for test
# k = years of data required to make a forecast

rolling_sim <- function(mort, fert, mig, pop, t, H, K) {
  ######## Run a rolling forecast origin
  rolling_sim <- NULL
  for (i in seq(t - K - H))
  {
    ausmort.short <- extract.years(mort, years = 1949 + i + (0:(K - 1)))
    ausfert.short <- extract.years(fert, years = 1949 + i + (0:(K - 1)))
    ausmig.short <- extract.years(mig, years = 1949 + i + (0:(K - 1)))
    pop.short <- extract.years(pop, years = 1949 + i + (0:K))
    mort.fit.s <- coherentfdm(ausmort.short)
    mortfs <- forecast(mort.fit.s, H)
    fert.fit.s <- fdm(ausfert.short)
    # Fix problem
    #  miss <- is.na(fert.fit.s$residuals$y)
    #  fert.fit.s$residuals$y[miss] <- 0
    # End fix
    fertfs <- forecast(fert.fit.s, H)
    mig.fit.s <- coherentfdm(ausmig.short)
    migfs <- forecast(mig.fit.s, H)
    aus.sim.s <- pop.sim(
      mort = mortfs, fert = fertfs, mig = migfs,
      firstyearpop = pop.short, N = 3
    )
    # Combine simulation results into one tibble
    dimnames(aus.sim.s[["male"]]) <-
      dimnames(aus.sim.s[["female"]]) <-
      list(
        Age = dimnames(aus.sim.s[[1]])[[1]],
        Year = dimnames(aus.sim.s[[1]])[[2]],
        Rep = dimnames(aus.sim.s[[1]])[[3]]
      )
    rolling_sim <- bind_rows(
      rolling_sim,
      as.tbl_cube(aus.sim.s[["male"]] / 1e3, met_name = "Population") %>%
        as_tibble() %>% mutate(Sex = "Male", id = i),
      as.tbl_cube(aus.sim.s[["female"]] / 1e3, met_name = "Population") %>%
        as_tibble() %>% mutate(Sex = "Female", id = i)
    )
  }
  return(rolling_sim)
}


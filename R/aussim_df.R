make_aussim_df <- function(aus.sim, maxyear = 2050) {
  # Combine simulation results into one tibble
  dimnames(aus.sim[["male"]]) <- dimnames(aus.sim[["female"]]) <- list(
    Age = dimnames(aus.sim[[1]])[[1]],
    Year = as.numeric(dimnames(aus.sim[[1]])[[2]]) + 1,
    Rep = dimnames(aus.sim[[1]])[[3]]
  )
  aussim_df <- bind_rows(
    as.tbl_cube(aus.sim[["male"]] / 1e3, met_name = "Population") %>%
      as_tibble() %>% mutate(Sex = "Male"),
    as.tbl_cube(aus.sim[["female"]] / 1e3, met_name = "Population") %>%
      as_tibble() %>% mutate(Sex = "Female")
  ) %>%
    filter(Year <= maxyear)

  return(aussim_df)
}

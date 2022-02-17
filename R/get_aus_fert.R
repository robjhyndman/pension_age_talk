get_aus_fert <- function(abs_fertility_file) {
  aus.fert <- addb::aus.fertility
  births <- aus.fert$rate$female * aus.fert$pop$female
  # Get latest fertility data from ABS (downloaded 30 April 2020)
  ausf <- readr::read_csv(abs_fertility_file) %>%
    filter(
      Region == "Australia",
      Age %in% paste(15:49)
    ) %>%
    mutate(
      Year = Time,
      Age = as.numeric(Age)
    ) %>%
    select(Year, Age, Measure, Value)

  # Extract births and population values
  newbirths <- ausf %>%
    filter(Measure == "Births") %>%
    mutate(Value = Value * 1000) %>%
    select(-Measure) %>%
    pivot_wider(names_from = Year, values_from = Value) %>%
    select(-Age) %>%
    as.matrix()
  newpop <- ausf %>%
    filter(Measure == "Female population") %>%
    mutate(Value = Value) %>%
    select(-Measure) %>%
    pivot_wider(names_from = Year, values_from = Value) %>%
    select(-Age) %>%
    as.matrix()
  firstyear_abs <- as.numeric(colnames(newbirths)[1])
  rownames(newbirths) <- rownames(newpop) <- 15:49
  colnames(newbirths) <- colnames(newpop) <- firstyear_abs -1 + seq(NCOL(newbirths))

  # Use ABS data where available
  firstyear_births <- as.numeric(colnames(births)[1])
  births <- cbind(births[, seq(firstyear_abs - firstyear_births)], newbirths)
  fpop <- cbind(aus.fert$pop$female[, seq(firstyear_abs - firstyear_births)], newpop)
  # Use populations in ausf rather than aus.pop to get the same rates
  aus.fert$rate$female <- births / fpop
  aus.fert$pop$female <- fpop
  aus.fert$year <- firstyear_births - 1 + seq(NCOL(births))


  return(aus.fert)
}

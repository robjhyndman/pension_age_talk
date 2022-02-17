# Function to produce old age dependency ratio using tibbles
# df is data frame with columns Age, Sex, Year, Population
# pension_age is dataframe with columns Year, Age.
oadr <- function(df, pension) {
  pension <- pension %>%
    dplyr::select(Year, Age) %>%
    rename(Pension = Age)
  df %>%
    left_join(pension, by = "Year") %>%
    mutate(
      # Fill historical years with minimum pension age
      Pension = replace_na(Pension, min(Pension, na.rm = TRUE)),
      # Divide population into three groups
      Group = case_when(
        (Age >= 15 & Age < Pension) ~ "Workers",
        (Age >= Pension) ~ "Pension",
        TRUE ~ "Child"
      ),
      Remainder = Pension - trunc(Pension),
    ) %>%
    group_by(Year) %>%
    mutate(
      Population = case_when(
        Remainder == 0 ~ Population,
        Age == Pension-Remainder ~ Population * Remainder,
        Age == Pension-Remainder + 1 ~ Population + (1-Remainder) * dplyr::lag(Population),
        TRUE ~ Population,
      )
    ) %>%
    ungroup() %>%
    group_by(Year, Group) %>%
    summarise(Population = sum(Population), .groups="keep") %>%
    ungroup() %>%
    pivot_wider(Year, names_from = Group, values_from = Population) %>%
    mutate(OADR = Pension / Workers) %>%
    select(Year, OADR)
}

# As above but df now contains Reps.
oadr_sim <- function(df, pension, level = 80) {
  # Simulated OADR
  df %>%
    group_by(Rep) %>%
    group_modify(~ oadr(.x, pension = pension)) %>%
    group_by(Year) %>%
    summarise(
      Lo = quantile(OADR, prob = (0.5 - level / 200)),
      Hi = quantile(OADR, prob = 1 - (0.5 - level / 200)),
      OADR = mean(OADR),
      .groups = "keep"
    ) %>%
    ungroup()
}

# As above but df now contains Reps and id
calc_oadr_rolling_sim <- function(df, pension, level = 80) {
  # Simulated OADR
  df %>%
    group_by(Rep, id) %>%
    group_modify(~ oadr(.x, pension = pension)) %>%
    group_by(Year, id) %>%
    summarise(
      Lo = quantile(OADR, prob = (0.5 - level / 200)),
      Hi = quantile(OADR, prob = 1 - (0.5 - level / 200)),
      OADR = mean(OADR),
      .groups = "keep"
    ) %>%
    ungroup()
}

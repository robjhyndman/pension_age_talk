aus_pop_df <- function(aus.pop) {
  bind_rows(
    as_tibble(aus.pop$pop$male / 1000) %>%
      mutate(
        Age = aus.pop$age,
        Sex = "Male"
      ) %>%
      pivot_longer(c(-Age, -Sex), names_to = "Year", values_to = "Population"),
    as_tibble(aus.pop$pop$female / 1000) %>%
      mutate(
        Age = aus.pop$age,
        Sex = "Female"
      ) %>%
      pivot_longer(c(-Age, -Sex), names_to = "Year", values_to = "Population"),
  )
}

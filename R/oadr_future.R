estimate_oadr_future <- function(sim, pension_age) {
  bind_rows(
    oadr_sim(sim, filter(pension_age, Policy == "Age65")) %>%
      mutate(Policy = "Age65"),
    oadr_sim(sim, filter(pension_age, Policy == "Current")) %>%
      mutate(Policy = "Current"),
    oadr_sim(sim, filter(pension_age, Policy == "Proposed")) %>%
      mutate(Policy = "Proposed")
  ) %>%
  mutate(
    Policy = factor(Policy, levels = c("Proposed","Current","Age65"), ordered = TRUE)
  )
}

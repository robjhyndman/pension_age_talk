optimal_pension_scheme_plot <- function(pension_age, pension.fc, pension_colors, oadr_target) {
  pension_schemes <- pension_age %>%
    bind_rows(
      tibble(Year = time(pension.fc$mean),
             Age = pension.fc$mean,
             Policy="Optimal")
    ) %>%
    mutate(
      Policy = factor(Policy, levels = c("Optimal","Proposed","Current","Age65"), ordered = TRUE)
    ) %>%
    filter(Year >= 2005)
  pension.fc$x <- ts(rep(65,3), start=2016)
  autoplot(pension.fc, fcol=pension_colors["Optimal"]) +
    geom_line(data= pension_schemes, mapping=aes(x=Year, y=Age, col=Policy, group=Policy),
      lwd=1) +
    xlab("Year") +
    ylab("Minimum pension age") +
    scale_color_manual(
      values = pension_colors,
      labels = c(paste("Pension age to give OADR =",oadr_target),
                 "Proposed policy",
                 "Current policy",
                 "Pension age 65")
    ) +
    scale_fill_manual(
      values = pension_colors,
      labels = c(paste("Pension age to give OADR =",oadr_target),
                 "Proposed policy",
                 "Current policy",
                 "Pension age 65")
    ) +
    guides(col=FALSE, lty = guide_legend("Policy")) +
    ggtitle("") 
}

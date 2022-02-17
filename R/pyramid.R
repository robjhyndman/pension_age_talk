pyramid <- function(history, future) {
  data <- bind_rows(history, future)
  p_male <- data %>%
    filter(Sex == "Male") %>%
    ggplot(aes(x = Age, y = Population, group = Year)) +
    labs(y = "Population ('000)") +
    geom_ribbon(aes(x = Age, ymin = Lower, ymax = Upper, y = Mean), fill = "#7d7def") +
    geom_line(linetype = 2) +
    geom_line(aes(y = Mean), col = "#0000cc") +
    coord_flip() +
    scale_y_reverse(expand = c(0.012, 0.012)) +
    scale_x_continuous(expand = c(0.012, 0.012)) +
    facet_grid(. ~ Sex) 
  p_female <- data %>%
    filter(Sex == "Female") %>%
    ggplot(aes(x = Age, y = Population, group = Year)) +
    labs(y = "Population ('000)") +
    geom_ribbon(aes(x = Age, ymin = Lower, ymax = Upper, y = Mean), fill = "#7d7def") +
    geom_line(linetype = 2) +
    geom_line(aes(y = Mean), col = "#0000cc") +
    coord_flip() +
    scale_y_continuous(expand = c(0.012, 0.012)) +
    theme(
      axis.title.y.left = element_blank(),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank()
    ) +
    scale_x_continuous(position = "top", sec.axis = sec_axis(~.), expand = c(0.012, 0.012)) +
    facet_grid(. ~ Sex)

  (p_male | p_female)
}

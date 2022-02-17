# Autoplot for fmforecast objects
autoplot.fmforecast <- function(data) {
  require(latex2exp)
  p1 <- ggplot(mapping = aes(x = data$age, y = data$model$basis[, 1])) +
    geom_line() +
    xlab("Age (x)") +
    ylab(TeX("$\\mu(x)")) 
  p2 <- ggplot(mapping = aes(x = data$age, y = data$model$basis[, 2])) +
    geom_line() +
    xlab("Age (x)") +
    ylab(TeX("$\\phi_1(x)$"))
  p3 <- ggplot(mapping = aes(x = data$age, y = data$model$basis[, 3])) +
    geom_line() +
    xlab("Age (x)") +
    ylab(TeX("$\\phi_2(x)$"))
  # Fix bug in fertf output
  for(i in 2:3) {
    data$coeff[[i]]$lower <- ts(as.matrix(data$coeff[[i]]$lower))
    data$coeff[[i]]$upper <- ts(as.matrix(data$coeff[[i]]$upper))
    tsp(data$coeff[[i]]$lower) <- tsp(data$coeff[[i]]$upper) <- tsp(data$coeff[[i]]$mean)
  }
  p4 <- autoplot(data$coeff[[2]]) +
    ggtitle("") + xlab("Year (t)") + ylab(TeX("$\\beta_{1,t}$"))
  p5 <- autoplot(data$coeff[[3]]) +
    ggtitle("") + xlab("Year (t)") + ylab(TeX("$\\beta_{2,t}$"))

  print((p1 | p2 | p3) / (plot_spacer() | p4 | p5))
}

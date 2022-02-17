the_plan <-
  drake_plan(

  ## Pension age schemes
  pension_age = tibble(Year = 1920:2050, Age65 = 65) %>%
    mutate(
      Proposed = pmin(70, Age65 + 0.5 * pmax(0, trunc((Year - 2015) / 2))),
      Current = pmin(Proposed, 67),
    ) %>%
    pivot_longer(-Year, values_to = "Age", names_to = "Policy") %>%
    mutate(
      Policy = factor(Policy, levels = c("Proposed","Current","Age65"), ordered = TRUE)
    ),

  # Okabe-Ito colors for pension age schemes
  pension_colors = c(
    Optimal = "#0072B2",
    Proposed = "#E69F00",
    Current = "#D55E00",
    Age65 = "#009E73"
  ),

  # Pension age scheme plot
  pension_age_plot = pension_age %>%
    filter(Year >= 2014) %>%
    ggplot(aes(x = Year, y = Age, group = Policy, lty= Policy)) +
    geom_line() +
    ylab("Minimum pension age") +
    scale_color_manual(
      values = pension_colors,
      labels = c("Proposed further change",
                 "Current policy",
                 "Pension age at 65")
    ),

  # Get population data to 1 Jan 2019
  aus.pop = hmd.pop(country = "AUS",
                    username = username,
                    password = password,
                    label = "Australia") %>%
      extract.ages(ages = 0:100),

  # Get mortality data to 2018
  aus.mort = hmd.mx("AUS", username, password, "Australia") %>%
      extract.ages(ages = 0:100, combine.upper = FALSE) %>%
      extract.years(years = 1950:2100),

  # Get fertility data to 2018
  abs_fertility_file = "./data/FERTILITY_AGE_STATE_30042020143630412.csv",
  aus.fert = get_aus_fert(abs_fertility_file),

  # Compute migration data
  aus.mig = netmigration(mort = aus.mort, fert = aus.fert,
    startyearpop = aus.pop, mfratio = 1.05),

  # Population data frame
  auspop_df = aus_pop_df(aus.pop),

  # SMOOTHING
  ausmort.sm = smooth.demogdata(aus.mort, obs.var = "theoretical"),
  ausfert.sm = smooth.demogdata(aus.fert, obs.var = "theoretical"),
  ausmig.sm = smooth.demogdata(aus.mig),

  # MORTALITY model
  mort.fit = coherentfdm(ausmort.sm),
  mortf = forecast(mort.fit, h = 33),

  # FERTILITY model
  fert.fit = fdm(ausfert.sm),
  fertf = forecast(fert.fit, h = 33),
  fertf_plot = autoplot.fmforecast(fertf),

  # MIGRATION
  mig.fit = coherentfdm(ausmig.sm),
  migf = forecast(mig.fit, h = 33),

  # SIMULATED populations
  aus.sim = pop.sim(mort = mortf, fert = fertf, mig = migf, firstyearpop = aus.pop, N = 2000),
  # Convert to tibble
  aussim_df = make_aussim_df(aus.sim, 2050),

  # Combine simulations to create means and quantiles of future years
  future_pop = aussim_df %>%
    group_by(Sex, Age, Year) %>%
    summarise(
      Mean = mean(Population),
      Lower = quantile(Population, prob = 0.1),
      Upper = quantile(Population, prob = 0.9),
      .groups = "keep"
    ) %>%
    ungroup() %>%
    filter(Year <= 2050),

  # Historical data as a tibble
  history_pop = bind_rows(
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
  ) %>%
    mutate(Year = as.numeric(Year)),

  # Future population simulation
  future_tot_pop = aussim_df %>%
    group_by(Rep, Sex, Year) %>%
    summarise(Population = sum(Population) / 1e3, .groups = "keep") %>%
    group_by(Sex, Year) %>%
    summarise(
      Mean = mean(Population),
      Median = median(Population),
      Lo95 = quantile(Population, prob = 0.025),
      Up95 = quantile(Population, prob = 0.975),
      Lo80 = quantile(Population, prob = 0.1),
      Up80 = quantile(Population, prob = 0.9),
      .groups = "keep"
    ) %>%
    ungroup(),

  population_plot = history_pop %>%
    group_by(Sex, Year) %>%
    summarise(Population = sum(Population) / 1e3) %>%
    ggplot(aes(x = Year, y = Population, group = Sex)) +
    geom_line(size = 1) +
    ylab("Population (millions)") +
    ggtitle("Total population") +
    facet_grid(. ~ Sex) +
    geom_ribbon(aes(y = Mean, ymin = Lo80, ymax = Up80), fill = "#7d7def", data = future_tot_pop) +
    geom_line(data = future_tot_pop, col = "#0000cc", mapping = aes(y = Mean), size = 1),

  pyramid_plot = pyramid(
    history_pop %>% filter(Year == max(Year)),
    future_pop %>% filter(Year == max(history_pop$Year) + 20)
  ),

  # OADR values
  OADR_history = oadr(history_pop, filter(pension_age, Policy == "Current")),
  OADR_future = estimate_oadr_future(aussim_df, pension_age),

  oadr_plots = OADR_future %>%
    mutate(
      Policy = factor(Policy, levels = c("Age65", "Current", "Proposed"), ordered = TRUE)
    ) %>%
    ggplot(aes(x = Year, y = OADR)) +
    #geom_hline(yintercept = oadr_target, col = "gray") +
    geom_ribbon(aes(ymin = Lo, ymax = Hi, fill = Policy, group = Policy),
                alpha = 0.5) +
    geom_line(aes(col = Policy, group = Policy), lwd=1) +
    geom_line(col = "black", size = .75, data = OADR_history) +
    xlab("Year") +
    ylab("Old Age Dependency Ratio") +
    ggtitle("OADR forecasts under different pension schemes") +
    scale_color_manual(
      values = pension_colors[4:2],
      labels = c("Pension age 65",
                 "Current policy",
                 "Proposed policy")
    ) +
    scale_fill_manual(values = pension_colors[4:2],
                      labels = c("Pension age 65",
                                 "Current policy",
                                 "Proposed policy")
    ) +
    guides(fill=FALSE),

  # Target OADR
  oadr_target = 0.23,

  ### find the pension.age scheme with OADR closest to target
  ### find upper boundary of pension ages whose OADR PI contains target
  ### find Lower boundary of pension ages whose OADR PI contains target
  ### Constrained such that solution is in minimum jumps of 1 month
  pension_age_approved = pension_age %>%
    filter(Policy == "Current", Year >= 2019) %>%
    select(-Policy),
  pension_age_lower = pension_scheme(aussim_df, pension_age_approved, oadr_target, "Lo"),
  pension_age_optimal = pension_scheme(aussim_df, pension_age_lower, oadr_target, "OADR"),
  pension_age_upper = pension_scheme(aussim_df, pension_age_optimal, oadr_target, "Hi"),

  pension.fc = structure(
    list(
      mean = ts(pension_age_optimal$Age, start = min(pension_age_optimal$Year)),
      lower = ts(as.matrix(pension_age_lower$Age), start = min(pension_age_optimal$Year)),
      upper = ts(as.matrix(pension_age_upper$Age), start = min(pension_age_optimal$Year)),
      level = 80
    ),
    class = "forecast"
  ),

  proposed_pension_scheme_plot = optimal_pension_scheme_plot(
      pension_age, pension.fc, pension_colors, oadr_target),

  # Final results
  OADR_optimal = oadr_sim(aussim_df, pension_age_optimal),
  OADR_lower = oadr_sim(aussim_df, pension_age_lower),
  OADR_upper = oadr_sim(aussim_df, pension_age_upper),

  proposed_oadr_plot = OADR_optimal %>%
    ggplot(aes(x = Year)) +
    geom_hline(yintercept = oadr_target, color="#0072B2") +
    annotate("text", x=1999, y=oadr_target+.01, label="Target OADR", color="#0072B2") +
    geom_ribbon(aes(ymin = Lo, ymax = Hi), fill = "#7dcfef") +
    geom_line(aes(y = OADR), col = pension_colors["Optimal"], lwd=1) +
    geom_line(data = OADR_history, aes(y = OADR), lwd=1) +
    ggtitle("OADR forecast estimates for the target pension age scheme") +
    ylab("Old age dependency ratio"),

  oadr_upper_lower_plot = bind_rows(
        OADR_upper %>% mutate(Series = "Upper boundary"),
        OADR_lower %>% mutate(Series = "Lower boundary"),
      ) %>%
      ggplot(aes(x = Year)) +
      geom_hline(yintercept = oadr_target, color="#0072B2") +
      annotate("text", x=1999, y=oadr_target+.01, label="Target OADR", color="#0072B2") +
      geom_ribbon(aes(ymin = Lo, ymax = Hi), fill = "#7dcfef") +
      geom_line(aes(y = OADR), col = pension_colors["Optimal"], lwd=1) +
      geom_line(data = OADR_history, aes(y = OADR), lwd=1) +
      facet_grid(. ~ Series) +
      ggtitle("OADR forecasts for boundary pension age schemes") +
      ylab("Old age dependency ratio")

  # Forecast accuracy via tscv
  #rollingsim = rolling_sim(ausmort.sm, ausfert.sm, ausmig.sm, aus.pop,
  #                         t = 2018 - 1950 + 1, H = 25, K = 25),
  #oadr_rolling_sim = calc_oadr_rolling_sim(rollingsim, filter(pension_age, Policy == "Current")),
  #oadr_rolling_rmse_plot = oadr_rolling_sim %>%
  #  group_by(id) %>%
  #  mutate(h = row_number()) %>%
  #  ungroup() %>%
  # select(Year, h, OADR) %>%
  # rename(Forecast = OADR) %>%
  # left_join(OADR_history, by = "Year") %>%
  # mutate(e = OADR - Forecast) %>%
  # group_by(h) %>%
  # summarise(RMSE = sqrt(mean(e^2))) %>%
  # ggplot(aes(x = h, y = RMSE)) +
  # geom_point()
)

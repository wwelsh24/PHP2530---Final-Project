##Visualizations

library(ggplot2)
library(dplyr)

cms_data <- read_csv('Data/final_dataset.csv')
df_missing_if_any <- cms_data %>%
  filter(if_any(everything(), is.na))

complete_data <- cms_data %>% filter(rowSums(is.na(.)) < 1) %>% group_by(State) %>%
  mutate(
    state_mean_avg_wait_time = mean(avg_wait_time),
    state_mean_avg_abandon_rate = mean(avg_abandonment_rate),
    state_mean_total_calls = mean(total_calls),
    state_mean_C1_indicator = mean(C1_indicator),
    state_mean_C2_indicator = mean(C2_indicator),
    state_mean_C3_indicator = mean(C3_indicator),
    state_mean_C4_indicator = mean(C4_indicator),
    state_mean_C5_indicator = mean(C5_indicator),
    state_mean_C6_indicator = mean(C6_indicator),
    state_mean_C7_indicator = mean(C7_indicator),
    state_mean_C8_indicator = mean(C8_indicator),
    state_mean_C9_indicator = mean(C9_indicator),
    state_mean_C10_indicator = mean(C10_indicator),
    state_mean_C11_indicator = mean(C11_indicator),
    state_mean_C12_indicator = mean(C12_indicator),
    state_mean_C13_indicator = mean(C13_indicator),
    state_mean_R1_indicator = mean(R1_indicator),
    state_mean_R2_indicator = mean(R2_indicator),
    state_mean_R3_indicator = mean(R3_indicator),
    state_mean_R4_indicator = mean(R4_indicator),
    state_mean_R5_indicator = mean(R5_indicator),
    state_mean_R6_indicator = mean(R6_indicator),
    state_mean_R7_indicator = mean(R7_indicator),
    state_mean_R8_indicator = mean(R8_indicator),
    state_mean_R9_indicator = mean(R9_indicator),
    state_mean_R10_indicator = mean(R10_indicator),
    personal_income = personal_income/100000,
    state_mean_wait_lag1 = mean(wait_lag1),
    state_mean_personal_income = mean(personal_income),
    state_mean_limited_english_rate = mean(limited_english_rate),
    state_mean_wait_lag10 = mean(wait_lag10),
    state_mean_abandon_lag1 = mean(abandon_lag1),
    state_mean_abandon_lag10 = mean(abandon_lag10),
    state_mean_Unemployment_Rate = mean(Unemployment_Rate)
  )

complete_data <- complete_data %>%
  mutate(
    reporting_date = as.Date(paste0(month_year, "01"), format = "%Y%m%d")
  )


trend_plot <- ggplot(complete_data, aes(x = reporting_date, y = percent_administrative_terminated)) +
  geom_point(color="purple",alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "cyan", size = 1.2) +
  labs(
    title = "Administrative Termination Rate by Month",
    x = "Month-Year",
    y = "Termination Rate (%)"
  ) +
  theme_minimal(base_size = 13) 

trend_plot
ggsave(
  'Time Trend Plot.png')

# Load libraries
library(rstan)
library(kableExtra)
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


complete_data$state_id <- as.integer(factor(complete_data$State))


X <- model.matrix(~ avg_wait_time + state_mean_avg_wait_time +
                    C1_indicator + state_mean_C1_indicator +
                    C2_indicator +
                    C3_indicator + state_mean_C3_indicator +
                    C4_indicator + state_mean_C4_indicator +
                    C5_indicator + state_mean_C5_indicator +
                    C6_indicator +
                    C7_indicator +
                    C8_indicator + state_mean_C8_indicator +
                    C9_indicator +
                    C11_indicator + state_mean_C11_indicator +
                    C13_indicator +
                    R1_indicator + state_mean_R1_indicator +
                    R2_indicator + state_mean_R2_indicator +
                    R4_indicator + state_mean_R4_indicator +
                    R5_indicator + state_mean_R5_indicator +
                    R6_indicator + state_mean_R6_indicator +
                    state_mean_personal_income + personal_income +
                    limited_english_rate +
                    state_mean_Unemployment_Rate + Unemployment_Rate +
                    factor(reporting_period),
                  data = complete_data)[, -1]

y <- complete_data$percent_administrative_terminated

stan_data <- list(
  N = nrow(X),
  K = ncol(X),
  X = X,
  y = y,
  J = length(unique(complete_data$state_id)),
  state_id = complete_data$state_id
)


stan_model_code <- "
data {
  int<lower=0> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
  int<lower=1> J;
  int<lower=1, upper=J> state_id[N];
}
parameters {
  real alpha;
  vector[K] beta;
  vector[J] alpha_state;
  real<lower=0> sigma_state;
  real<lower=0> sigma;
}
model {
  alpha ~ normal(0.5, 0.5);
  beta ~ normal(0, 1);
  alpha_state ~ normal(0, sigma_state);
  sigma_state ~ student_t(3, 0, 2.5);
  sigma ~ student_t(3, 0, 2.5);

  for (n in 1:N)
    y[n] ~ normal(alpha + alpha_state[state_id[n]] + X[n] * beta, sigma);
}
"


fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  seed = 123
)

fit_summary <- summary(fit, pars = "beta", probs = c(0.025, 0.5, 0.975))$summary


fit_summary_df <- as.data.frame(fit_summary)
fit_summary_df$Variable <- beta_names


final_summary <- fit_summary_df %>%
  select(Variable,
         mean = mean,
         mcse = se_mean,
         sd = sd,
         `2.5%` = `2.5%`,
         `50%` = `50%`,
         `97.5%` = `97.5%`,
         n_eff = n_eff,
         Rhat = Rhat)


final_summary %>%
  kbl(
    digits = 4,
    caption = "Posterior Summary of Fixed Effects (with diagnostics)",
    format = "latex",
    booktabs = TRUE,
    col.names = c("Variable", "Mean", "MCSE", "SD", "2.5\\%", "Median", "97.5\\%", "Eff. N", "$\\hat{R}$")
  ) %>%
  save_kable("Multilevel Fit Table.tex",float = FALSE)


posterior <- as.array(fit)
fe_mcmc_trace <- mcmc_trace(posterior, pars = c("beta[1]", "beta[2]", "beta[3]",'beta[4]'), facet_args = list(ncol = 2, nrow = 2))

ggsave('ME trace plot.png')

fe_ac_plot <- mcmc_acf(posterior, pars = c("beta[1]", "beta[2]"))
ggsave('ME AC plot.png')
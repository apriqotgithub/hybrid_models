# Load necessary libraries
library(rstan)
library(dplyr)
library(tidyr)

# Load the data
# data <- read.csv("me21_cleaned_county_2020.csv")
data <- read.csv("me21_cleaned_county_2020_65.csv")

# Convert CNF_MMRY to a binary variable (1 for yes, 0 for no)
data$CNF_MMRY <- ifelse(data$CNF_MMRY == 1, 1, 0)

# Remove rows with missing values
data <- data %>% drop_na(AGE_GROUP, RACE_GROUP, EDUCA, SEX_GROUP, CNF_MMRY, LLCPWT)

# Ensure EDUCA values are within the expected range (1-6)
data <- data %>%
  mutate(EDUCA = case_when(
    EDUCA == 0 ~ 1,
    EDUCA == 9 ~ 2,
    EDUCA == 12 ~ 3,
    EDUCA == 14 ~ 4,
    EDUCA == 16 ~ 5,
    EDUCA == 19 ~ 6,
    TRUE ~ as.numeric(EDUCA)
  ))

# Convert categorical variables to factors
data <- data %>%
  mutate(
    AGE_GROUP = as.factor(AGE_GROUP),
    RACE_GROUP = as.factor(RACE_GROUP),
    EDUCA = as.factor(EDUCA),
    SEX_GROUP = as.factor(SEX_GROUP)
  )

# Create a numeric index for counties
data <- data %>%
  mutate(IMPCTY = as.factor(IMPCTY)) %>%
  mutate(county_idx = as.numeric(IMPCTY))

# Display the first few rows of the dataframe
head(data)

# Data preparation for Stan
N <- nrow(data)
J <- length(unique(data$county_idx))
county <- data$county_idx
y <- data$CNF_MMRY
AGE_GROUP <- as.numeric(data$AGE_GROUP)
RACE_GROUP <- as.numeric(data$RACE_GROUP)
EDUCA <- as.numeric(data$EDUCA)
SEX_GROUP <- as.numeric(data$SEX_GROUP)
LLCPWT <- data$LLCPWT

# List to be passed to Stan
stan_data <- list(N = N, J = J, county = county, y = y, AGE_GROUP = AGE_GROUP, RACE_GROUP = RACE_GROUP, EDUCA = EDUCA, SEX_GROUP = SEX_GROUP, LLCPWT = LLCPWT)

# Define the Stan model as a string
stan_model_code <- "
data {
  int<lower=0> N; // number of individuals
  int<lower=0> J; // number of counties
  int<lower=1,upper=J> county[N]; // county indicator for each individual
  int<lower=0,upper=1> y[N]; // cognitive decline indicator
  int<lower=1,upper=5> AGE_GROUP[N]; // Age group (5 categories)
  int<lower=1,upper=6> RACE_GROUP[N]; // Race group (6 categories)
  int<lower=1,upper=6> EDUCA[N]; // Education level (6 categories)
  int<lower=1,upper=2> SEX_GROUP[N]; // Sex group (2 categories)
  real LLCPWT[N]; // weights
}

parameters {
  real beta_0;
  vector[5] beta_age; // 5 age groups
  vector[6] beta_race; // 6 race groups
  vector[6] beta_educa; // 6 education levels
  vector[2] beta_sex; // 2 sex groups
  vector[J] u; // county-level random effect
  real<lower=0> sigma_u; // standard deviation of random effect
}

model {
  // Priors
  beta_0 ~ normal(0, 10);
  beta_age ~ normal(0, 10);
  beta_race ~ normal(0, 10);
  beta_educa ~ normal(0, 10);
  beta_sex ~ normal(0, 10);
  u ~ normal(0, sigma_u);
  sigma_u ~ cauchy(0, 2.5);
  
  // Likelihood
  for (i in 1:N) {
    target += LLCPWT[i] * bernoulli_logit_lpmf(y[i] | beta_0 +
                           beta_age[AGE_GROUP[i]] +
                           beta_race[RACE_GROUP[i]] +
                           beta_educa[EDUCA[i]] +
                           beta_sex[SEX_GROUP[i]] +
                           u[county[i]]);
  }
}
"

# Compile the Stan model
stan_model <- stan_model(model_code = stan_model_code)

# Fit the Stan model
# fit <- sampling(stan_model, data = stan_data, iter = 500, chains = 4, seed = 123)
# fit_65 <- sampling(stan_model, data = stan_data, iter = 500, chains = 4, seed = 123)
fit_65 <- sampling(stan_model, data = stan_data, iter = 1000, chains = 4, seed = 123, control = list(max_treedepth = 15))


# Print the summary of the model
# print(fit, pars=c("beta_0", "beta_age", "beta_race", "beta_educa", "beta_sex", "sigma_u", "u"))
print(fit_65, pars=c("beta_0", "beta_age", "beta_race", "beta_educa", "beta_sex", "sigma_u", "u"))

# Extract summary of the model parameters
# model_summary <- summary(fit, pars=c("beta_0", "beta_age", "beta_race", "beta_educa", "beta_sex", "sigma_u", "u"))$summary
model_summary_65 <- summary(fit_65, pars=c("beta_0", "beta_age", "beta_race", "beta_educa", "beta_sex", "sigma_u", "u"))$summary
# Convert the summary to a data frame
model_summary_df_65 <- as.data.frame(model_summary_65)

# Save the model summary to a CSV file
# write.csv(model_summary_df, "model_summary_45.csv", row.names = TRUE)
write.csv(model_summary_df, "model_summary_65.csv", row.names = TRUE)

# Extract and analyze results
# posterior_samples <- rstan::extract(fit)
posterior_samples_65 <- rstan::extract(fit_65)

# Compute the linear predictor for each observation
compute_linear_predictor_65 <- function(age_group, race_group, educa, sex_group, county_idx, posterior_samples) {
  # Extract the posterior means of the parameters
  beta_0 <- mean(posterior_samples_65$beta_0)
  beta_age <- apply(posterior_samples_65$beta_age, 2, mean)
  beta_race <- apply(posterior_samples_65$beta_race, 2, mean)
  beta_educa <- apply(posterior_samples_65$beta_educa, 2, mean)
  beta_sex <- apply(posterior_samples_65$beta_sex, 2, mean)
  u <- apply(posterior_samples_65$u, 2, mean)
  
  # Compute the linear predictor
  linear_predictor <- beta_0 +
    beta_age[age_group] +
    beta_race[race_group] +
    beta_educa[educa] +
    beta_sex[sex_group] +
    u[county_idx]
  
  return(linear_predictor)
}

# Apply the logistic function to compute the probability
logistic <- function(x) {
  1 / (1 + exp(-x))
}

# Compute the probabilities for each observation in the dataset
data$linear_predictor_65 <- mapply(compute_linear_predictor_65,
                                age_group = data$AGE_GROUP,
                                race_group = data$RACE_GROUP,
                                educa = data$EDUCA,
                                sex_group = data$SEX_GROUP,
                                county_idx = data$county_idx,
                                MoreArgs = list(posterior_samples = posterior_samples))

# Transform the linear predictor to probability
data$probability <- logistic(data$linear_predictor_65)
head(data)

write.csv(data,"hbsae_linear_probability_65.csv")

# Calculate the average probability by county (IMPCTY)
average_probability_by_county <- data %>%
  group_by(IMPCTY) %>%
  summarise(avg_probability = mean(probability, na.rm = TRUE))

# write.csv(average_probability_by_county,"hbsae_linear_probability_county_45.csv")
write.csv(average_probability_by_county,"hbsae_linear_probability_county_65.csv")

# View the results
print(average_probability_by_county)


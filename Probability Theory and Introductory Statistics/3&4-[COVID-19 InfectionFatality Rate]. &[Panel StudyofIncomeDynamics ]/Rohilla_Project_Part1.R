#install.packages('dplyr')

df <- read.csv('comorbidities (any) expanded.csv')

georgia_cases <- nrow(df)

library(dplyr)
new_df <-df %>%
  filter(df$death==1)

georgia_deaths <- nrow(new_df)
georgia_deaths

observed_rate <- georgia_deaths / georgia_cases
observed_rate

hypothesized_rate <- 0.042
prop_test_result <- prop.test(x = georgia_deaths, n = georgia_cases, p = hypothesized_rate, alternative = "two.sided")


# Store the results in a data frame
prop_test_table <- data.frame(
  Test = "1-sample proportions test",
  Data = "georgia_deaths out of georgia_cases",
  Null_Probability = "hypothesized_rate",
  Chi_Squared = 46.683,
  Degrees_of_Freedom = 1,
  P_Value = 8.347e-12,
  Alternative_Hypothesis = "true p is not equal to 0.042",
  Confidence_Interval_Lower = 0.04392237,
  Confidence_Interval_Upper = 0.04553391,
  Sample_Estimate = 0.04472124
)

# Print the table
print(prop_test_table)
write.csv(prop_test_table, file ="prop_test_table.csv" , row.names = FALSE)

cat("Observed Infection Fatality Rate in Georgia:", observed_rate, "\n")
cat("Hypothesized Infection Fatality Rate:", hypothesized_rate, "\n")
print(prop_test_result)


# Assuming you have the necessary data and libraries loaded

# Data
observed_deaths <- 11360
observed_rate <- 0.04472124
georgia_cases <- 254018
hypothesized_rate <- 0.042

# Calculate the expected deaths based on the hypothesized rate
expected_deaths <- georgia_cases * hypothesized_rate

# Create a data frame for visualization
chart_data <- data.frame(
  Category = c("Observed Deaths", "Expected Deaths"),
  Value = c(observed_deaths, expected_deaths)
)

# Create a pie chart
library(ggplot2)

Observed_ExpectedDeaths <- ggplot(chart_data, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Comparison of Observed and Expected Deaths in Georgia",
       caption = sprintf("Observed Rate: %.5f\nHypothesized Rate: %.5f", observed_rate, hypothesized_rate)) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())


ggsave("Observed_ExpectedDeaths.png", plot = Observed_ExpectedDeaths, width = 8, height = 6)



################
observed_rate <- 0.04472124
hypothesized_rate <- 0.042

prop_test_result <- prop.test(x = georgia_deaths, n = georgia_cases, p = hypothesized_rate)

binom_test_result <- binom.test(x = georgia_deaths, n = georgia_cases, p = hypothesized_rate)

# Create a binary variable representing death (1 for death, 0 for no death)
death_binary <- rep(1, georgia_deaths)  # 1 for death
non_death_binary <- rep(0, georgia_cases - georgia_deaths)  # 0 for no death
binary_data <- c(death_binary, non_death_binary)

# One-sample t-test
t_test_result <- t.test(binary_data, mu = hypothesized_rate)

# Print the results
print(t_test_result)

results_table <- data.frame(
  Test = c("Prop. Test", "Binom. Test", "T Test"),
  P_Value = c(prop_test_result$p.value, binom_test_result$p.value, t_test_result$p.value)
)

print(results_table)
write.csv(results_table, file ="Results_table.csv" , row.names = FALSE)









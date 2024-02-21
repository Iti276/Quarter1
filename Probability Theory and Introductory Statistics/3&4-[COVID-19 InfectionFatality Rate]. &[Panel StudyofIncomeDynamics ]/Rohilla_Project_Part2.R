data <-read.csv('small sample PSID (4 variables).csv')

male <- subset(data,subset=(data$gender=='male'))
female <- subset(data,subset=(data$gender=='female'))

result <- t.test(male$wage2,female$wage2,var.equal=FALSE)
result

# Store the results in a data frame
t_test_table <- data.frame(
  Test = "Welch Two Sample t-test",
  Data = "male$wage2 and female$wage2",
  t_value = 4.8005,
  Degrees_of_Freedom = 40.041,
  P_Value = 2.226e-05,
  Alternative_Hypothesis = "true difference in means is not equal to 0",
  Confidence_Interval_Lower = 793.1459,
  Confidence_Interval_Upper = 1946.5482,
  Mean_of_x = 3484.452,
  Mean_of_y = 2114.605
)

# Print the table
print(t_test_table)
write.csv(t_test_table, file ="t_test_table.csv" , row.names = FALSE)


# Assuming a significance level of 0.01
alpha <- 0.01

if (result$p.value < alpha) {
  cat("Reject the null hypothesis.\n")
} else {
  cat("Fail to reject the null hypothesis.\n")
}

# Assuming you have the necessary data and libraries loaded

# Combine male and female data
all_data <- rbind(male, female)

library(ggplot2)

Avg_Wage_comparison  <-ggplot(all_data, aes(x = factor(year), y = wage2, fill = gender)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Wage Comparison Between Males and Females",
       x = "Year",
       y = "Average Wage",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

ggsave("Avg_Wage_comparison.png", plot = Avg_Wage_comparison, width = 8, height = 6)
#######################################

paired_data <- merge(subset(data, year == 1980), subset(data, year == 1981), by = "id", suffixes = c("_1980", "_1981"))

paired_t_test_result <- t.test(paired_data$wage2_1980/100, paired_data$wage2_1981/100, paired = TRUE)
paired_t_test_result

# Store the results in a data frame
paired_t_test_table <- data.frame(
  Test = "Paired t-test",
  Data = "paired_data$wage2_1980/100 and paired_data$wage2_1981/100",
  t_value = -0.49462,
  Degrees_of_Freedom = 29,
  P_Value = 0.6246,
  Alternative_Hypothesis = "true mean difference is not equal to 0",
  Confidence_Interval_Lower = -1.802046,
  Confidence_Interval_Upper = 1.100176,
  Mean_Difference = -0.3509352
)

# Print the table
print(paired_t_test_table)

write.csv(paired_t_test_table, file ="paired_t_test_table.csv" , row.names = FALSE)


alpha <- 0.01

if (paired_t_test_result$p.value < alpha) {
  cat("Reject the null hypothesis.\n")
} else {
  cat("Fail to reject the null hypothesis.\n")
}
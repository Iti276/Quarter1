####################################PART-1#######################################
installed.packages('tidyverse')
library(tidyverse)

df <- read.csv("PSID (14 variables).csv")

df <- df %>%
  mutate(lnWage = log(wage2))


model <- lm(lnWage ~ experience + education + gender + married, data = df)

model_summary <-summary(model)

coefficients_table <- as.data.frame(model_summary$coefficients)

write.csv(coefficients_table, file = "coefficients_table.csv", row.names = TRUE)
####################################PART-2#######################################
occupation_levels <- unique(df$occupation)

# Function to fit the linear model and extract coefficients for each occupation level
fit_lm_and_extract_coeffs <- function(level) {
  subset_df <- df %>% filter(occupation == level)
  model <- lm(lnWage ~ experience + education + gender + married, data = subset_df)
  model_summary <- summary(model)
  coefficients_table <- as.data.frame(model_summary$coefficients)
  return(coefficients_table)
}

coefficients_by_occupation <- map_df(occupation_levels, fit_lm_and_extract_coeffs)

write.csv(coefficients_by_occupation, file = "coefficients_by_occupation.csv", row.names = TRUE)
coefficients_by_occupation
###################################PART-3########################################
dummy_variables <- model.matrix(~ occupation - 1, data = df)
df_with_dummies <- cbind(df, dummy_variables)
model_with_dummies <- lm(lnWage ~ experience + education + gender + married + occupation, data = df_with_dummies)
summary_with_dummies <-summary(model_with_dummies)

summary_with_dummies_table <- as.data.frame(summary_with_dummies$coefficients)
write.csv(summary_with_dummies_table, file = "summary_with_dummies_table.csv", row.names = TRUE)



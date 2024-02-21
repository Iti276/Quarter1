###Name=Iti Rohilla####Date-6Nov2023###Class-ALY6010:Probability Theory and Introductory Statistics####Assignment-1######################
#install.packages("pacman")
cat("\014") # clears console
rm(list = ls()) # clears global environment try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session

data <- read.csv("comorbidities (any).csv")

#install.packages("dplyr")
library(dplyr)

group_data <- data %>%
  group_by(ethnicity) %>%
  mutate(sum=sum(deaths)) %>%
  arrange(desc(cases)) %>%
  rename(total_cases = cases)

data$cases <- as.integer(data$cases)
data$deaths <- as.integer(data$deaths)
data$comorbidity <- as.factor(data$comorbidity)
data$sex <- as.factor(data$sex)
data$ethnicity <- as.factor(data$ethnicity)
data$race <- as.factor(data$race)
 

#install.packages("stringr")
library(stringr)
data_filtered <- data %>%
  filter(str_detect(race, "American")) %>%
  rename(diseases=comorbidity)
 

total_cases <- sum(data$cases)
total_deaths <- sum(data$deaths)

race_frequencycounts <-  table(data$race)

Blacks_deaths <- sum(data$deaths[data$race=="African-American/ Black"])
proportion=Blacks_deaths/total_deaths


library(dplyr)
african_american_data <- data %>%
  filter(race == "African-American/ Black")

mean_cases <- mean(african_american_data$cases)
sd_cases <- sd(african_american_data$cases)

mean_deaths <- mean(african_american_data$deaths)
sd_deaths <- sd(african_american_data$deaths)

cross_table <- table(data$race, data$deaths > 0)
chi_squared_test <- chisq.test(cross_table)

library(dplyr)
library(ggplot2)


summary_table <- data %>%
  group_by(race) %>%
  summarise(total_cases = sum(cases), total_deaths = sum(deaths))

ggplot(summary_table, aes(x = race, y = total_cases, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "COVID-19 Cases and Deaths by Race",
       x = "Race", y = "Count") +
  scale_fill_brewer(palette = "Set1") +  # Specify the color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Race"))

#write.csv(summary_table, file = "/Users/itirohilla/Documents/Courses/Quarter1/ALY6010-ProbabilityAndStatistics/Assignment/Week1/Rohilla_Project1.csv", row.names = FALSE)

#write.csv(combined_data, file = "/Users/itirohilla/Documents/Courses/Quarter1/ALY6010-ProbabilityAndStatistics/Assignment/Week1/Rohilla_Project1.csv", row.names = FALSE)

library(ggplot2)
#install.packages(tidyr)
library(tidyr)  # Load the tidyr package

total_counts <- data %>%
  group_by(sex) %>%
  summarize(total_cases = sum(cases), total_deaths = sum(deaths))

combined_data <- total_counts %>%
  pivot_longer(cols = c(total_cases, total_deaths), names_to = "Metric", values_to = "Value")


ggplot(combined_data, aes(x = sex, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "COVID-19 Cases and Deaths by Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Metric"))













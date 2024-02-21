cat("\014") # clears console
rm(list = ls()) # clears global environment try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session


123 * 453
5^2 * 40
TRUE & FALSE
TRUE | FALSE
75 %% 10 
75 / 10

first_vector <- c(17,12,-33,5)
first_vector

counting_by_fives <- c(5, 10, 15, 20, 25, 30, 35)
counting_by_fives

second_vector <- seq(10, 30, by = 2)
second_vector

counting_by_fives_with_seq <- seq(5, 35, by = 5)
counting_by_fives_with_seq

third_vector <- rep(first_vector,10)
third_vector

rep_vector <- rep(0,20)
rep_vector

fourth_vector <- 10:1
fourth_vector

counting_vector <- 5:15
counting_vector

grades <- c(96, 100, 85, 92, 81, 72)
grades

bonus_points_added <- grades+3
bonus_points_added

one_to_one_hundred <- 1:100
one_to_one_hundred

reverse_numbers <- seq(100, -100, by = -3)
reverse_numbers


second_vector + 20

second_vector * 20

second_vector >= 20

second_vector != 20 # != means "not equal"

total <- sum(1:100)
total

average_value <- mean(1:100)
average_value

median_value <- median(1:100)
median_value

max_value <- max(1:100)
max_value

min_value <- min(1:100)
min_value

first_value <- second_vector[1]
first_value

first_three_values <- second_vector[1:3]
first_three_values

vector_from_brackets <- second_vector[c(1,5,10,11)]
vector_from_brackets

vector_from_boolean_brackets <- first_vector[c(FALSE, TRUE, FALSE, TRUE)]
vector_from_boolean_brackets

second_vector >= 20

ages_vector <- seq(from = 10, to = 30, by = 2)
ages_vector

ages_vector [ages_vector >= 20]

lowest_grades_removed <- grades [grades >= 85]
lowest_grades_removed

negative_index <- c(3,4)
middle_grades_removed <- grades[- negative_index]
middle_grades_removed

minus_index <- c(5,10)
fifth_vector <- second_vector[-minus_index]
fifth_vector

set.seed(5)
random_vector <- runif(n=10, min = 0, max = 1000)
random_vector

sum_vector <- sum(random_vector)
sum_vector

cumsum_vector <- cumsum(random_vector)
random_vector

mean_vector <- mean(random_vector)
mean_vector

sd_vector <- sd(random_vector)
sd_vector

round_vector <- round(random_vector)
round_vector

sort_vector <- sort(random_vector)
sort_vector

set.seed(5)
random_vector <- rnorm(n=1000, mean = 50, sd = 15)
random_vector


hist(random_vector, breaks = 30, main = "Histogram of Random Values", xlab = "Value")


mean_value <- mean(random_vector)
sd_value <- sd(random_vector)

legend("topright", legend = paste("Mean =", round(mean_value, 2), "\nSD =", round(sd_value, 2)), bg = "white")

library(tidyverse)

filename <- 'ds_salaries.csv'

first_dataframe <- read_csv(filename)

head(first_dataframe)
head(first_dataframe, n = 7)
names(first_dataframe)
smaller_dataframe <- select(first_dataframe, job_title, salary_in_usd) 
smaller_dataframe

better_smaller_dataframe <- arrange (smaller_dataframe, desc(salary_in_usd))
better_smaller_dataframe
better_smaller_dataframe <- filter (smaller_dataframe,salary_in_usd > 80000)
better_smaller_dataframe
better_smaller_dataframe <- mutate(smaller_dataframe, salary_in_euros = salary_in_usd * .94)
better_smaller_dataframe
better_smaller_datafname <- slice (smaller_dataframe, 1, 1, 2, 3, 4, 10, 1)
better_smaller_dataframe

ggplot(data = better_smaller_dataframe, aes(x = job_title, y = salary_in_usd)) +
  geom_col(fill = "blue") +
  xlab("Job Title") + 
  ylab("Salary in US Dollars") +
  labs(title = "Comparison of Jobs ") +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

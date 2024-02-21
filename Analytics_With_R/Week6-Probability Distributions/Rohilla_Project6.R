###Name=Iti Rohilla####Date-29Oct2023###Class-Intro to Analyticsz####Assignment-6######################
#install.packages("pacman")
cat("\014") # clears console
rm(list = ls()) # clears global environment try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session

#install.packages("palmerpenguins")
#install.packages("pacman")

library(pacman)

n=7
x=5
p=0.65
q=1-0.65
prob1_result= factorial(n)/(factorial(n-x)*factorial(x))*((p)^x)*(q^(n-x))


library(dplyr)
n <-7
p <-0.65
q <- 1-0.65
outcomes <- numeric(0)
probabilities <- numeric(0)
for (x in 0:n) {
  outcome <- x
  probability = factorial(n)/(factorial(n-x)*factorial(x))*((p)^x)*(q^(n-x)) 
  outcomes <- c(outcomes, outcome)
  probabilities <- c(probabilities, probability)  
}
prob2_result <- tibble(wins = outcomes, probability = probabilities)
prob2_result


p <- 0.65
q <- 1 - p
n <- 7
prob3_result <- 0
for (x in 0:4) {
  probability <- factorial(n) / (factorial(n - x) * factorial(x)) * (p^x) * (q^(n - x))
  prob3_result <- prob3_result + probability
}
prob3_result


p <- 0.65
q <- 1 - p
n <- 7
prob4_result <- 0
for (x in 3:5) {
  probability <- factorial(n) / (factorial(n - x) * factorial(x)) * (p^x) * (q^(n - x))
  prob4_result <- prob4_result + probability
}
prob4_result

p <- 0.65
q <- 1 - p
n <- 7
prob5_result <- 0
for (x in 5:7) {
  probability <- factorial(n) / (factorial(n - x) * factorial(x)) * (p^x) * (q^(n - x))
  prob5_result <- prob5_result + probability
}
prob5_result


n <- 7
p <- 0.65
prob6_result=n*p
prob6_result

n <- 7
p <- 0.65
q <- 1 - p
prob7_result=n*p*q
prob7_result


set.seed(10)
n <- 7  
p <- 0.65  
random_wins <- rbinom(1000, n, p)
head(random_wins)


prob9_result <- mean(random_wins)
prob9_result

prob10_result <- var(random_wins)
prob10_result


lambda <- 7  
k <- 6  
prob11_result <- dpois(k, lambda)
prob11_result


lambda <- 7 
hours <- 8 
x <- 40
prob12_result <- ppois(x, lambda * hours)
prob12_result


lambda <- 7 * 8  
num_employees <- 5  
x <- 275
prob13_result <- 1 - ppois(x - 1, lambda * num_employees)
prob13_result

lambda <- 7 * 8  
num_employees <- 4  
x <- 275
prob14_result <- 1 - ppois(x - 1, lambda * num_employees)
prob14_result


lambda <- 7 * 8
prob15_result <- qpois(0.9, lambda)
prob15_result


set.seed(15)
lambda <- 7 * 8
random_calls <- rpois(1000, lambda)
head(random_calls)


set.seed(10)
prob17_result <- mean(random_calls)
prob17_result


set.seed(10)
random_calls <- rpois(1000, lambda)
prob18_result <- var(random_calls)
prob18_result


mean_lifespan <- 2000  
std_deviation <- 100   
lower_limit <- 1800    
upper_limit <- 2200    
prob19_result <- pnorm(upper_limit, mean_lifespan, std_deviation) - pnorm(lower_limit, mean_lifespan, std_deviation)
prob19_result


mean_lifespan <- 2000  
std_deviation <- 100   
value <- 2500         
prob20_result <- 1 - pnorm(value, mean_lifespan, std_deviation)
prob20_result


mean_lifespan <- 2000  
std_deviation <- 100  
percentile <- 0.10   
prob21_result <- qnorm(percentile, mean_lifespan, std_deviation, lower.tail = TRUE)
prob21_result <- ceiling(prob21_result)
prob21_result

set.seed(25)
mean_lifespan <- 2000
std_deviation <- 100
population_life_spans <- rnorm(10000, mean = mean_lifespan, sd = std_deviation)
head(population_life_spans)


prob23_result <- mean(population_life_spans)
prob23_result

prob24_result <- sd(population_life_spans)
prob24_result


set.seed(1)
num_samples <- 1000
sample_size <- 100
sample_means <- numeric(num_samples)
for (i in 1:num_samples) {
  sample <- sample(population_life_spans, size = sample_size, replace = TRUE)
  sample_means[i] <- mean(sample)
}
head(sample_means)
prob25_result <- head(sample_means)

par(mar=c(4, 4, 2, 2))  
hist(sample_means, main = "Sample Means Histogram", xlab = "Sample Means", ylab = "Frequency")



prob27_result <- mean(prob25_result)
prob27_result

library(palmerpenguins)
# Filter the data to include only Adélie penguins
adelie_penguins <- subset(penguins, species == "Adelie")
hist(adelie_penguins$flipper_length_mm, main = "Flipper Length Distribution of Adélie Penguins", xlab = "Flipper Length (mm)", ylab = "Frequency")
# Fit a normal distribution curve to the data
curve(dnorm(x, mean = mean(adelie_penguins$flipper_length_mm), sd = sd(adelie_penguins$flipper_length_mm)), col = "blue", lwd = 2, add = TRUE)



library(palmerpenguins)
# Filter the data to include only Gentoo penguins
gentoo_penguins <- subset(penguins, species == "Gentoo")
# Create a scatterplot of flipper length vs. beak depth
plot(gentoo_penguins$flipper_length_mm, gentoo_penguins$bill_depth_mm,
     main = "Scatterplot of Flipper Length vs. Beak Depth for Gentoo Penguins",
     xlab = "Flipper Length (mm)", ylab = "Beak Depth (mm)")
# Calculate the correlation coefficient
correlation <- cor(gentoo_penguins$flipper_length_mm, gentoo_penguins$bill_depth_mm)
cat("Correlation Coefficient:", correlation, "\n")

library(pacman)
p_load(testthat)
test_file("project6_tests.R")



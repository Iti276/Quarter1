###Name=Iti Rohilla####Date-24Oct2023###Class-Intro to Analyticsz####Assignment-5######################
#install.packages("pacman")
cat("\014") # clears console
rm(list = ls()) # clears global environment try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session
library(pacman)

ball <- read.csv("ball-dataset.csv")
ball



freq_color1 <- table(ball$color)
freq_color <- as.data.frame(freq_color1)
colnames(freq_color) <- c("color","counts")
print(freq_color)




freq_label1 <- table(ball$label)
freq_label <- as.data.frame(freq_label1)
colnames(freq_label) <- c("label", "counts")
print(freq_label)


library(ggplot2)
color_chart <- ggplot(freq_color, aes(x = color, y = counts, fill = color)) +
  geom_bar(stat = "identity") +
  labs(title = "Ball Colors Frequency",
       x = "Color",
       y = "Counts") +
  theme_minimal() +
  scale_fill_manual(values = c("blue" = "blue", "green" = "green", "red" = "red", "yellow" = "yellow"))


color_chart


library(ggplot2)

label_chart <- ggplot(freq_label, aes(x = label, y = counts, fill = label)) +
  geom_bar(stat = "identity") +
  labs(title = "Ball Colors Frequency",
       x = "Color",
       y = "Counts") +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "orange", "B" = "darkolivegreen", "C" = "aquamarine", "D" = "deepskyblue", "E" = "darkorchid"))

label_chart



green_count <- freq_color[freq_color$color == "green", "counts"]
Total <- sum(freq_color$counts)
prob6_result <- green_count/Total
prob6_result


blue_count <- freq_color[freq_color$color == "blue", "counts"]
red_count <- freq_color[freq_color$color == "red", "counts"]
prob7_result <- (blue_count+red_count)/Total
prob7_result

A_count <- freq_label[freq_label$label == "A", "counts"]
C_count <- freq_label[freq_label$label == "C", "counts"]
Total <- sum(freq_label$counts)
prob8_result <- (A_count+C_count)/Total
prob8_result


filtered_df <- ball[ball$color == "yellow" & ball$label == "D",]
prob9_result=nrow(filtered_df)/nrow(ball)
prob9_result


yellow_count <- freq_color[freq_color$color == "yellow", "counts"]
total_count_color <- sum(freq_color$counts)
probability_yellow <- yellow_count / total_count_color
D_count <- freq_label[freq_label$label == "D", "counts"]
total_count_label <- sum(freq_label$counts)
probability_D <- D_count / total_count_label
prob10_result <- probability_yellow + probability_D - prob9_result
prob10_result


blue_count <- freq_color[freq_color$color == "blue", "counts"]
Total <- sum(freq_color$counts)
probability_blue <- blue_count/Total
red_count <- freq_color[freq_color$color == "red", "counts"]
Total <- sum(freq_color$counts)
probability_red <- red_count/(Total-1) ##without replacement
prob11_result=probability_blue*probability_red
prob11_result




P_Green_1 <- green_count / Total
P_Green_2 <- (green_count - 1) / (Total - 1)
P_Green_3 <- (green_count - 2) / (Total - 2)
P_Green_4 <- (green_count - 3) / (Total - 3)
prob12_result <- P_Green_1 * P_Green_2 * P_Green_3 * P_Green_4
# Print the result
prob12_result



red_count <- freq_color[freq_color$color == "red", "counts"]
Total <- sum(freq_color$counts)
Blabel_count <- freq_label[freq_label$label == "B", "counts"]
P_Red <- red_count / Total
P_Label_B_given_Red <- Blabel_count / (Total - 1)
prob13_result <- P_Red * P_Label_B_given_Red
print(prob13_result)


filtered_A <- ball[ball$label == "A",]
nrow(filtered_A)
filtered_E <- ball[ball$label == "E",] 
nrow(filtered_E)
filtered_C <- ball[ball$label == "C",] 
nrow(filtered_C)
prob14_result <-nrow(filtered_A)*nrow(filtered_E)*nrow(filtered_C)

prob15_result <- 6*nrow(filtered_A)*nrow(filtered_E)*nrow(filtered_C)


green_counts <- freq_color[freq_color$color == "green", "counts"]
prob16_result <- (green_counts/Total)*(green_counts-1)/(Total-1)*(green_counts-2)/(Total-2)
  
factorial <- function(n) {
  if (n < 0) {
    return(-1)
  } else if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}

print(factorial(0))    # Output: 1
print(factorial(3))    # Output: 6
print(factorial(5))    # Output: 120
print(factorial(-10))  # Output: -1


freq_all1 <- table(ball)
freq_all <- as.data.frame(freq_all1)






# install.packages("tidyverse")
library(dplyr)

coin_outcomes <- expand.grid(first = c("H", "T"),
                             second = c("H", "T"),
                             third = c("H", "T"),
                             fourth = c("H", "T")) %>%
  mutate(across(everything(), as.character))



total_outcomes <- nrow(coin_outcomes)
probability <- 1 / total_outcomes
coin_outcomes <- coin_outcomes %>%
  mutate(probability = probability)

print(coin_outcomes)

library(dplyr)


coin_outcomes <- expand.grid(first = c("H", "T"),
                             second = c("H", "T"),
                             third = c("H", "T"),
                             fourth = c("H", "T")) %>%
  mutate(across(everything(), as.character))


num_combinations <- coin_outcomes %>%
  rowwise() %>%
  mutate(Num_H = sum(c_across(everything()) == "H"))


num_combinations_0H <- sum(num_combinations$Num_H == 0)
num_combinations_1H <- sum(num_combinations$Num_H == 1)
num_combinations_2H <- sum(num_combinations$Num_H == 2)
num_combinations_3H <- sum(num_combinations$Num_H == 3)
num_combinations_4H <- sum(num_combinations$Num_H == 4)

prob_0H <- num_combinations_0H / total_outcomes
prob_1H <- num_combinations_1H / total_outcomes
prob_2H <- num_combinations_2H / total_outcomes
prob_3H <- num_combinations_3H / total_outcomes
prob_4H <- num_combinations_4H / total_outcomes


num_heads_prob <- data.frame(
  Num_Heads = 0:4,
  Probability = c(prob_0H, prob_1H, prob_2H, prob_3H, prob_4H)
)


print(num_heads_prob)

prob21_result <- (num_combinations_3H +1)/ total_outcomes
prob21_result
print(prob21_result)

prob22_result = prob_2H + prob_4H
prob22_result

prob23_result = prob_3H + prob_2H + prob_1H + prob_0H
prob23_result

library(ggplot2)
n_flips <- 4
probabilities <- dbinom(0:n_flips, n_flips, 0.5)
data <- data.frame(Heads = 0:n_flips, Probability = probabilities)
bar_chart <- ggplot(data, aes(x = Heads, y = Probability)) +
  geom_bar(stat = "identity", fill = "turquoise1") +
  xlab("Number of Heads") +
  ylab("Probability") +
  ggtitle("Probability Distribution of Heads for 4 Flips")
print(bar_chart)




p_home = 0.75
p_away = 0.50
n_games = 10
prob25_result = p_home^5 * p_away^5
prob25_result


prob_not_more_than_one = 
  (0.25^5) * (0.50^5) + 
  (choose(5, 1) * p_home * (0.25^4) * (0.50^5)) +
  (choose(5, 0) * (0.25^5) * (0.50^5))


prob26_result = 1 - prob_not_more_than_one
prob26_result

prob27_result = choose(5, 3) * choose(5, 2)
prob27_result




























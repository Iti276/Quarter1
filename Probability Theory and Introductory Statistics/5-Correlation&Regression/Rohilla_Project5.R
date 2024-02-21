#chooseCRANmirror(graphics = FALSE)
#installed.packages("corrplot")
#install.packages("ggplot2")
library(corrplot)
library(ggplot2)
data <- read.csv("Guns_1999.csv")


data_subset <- data[, c('violent', 'murder', 'robbery', 'prisoners', 'afam')]
cor_matrix <- cor(data_subset)
write.csv(cor_matrix, file = "correlation_matrix.csv", row.names = TRUE)

correlation_chart <- corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, title = "Correlation Chart", title.col = "black", mar = c(5, 5, 5, 5))
png("correlation_chart.png", width = 12, height = 10, units = "in", res = 300)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45,addrect = 6)
dev.off()


data_subset2 <- data[, c('violent', 'murder', 'robbery', 'prisoners', 'income')]
cor_matrix2 <- cor(data_subset2)
write.csv(cor_matrix2, file = "correlation_matrix2.csv", row.names = TRUE)

correlation_chart4 <- corrplot(cor_matrix2, method = "number", type = "upper", tl.col = "black", tl.srt = 45, title = "Correlation Chart", title.col = "black", mar = c(5, 5, 5, 5))
png("correlation_chart4.png", width = 12, height = 10, units = "in", res = 300)
corrplot(cor_matrix2, method = "color", type = "upper", tl.col = "black", tl.srt = 45,addrect = 6)
dev.off()
##########################################
model_multi <- lm(violent ~ robbery + prisoners + afam + income + cauc, data = data)
summary(model_multi)

regression_table_multi <- broom::tidy(model_multi)
write.csv(regression_table_multi, file = "regression_table_multi.csv", row.names = FALSE)


model_multi2 <- lm(income ~ robbery + prisoners + violent + murder, data = data)
summary(model_multi2)
regression_table_multi2 <- broom::tidy(model_multi2)
regression_table_multi2
write.csv(regression_table_multi2, file = "regression_table_multi2.csv", row.names = FALSE)


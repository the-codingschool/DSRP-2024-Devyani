install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("pROC")

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pROC)

# Load cleaned data
cleaned_squirrel_data <- read_csv("cleaned_squirrel_data.csv")
cleaned_squirrel_data_filtered <- cleaned_squirrel_data %>% filter(Age != "?" & Age != "Unknown")

cleaned_squirrel_data_filtered$Age <- factor(cleaned_squirrel_data_filtered$Age)
cleaned_squirrel_data_filtered$`Primary Fur Color` <- factor(cleaned_squirrel_data_filtered$`Primary Fur Color`)

cleaned_squirrel_data_filtered <- cleaned_squirrel_data_filtered %>% 
  drop_na(Age, `Primary Fur Color`, Running)

numeric_columns <- sapply(cleaned_squirrel_data_filtered, is.numeric)
cleaned_squirrel_data_filtered[numeric_columns] <- scale(cleaned_squirrel_data_filtered[numeric_columns])

print(names(cleaned_squirrel_data_filtered))

contingency_table <- table(cleaned_squirrel_data_filtered$Age, cleaned_squirrel_data_filtered$Running)
chi_squared_test <- chisq.test(contingency_table)
print(chi_squared_test)

if (chi_squared_test$p.value < 0.05) {
  print("There is a significant association between Age and Running activity.")
} else {
  print("There is no significant association between Age and Running activity.")
}

logistic_model <- glm(Running ~ Age + `Primary Fur Color` + Climbing + Kuks + Quaas + Moans, 
                      data = cleaned_squirrel_data_filtered, family = binomial)
summary(logistic_model)

predicted_probabilities <- predict(logistic_model, type = "response", newdata = cleaned_squirrel_data_filtered)
actual_classes <- as.numeric(cleaned_squirrel_data_filtered$Running)

length(actual_classes)
length(predicted_probabilities)

roc_curve <- roc(actual_classes, predicted_probabilities)
plot.roc(roc_curve, col = "#1f77b4", main = "ROC Curve for Logistic Regression Model")

auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Independent T-Test
t_test_result <- t.test(`Hectare Squirrel Number` ~ Running, data = cleaned_squirrel_data_filtered)
print(t_test_result)

if (t_test_result$p.value < 0.05) {
  print("There is a significant difference in the mean Hectare Squirrel Number between running and non-running squirrels.")
} else {
  print("There is no significant difference in the mean Hectare Squirrel Number between running and non-running squirrels.")
}

# - The logistic regression model assumes a linear relationship between the predictors and the log odds of the response.
# - The chi-squared test assumes that the expected frequencies in each cell of the contingency table are sufficiently large.
# - The independent t-test assumes that the data is normally distributed and that the variances of the two groups are equal.
# - Limitations may include potential biases in the data collection process or missing data.

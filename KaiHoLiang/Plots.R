# Install and load necessary packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")

library(readr)
library(dplyr)
library(ggplot2)

# Load cleaned data
cleaned_squirrel_data <- read_csv("cleaned_squirrel_data.csv")

# Filter out unknown age categories
cleaned_squirrel_data_filtered <- cleaned_squirrel_data %>% filter(Age != "?" & Age != "Unknown")

head(cleaned_squirrel_data)
str(cleaned_squirrel_data)
summary(cleaned_squirrel_data)

# Plot distribution of Age (Pie Chart)
age_distribution <- cleaned_squirrel_data_filtered %>%
  count(Age) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(age_distribution, aes(x = "", y = Percentage, fill = Age)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribution of Age") +
  scale_fill_manual(values = c("Adult" = "#1f77b4", "Juvenile" = "#ff7f0e", "Unknown" = "#2ca02c", "?" = "#d62728")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Plot distribution of Primary Fur Color (Pie Chart)
fur_color_distribution <- cleaned_squirrel_data_filtered %>%
  count(`Primary Fur Color`) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(fur_color_distribution, aes(x = "", y = Percentage, fill = `Primary Fur Color`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribution of Primary Fur Color") +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Plot activity levels (Running) by Age (Stacked Bar Chart)
age_running_distribution <- cleaned_squirrel_data_filtered %>%
  count(Age, Running) %>%
  group_by(Age) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(age_running_distribution, aes(x = Age, y = Percentage, fill = Running)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Running Activity by Age", x = "Age", y = "Percentage") +
  scale_fill_manual(values = c("FALSE" = "#d62728", "TRUE" = "#1f77b4"), name = "Running Activity",
                    labels = c("Not Running", "Running")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Plot activity levels (Running) by Primary Fur Color (Stacked Bar Chart)
fur_color_running_distribution <- cleaned_squirrel_data_filtered %>%
  count(`Primary Fur Color`, Running) %>%
  group_by(`Primary Fur Color`) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(fur_color_running_distribution, aes(x = `Primary Fur Color`, y = Percentage, fill = Running)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Running Activity by Primary Fur Color", x = "Primary Fur Color", y = "Percentage") +
  scale_fill_manual(values = c("FALSE" = "#d62728", "TRUE" = "#1f77b4"), name = "Running Activity",
                    labels = c("Not Running", "Running")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Convert logical to numeric for scatter plot
cleaned_squirrel_data_filtered$Running_numeric <- as.numeric(cleaned_squirrel_data_filtered$Running)
cleaned_squirrel_data_filtered$Climbing_numeric <- as.numeric(cleaned_squirrel_data_filtered$Climbing)

# Plot correlation between Running and Climbing (Scatter Plot)
ggplot(cleaned_squirrel_data_filtered, aes(x = Running_numeric, y = Climbing_numeric)) +
  geom_jitter(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Scatter Plot of Running vs Climbing", x = "Running (0 = No, 1 = Yes)", y = "Climbing (0 = No, 1 = Yes)")

# Aggregate data by Age and Running
age_running_data <- cleaned_squirrel_data_filtered %>%
  group_by(Age, Running) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Plot running activity by Age (Pie Chart) with percentage labels
ggplot(age_running_data, aes(x = "", y = Percentage, fill = Running)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Age) +
  theme_void() +
  labs(title = "Running Activity by Age") +
  scale_fill_manual(values = c("FALSE" = "#d62728", "TRUE" = "#1f77b4"), name = "Running Activity",
                    labels = c("Not Running", "Running")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3)

ggplot(cleaned_squirrel_data, aes(x = `Primary Fur Color`, y = as.numeric(Running), color = `Primary Fur Color`)) +
  geom_jitter(alpha = 0.5, width = 0.3, height = 0.1) +
  theme_minimal() +
  labs(title = "Scatter Plot of Primary Fur Color vs Running", x = "Primary Fur Color", y = "Running (0 = No, 1 = Yes)") +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  theme(legend.position = "none")

# Plot activity levels (Running) by Age (Stacked Bar Chart)
age_running_distribution_all <- cleaned_squirrel_data_filtered %>%
  count(Age, Running) %>%
  group_by(Age) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(age_running_distribution_all, aes(x = Age, y = Percentage, fill = Running)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Running Activity by Age", x = "Age", y = "Percentage") +
  scale_fill_manual(values = c("FALSE" = "#d62728", "TRUE" = "#1f77b4"), name = "Running Activity",
                    labels = c("Not Running", "Running")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))
                       
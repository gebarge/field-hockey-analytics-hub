# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data 
data <- read.csv("field_hockey_data.csv")

# View the first few rows of the data 
head(data)

# Bar plot for Goals For vs Opponent
ggplot(data, aes(x = reorder(opponent, goals_for), y = goals_for)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Goals Scored Against Each Opponent", x = "Opponent", y = "Goals For")

# Scatter plot for Defensive Recovery Rate vs Circle Entries
ggplot(data, aes(x = circle_entries, y = defensive_recovery_rate)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Defensive Recovery Rate vs Circle Entries", x = "Circle Entries", y = "Defensive Recovery Rate")

# Boxplot of Shots on Goal Percentage by Win
ggplot(data, aes(x = factor(win), y = sog_percentage)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Shots on Goal Percentage by Win", x = "Win (1 = Yes, 0 = No)", y = "SOG Percentage")

# Correlation Matrix to identify interesting trends
# Select relevant numeric columns for correlation
numeric_columns <- data %>%
  select(goals_for, goals_against, circle_entries, positive_circle_entries, circle_outcome_rate,
         defensive_recovery_rate, press_success_rate, outlet_success_rate, sog_percentage, shots)

cor_matrix <- cor(numeric_columns, use = "complete.obs")

# Print correlation matrix to console
print(cor_matrix)

# Visualize the correlation matrix with a heatmap
library(reshape2)
cor_data <- melt(cor_matrix)
ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "")

#Other visualizations and correlation trends

ggplot(data, aes(x = positive_circle_entries, y = circle_outcome_rate)) +
  geom_point(color = "purple", size = 3) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(title = "Positive Circle Entries vs Circle Outcome Rate", x = "Positive Circle Entries", y = "Circle Outcome Rate")


ggplot(data, aes(x = press_success_rate, y = defensive_recovery_rate)) +
  geom_point(color = "green", size = 3) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Press Success Rate vs Defensive Recovery Rate", x = "Press Success Rate", y = "Defensive Recovery Rate")

ggplot(data, aes(x = defensive_recovery_rate, y = goals_against)) +
  geom_point(color = "purple", size = 3) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(title = "Defensive Recovery Rate vs Goals Against", x = "Defensive Recovery Rate", y = "Goals Against")

ggplot(data, aes(x = outlet_success_rate, y = goals_against)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Outlet Success Rate vs Goals Against", x = "Outlet Success Rate", y = "Goals Against")

correlation_with_win <- cor(numeric_columns, data$win)
print(correlation_with_win)

ggplot(data, aes(x = sog_percentage, y = circle_outcome_rate)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(title = "SOG% vs Circle Outcome Rate", x = "SOG Percentage", y = "Circle Outcome Rate")

ggplot(data, aes(x = circle_entries, y = goals_for)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  labs(title = "Circle Entries vs Goals Scored", x = "Circle Entries", y = "Goals Scored")



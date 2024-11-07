library(dplyr)
library(tidyr)
library(ggplot2)

# Read the data 
data <- read.csv("fieldhockeyshotdata.csv")

# View the first few rows
head(data)

# Frequency of shot types per player
shot_type_summary <- data %>%
  group_by(Player, Shot.Type) %>%
  summarize(Shot_Count = n()) %>%
  arrange(desc(Shot_Count))

print(shot_type_summary)
View(shot_type_summary)

# Calculate outcome summary for each shot type
shot_outcomes <- data %>%
  group_by(Shot.Type) %>%
  summarize(
    Total_Shots = n(),
    SOG = sum(SOG),
    Goals = sum(Goal),
    Missed = sum(Missed),
    Blocked = sum(Blocked),
    Saved = sum(Save)
  ) %>%
  mutate(Goal_Rate = Goals / Total_Shots)

View(shot_outcomes)
stargazer(shot_outcomes)
# Bar plot of shot types per player
ggplot(shot_type_summary, aes(x = Player, y = Shot_Count, fill = Shot.Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Shot Types per Player", x = "Player", y = "Number of Shots") +
  theme_minimal()

# Stacked bar plot of shot outcomes by type
shot_outcomes_long <- data %>%
  pivot_longer(cols = c(SOG, Missed, Blocked, Save, Goal), names_to = "Outcome", values_to = "Count") %>%
  group_by(Shot.Type, Outcome) %>%
  summarize(Count = sum(Count))

ggplot(shot_outcomes_long, aes(x = Shot.Type, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Shot Outcomes by Type", x = "Shot Type", y = "Count") +
  theme_minimal()

# Calculate goal efficiency for each player
goal_efficiency <- data %>%
  group_by(Player) %>%
  summarize(
    Total_Shots = n(),
    Goals = sum(Goal),
    Goal_Efficiency = Goals / Total_Shots
  ) %>%
  arrange(desc(Goal_Efficiency))

print(goal_efficiency)

# Bar plot of goal efficiency per player
ggplot(goal_efficiency, aes(x = reorder(Player, -Goal_Efficiency), y = Goal_Efficiency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Goal Efficiency per Player", x = "Player", y = "Goal Efficiency") +
  theme_minimal()

# Summarize shot types by player
shot_type_grid <- data %>%
  group_by(Player, Shot.Type) %>%
  summarize(Shot_Count = n()) %>%
  ungroup()

print(shot_type_grid)

# Heatmap of shot types by player
ggplot(shot_type_grid, aes(x = Player, y = Shot.Type, fill = Shot_Count)) +
  geom_tile(color = "white") +  # Adds a white border to separate cells
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(title = "Shot Type Frequency by Player", x = "Player", y = "Shot Type", fill = "Shot Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Shooting Accuracy by Shot Type and Opponent
shot_accuracy <- data %>%
  group_by(Opponent, Shot.Type) %>%
  summarize(
    SOG = sum(SOG),
    Goals = sum(Goal),
    Accuracy = sum(Goal) / sum(SOG)
  )

# View the results
print(shot_accuracy)
View(shot_accuracy)

# Plot Shooting Accuracy by Shot Type
ggplot(shot_accuracy, aes(x = Shot.Type, y = Accuracy, fill = Opponent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Shooting Accuracy by Shot Type and Opponent", x = "Shot Type", y = "Shooting Accuracy") +
  theme_minimal()

# Shot Volume by Shot Type and Player
shot_volume <- data %>%
  group_by(Player, Shot.Type) %>%
  summarize(ShotsAttempted = n())

# Plot Shot Volume by Shot Type and Player
ggplot(shot_volume, aes(x = Shot.Type, y = ShotsAttempted, fill = Player)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Shot Volume by Shot Type for Each Player", x = "Shot Type", y = "Shots Attempted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Shot Efficiency by Opponent and Player
player_efficiency <- data %>%
  group_by(Opponent, Player) %>%
  summarize(
    SOG = sum(SOG),
    Goals = sum(Goal),
    Efficiency = sum(Goal) / sum(SOG)
  ) %>%
  filter(SOG > 0) # Filter out cases where there were no shots on goal

# Plot Shot Efficiency by Opponent and Player
ggplot(player_efficiency, aes(x = Player, y = Efficiency, fill = Opponent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Shooting Efficiency by Player and Opponent", x = "Player", y = "Efficiency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

View(player_efficiency)

# Load required libraries
library(ggplot2)
library(dplyr)

# Plotting the data
ggplot(data_clean, aes(x = Region, y = Cstock)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() 

# Load necessary library


# Summary statistics
summary_stats <- data_clean %>%
  group_by(Region) %>%
  summarize(
    mean_Cstock = mean(Cstock),
    sd_Cstock = sd(Cstock),
    n = n()
  )

print(summary_stats)

# T-test
t_test_result <- t.test(Cstock ~ Region, data = data_clean)

print(t_test_result)




# Load necessary library

df <- data_clean %>%
  mutate(Region = case_when(
    grepl("Kempen", Region, ignore.case = TRUE) ~ "Kempen",
    grepl("Leemstreek", Region, ignore.case = TRUE) ~ "Leemstreek",
    TRUE ~ NA_character_
  ))

df <- df %>% filter(!is.na(Region))

# Check the cleaned data
unique(df$Region)

# Summary statistics for cleaned data
summary_stats <- df %>%
  group_by(Region) %>%
  summarize(
    mean_Cstock = mean(Cstock, na.rm = TRUE),
    sd_Cstock = sd(Cstock, na.rm = TRUE),
    n = n()
  )

print(summary_stats)

# T-test on the cleaned data
t_test_result <- t.test(Cstock ~ Region, data = df)

print(t_test_result)

ggplot(df, aes(x = Region, y = Cstock)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() 
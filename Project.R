
library(dplyr)
library(ggplot2)
library(corrplot)
library(lme4)     
library(car)      


data <- read.csv("D:/First Semester/Project Learning from Data/final data.txt", sep = ";")


print("Original Data:")
print(head(data))


group_number <- 4
rows_to_remove <- c(group_number * 11, group_number^2, 2 * group_number + 21)

print(rows_to_remove)


removed_rows <- data[-rows_to_remove, ]


print(head(removed_rows))


write.csv(removed_rows, "cleaned_data_group4.csv", row.names = FALSE)

print(summary(removed_rows))

qqnorm(removed_rows$Cstock, main = "Q-Q Plot for Cstock")
qqline(removed_rows$Cstock, col = "red", lwd = 2)

ggplot(removed_rows, aes(x = Cstock)) +
  geom_histogram(aes(y = ..density..), bins = 10, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram and Density Plot for Cstock",
       x = "Soil Organic Carbon Stock (Cstock)",
       y = "Density") +
  theme_minimal()

qqnorm(removed_rows$pH, main = "Q-Q Plot for pH")
qqline(removed_rows$pH, col = "red", lwd = 2)

ggplot(removed_rows, aes(x = pH)) +
  geom_histogram(aes(y = ..density..), bins = 10, color = "black", fill = "lightgreen") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram and Density Plot for pH",
       x = "Soil pH",
       y = "Density") +
  theme_minimal()

ggplot(removed_rows, aes(x = Crop, y = Cstock, fill = Crop)) +
  geom_boxplot() +
  labs(
    title = "Cstock by Crop Type",
    x = "Crop Type",
    y = "Soil Organic Carbon Stock (MgC/ha)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

library(corrplot)


numeric_data <- removed_rows %>%
  select(Lutum, Sand, Silt, pH, C, Cstock, Years)


cor_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(
  cor_matrix, 
  method = "color",           
  type = "upper",             
  tl.col = "black",           
  tl.srt = 45,                
  title = "Correlation Heatmap for Numeric Variables", 
  addCoef.col = "black",      
  number.cex = 0.7,          
  col = colorRampPalette(c("blue", "white", "red"))(200),  
  mar = c(0, 0, 1, 0)        
)



ggplot(removed_rows, aes(x = C, y = Cstock)) +
  geom_point(aes(color = C), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Cstock vs. C", x = "C (%)", y = "Soil Organic Carbon Stock (MgC/ha)") +
  theme_minimal()

ggplot(removed_rows, aes(x = Silt, y = Cstock)) +
  geom_point(aes(color = Crop), size = 3, alpha = 0.8) +  # Slight transparency for clarity
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +  # Add confidence interval with a dashed line
  scale_color_brewer(palette = "Set2") +  # A clean and visually appealing color palette
  labs(
    title = "Scatterplot of Cstock vs. Silt",
    subtitle = "Relationship of Soil Organic Carbon Stock with Silt content",
    x = "Silt (%)",
    y = "Soil Organic Carbon Stock (MgC/ha)",
    color = "Crop Type"  # Add a clear legend title
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center and bolden the title
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),  # Add subtitle with subdued color
    axis.title = element_text(face = "bold"),  # Bold axis titles
    legend.position = "right"  # Place legend on the right
  )



full_linear_model <- lm(Cstock ~ Crop + Region + Lutum + Sand + Silt + pH + Years, data = removed_rows)

print("Full Linear Model Summary:")
summary(full_linear_model)


linear_model <- lm(Cstock ~ Crop + Region + Lutum + Silt +  pH + Years, data = removed_rows)

print("Simplified Linear Model Summary:")
summary(linear_model)

print("VIF Scores for Simplified Model:")
print(vif(linear_model))

par(mfrow = c(2, 2)) 
plot(linear_model)    


par(mfrow = c(1, 1))



anova_model <- aov(Cstock ~ Crop + Region, data = removed_rows)

print("ANOVA Model Summary:")
summary(anova_model)

# -------------------------------------------
# 6. Predictions and Report Export
# -------------------------------------------

# Add predictions to the cleaned data
removed_rows$Predicted_Cstock <- predict(linear_model, newdata = removed_rows)
removed_rows

# Export the cleaned data with predictions
write.csv(removed_rows, "cleaned_data_with_predictions.csv", row.names = FALSE)

# Save linear model summary to a text file
sink("linear_model_summary.txt")
summary(linear_model)
sink()

# Save ANOVA model summary to a text file
sink("anova_model_summary.txt")
summary(anova_model)
sink()


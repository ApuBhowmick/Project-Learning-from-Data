# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(MASS)    
library(corrplot) 
library(car)      

# Load data
data <- read.csv("D:/First Semester/Project Learning from Data/final data.txt", sep = ";")

# Inspect the data
head(data)
print(data)

# Define group number and rows to remove
group_number <- 4
rows_to_remove <- c(group_number * 11, group_number^2, 2 * group_number + 21)

# Print rows to remove for verification
print(rows_to_remove)

# Remove specified rows
removed_rows <- data[-rows_to_remove, ]

# Print first few rows of the modified data
print(head(removed_rows))
print(removed_rows)

#Check how many null we have
sum(rowSums(is.na(removed_rows)) > 0)

which(rowSums(is.na(removed_rows)) > 0)


#Alternative method to remove null values

indNA <- !is.na(data_clean$Cstock) & 
  !is.na(data_clean$Lutum) & 
  !is.na(data_clean$Silt) & 
  !is.na(data_clean$Sand) & 
  !is.na(data_clean$pH) & 
  !is.na(data_clean$Crop) & 
  !is.na(data_clean$Years) &
  !is.na(data_clean$Cstock) &
  !is.na(data_clean$C)

Y <- data_clean$Cstock[indNA]
clean_data <- as.matrix(data_clean[indNA, c("Lutum", "Silt", "Sand", "pH", "Crop", "C", "Cstock", "Years")])

print(clean_data)




# Remove rows with missing values
data_clean <- na.omit(removed_rows)

# Verify the cleaned data
print(data_clean)

summary(data_clean)

# Q-Q Plot for Carbon Stock
qqnorm(data_clean$Cstock)
qqline(data_clean$Cstock, col = "red")
title(main = "Q-Q Plot for Carbon Stock")


# Q-Q Plot for pH
qqnorm(data_clean$pH)
qqline(data_clean$pH, col = "red")
title(main = "Q-Q Plot for pH")

# Check for deviation and apply log transformation
data_clean$log_Cstock <- log(data_clean$Cstock + 1)  # Adding 1 to avoid log(0)

# Q-Q plot for log-transformed Carbon Stock
qqnorm(data_clean$log_Cstock)
qqline(data_clean$log_Cstock, col = "red")
title(main = "Q-Q Plot for Log-Transformed Carbon Stock")

#Boxplot 
ggplot(data_clean, aes(y = Cstock)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Carbon Stock (Cstock)", y = "Cstock") +
  theme_minimal()

# Identify outliers using the IQR method for log-transformed Carbon Stock
Q1 <- quantile(data_clean$log_Cstock, 0.25, na.rm = TRUE)
Q3 <- quantile(data_clean$log_Cstock, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Calculate lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter out outliers
data_clean <- subset(data_clean, log_Cstock >= lower_bound & log_Cstock <= upper_bound)

# Verify the data after outlier removal
print(head(data_clean))
print(data_clean)

#QQ plot
qqnorm(data_clean$log_Cstock)
qqline(data_clean$log_Cstock, col = "red")
title(main = "Q-Q Plot for Log-Transformed Carbon Stock (After Outlier Removal)")

#Boxplot
ggplot(data_clean, aes(y = Cstock)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Carbon Stock (Cstock)", y = "Cstock") +
  theme_minimal()

#Histogram

ggplot(data_clean, aes(x = Cstock)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightgreen") +
  labs(title = "Histogram of Carbon Stock (Cstock)",
       x = "Cstock",
       y = "Frequency") +
  theme_minimal()


#remove again

final_remove <- na.omit(data_clean)
print(final_remove)

#QQ plot
qqnorm(final_remove$log_Cstock)
qqline(final_remove$log_Cstock, col = "red")
title(main = "Q-Q Plot for Log-Transformed Carbon Stock (After Outlier Removal)")

#Boxplot
ggplot(final_remove, aes(y = Cstock)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Carbon Stock (Cstock)", y = "Cstock") +
  theme_minimal()

#Histogram
ggplot(final_remove, aes(x = Cstock)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightgreen") +
  labs(title = "Histogram of Carbon Stock (Cstock)",
       x = "Cstock",
       y = "Frequency") +
  theme_minimal()

# Summary statistics by Region and Crop
data_summary <- final_remove %>%
  group_by(Region, Crop) %>%
  summarise(
    mean_Cstock = mean(Cstock, na.rm = TRUE),
    sd_Cstock = sd(Cstock, na.rm = TRUE),
    mean_pH = mean(pH, na.rm = TRUE),
    sd_pH = sd(pH, na.rm = TRUE),
    mean_Lutum = mean(Lutum, na.rm = TRUE),
    sd_Lutum = sd(Lutum, na.rm = TRUE),
    mean_Silt = mean(Silt, na.rm = TRUE),
    sd_Silt = sd(Silt, na.rm = TRUE),
    mean_Sand = mean(Sand, na.rm = TRUE),
    sd_Sand = sd(Sand, na.rm = TRUE)
  )

# Print summary statistics
print(data_summary)

# Correlation matrix for selected numerical columns
cor_matrix <- cor(final_remove[, c("Lutum", "Silt", "Sand", "pH", "C", "Cstock")], use = "complete.obs")

# Correlation plot
corrplot(cor_matrix, method = "circle")

# Build linear regression model
model <- lm(Cstock ~ Lutum + Silt + Sand + pH + Crop + Years, data = final_remove)

# Summary of the model
summary(model)

# Diagnostic plots for the model
par(mfrow = c(2, 2))
plot(model)

# Two-way ANOVA model with interaction between Crop and Region
anova_model <- aov(log_Cstock ~ Crop * Region, data = final_remove)

# Summary of the ANOVA model
summary(anova_model)

# Tukey's HSD post-hoc test
TukeyHSD(anova_model)

# Linear model for VIF calculation
lm_model <- lm(log_Cstock ~ Crop * Region, data = data_clean)

# Calculate VIF for the regression model
vif_values <- vif(lm_model)
print(vif_values)


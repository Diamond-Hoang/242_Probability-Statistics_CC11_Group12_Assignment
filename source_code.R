library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(car)
library(corrplot)
install.packages("BSDA")  
library(BSDA)             


raw_data <- read.csv("D:/XSTK/tourism_dataset_5000.csv")
head(raw_data)
#####################################Data Preprocessing#####################################
#creating data with main variables
new_data <- raw_data[,c("Age","Preferred.Tour.Duration","Accessibility",
                       "Site.Name","Tour.Duration","Tourist.Rating","System.Response.Time",
                       "Recommendation.Accuracy","VR.Experience.Quality","Satisfaction")]

#check NA
anyNA(new_data)

#check negative values
any(new_data$Age < 0)
any(new_data$Preferred.Tour.Duration < 0)
any(new_data$Tour.Duration < 0)
any(new_data$Tourist.Rating < 0)
any(new_data$System.Response.Time < 0)
any(new_data$Recommendation.Accuracy < 0)
any(new_data$VR.Experience.Quality < 0)
any(new_data$Satisfaction < 0)

# find outliners
find_outliers <- function(data, variable) {
  # Calculate the first and third quartiles, and the IQR
  Q1 <- quantile(data[[variable]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[variable]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Define the outlier and extreme outlier thresholds
  lower_threshold <- Q1 - 1.5 * IQR
  upper_threshold <- Q3 + 1.5 * IQR
  extreme_lower_threshold <- Q1 - 3 * IQR
  extreme_upper_threshold <- Q3 + 3 * IQR
  
  # Identify outliers and extreme outliers
  outliers <- data[[variable]][data[[variable]] < lower_threshold | data[[variable]] > upper_threshold]
  extreme_outliers <- data[[variable]][data[[variable]] < extreme_lower_threshold | data[[variable]] > extreme_upper_threshold]
  
  # Check if there are valid outliers
  if (length(outliers) == 0) {
    print("All values lie within the acceptable range of outliers.")
  } else {
    # If extreme outliers exist, print them
    if (length(extreme_outliers) > 0) {
      print(paste("Extreme outliers are:", toString(extreme_outliers)))
    } else {
      print("All values lie within the acceptable range of outliers.")
    }
  }
}
find_outliers(new_data, "Age")
find_outliers(new_data, "Preferred.Tour.Duration")
find_outliers(new_data, "Tour.Duration")
find_outliers(new_data, "Tourist.Rating")
find_outliers(new_data, "System.Response.Time")
find_outliers(new_data, "Recommendation.Accuracy")
find_outliers(new_data, "VR.Experience.Quality")
find_outliers(new_data, "Satisfaction")

#check duplicated data
table(duplicated(new_data))
main_data = new_data

#####################################Descriptive Statistics#####################################
# Use lapply() to apply the summary() function to each variable
custom_summary <- function(x) {
  # Use the summary function to get basic parameters
  summary_stats <- summary(x)
  # Calculate standard deviation 
  std_dev <- sd(x, na.rm = TRUE)
  # Combine results from summary and standard deviation
  c(summary_stats, Std_Dev = std_dev)
}
stat <- lapply(main_data, custom_summary)

# Print results
print(stat)

table(main_data$Accessibility)
table(main_data$Site.Name)

#histogram
numeric <- main_data[,c("Age","Preferred.Tour.Duration","Tour.Duration","Tourist.Rating","System.Response.Time","VR.Experience.Quality","Satisfaction")]
numeric_long <- gather(numeric) %>%
  mutate(key = str_replace_all(key, "\\.", " "))  # remove dots in facet labels

ggplot(numeric_long, aes(value)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "black") +
  facet_wrap(~key, scales = "free") +
  labs(y = "frequency", x = NULL)

#boxplot
plot_boxplot <- function(df, x_col, y_col) {
  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(
      title = paste("Boxplot of", y_col, "grouped by", x_col),
      x = x_col,
      y = y_col
    ) +
    theme_minimal()
}

plot_boxplot(main_data, "Accessibility", "Satisfaction")
plot_boxplot(main_data, "Site.Name", "Satisfaction")

#Draw correlation chart
correlation_matrix <- cor(numeric)
# Draw correlation graph
corrplot(correlation_matrix, method = "circle", 
         type = "full", order = "hclust",
         tl.col = "black", tl.srt = 45, 
         diag = TRUE, 
         addCoef.col = "black")

#####################################Statistical Inference#####################################

# One-sample z-test
one_sample <- z.test(main_data$Satisfaction, mu = 3.5, sigma.x = sd(main_data$Satisfaction, na.rm = TRUE), alternative = "greater")
print(one_sample)

# Two-sample z-test
satisfaction_accessible <- subset(main_data, Accessibility == "True")$Satisfaction
satisfaction_inaccessible <- subset(main_data, Accessibility == "False")$Satisfaction
two_sample <- z.test(satisfaction_accessible, satisfaction_inaccessible, sigma.x = sd(satisfaction_accessible, na.rm = TRUE), sigma.y = sd(satisfaction_inaccessible, na.rm = TRUE))
print(two_sample)


main_data <- main_data %>%
  mutate(Tourist_Rating_Group = case_when(
    Tourist.Rating >= 1 & Tourist.Rating < 2 ~ "Very Low",
    Tourist.Rating >= 2 & Tourist.Rating < 3 ~ "Low",
    Tourist.Rating >= 3 & Tourist.Rating < 4 ~ "Moderate",
    Tourist.Rating >= 4 & Tourist.Rating <= 5 ~ "High"
  ))

# Convert to factor
main_data$Tourist_Rating_Group <- as.factor(main_data$Tourist_Rating_Group)

# Levene’s Test 
print(leveneTest(Satisfaction ~ Tourist_Rating_Group, data = main_data))

# Kruskal-Wallis test (non-parametric alternative)
kruskal_result <- kruskal.test(Satisfaction ~ Tourist_Rating_Group, data = main_data)
print(kruskal_result)

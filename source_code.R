library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(car)

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

#pairplot
pairs(numeric,col = "#6F8FAF", main = "PAIRPLOT")

#####################################Statistical Inference#####################################
#normality
satisfaction <- main_data$Satisfaction

# Set seed for reproducibility
set.seed(42)
# Generate 10000 sample means , each from a sample of size 30
sample_means_30 <- replicate (10000 , mean(sample(satisfaction, size = 30, replace = TRUE)))

# Convert to data frame for ggplot
df <- data.frame(sample_means_30 = sample_means_30)

ggplot(df, aes(x = sample_means_30)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  labs(title = "Sampling Distribution of Sample Mean (n=30)",
       x = "Sample Mean of Satisfaction",
       y = "Frequency") +
  theme_minimal()

set.seed(42)
# Generate 10000 sample means , each from a sample of size 1000
sample_means_1000 <- replicate (10000 , mean(sample(satisfaction, size = 1000, replace = TRUE)))

# Convert to data frame for ggplot
df <- data.frame(sample_means_1000 = sample_means_1000)

ggplot(df, aes(x = sample_means_1000)) +
  geom_histogram(bins = 60, fill = "salmon", color = "black") +
  labs(title = "Sampling Distribution of Sample Mean (n=1000)",
       x = "Sample Mean of Satisfaction",
       y = "Frequency") +
  theme_minimal()

set.seed(42)
# Generate 10000 sample means , each from a sample of size 1000
sample_means_5000 <- replicate (10000 , mean(sample(satisfaction, size = 5000, replace = TRUE)))

# Convert to data frame for ggplot
df <- data.frame(sample_means_5000 = sample_means_5000)

ggplot(df, aes(x = sample_means_5000)) +
  geom_histogram(bins = 120, fill = "salmon", color = "black") +
  labs(title = "Sampling Distribution of Sample Mean (n=5000)",
       x = "Sample Mean of Satisfaction",
       y = "Frequency") +
  theme_minimal()

#normal qqplot
qqnorm(sample_means_30)
qqline(sample_means_30, col="red")

qqnorm(sample_means_1000)
qqline(sample_means_1000, col="red")

qqnorm(sample_means_5000)
qqline(sample_means_5000, col="red")

# One-sample t-test (Satisfaction vs. 3.5)
one_sample <- t.test(main_data$Satisfaction, mu = 3.5, alternative = "greater")
print(one_sample)
#qt(p=0.05, df=5000-1, lower.tail=FALSE)

# Two-sample t-test
satisfaction_accessible <- subset(main_data, Accessibility == "True")$Satisfaction
satisfaction_inaccessible <- subset(main_data, Accessibility == "False")$Satisfaction

satisfaction_combined <- c(satisfaction_accessible, satisfaction_inaccessible)
group <- factor(c(rep("Accessible", length(satisfaction_accessible)), rep("Inaccessible", length(satisfaction_inaccessible))))
leveneTest(satisfaction_combined ~ group)

two_sample <- t.test(satisfaction_accessible, satisfaction_inaccessible, var.equal = TRUE)
print(two_sample)
#qt(p=0.05/2, df=5000-2, lower.tail=FALSE)

#ANOVA
# Create age group variable
main_data <- main_data %>%
  mutate(Age_Group = case_when(
    Age < 30 ~ " <30 ",
    Age >= 30 & Age < 40 ~ " 30 to 39 ",
    Age >= 40 & Age < 50 ~ " 40 to 49 ",
    Age >= 50 & Age < 60 ~ " 50 to 59 ",
    Age >= 60 ~ "60+ "
  ))
main_data$Age_Group <- as.factor(main_data$Age_Group)

print(leveneTest(Satisfaction ~ Age_Group, data = main_data))

# Perform ANOVA test
anova_result <- aov(Satisfaction ~ Age_Group, data = main_data)

# View ANOVA results
print(summary(anova_result))

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(Satisfaction ~ Age_Group, data = main_data)

# View Kruskal-Wallis test result
print(kruskal_result)

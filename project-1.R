###################################################################
            ######------ PRE - PROCESSING -----########
###################################################################

# Load data

data <- read.csv("D:/r studio/project/weatherAUS.csv")

# Load the required Packages

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

# Remove unnecessary columns

data <- select(data, -c("Date", "Location", "RISK_MM"))

# Remove missing values

data <- na.omit(data)

# Replace missing values with column median and scale the data

cols <- c("MinTemp", "MaxTemp", "Rainfall", "Evaporation", "Sunshine", 
          "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm", "Humidity9am",
          "Humidity3pm", "Pressure9am", "Pressure3pm", "Cloud9am", "Cloud3pm", 
          "Temp9am", "Temp3pm")
data[ ,cols] <- apply(data[ ,cols], 2, function(x) ifelse(is.na(x), median(x,
                                                                           na.rm = TRUE), x))
data[ ,cols] <- scale(data[ ,cols])

summary(data)

# Create a scatterplot matrix

ggplot(data, aes(x = Rainfall)) +
  geom_histogram() +
  labs(x = "Rainfall (scaled)", y = "Count")

# Calculate the proportion of days where it rained vs. did not rain

prop_rain <- data %>%
  group_by(RainTomorrow) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

# Create a pie chart of the proportion of days where it rained vs. did not rain

ggplot(prop_rain, aes(x = "", y = prop, fill = RainTomorrow)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Rain Tomorrow") +
  theme_void()

# Create a strip chart of minimum temperature values

stripchart(data$MinTemp, method = "jitter", pch = 20, col = "steelblue", main = "Strip Chart of Minimum Temperature")

# Create a normal QQ plot of minimum temperature values

ggplot(data, aes(sample = MinTemp)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal QQ Plot of Minimum Temperature") +
  theme_classic()

# Create a boxplot of minimum temperature values

ggplot(data, aes(y = MinTemp)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5) +
  labs(x = "", y = "Minimum Temperature", title = "Boxplot of Minimum Temperature") +
  theme_classic()


###################################################################
             ######------ MODEL -----########
###################################################################
# Load the required libraries

library(randomForest)

# Split the dataset into training and testing sets

set.seed(123)
train_idx <- sample(nrow(data), 0.7 * nrow(data))
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Random Forest Model

train_data$RainTomorrow <- as.factor(train_data$RainTomorrow)

rf_model <- randomForest(RainTomorrow ~ ., data = train_data, importance = TRUE)
rf_pred <- predict(rf_model, test_data[, -24])
rf_accuracy <- sum(rf_pred == test_data$RainTomorrow) / nrow(test_data)
cat("Random Forest Model Accuracy:", rf_accuracy, "\n")

library(ggplot2)

# Get the variable importance

var_imp <- importance(rf_model)
var_imp_df <- data.frame(Variable = row.names(var_imp), Importance = var_imp[, 1])

# Create a barplot of variable importance

ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) + 
  geom_bar(stat = "identity") + 
  xlab("Variable") + 
  ylab("Importance") +
  ggtitle("Random Forest Variable Importance")


###################################################################
              ######------ CLUSTERING -----########
###################################################################

library(cluster)

# Select only numeric columns for clustering

num_cols <- sapply(train_data, is.numeric)
num_cols[which(names(num_cols) %in% c("RainToday", "MaxTemp", "RainTomorrow"))] <- FALSE
clustering_data <- train_data[, num_cols]
clustering_data <- na.omit(clustering_data)

# Scale the data

scaled_data <- scale(clustering_data)

# Determine the number of clusters using the elbow method

wss <- sapply(1:10, function(k) {
  kmeans(scaled_data, k, nstart = 10)$tot.withinss
})
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# Perform k-means clustering with chosen number of clusters

k <- 3
kmeans_model <- kmeans(scaled_data, k, nstart = 10)

# Add cluster labels to original dataset

train_data$cluster <- kmeans_model$cluster

# Load the required libraries
library(ggplot2)

# Plot the clustering results
ggplot(train_data, aes(x = MinTemp, y = MaxTemp, color = factor(cluster))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(x = "Minimum Temperature", y = "Maximum Temperature", color = "Cluster") +
  theme_minimal()


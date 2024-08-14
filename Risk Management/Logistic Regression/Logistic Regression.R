# Load necessary libraries
library(dplyr)
library(caret)

# Load data
data <- read.csv("C:\\Users\\DELL\\OneDrive\\Bureau\\german.csv")

# Data preprocessing
data <- data %>%
  mutate_if(is.factor, as.numeric)  # Convert categorical variables to numeric

# Define features and target variable
X <- data %>% select(-Creditability)
y <- data$Creditability

# Split the dataset
set.seed(42)
trainIndex <- createDataPartition(y, p = .8, 
                                  list = FALSE, 
                                  times = 1)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

# Fit logistic regression model
model <- glm(y_train ~ ., data = X_train, family = binomial)

# Predictions
y_pred <- predict(model, newdata = X_test, type = "response")
y_pred_class <- ifelse(y_pred > 0.5, 1, 0)

# Evaluation
confusionMatrix(as.factor(y_pred_class), as.factor(y_test))
roc_auc <- pROC::roc(y_test, y_pred)
plot(roc_auc)
print(paste("ROC-AUC:", auc(roc_auc)))






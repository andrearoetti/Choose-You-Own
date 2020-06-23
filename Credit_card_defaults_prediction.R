#### INTRODUCTION ####

# Installing and loading necessary packages

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(lattice)) install.packages('lattice')
if (!require(caret)) install.packages('caret')
if (!require(readxl)) install.packages('readxl')
if (!require(gam)) install.packages('gam')
library(tidyverse)
library(lattice)
library(caret)
library(readxl)
library(gam)

# File reading

credit_card <- read_xlsx("data/default of credit card clients.xlsx")


#### DATA CLEANING ####

# First data inspection

head(credit_card)

nrow(credit_card)
n_distinct(credit_card$...1)

# Removing first column, saving and removing first row

credit_card <- credit_card[,-1]
col_description <- credit_card[1,]
credit_card <- credit_card[-1,]

# Exploring column names

class(col_description)
t(col_description)

# Formatting columns as factors/numbers

credit_card[,-c(2,3,4,24)] <- as_tibble(sapply(credit_card[,-c(2,3,4,24)], as.numeric))
credit_card$X2 <- as.factor(credit_card$X2)
credit_card$X3 <- as.factor(credit_card$X3)
credit_card$X4 <- as.factor(credit_card$X4)
credit_card$Y <- as.factor(credit_card$Y)

# Creating train and test partitions

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = credit_card$Y, times = 1,p = 0.2, list = FALSE)
historic <- credit_card[-test_index,]
validation <- credit_card[test_index,]


#### EXPLORATORY DATA ANALYSIS ####

# Preliminary summary and statistics

summary(historic)

mean(historic$Y == "1")
mean(historic$X2 == "1")
mean(historic$X4 == "1")

# Plot and table for gender effect

historic %>% group_by(X2,Y) %>% summarize(n = n()) %>% ggplot() + geom_col(aes(X2,n, fill = Y))
historic %>% group_by(X2) %>% summarize(rate = mean(Y == "1"), n = n())

# Plot and table for educational level effect

historic %>% group_by(X3,Y) %>% summarize(n = n()) %>% ggplot() + geom_col(aes(X3,n, fill = Y))
historic %>% group_by(X3) %>% summarize(rate = mean(Y == "1"), n = n())

# Plot and table for marital status effect

historic %>% group_by(X4,Y) %>% summarize(n = n()) %>% ggplot() + geom_col(aes(X4,n, fill = Y))
historic %>% group_by(X4) %>% summarize(rate = mean(Y == "1"), n = n())

# Plot and table for age effect

historic %>% group_by(X5,Y) %>% summarize(n = n()) %>% ggplot(aes(X5, n, color = Y)) + geom_smooth(span = 0.4) + geom_point()
historic %>% group_by(Y) %>% summarize(rate = mean(X5), n = n())

# Plot and table for credit card limit effect

historic %>% group_by(X1,Y) %>% summarize(n = n()) %>% ggplot(aes(X1, n, color = Y)) + geom_smooth(span = 0.3) + geom_point() + scale_x_continuous(trans='log2')
historic %>% group_by(Y) %>% summarize(rate = mean(X1), n = n())


#### MODEL ####

# Setting the benchmark

zeros <- sample(0, nrow(historic), replace = TRUE, prob = 1)
benchmark <- mean(zeros == historic$Y)
benchmark

# Creating the new predictors

historic <- historic %>% mutate(sum_delays = X6 + X7 + X8 + X9 + X10 + X11,
                                billed_tot = X12 + X13 + X14 + X15 + X16 + X17,
                                paid_tot = X18 + X19 + X20 + X21 + X22 + X23)

# Fitting 5 models to all the predictors

models <- c("glm", "lda", "naive_bayes", "svmLinear", "gamLoess")
fits <- lapply(models, function(model){
               train(Y ~ X1 + X2 + X3 + X4 + sum_delays + billed_tot + paid_tot,
               method = model, data = historic)})
fits

# Tuning gamLoess model

gridloess <- expand.grid(span = seq(0.2, 0.6, len = 5), degree = 1)
train_gamloess <- train(Y ~ X1 + X2 + X3 + X4 + sum_delays + billed_tot + paid_tot,
                        method = "gamLoess",
                        data = historic,
                        tuneGrid = gridloess)
ggplot(train_gamloess, highlight = TRUE)

# Training LDA model seprately

train_lda <- train(Y ~ X1 + X2 + X3 + X4 + sum_delays + billed_tot + paid_tot,
                   method = "lda",
                   data = historic)

# Fitting naive Bayes and Random Forest to only one parameter

models2 <- c("naive_bayes", "rf")
fits2 <- lapply(models2, function(model){
                train(Y ~ sum_delays,
                      method = model,
                      data = historic)})
fits2

# Tuning naive Bayes model

gridbayes <- expand.grid(laplace = 0,
                         usekernel = TRUE,
                         adjust = seq(1, 5, len = 10))
train_bayes <- train(Y ~ sum_delays,
                     method = "naive_bayes",
                     data = historic,
                     tuneGrid = gridbayes)
ggplot(train_bayes, highlight = TRUE)

# Tuning Random Forest model

gridrf <- data.frame(mtry = seq(2, 150, len = 7))
train_rf <- train(Y ~ sum_delays,
                  method = "rf",
                  data = historic,
                  nodesize = 1,
                  tuneGrid = gridrf)
ggplot(train_rf, highlight = TRUE)


#### RESULT ####

# Adding the new predictors to validation set

validation <- validation %>% mutate(sum_delays = X6 + X7 + X8 + X9 + X10 + X11,
                                    billed_tot = X12 + X13 + X14 + X15 + X16 + X17,
                                    paid_tot = X18 + X19 + X20 + X21 + X22 + X23)

# Creating the predictions, calculating accuracy and kappa

pred_lda <- predict(train_lda, validation)
pred_gamloess <- predict(train_gamloess, validation)
pred_bayes <- predict(train_bayes, validation)
pred_rf <- predict(train_rf, validation)

acc_lda <- confusionMatrix(data = pred_lda,
                           reference = validation$Y)$overall["Accuracy"]
acc_gamloess <- confusionMatrix(data = pred_gamloess,
                                reference = validation$Y)$overall["Accuracy"]
acc_bayes <- confusionMatrix(data = pred_bayes,
                             reference = validation$Y)$overall["Accuracy"]
acc_rf <- confusionMatrix(data = pred_rf,
                          reference = validation$Y)$overall["Accuracy"]

k_lda <- confusionMatrix(data = pred_lda,
                         reference = validation$Y)$overall["Kappa"]
k_gamloess <- confusionMatrix(data = pred_gamloess,
                              reference = validation$Y)$overall["Kappa"]
k_bayes <- confusionMatrix(data = pred_bayes,
                           reference = validation$Y)$overall["Kappa"]
k_rf <- confusionMatrix(data = pred_rf,
                        reference = validation$Y)$overall["Kappa"]

# Final table with target, accuracy and kappa

target <- mean(validation$Y == "0")
accuracies <- tibble(method = c("target", "LDA", "GamLoess", "naive Bayes", "Random Forest"),
                     accuracy = c(target, acc_lda, acc_gamloess, acc_bayes, acc_rf),
                     kappa = c(0, k_lda, k_gamloess, k_bayes, k_rf))
accuracies %>% knitr::kable()


#### CONCLUSION ####

# Variables importance for Random Forest model if trained on all the predictors

train_rf2 <- train(Y ~ X1 + X2 + X3 + X4 + sum_delays + billed_tot + paid_tot,
                   method = "rf",
                   data = historic,
                   nodesize = 1)
varImp(train_rf2)

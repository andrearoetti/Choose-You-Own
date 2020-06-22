## Introduction ##

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(lattice)) install.packages('lattice')
if (!require(caret)) install.packages('caret')
if (!require(readxl)) install.packages('readxl')
library(tidyverse)
library(lattice)
library(caret)
library(readxl)

credit_card <- read_xlsx("data/default of credit card clients.xlsx")

head(credit_card)

nrow(credit_card)
n_distinct(credit_card$...1)

credit_card <- credit_card[,-1]
col_description <- credit_card[1,]
credit_card <- credit_card[-1,]

class(col_description)
t(col_description)

credit_card[,-c(2,3,4,24)] <- as_tibble(sapply(credit_card[,-c(2,3,4,24)], as.numeric))
credit_card$X2 <- as.factor(credit_card$X2)
credit_card$X3 <- as.factor(credit_card$X3)
credit_card$X4 <- as.factor(credit_card$X4)
credit_card$Y <- as.factor(credit_card$Y)

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = credit_card$Y, times = 1,p = 0.2, list = FALSE)
historic <- credit_card[-test_index,]
validation <- credit_card[test_index,]

summary(historic)

mean(historic$Y == "1")
mean(historic$X2 == "1")
mean(historic$X4 == "1")

historic %>% group_by(X2,Y) %>% summarize(n = n()) %>% ggplot() + geom_col(aes(X2,n, fill = Y))
historic %>% group_by(X2) %>% summarize(rate = mean(Y == "1"), n = n())

historic %>% group_by(X3,Y) %>% summarize(n = n()) %>% ggplot() + geom_col(aes(X3,n, fill = Y))
historic %>% group_by(X3) %>% summarize(rate = mean(Y == "1"), n = n())

historic %>% group_by(X4,Y) %>% summarize(n = n()) %>% ggplot() + geom_col(aes(X4,n, fill = Y))
historic %>% group_by(X4) %>% summarize(rate = mean(Y == "1"), n = n())

historic %>% group_by(X5,Y) %>% summarize(n = n()) %>% ggplot(aes(X5, n, color = Y)) + geom_smooth(span = 0.4) + geom_point()
historic %>% group_by(Y) %>% summarize(rate = mean(X5), n = n())

historic %>% group_by(X1,Y) %>% summarize(n = n()) %>% ggplot(aes(X1, n, color = Y)) + geom_smooth(span = 0.3) + geom_point() + scale_x_continuous(trans='log2')
historic %>% group_by(Y) %>% summarize(rate = mean(X1), n = n())




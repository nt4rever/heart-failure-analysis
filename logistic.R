rm(list = ls())

library(tidyverse)
library(caTools)
library(caret)

require(ggiraph)
require(ggiraphExtra)
# read data from csv file
df <- read.csv("heart.csv")

df <- transform(df, Sex = as.numeric(as.factor(Sex)))
df <- transform(df, ChestPainType = as.numeric(as.factor(ChestPainType)))
df <- transform(df, RestingECG = as.numeric(as.factor(RestingECG)))
df <- transform(df, ExerciseAngina = as.numeric(as.factor(ExerciseAngina)))
df <- transform(df, ST_Slope = as.numeric(as.factor(ST_Slope)))


split <- sample.split(df$HeartDisease, SplitRatio = 0.8)
Train <- subset(df, split == TRUE)
Test <- subset(df, split == FALSE)

fit <- glm(HeartDisease ~ ., data = Train, family = binomial)
summary(fit)

predict_res <- predict(fit, type = "response", newdata = Test)
table(Test$HeartDisease, predict_res > 0.5)

Test$Prediction <- 
(68+91)/nrow(Test)
(68+14)/nrow(Test)

library(ROCR)
ROCRpred <- prediction(predict_res, Test$HeartDisease)
ROCperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCperf, colorize = TRUE, print.cutoffs.at=seq(0,1, by=0.1), text.adj=c(-0.2, 1.7))

as.numeric(performance(ROCRpred, "auc")@y.values)

pred_ <- as.factor(ifelse(predict(fit, type = "response", newdata = Test)>0.5,1,0))


confusionMatrix(pred_, as.factor(Test$HeartDisease))

ggPredict(fit,interactive=TRUE)

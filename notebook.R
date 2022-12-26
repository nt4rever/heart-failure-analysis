library(caret)
library(ROCR)
library(caTools)

df <- read.csv("heart.csv")
# remove all NA values
df <- na.omit(df)
# remove duplicate rows
df <- distinct(df)

# normalize data
df_model <- data.frame(df)
df_model <- transform(df_model, Sex = as.numeric(as.factor(Sex)))
df_model <-
  transform(df_model, ChestPainType = as.numeric(as.factor(ChestPainType)))
df_model <-
  transform(df_model, RestingECG = as.numeric(as.factor(RestingECG)))
df_model <-
  transform(df_model, ExerciseAngina = as.numeric(as.factor(ExerciseAngina)))
df_model <-
  transform(df_model, ST_Slope = as.numeric(as.factor(ST_Slope)))

split <- sample.split(df_model$HeartDisease, SplitRatio = 0.8)
Train <- subset(df_model, split == TRUE)
Test <- subset(df_model, split == FALSE)

model_logistic <-
  glm(HeartDisease ~ ., data = Train, family = binomial)

summary(model_logistic)
predict(model_logistic, type = "response", newdata = data.frame(Age = 48, Sex = 1, ChestPainType =1, RestingBP = 138, Cholesterol = 214, FastingBS = 0,RestingECG = 2, MaxHR= 108, ExerciseAngina = 2, Oldpeak = 1.5, ST_Slope=2))
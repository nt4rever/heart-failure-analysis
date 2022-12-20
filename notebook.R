rm(list = ls())

# import library
library(tidyverse)
library(ggExtra)

# read data from csv file
df <- read.csv("heart.csv")

ChestPainType_vs_Sex <-
  df %>% select(ChestPainType, Sex) %>% group_by(ChestPainType, Sex)  %>% summarise(Count = n(),
                                                                                    .groups = 'drop')
ChestPainType_vs_Sex$Pct = round(ChestPainType_vs_Sex$Count / sum(ChestPainType_vs_Sex$Count),
                                 2) * 100

ggplot(data = ChestPainType_vs_Sex, aes(x = Sex, fill = ChestPainType, y = Pct)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
  label = paste(Pct , "%", sep = ""),
  x = Sex,
  y = Pct + 1,
  group = ChestPainType
),
position = position_dodge(width = .9)) + theme_minimal() + labs(title = "Regardless of the proportion of Males and Females,\n Men have high ASY compared with Women, and the pattern is different.")

RestingECG_vs_Sex <-
  df %>% select(RestingECG, Sex) %>% group_by(RestingECG, Sex)  %>% summarise(Count = n(),
                                                                              .groups = 'drop')
RestingECG_vs_Sex$Pct <-
  round(RestingECG_vs_Sex$Count / sum(RestingECG_vs_Sex$Count), 2) * 100

ExerciseAngina_vs_Sex <-
  df %>% select(ExerciseAngina, Sex)  %>% group_by(ExerciseAngina, Sex)  %>% summarise(Count = n(),   .groups = 'drop')

ExerciseAngina_vs_Sex$Pct <- round(ExerciseAngina_vs_Sex$Count / sum(ExerciseAngina_vs_Sex$Count), 2)*100

ggplot(data = ExerciseAngina_vs_Sex, aes(x = Sex, fill = ExerciseAngina, y = Pct)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
  label = paste(Pct , "%", sep = ""),
  x = Sex,
  y = Pct + 1,
  group = ExerciseAngina
),
position = position_dodge(width = .9)) + theme_minimal() + labs(title = "Almost a similar pattern between Men and Women. (ExerciseAngina)")

ST_Slope_vs_Sex <-
  df %>% select(ST_Slope, Sex)  %>% group_by(ST_Slope, Sex)  %>% summarise(Count = n(),   .groups = 'drop')
ST_Slope_vs_Sex$Pct <- round(ST_Slope_vs_Sex$Count / sum(ST_Slope_vs_Sex$Count), 2)*100


Cholesterol_plot <- ggplot(df) + geom_point(mapping = aes(x = Age, y = Cholesterol, color = Sex))
ggMarginal(Cholesterol_plot, type = "histogram",  groupColour = TRUE, groupFill = TRUE)


heart_dft_chol_n0 <- filter(df, Cholesterol> 0)

ggplot(heart_dft_chol_n0) + geom_point(aes(x = Age, y = Cholesterol, color = Sex)) + geom_smooth(aes(x = Age, y = Cholesterol, color = Sex),method = "lm", formula = y ~ x)
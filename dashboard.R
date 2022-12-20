# library
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(ggExtra)
library(caret)
library(ROCR)

# read data from csv file
df <- read.csv("heart.csv")
# remove all NA values
df <- na.omit(df)
# remove duplicate rows
df <- distinct(df)

# -----------plot chart------------
# age plot
age_plot <- ggplot(data = df, aes(x = Age)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "Age Histogram Plot", x = "Age", y = "Count") +
  theme_minimal()

# sex plot
sex_df <- df %>%
  group_by(Sex) %>%
  summarise(Count = n(),
            .groups = 'drop')
sex_plot <-
  ggplot(sex_df, aes(x = Sex, y = Count, fill = Sex)) + geom_bar(stat = "identity") + geom_text(aes(label = Count, y = Count +
                                                                                                      20)) + theme_minimal() + labs(title = "Almost 80% percent of the gender category are Males, ~21% Females")

# ChestPainType_vs_Sex
ChestPainType_vs_Sex <-
  df %>% select(ChestPainType, Sex) %>% group_by(ChestPainType, Sex)  %>% summarise(Count = n(),
                                                                                    .groups = 'drop')
ChestPainType_vs_Sex$Pct = round(ChestPainType_vs_Sex$Count / sum(ChestPainType_vs_Sex$Count),
                                 2) * 100
ChestPainType_vs_Sex_plot <-
  ggplot(data = ChestPainType_vs_Sex, aes(x = Sex, fill = ChestPainType, y = Pct)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = paste(Pct , "%", sep = ""),
    x = Sex,
    y = Pct + 1,
    group = ChestPainType
  ),
  position = position_dodge(width = .9)) + theme_minimal() + labs(title = "Regardless of the proportion of Males and Females,\n Men have high ASY compared with Women, and the pattern is different") + theme(plot.title = element_text(hjust = 0.5))

# RestingECG_vs_Sex
RestingECG_vs_Sex <-
  df %>% select(RestingECG, Sex) %>% group_by(RestingECG, Sex)  %>% summarise(Count = n(),
                                                                              .groups = 'drop')
RestingECG_vs_Sex$Pct <-
  round(RestingECG_vs_Sex$Count / sum(RestingECG_vs_Sex$Count), 2) * 100
RestingECG_vs_Sex_plot <-
  ggplot(RestingECG_vs_Sex, aes(x = Sex, y = Pct, fill = RestingECG)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = paste(Pct , "%", sep = ""),
    x = Sex,
    y = Pct + 1,
    group = RestingECG
  ),
  position = position_dodge(width = .9)) + theme_minimal() + labs(title = "Men and Women have somehow same pattern of RestingEC") + theme(plot.title = element_text(hjust = 0.5))

ExerciseAngina_vs_Sex <-
  df %>% select(ExerciseAngina, Sex)  %>% group_by(ExerciseAngina, Sex)  %>% summarise(Count = n(),   .groups = 'drop')

ExerciseAngina_vs_Sex$Pct <-
  round(ExerciseAngina_vs_Sex$Count / sum(ExerciseAngina_vs_Sex$Count),
        2) * 100

ExerciseAngina_vs_Sex_plot <-
  ggplot(data = ExerciseAngina_vs_Sex, aes(x = Sex, fill = ExerciseAngina, y = Pct)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = paste(Pct , "%", sep = ""),
    x = Sex,
    y = Pct + 1,
    group = ExerciseAngina
  ),
  position = position_dodge(width = .9)) + theme_minimal() + labs(title = "Almost a similar pattern between Men and Women. (ExerciseAngina)") + theme(plot.title = element_text(hjust = 0.5))

ST_Slope_vs_Sex <-
  df %>% select(ST_Slope, Sex)  %>% group_by(ST_Slope, Sex)  %>% summarise(Count = n(),   .groups = 'drop')
ST_Slope_vs_Sex$Pct <-
  round(ST_Slope_vs_Sex$Count / sum(ST_Slope_vs_Sex$Count), 2) * 100

ST_Slope_vs_Sex_plot <-
  ggplot(data = ST_Slope_vs_Sex, aes(x = Sex, fill = ST_Slope, y = Pct)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = paste(Pct , "%", sep = ""),
    x = Sex,
    y = Pct + 1,
    group = ST_Slope
  ),
  position = position_dodge(width = .9)) + theme_minimal() + labs(title = "A different pattern between Men and Women (ExerciseAngina)") + theme(plot.title = element_text(hjust = 0.5))

Cholesterol_plot <-
  ggplot(df) + geom_point(mapping = aes(x = Age, y = Cholesterol, color = Sex))

heart_dft_chol_n0 <- filter(df, Cholesterol > 0)
heart_dft_chol_n0_plot <-
  ggplot(heart_dft_chol_n0) + geom_point(aes(x = Age, y = Cholesterol, color = Sex)) + geom_smooth(aes(x = Age, y = Cholesterol, color = Sex),
                                                                                                   method = "lm",
                                                                                                   formula = y ~ x)


HeartDisease_df <-
  df %>% select(HeartDisease)  %>% group_by(HeartDisease) %>%
  summarise(Count = n(),
            .groups = 'drop')

HeartDisease_df$HeartDisease <-
  factor(
    HeartDisease_df$HeartDisease,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
HeartDisease_plot <-
  ggplot(HeartDisease_df,
         aes(x = HeartDisease, y = Count, fill = HeartDisease)) + geom_bar(stat = "identity") + geom_text(aes(label = Count, y = Count +
                                                                                                                10)) + theme_minimal() + labs(title = "Acceptably balanced target variable")


HeartDisease_by_Sex_df <-
  df %>% select(Sex, HeartDisease) %>% group_by(Sex, HeartDisease) %>%  summarise(Count = n(), .groups = 'drop')

HeartDisease_by_Sex_df$HeartDisease <-
  as.factor(HeartDisease_by_Sex_df$HeartDisease)

HeartDisease_by_Sex_plot <-
  ggplot(data = HeartDisease_by_Sex_df, aes(x = Sex, fill = HeartDisease, y = Count)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = Count,
    y = Count + 10,
    group = HeartDisease
  ),
  position = position_dodge(width = .9)) + theme_minimal() + theme_minimal()

# ----------------end plot-------

# -------------Logistic regression-------------
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

# shiny app
source("./ui.R")

server <- function(input, output) {
  output$dataset <- renderDT(
    df,
    class = 'cell-border stripe',
    # filter = "top",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      stringsAsFactors = FALSE
    )
  )
  
  output$strDataset <- renderPrint({
    str(df)
    summary(df)
  })
  
  output$age_plot <- renderPlot(age_plot)
  
  output$sex_plot <- renderPlot(sex_plot)
  output$ChestPainType_vs_Sex_plot <-
    renderPlot(ChestPainType_vs_Sex_plot)
  output$RestingECG_vs_Sex_plot <-
    renderPlot(RestingECG_vs_Sex_plot)
  output$ExerciseAngina_vs_Sex_plot <-
    renderPlot(ExerciseAngina_vs_Sex_plot)
  output$ST_Slope_vs_Sex_plot <- renderPlot(ST_Slope_vs_Sex_plot)
  output$Cholesterol_point_plot <- renderPlot({
    ggMarginal(
      Cholesterol_plot,
      type = "histogram",
      groupColour = TRUE,
      groupFill = TRUE
    )
  })
  output$heart_dft_chol_n0_plot <-
    renderPlot(heart_dft_chol_n0_plot)
  output$HeartDisease_plot <- renderPlot(HeartDisease_plot)
  output$HeartDisease_by_Sex_plot <-
    renderPlot(HeartDisease_by_Sex_plot)
  
  output$strDfModel <- renderPrint({
    str(df_model)
  })
  
  output$datasetModel <- renderDT(
    df_model,
    class = 'cell-border stripe',
    # filter = "top",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      stringsAsFactors = FALSE
    )
  )
  
  output$summaryLogisticModelAllVar <- renderPrint({
    summary(model_logistic)
  })
  
  output$predictModelLogistic <- renderPrint({
    pred_ <- predict(
      fit, type = "response", newdata = Test
    )
    predict_res <-
      as.factor(ifelse(pred_ > 0.5, 1, 0))
    
    
    ROCRpred <- prediction(pred_, Test$HeartDisease)
    ROCperf <- performance(ROCRpred, "tpr", "fpr")
    output$logistic_plot <- renderPlot({
      plot(
        ROCperf,
        colorize = TRUE,
        print.cutoffs.at = seq(0, 1, by = 0.1),
        text.adj = c(-0.2, 1.7)
      )
    })
    confusionMatrix(data = predict_res,
                    reference = as.factor(Test$HeartDisease))
  })
}

shinyApp(ui, server)
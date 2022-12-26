# LIBRARY
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(ggExtra)
library(caret)
library(ROCR)
library(caTools)
library(equatiomatic)
# library(epiDisplay)

# ----DATATSET----
# read data from csv file
df <- read.csv("heart.csv")
# remove all NA values
df <- na.omit(df)
# remove duplicate rows
df <- distinct(df)
# ----END DATASET----

# ----CHART----
# 1. Age variable plot
age_plot <- ggplot(data = df, aes(x = Age)) +
  geom_histogram(color = "darkblue",
                 fill = "lightblue",
                 bins = 30) +
  labs(title = "Age falls between 50 and 65 years old, the mean is 53.5 years old", x = "Age", y = "Count") +
  theme_minimal()

# 2. Sex variable plot
sex_df <- df %>%
  group_by(Sex) %>%
  summarise(Count = n(),
            .groups = 'drop')
sex_plot <-
  ggplot(sex_df, aes(x = Sex, y = Count, fill = Sex)) + geom_bar(stat = "identity") + geom_text(aes(label = Count, y = Count +
                                                                                                      20)) + theme_minimal() + labs(title = "Almost 80% percent of the gender category are Males, ~20% Females")

# 3. HeartDisease variable plot
HeartDisease_df <-
  df %>% select(HeartDisease)  %>% group_by(HeartDisease) %>%
  summarise(Count = n(),
            .groups = 'drop')
HeartDisease_df$Pct <-
  round(HeartDisease_df$Count / sum(HeartDisease_df$Count) , 2) * 100
HeartDisease_df$HeartDisease <-
  factor(
    HeartDisease_df$HeartDisease,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
HeartDisease_plot <-
  ggplot(HeartDisease_df,
         aes(x = HeartDisease, y = Count, fill = HeartDisease)) + geom_bar(stat = "identity") + geom_text(aes(label = Count, y = Count + 10)) + theme_minimal() + labs(title = "Acceptably balanced target variable")

HeartDisease_df <- HeartDisease_df %>%
  arrange(desc(HeartDisease)) %>%
  mutate(lab.ypos = cumsum(Pct) - 0.6 * Pct)

HeartDisease_plot_pie <-
  ggplot(HeartDisease_df, aes(x = "", y = Pct, fill = HeartDisease)) + geom_bar(stat = "identity") + coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = paste(Pct , "%", sep = "")), color = "#3e3e3e") + theme_void()

# 4. Sex vs HearDisease
HeartDisease_by_Sex_df <-
  df %>% select(Sex, HeartDisease) %>% group_by(Sex, HeartDisease) %>%  summarise(Count = n(), .groups = 'drop')
HeartDisease_by_Sex_df$HeartDisease <-
  factor(
    HeartDisease_by_Sex_df$HeartDisease,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
HeartDisease_by_Sex_plot <-
  ggplot(data = HeartDisease_by_Sex_df, aes(x = Sex, fill = HeartDisease, y = Count)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = Count,
    y = Count + 10,
    group = HeartDisease
  ),
  position = position_dodge(width = .9)) + theme_minimal() + theme_minimal()

# 5. ChestPainType vs HeartDisease
ChestPainType_vs_HeartDisease_df <-
  df %>% select(ChestPainType, HeartDisease) %>% group_by(ChestPainType, HeartDisease) %>%  summarise(Count = n(), .groups = 'drop')
ChestPainType_vs_HeartDisease_df$HeartDisease <-
  factor(
    ChestPainType_vs_HeartDisease_df$HeartDisease,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
ChestPainType_vs_HeartDisease_plot <-
  ggplot(data = ChestPainType_vs_HeartDisease_df, aes(x = ChestPainType, fill = HeartDisease, y = Count)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = Count,
    y = Count + 10,
    group = HeartDisease
  ),
  position = position_dodge(width = .9)) + theme_minimal() + theme_minimal()

# 6. FastingBS vs HeartDisease
FastingBS_vs_HeartDisease_df <-
  df %>% select(FastingBS, HeartDisease) %>% group_by(FastingBS, HeartDisease) %>%  summarise(Count = n(), .groups = 'drop')
FastingBS_vs_HeartDisease_df$HeartDisease <-
  factor(
    FastingBS_vs_HeartDisease_df$HeartDisease,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
FastingBS_vs_HeartDisease_plot <-
  ggplot(data = FastingBS_vs_HeartDisease_df, aes(x = FastingBS, fill = HeartDisease, y = Count)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = Count,
    y = Count + 10,
    group = HeartDisease
  ),
  position = position_dodge(width = .9)) + theme_minimal() + theme_minimal()

# 7. ExerciseAngina vs HeartDisease
ExerciseAngina_vs_HeartDisease_df <-
  df %>% select(ExerciseAngina, HeartDisease) %>% group_by(ExerciseAngina, HeartDisease) %>%  summarise(Count = n(), .groups = 'drop')
ExerciseAngina_vs_HeartDisease_df$HeartDisease <-
  factor(
    ExerciseAngina_vs_HeartDisease_df$HeartDisease,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
ExerciseAngina_vs_HeartDisease_plot <-
  ggplot(data = ExerciseAngina_vs_HeartDisease_df, aes(x = ExerciseAngina, fill = HeartDisease, y = Count)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = Count,
    y = Count + 10,
    group = HeartDisease
  ),
  position = position_dodge(width = .9)) + theme_minimal() + theme_minimal()

# 8. RestingECG vs HeartDisease
RestingECG_vs_HeartDisease_df <-
  df %>% select(RestingECG, HeartDisease) %>% group_by(RestingECG, HeartDisease) %>%  summarise(Count = n(), .groups = 'drop')
RestingECG_vs_HeartDisease_df$HeartDisease <-
  factor(
    RestingECG_vs_HeartDisease_df$HeartDisease,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
RestingECG_vs_HeartDisease_plot <-
  ggplot(data = RestingECG_vs_HeartDisease_df, aes(x = RestingECG, fill = HeartDisease, y = Count)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = Count,
    y = Count + 10,
    group = HeartDisease
  ),
  position = position_dodge(width = .9)) + theme_minimal() + theme_minimal()

# 9. ST_Slope vs HeartDisease
ST_Slope_vs_HeartDisease_df <-
  df %>% select(ST_Slope, HeartDisease) %>% group_by(ST_Slope, HeartDisease) %>%  summarise(Count = n(), .groups = 'drop')
ST_Slope_vs_HeartDisease_df$HeartDisease <-
  factor(
    ST_Slope_vs_HeartDisease_df$HeartDisease,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
ST_Slope_vs_HeartDisease_plot <-
  ggplot(data = ST_Slope_vs_HeartDisease_df, aes(x = ST_Slope, fill = HeartDisease, y = Count)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
    label = Count,
    y = Count + 10,
    group = HeartDisease
  ),
  position = position_dodge(width = .9)) + theme_minimal() + theme_minimal()

# 10. Age vs Heart disease
hd_vs_age <-
  ggplot(df, aes(x = Age, y = HeartDisease, color = Sex)) + geom_point() +
  geom_smooth(method = lm,
              formula = 'y ~ x' ,
              se = FALSE) +
  labs(title = "As expected, as patients get older, tend to have more heart diseases.")

# 11. Cholesterol density
Cholesterol_plot <-
  ggplot(df) + geom_point(mapping = aes(x = Age, y = Cholesterol, color = Sex))

heart_dft_chol_n0 <- filter(df, Cholesterol > 0)
heart_dft_chol_n0_plot <-
  ggplot(heart_dft_chol_n0) + geom_point(aes(x = Age, y = Cholesterol, color = Sex)) + geom_smooth(aes(x = Age, y = Cholesterol, color = Sex),
                                                                                                   method = "lm",
                                                                                                   formula = y ~ x)

# 12. Heart Disease vs Maximum Heart Rate
hd_vs_maxhr <-
  ggplot(df, aes(x = MaxHR, y = HeartDisease, color = Sex)) +
  geom_point() +
  geom_smooth(method = lm,
              se = FALSE,
              formula = y ~ x) +
  labs(title = "Heart Disease vs Maximum Heart Rate")

# 13. Heart Diseases vs Sugar Blood Level
hd_vs_sugar <-
  ggplot(df, aes(x = FastingBS, y = HeartDisease, color = Sex)) +
  geom_point() +
  geom_smooth(method = lm,
              se = FALSE,
              formula = y ~ x) +
  labs(title = "Heart Diseases vs Sugar Blood Level")

# ----END CHART----

# ----Logistic regression----
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
# model_logistic <-
#   glm(HeartDisease ~ Sex + ChestPainType + Cholesterol + FastingBS + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = Train, family = binomial)
# ----END-----

# ----Linear regression----

# find best model

# define intercept only model
intercept_only <-
  lm(as.formula(paste("HeartDisease", "~1", sep = "")), data = df_model)
#define model with all predictors
all <-
  lm(as.formula(paste("HeartDisease", "~.", sep = "")), data = df_model)
#perform forward stepwise regression

forward <-
  step(
    intercept_only,
    direction = "forward",
    scope = formula(all),
    trace = 0
  )

# backward stepwise regression
backward <-
  step(all,
       direction = "backward",
       scope = formula(all),
       trace = 0)

# both
both <-
  step(
    intercept_only,
    direction = "both",
    scope = formula(all),
    trace = 0
  )
# ----END----

# shiny app
source("./ui.R")

server <- function(input, output) {
  # dataset
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
  
  # chart
  output$age_plot <- renderPlot(age_plot)
  output$sex_plot <- renderPlot(sex_plot)
  
  output$HeartDisease_plot <- renderPlot(HeartDisease_plot)
  output$HeartDisease_plot_pie <- renderPlot(HeartDisease_plot_pie)
  
  output$HeartDisease_by_Sex_plot <-
    renderPlot(HeartDisease_by_Sex_plot)
  output$ChestPainType_vs_HeartDisease_plot <-
    renderPlot(ChestPainType_vs_HeartDisease_plot)
  
  output$FastingBS_vs_HeartDisease_plot <-
    renderPlot(FastingBS_vs_HeartDisease_plot)
  output$RestingECG_vs_HeartDisease_plot <-
    renderPlot(RestingECG_vs_HeartDisease_plot)
  
  output$ExerciseAngina_vs_HeartDisease_plot <-
    renderPlot(ExerciseAngina_vs_HeartDisease_plot)
  output$ST_Slope_vs_HeartDisease_plot <-
    renderPlot(ST_Slope_vs_HeartDisease_plot)
  
  output$Cholesterol_point_plot <- renderPlot({
    ggMarginal(
      Cholesterol_plot,
      type = "histogram",
      groupColour = TRUE,
      groupFill = TRUE
    )
  })
  output$hd_vs_age <-
    renderPlot(hd_vs_age)
  
  output$heart_dft_chol_n0_plot <-
    renderPlot(heart_dft_chol_n0_plot)
  
  
  output$Cholesterol_by_gender_plot <-
    renderPlot({
      ggplot(data = df, aes(x = Cholesterol)) +
        geom_density(aes(color = Sex)) +
        labs(title = "Cholesterol Distribution")
    })
  
  output$hd_vs_maxhr <- renderPlot(hd_vs_maxhr)
  output$hd_vs_sugar <- renderPlot(hd_vs_sugar)
  
  # logistic regression
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
    # epiDisplay::logistic.display(model_logistic)
  })
  
  output$equationLogistic <- renderUI({
    text = extract_eq(
      model_logistic,
      use_coefs = TRUE,
      # display coefficients
      wrap = TRUE,
      # multiple lines
      terms_per_line = 5
    )
    withMathJax(tags$p(text))
  })
  
  output$predictModelLogistic <- renderPrint({
    pred_ <- predict(model_logistic, type = "response", newdata = Test)
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
    # as.numeric(performance(ROCRpred, "auc")@y.values)
  })
  
  output$uiSelectVarLogisticPlot <- renderUI({
    selectInput(
      'logisticIndependentVarPlot',
      "Select the independent variables you would like to plot:",
      choices = names(select(df_model,-HeartDisease))
    )
  })
  
  observeEvent(input$logisticIndependentVarPlot, {
    if (!is.null(input$logisticIndependentVarPlot))
      output$uiLogisticPlot <- renderUI({
        renderPlot({
          ggplot(df_model,
                 aes_string(x = input$logisticIndependentVarPlot, y = "HeartDisease")) + geom_point(alpha = 0.15) + geom_smooth(method = "glm",
                                                                                                    method.args = list(family = "binomial"))
        })
      })
    
  })
  
  # linear regression
  output$uiSelectVarLinear <- renderUI({
    checkboxGroupInput(
      'linearIndependentVar',
      "Select the independent variables you would like to include:",
      names(select(df_model,-HeartDisease))
    )
  })
  
  output$uiSelectVarLinearPlot <- renderUI({
    selectInput(
      'linearIndependentVarPlot',
      "Select the independent variables you would like to plot:",
      choices = names(select(df_model,-HeartDisease))
    )
  })
  
  observeEvent(input$linearIndependentVarPlot, {
    if (!is.null(input$linearIndependentVarPlot))
      output$uiLinearPlot <- renderUI({
        renderPlot({
          ggplot(
            df_model,
            aes_string(
              x = input$linearIndependentVarPlot,
              y = "HeartDisease",
              color = "Sex"
            )
          ) +
            geom_point() +
            geom_smooth(method = lm,
                        se = FALSE,
                        formula = y ~ x)
        })
      })
  })
  
  
  output$equationLinear <- renderText("Equation: NULL")
  
  observeEvent(input$buttonVarLinear, {
    if (!is.null(input$linearIndependentVar)) {
      formula = as.formula(paste(
        "HeartDisease~",
        paste(input$linearIndependentVar, collapse = "+"),
        sep = ""
      ))
      
      linear_model = lm(formula, data = df_model)
      
      output$modelVarLinear <- renderPrint({
        text = extract_eq(
          linear_model,
          use_coefs = TRUE,
          # display coefficients
          wrap = TRUE,
          # multiple lines
          terms_per_line = 5
        )
        output$equationLinear <- renderUI({
          withMathJax(tags$p(text))
        })
        summary(linear_model)
      })
    }
  })
  
  output$equationLinearBackward <- renderUI({
    text = extract_eq(
      backward,
      use_coefs = TRUE,
      # display coefficients
      wrap = TRUE,
      # multiple lines
      terms_per_line = 5
    )
    withMathJax(tags$p(text))
  })
  
  output$equationLinearForward <- renderUI({
    text = extract_eq(
      forward,
      use_coefs = TRUE,
      # display coefficients
      wrap = TRUE,
      # multiple lines
      terms_per_line = 5
    )
    withMathJax(tags$p(text))
  })
  
  output$equationLinearBoth <- renderUI({
    text = extract_eq(
      both,
      use_coefs = TRUE,
      # display coefficients
      wrap = TRUE,
      # multiple lines
      terms_per_line = 5
    )
    withMathJax(tags$p(text))
  })
  
  output$linear_best_model_compare <- renderPrint({
    performance::compare_performance(forward, backward, both, rank = TRUE)
  })
}

shinyApp(ui, server)
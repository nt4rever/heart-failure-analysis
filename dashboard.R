# library
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(ggExtra)

# read data from csv file
df <- read.csv("heart.csv")
# remove all NA values
df <- na.omit(df)
# remove duplicate rows
df <- distinct(df)

# -----------plot chart------------
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
ChestPainType_vs_Sex_plot <- ggplot(data = ChestPainType_vs_Sex, aes(x = Sex, fill = ChestPainType, y = Pct)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
  label = paste(Pct , "%", sep = ""),
  x = Sex,
  y = Pct + 1,
  group = ChestPainType
),
position = position_dodge(width = .9)) + theme_minimal() + labs(title = "Regardless of the proportion of Males and Females,\n Men have high ASY compared with Women, and the pattern is different") + theme(plot.title = element_text(hjust = 0.5))

# RestingECG_vs_Sex
RestingECG_vs_Sex <- df %>% select(RestingECG, Sex) %>% group_by(RestingECG, Sex)  %>% summarise(Count = n(),
                                                                                                       .groups = 'drop')
RestingECG_vs_Sex$Pct <- round(RestingECG_vs_Sex$Count / sum(RestingECG_vs_Sex$Count), 2) * 100
RestingECG_vs_Sex_plot <- ggplot(RestingECG_vs_Sex, aes(x = Sex, y = Pct, fill = RestingECG)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
  label = paste(Pct , "%", sep = ""),
  x = Sex,
  y = Pct + 1,
  group = RestingECG
),
position = position_dodge(width = .9)) + theme_minimal() + labs(title = "Men and Women have somehow same pattern of RestingEC") + theme(plot.title = element_text(hjust = 0.5))

ExerciseAngina_vs_Sex <-
  df %>% select(ExerciseAngina, Sex)  %>% group_by(ExerciseAngina, Sex)  %>% summarise(Count = n(),   .groups = 'drop')

ExerciseAngina_vs_Sex$Pct <- round(ExerciseAngina_vs_Sex$Count / sum(ExerciseAngina_vs_Sex$Count), 2)*100

ExerciseAngina_vs_Sex_plot <- ggplot(data = ExerciseAngina_vs_Sex, aes(x = Sex, fill = ExerciseAngina, y = Pct)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
  label = paste(Pct , "%", sep = ""),
  x = Sex,
  y = Pct + 1,
  group = ExerciseAngina
),
position = position_dodge(width = .9)) + theme_minimal() + labs(title = "Almost a similar pattern between Men and Women. (ExerciseAngina)") + theme(plot.title = element_text(hjust = 0.5))

ST_Slope_vs_Sex <-
  df %>% select(ST_Slope, Sex)  %>% group_by(ST_Slope, Sex)  %>% summarise(Count = n(),   .groups = 'drop')
ST_Slope_vs_Sex$Pct <- round(ST_Slope_vs_Sex$Count / sum(ST_Slope_vs_Sex$Count), 2)*100

ST_Slope_vs_Sex_plot <- ggplot(data = ST_Slope_vs_Sex, aes(x = Sex, fill = ST_Slope, y = Pct)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(
  label = paste(Pct , "%", sep = ""),
  x = Sex,
  y = Pct + 1,
  group = ST_Slope
),
position = position_dodge(width = .9)) + theme_minimal() + labs(title = "A different pattern between Men and Women (ExerciseAngina)") + theme(plot.title = element_text(hjust = 0.5))

Cholesterol_plot <- ggplot(df) + geom_point(mapping = aes(x = Age, y = Cholesterol, color = Sex))
Cholesterol_point_plot <- ggMarginal(Cholesterol_plot, type = "histogram",  groupColour = TRUE, groupFill = TRUE)

heart_dft_chol_n0 <- filter(df, Cholesterol> 0)
heart_dft_chol_n0_plot <- ggplot(heart_dft_chol_n0) + geom_point(aes(x = Age, y = Cholesterol, color = Sex)) + geom_smooth(aes(x = Age, y = Cholesterol, color = Sex),method = "lm", formula = y ~ x)

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
  
  output$sex_plot <- renderPlot(sex_plot)
  output$ChestPainType_vs_Sex_plot <- renderPlot(ChestPainType_vs_Sex_plot)
  output$RestingECG_vs_Sex_plot <- renderPlot(RestingECG_vs_Sex_plot)
  output$ExerciseAngina_vs_Sex_plot <- renderPlot(ExerciseAngina_vs_Sex_plot)
  output$ST_Slope_vs_Sex_plot <- renderPlot(ST_Slope_vs_Sex_plot)
  output$Cholesterol_point_plot <- renderPlot(Cholesterol_point_plot)
  output$heart_dft_chol_n0_plot <- renderPlot(heart_dft_chol_n0_plot)
}

shinyApp(ui, server)
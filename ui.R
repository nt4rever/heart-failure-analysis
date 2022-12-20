ui <- dashboardPage(
  dashboardHeader(title = "Heart Failure Analysis"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dataset", tabName = "dataset", icon = icon("table")),
    menuItem("Chart", tabName = "chart", icon = icon("chart-pie")),
    menuItem(
      "Linear Regression",
      tabName = "linear",
      icon = icon("chart-line"),
      badgeLabel = "New",
      badgeColor = "green"
    ),
    menuItem(
      "Logistic Regression",
      tabName = "logistic",
      icon = icon("chart-column"),
      badgeLabel = "New",
      badgeColor = "green"
    )
  )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "dataset",
              DTOutput("dataset"),
              br(),
              fluidRow(
                tabBox(
                  title = "About Dataset",
                  width = 12,
                  tabPanel("Context", tags$div(
                    tags$div(
                      class = "dataset_context",
                      h4("HEART FAILURE IS TRULY ONE OF THE MOST DEADLY DISEASE"),
                      img(src = 'heart-beating.gif', width = 200)
                    ),
                    p(
                      "Cardiovascular diseases (CVDs) are the number 1 cause of death globally, taking an estimated 17.9 million lives each year, which accounts for 31% of all deaths worldwide. Four out of 5CVD deaths are due to heart attacks and strokes, and one-third of these deaths occur prematurely in people under 70 years of age. Heart failure is a common event caused by CVDs and this dataset contains 11 features that can be used to predict a possible heart disease."
                    ),
                    p(
                      "People with cardiovascular disease or who are at high cardiovascular risk (due to the presence of one or more risk factors such as hypertension, diabetes, hyperlipidaemia or already established disease) need early detection and management wherein a machine learning model can be of great help."
                    ),
                    tags$div(class = "center", img(src = 'symptom_of_heart.jpg', width = 600))
                  )),
                  tabPanel("Attribute Information", tags$div(
                    tags$ol(
                      tags$li("Age: age of the patient [years]"),
                      tags$li("Sex: sex of the patient [M: Male, F: Female]"),
                      tags$li(
                        "ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]"
                      ),
                      tags$li("RestingBP: resting blood pressure [mm Hg]"),
                      tags$li("Cholesterol: serum cholesterol [mm/dl]"),
                      tags$li(
                        "FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]"
                      ),
                      tags$li(
                        "RestingECG: resting electrocardiogram results [Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria]"
                      ),
                      tags$li(
                        "MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]"
                      ),
                      tags$li("ExerciseAngina: exercise-induced angina [Y: Yes, N: No]"),
                      tags$li("Oldpeak: oldpeak = ST [Numeric value measured in depression]"),
                      tags$li(
                        "ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping]"
                      ),
                      tags$li("HeartDisease: output class [1: heart disease, 0: Normal]")
                    )
                  )),
                  tabPanel("Source", tags$div(
                    p(
                      "This dataset was created by combining different datasets already available independently but not combined before. In this dataset, 5 heart datasets are combined over 11 common features which makes it the largest heart disease dataset available so far for research purposes. The five datasets used for its curation are:"
                    ),
                    tags$ul(
                      tags$li("Cleveland: 303 observations"),
                      tags$li("Hungarian: 294 observations"),
                      tags$li("Switzerland: 123 observations"),
                      tags$li("Long Beach VA: 200 observations"),
                      tags$li("Stalog (Heart) Data Set: 270 observations")
                    ),
                    p("Total: 1190 observations"),
                    p("Duplicated: 272 observations"),
                    tags$code("Final dataset: 918 observations"),
                    p(
                      "Every dataset used can be found under the Index of heart disease datasets from UCI Machine Learning Repository on the following link: https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/"
                    )
                  )),
                  tabPanel("Import data", tags$div(
                    verbatimTextOutput("strDataset"),
                    tags$div(
                      p("Observations:"),
                      tags$ul(
                        tags$li("We have a dataset with 12 variables and 918 rows"),
                        tags$li("We have different data types"),
                        tags$li("Seven(7) numeric variables and five(5) variables type object"),
                        tags$li("No missing data in this Dataset")
                      )
                    ),
                    # verbatimTextOutput("summaryDataset")
                  ))
                )
              )),
      tabItem(tabName = "chart",
              fluidRow(
                box(
                  width = 6,
                  collapsible = TRUE,
                  title = "Age Variable",
                  plotOutput("age_plot", height = 400)
                ),
                box(
                  width = 6,
                  title = 'HeartDisease Varible (Dependent Variable)',
                  collapsible = TRUE,
                  plotOutput('HeartDisease_plot', height = 400)
                ),
              ),
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Sex Bar Plot",
                    width = NULL,
                    collapsible = TRUE,
                    plotOutput('sex_plot'),
                    tags$div(class = "plot__container", tags$p(tags$b("Observation:")), tags$ul(
                      tags$li("Imbalanced data in terms of the Sex variable")
                    ))
                  ),
                  box(
                    title = "ExerciseAngina",
                    width = NULL,
                    collapsible = TRUE,
                    plotOutput('ExerciseAngina_vs_Sex_plot'),
                    tags$div(class = "plot__container", tags$p(tags$b("Note:")), tags$ul(
                      tags$li(
                        "ExerciseAngina: exercise-induced angina [Y: Yes, N: No] Angina is a type of chest pain caused by reduced blood flow to the heart"
                      )
                    ))
                  ),
                  box(
                    title = "ST_Slope",
                    width = NULL,
                    collapsible = TRUE,
                    plotOutput('ST_Slope_vs_Sex_plot'),
                    tags$div(class = "plot__container", tags$p(tags$b("Note:")), tags$ul(
                      tags$li(
                        "ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping] Mainly Flat in (Males) and relatively Up in (Females)"
                      )
                    ))
                  ),
                  box(
                    title = 'heart_dft_chol_n0_plot',
                    width = NULL,
                    collapsible = TRUE,
                    plotOutput('heart_dft_chol_n0_plot'),
                    tags$div(class = "plot__container", tags$p(tags$b("Observations:")), tags$ul(
                      tags$li(
                        "We may need to do some t-test to check whether the true means difference is equal to 0 or Not. we can scipy package to do that, but for now, let's move on..."
                      )
                    ))
                  ),
                ),
                column(
                  width = 6,
                  box(
                    title = "ChestPainType Vs Sex",
                    width = NULL,
                    collapsible = TRUE,
                    plotOutput('ChestPainType_vs_Sex_plot'),
                  ),
                  box(
                    title = "RestingECG",
                    width = NULL,
                    collapsible = TRUE,
                    plotOutput('RestingECG_vs_Sex_plot'),
                    tags$div(
                      class = "plot__container",
                      tags$p(tags$b("Observations:")),
                      tags$ul(
                        tags$li(
                          "ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]"
                        )
                      ),
                      p("The majority 46% of the Male participants are in ASY category"),
                      tags$ul(
                        tags$li(
                          "RestingBP: resting blood pressure taking a rest for min five(5) minutes before blood pressure measurement"
                        )
                      ),
                      p("The majority of the participants are in the Normal category"),
                      p(
                        "Note: The percentage is calculated using all participants, NOT within each group of Sex"
                      )
                    )
                  ),
                  box(
                    title = 'Numeric Vars ("Age", "Cholesterol", "MaxHR", "Oldpeak")',
                    width = NULL,
                    collapsible = TRUE,
                    plotOutput('Cholesterol_point_plot'),
                    tags$div(
                      class = "plot__container",
                      tags$p(tags$b("Observations:")),
                      tags$ul(
                        tags$li(
                          "It is somehow the same pattern for Male and Female relative to their counts!"
                        ),
                        tags$li("We have many of 0 values in the Cholesterol var!!"),
                        tags$li("Let's use lmplot to see the regression line of each group."),
                        tags$li(
                          "I think we have to remove the 0 values in the Cholesterol var and check again!"
                        )
                      )
                    )
                  ),
                  box(
                    title = 'HeartDisease_by_Sex',
                    width = NULL,
                    collapsible = TRUE,
                    plotOutput('HeartDisease_by_Sex_plot'),
                    tags$div(class = "plot__container",
                             tags$p(tags$b("Note:")),
                             tags$ul(
                               tags$li(
                                 "According to this dataset, 26% of females have developed Heart Disease, and 74% Have not."
                               ),
                               tags$li("63% of Males have developed Heart Disease, and 37% have not.")
                             ))
                  ),
                  
                )
              )),
      tabItem(tabName = "linear", fluidPage(h1("linear"))),
      tabItem(tabName = "logistic", fluidPage(
        h4("1. Normalize data"),
        DTOutput("datasetModel"),
        verbatimTextOutput("strDfModel"),
        h4("2. Split The Data"),
        p("The approach here will use Cross Validation on 80% of the dataset, and then judge the results on a final test set of 20% to evaluate the model."),
        h4("3. Summary logistic regression model with all variable"),
        verbatimTextOutput("summaryLogisticModelAllVar"),
        h4("4. Model Evaluation"),
        verbatimTextOutput("predictModelLogistic"),
        plotOutput("logistic_plot")
      ))
    )
  )
)
ui <- dashboardPage(
  dashboardHeader(title = "Heart Failure Analysis"),
  dashboardSidebar(
    sidebarMenu(
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
      ),
      menuItem("About Us", tabName = "about", icon = icon("address-card"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "about.css")
    ),
    tags$script(HTML("$('body').addClass('fixed');")),
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
                      tags$li(tags$b("Age"), ": age of the patient [years]"),
                      tags$li(tags$b("Sex"), ": sex of the patient [M: Male, F: Female]"),
                      tags$li(
                        tags$b("ChestPainType"),
                        ": chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]"
                      ),
                      tags$li(tags$b("RestingBP"), ": resting blood pressure [mm Hg]"),
                      tags$li(tags$b("Cholesterol"), ": serum cholesterol [mm/dl]"),
                      tags$li(
                        tags$b("FastingBS"),
                        ": fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]"
                      ),
                      tags$li(
                        tags$b("RestingECG"),
                        ": resting electrocardiogram results [Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria]"
                      ),
                      tags$li(
                        tags$b("MaxHR"),
                        ": maximum heart rate achieved [Numeric value between 60 and 202]"
                      ),
                      tags$li(
                        tags$b("ExerciseAngina"),
                        ": exercise-induced angina [Y: Yes, N: No]"
                      ),
                      tags$li(
                        tags$b("Oldpeak"),
                        ": oldpeak = ST [Numeric value measured in depression]"
                      ),
                      tags$li(
                        tags$b("ST_Slope"),
                        ": the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping]"
                      ),
                      tags$li(
                        tags$b("HeartDisease"),
                        ": output class [1: heart disease, 0: Normal]"
                      )
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
                    )
                  ))
                )
              )),
      tabItem(
        tabName = "chart",
        fluidRow(
          box(
            width = 6,
            collapsible = TRUE,
            title = "Age distribution",
            plotOutput("age_plot", height = 400)
          ),
          box(
            title = "Distribution of sex among patients",
            width = 6,
            collapsible = TRUE,
            plotOutput('sex_plot', height = 400)
          ),
        ),
        fluidRow(
          box(
            width = 6,
            title = 'Cases of HeartDisease',
            collapsible = TRUE,
            plotOutput('HeartDisease_plot', height = 400)
          ),
          box(
            width = 6,
            title = 'HeartDisease %',
            collapsible = TRUE,
            plotOutput('HeartDisease_plot_pie', height = 400)
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = 'Sex vs HeartDisease',
              width = NULL,
              collapsible = TRUE,
              plotOutput('HeartDisease_by_Sex_plot'),
              tags$div(class = "plot__container",
                       tags$p(tags$b("Note:")),
                       tags$ul(
                         tags$li(
                           "Male population has more heart disease patients than no heart disease patients. In the case of Female population, heart disease patients are less than no heart disease patients."
                         )
                       ))
            ),
            box(
              title = 'FastingBS vs HeartDisease',
              width = NULL,
              collapsible = TRUE,
              plotOutput('FastingBS_vs_HeartDisease_plot'),
              tags$div(class = "plot__container",
                       tags$p(tags$b("Note:")),
                       tags$ul(
                         tags$li(
                           "Fasting Blood Sugar is tricky! Patients diagnosed with Fasting Blood Sugar and no Fasting Blood Sugar have significant heart disease patients."
                         )
                       ))
            ),
            box(
              title = 'ExerciseAngina vs HeartDisease',
              width = NULL,
              collapsible = TRUE,
              plotOutput('ExerciseAngina_vs_HeartDisease_plot'),
              tags$div(class = "plot__container",
                       tags$p(tags$b("Note:")),
                       tags$ul(
                         tags$li(
                           "Exercise Induced Engina definitely bumps the probability of being diagnosed with heart diseases."
                         )
                       ))
            ),
            box(
              title = 'Plotting the density of Cholesterol by genders',
              width = NULL,
              collapsible = TRUE,
              plotOutput('Cholesterol_by_gender_plot')
            ),
          ),
          column(
            width = 6,
            box(
              title = 'ChestPainType vs HeartDisease',
              width = NULL,
              collapsible = TRUE,
              plotOutput('ChestPainType_vs_HeartDisease_plot'),
              tags$div(class = "plot__container",
                       tags$p(tags$b("Note:")),
                       tags$ul(
                         tags$li(
                           "ASY type of chest pain boldly points towards major chances of heart disease."
                         )
                       ))
            ),
            box(
              title = 'RestingECG vs HeartDisease',
              width = NULL,
              collapsible = TRUE,
              plotOutput('RestingECG_vs_HeartDisease_plot'),
              tags$div(class = "plot__container",
                       tags$p(tags$b("Note:")),
                       tags$ul(
                         tags$li(
                           "RestingECG does not present with a clear cut category that highlights heart disease patients. All the 3 values consist of high number of heart disease patients."
                         )
                       ))
            ),
            box(
              title = 'ST_Slope vs HeartDisease',
              width = NULL,
              collapsible = TRUE,
              plotOutput('ST_Slope_vs_HeartDisease_plot'),
              tags$div(class = "plot__container",
                       tags$p(tags$b("Note:")),
                       tags$ul(
                         tags$li(
                           "With the ST_Slope values, flat slope displays a very high probability of being diagnosed with heart disease. Down also shows the same output but in very few data points."
                         )
                       ))
            ),
            box(
              title = 'Age vs Heart disease',
              width = NULL,
              collapsible = TRUE,
              plotOutput('hd_vs_age')
            ),
          )
        ),
        fluidRow(
          box(
            title = 'Cholesterol and Age',
            width = 6,
            collapsible = TRUE,
            plotOutput('Cholesterol_point_plot')
          ),
          box(
            title = 'Cholesterol and Age (remove 0 value)',
            width = 6,
            collapsible = TRUE,
            plotOutput('heart_dft_chol_n0_plot')
          ),
          
        ),
        fluidRow(
          box(
            title = 'Heart Disease vs Maximum Heart Rate',
            width = 6,
            collapsible = TRUE,
            plotOutput('hd_vs_maxhr'),
            p(
              "As expected, people with heart disease have less maximum heart rate recorded. This could be due to the actual diseases, which could deteriorate the heart or, it could be due to the medicine making heart rate stay low, for example patients who suffer from tachycardias."
            )
          ),
          box(
            title = 'Heart Diseases vs Sugar Blood Level',
            width = 6,
            collapsible = TRUE,
            plotOutput('hd_vs_sugar'),
            p(
              "Patients with heart diseases had higher levels of blood sugar. This is expected."
            )
          )
        ),
        fluidPage(
          h3("Conclusions"),
          p(
            "As we have seen, this data set has predomenately male patients. This could represent that men are more likely to have a heart failure, but the sample is not big enough to reach that conclusion."
          ),
          p(
            "Most patients fall between the ages of 50 and 65 years old, where men are more clustered in that range, and women are more distributed. We can see a pattern where male patients above 50 years are more likely to have a heart failure, while age is not that big of a factor for women."
          ),
          p(
            "Almost 500 patients didn't have any symptoms when they had the failure, which represents more than 50% of total patients. Male patients are exponentially more asymptomatic than women, where women have a more distributed chest pain type."
          ),
          p(
            "This is very important information, specially for men, meaning that men who are 50 years or older, must be very aware that at any point they are very likely to suffer a heart attack. Heart failure awareness campaigns should focus on that group of the population."
          ),
          p(
            "Heart diseases don't play an important role on women, since only 28% had a previous heart disease. Men on the other hand, tend to have more heart diseases, 63% of them had at least one related disease."
          ),
          p(
            "Patients had a strong correlation between blood sugar level and heart disease, meaning that it could be a cause for more heart failures if people don't raise awareness on high level sugar foods and drinks."
          )
        )
      ),
      tabItem(
        tabName = "linear",
        fluidPage(
          h4("1. Preprocessing data"),
          DTOutput("datasetModel"),
          br(),
          verbatimTextOutput("strDfModel"),
        ),
        fluidPage(
          h4("2. Linear regresison model"),
          box(
            width = 3,
            uiOutput('uiSelectVarLinear'),
            actionButton("buttonVarLinear", "Submit")
          ),
          box(
            width = 9,
            title = "Model",
            tags$div(class = "equation", uiOutput('equationLinear')),
            verbatimTextOutput("modelVarLinear")
          )
        ),
        fluidPage(
          uiOutput('uiSelectVarLinearPlot'),
          uiOutput('uiLinearPlot')
        ),
        fluidPage(
          h4("3. Find best linear regression model"),
          h4("3.1. Equations"),
          tags$div(
            class = "linear__best__equation",
            tags$div(class = "equation", p("Backward:"), uiOutput('equationLinearBackward')),
            tags$div(class = "equation", p("Forward:"), uiOutput('equationLinearForward')),
            tags$div(class = "equation", p("Both:"), uiOutput('equationLinearBoth')),
          ),
          h4("3.2. Compare performance"),
          verbatimTextOutput('linear_best_model_compare')
        )
      ),
      tabItem(
        tabName = "logistic",
        fluidPage(
          fluidPage(
            uiOutput('uiSelectVarLogisticPlot'),
            uiOutput('uiLogisticPlot')
          ),
          h4("1. Split The Data"),
          p(
            "The approach here will use Cross Validation on 80% of the dataset, and then judge the results on a final test set of 20% to evaluate the model."
          ),
          h4("2. Summary logistic regression model with all variable"),
          verbatimTextOutput("summaryLogisticModelAllVar"),
          uiOutput("equationLogistic"),
          h4("3. Model Evaluation"),
          fluidRow(box(
            verbatimTextOutput("predictModelLogistic"),
          ),
          box(plotOutput("logistic_plot"))),
        )
      ),
      tabItem(tabName = "about",
              fluidPage(
                tags$div(
                  class = "about__container",
                  tags$div(class = "about__decor", img(src = "caythong.png")),
                  tags$div(class = "about__logo",
                           img(src = "logo_vku.png")),
                  tags$div(class = "about__topic",
                           h3("Topic:"),
                           h2("Heart Failure Analysis")),
                  tags$div(class = "about__profile",
                           h3("Group"),
                           tags$div(
                             class = "stack",
                             tags$div(
                               class = "left card",
                               tags$div(
                                 class = "about__content",
                                 tags$div(class = "ellipse-1"),
                                 tags$div(class = "ellipse-2"),
                                 tags$div(class = "avatar", img(src = "ly.jpg")),
                                 tags$div(class = "info", h4("Nguyen Thi Truc Ly"))
                                 
                               )
                             ),
                             tags$div(
                               class = "center card",
                               tags$div(
                                 class = "about__content",
                                 tags$div(class = "ellipse-1"),
                                 tags$div(class = "ellipse-2"),
                                 tags$div(class = "avatar", img(src = "tan.jpg")),
                                 tags$div(class = "info", h4("Le Van Tan"))
                                 
                               )
                             ),
                             tags$div(
                               class = "right card",
                               tags$div(
                                 class = "about__content",
                                 tags$div(class = "ellipse-1"),
                                 tags$div(class = "ellipse-2"),
                                 tags$div(class = "avatar", img(src = "tai.jpg")),
                                 tags$div(class = "info", h4("Dang Quang Tai"))
                                 
                               )
                             )
                           ))
                )
              ))
    )
  )
)
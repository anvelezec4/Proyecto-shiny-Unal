library(shiny)
shinyUI(pageWithSidebar(
  headerPanel( "Linear mixed model estimation", "MLM"),
  sidebarPanel(
      img(src="escudo.png",height = 100, width = 200),
      helpText("Tool for fitting linear mixed model with random component", align="center"),
      fileInput("file", label = h4("File input in txt"), 
                accept=c('text/csv', 
                         'text/comma-separated-values',
                         'text/plain', 
                         '.csv',
                         '.txt')
                ),
      
      checkboxInput('Slope', 'Random slope?', TRUE),
   # Sidebar with a slider input for the number of bins
   tags$hr(),
   sliderInput("bins1",
               "subjects to graph:",
               min = 1,
               max =  200,
               value = 50,
               step=5),
    selectInput("Vrespuesta", "Choose a answer variable (y)", ""),
   selectInput("tiempo", "Choose a time variable (x)", ""),
   selectInput("factor", "Choose a factor variable (x)", ""),
   selectInput("sujeto", "Choose a subject with t=1,2,3,... n", "")
      ),
      mainPanel(
                tabsetPanel(
                   tabPanel("Grafico perfiles",h4("Grafico de perfiles"),
                    plotOutput("pred_plot", width="100%"), h4("Datos"),
                    tableOutput('contents')),
                   tabPanel("Ranking",textOutput("b"),dataTableOutput("rank")),
                   tabPanel("Cluster",plotOutput("total_plot", width="100%")),
                   tabPanel("REML",verbatimTextOutput("summary"),dataTableOutput("REML")),
                   tabPanel("BAYES",verbatimTextOutput("summary2"),dataTableOutput("Bayes")),
                   tabPanel("EM",verbatimTextOutput("summary3"),dataTableOutput("EM"))
                                                      )
               
      )
   )
)
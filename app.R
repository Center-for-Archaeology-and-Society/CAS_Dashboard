library(shiny)
library(DT)
library(shinyjs)

source('functions.R')

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(script = "custom.js", functions = c("showLoading","hideLoading")),
  titlePanel("CAS Admin Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("options", "Choose an option:",
                  choices = c("box counts")),
      uiOutput("group_vars"),
      downloadButton('downloadData', 'Download Results')
    ),
    mainPanel(
      DT::DTOutput("table")
    )
  )
)

server <- function(input, output, session) {

  rvals <- reactiveValues()
  con <- connect()

  output$group_vars <- renderUI({
    if(input$options == "box counts"){
      selectInput("group_vars", "Group by:",
                  choices = c("None", "Accession", "Site", "Owner", "Land Status"),
                  selected = "None",
                  multiple = TRUE)
    }
  })

  observeEvent(input$group_vars, {
    if(input$options == "box counts"){
      js$showLoading()
      rvals$data <- boxcounts(input$group_vars, con)
      js$hideLoading()
    }
  })

  output$table <- DT::renderDT({
    validate(
      need(input$options, "Please select an option")
    )
    validate(
      need(inherits(rvals$data, "data.frame"), "No valid results")
    )
    rvals$data
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("results-", input$options, "-", as.character(as.Date(Sys.time())), ".xlsx")
    },
    content = function(file) {
      rio::export(rvals$data, file)
    }
  )

  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })
}

shinyApp(ui = ui, server = server)

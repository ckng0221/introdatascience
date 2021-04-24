library(shiny)
library(shinythemes)


# =========== User Interface ============= #

ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage("BMI Calculator (Tutorial 6):",
                           tabPanel("Home",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      sliderInput("height", 
                                                  label = "Height (cm)", 
                                                  value = 170, 
                                                  min = 40, 
                                                  max = 250),
                                      sliderInput("weight", 
                                                  label = "Weight (kg)", 
                                                  value = 60, 
                                                  min = 20, 
                                                  max = 100),
                                      
                                      actionButton("submitbutton", 
                                                   "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      tableOutput('tabledata') # Results table
                                    ) # mainPanel()
                                    
                           ), #tabPanel(), Home
                           
                           tabPanel("About", 
                                    titlePanel("About"), 
                                    div(includeMarkdown("about.md"), 
                                        align="justify")
                           ) #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()




# ======== Server ===========================
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # formula for bmi 
    bmi <<- input$weight/( (input$height/100) * (input$height/100) )
    bmi_df <- data.frame(bmi)
    names(bmi_df) <- "BMI"
    print(bmi_df)
    
    # print(bmi_status)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    
    #bmi weight status check
    bmi_status_func <- function(bmi_value) {
      if (bmi_value < 18.5) {
        return("Underweight")
      } else if (bmi_value >= 18.5 & bmi_value < 25) {
        return("Normal")
      } else if (bmi_value >= 25 & bmi_value < 30 ) {
        return("Overweight")
      } else if (bmi_value >= 30) {
        return("Obsese")
      }
    }
    
    if (input$submitbutton>0) { 
      isolate(bmi_status_func(bmi_value=bmi)) 
    } else {
      return("Please enter your weight and heights to calculate the BMI")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}



# Create Shiny Ap
shinyApp(ui = ui, server = server)

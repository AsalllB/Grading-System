library(shiny)

ui <- fluidPage(
    
    titlePanel("Class Presentation Grading System"),
    
    sidebarLayout(
        sidebarPanel(
            h4("Student Evaluation"),
            helpText("Enter the student's details and scores below."),

            textInput("studentName", "Student Name", placeholder = "e.g., John Doe"),
            textInput("topic", "Presentation Topic", placeholder = "e.g., Data Science"),

            hr(),

            sliderInput("score_content", "Content & Research (0-10):", 
                        min = 0, max = 10, value = 5),
            sliderInput("score_delivery", "Delivery & Speaking (0-10):", 
                        min = 0, max = 10, value = 5),
            sliderInput("score_qa", "Q&A Response (0-10):", 
                        min = 0, max = 10, value = 5),

            br(),

            actionButton("submit", "Submit Grade", class = "btn-primary"),
            downloadButton("downloadData", "Download CSV", class = "btn-success"),

            br(), br(),
            textOutput("current_total_display")
        ),
        
        mainPanel(
            tabsetPanel(
                type = "tabs",

                tabPanel("Scoreboard", 
                    br(),
                    h4("Class Gradebook"),
                    tableOutput("gradesTable"),
                    textOutput("no_data_msg")
                ),

                tabPanel("Performance Charts", 
                    br(),
                    h4("Score Distribution"),
                    plotOutput("scorePlot")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    values <- reactiveValues(df = data.frame(
        Name = character(),
        Topic = character(),
        Content = numeric(),
        Delivery = numeric(),
        QA = numeric(),
        Total = numeric(),
        Timestamp = character(),
        stringsAsFactors = FALSE
    ))
    
    current_total <- reactive({
        input$score_content + input$score_delivery + input$score_qa
    })
    
    output$current_total_display <- renderText({
        paste("Current Total Score:", current_total(), "/ 30")
    })
    
    observeEvent(input$submit, {
    
        if(input$studentName == "") {
            showNotification("Please enter a student name.", type = "error")
            return()
        }

        new_entry <- data.frame(
            Name = input$studentName,
            Topic = input$topic,
            Content = input$score_content,
            Delivery = input$score_delivery,
            QA = input$score_qa,
            Total = current_total(),
            Timestamp = format(Sys.time(), "%H:%M:%S"),
            stringsAsFactors = FALSE
        )

        values$df <- rbind(values$df, new_entry)

        showNotification(paste("Grade added for", input$studentName), type = "message")

        updateTextInput(session, "studentName", value = "")
        updateTextInput(session, "topic", value = "")
        updateSliderInput(session, "score_content", value = 5)
        updateSliderInput(session, "score_delivery", value = 5)
        updateSliderInput(session, "score_qa", value = 5)
    })

    output$gradesTable <- renderTable({
        if (nrow(values$df) == 0) return(NULL)
        values$df
    })
    
    output$no_data_msg <- renderText({
        if (nrow(values$df) == 0) "No students graded yet. Enter data on the left to begin."
    })
    
    output$scorePlot <- renderPlot({
        req(nrow(values$df) > 0)

        barplot(values$df$Total, 
                names.arg = values$df$Name,
                col = "skyblue", 
                border = "white",
                main = "Total Scores per Student",
                ylab = "Total Score (Max 30)",
                ylim = c(0, 30),
                las = 1)

        abline(h = mean(values$df$Total), col = "red", lwd = 2, lty = 2)
        legend("topright", legend = "Class Average", col = "red", lty = 2, lwd = 2)
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
        paste("class_grades-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(values$df, file, row.names = FALSE)
        }
    )
}

shinyApp(ui = ui, server = server)
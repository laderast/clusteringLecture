test <- shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(sliderInput(inputId = "iteration",
                               label = "Number Iterations",
                               min = 1, max=10,value = 1)),
      mainPanel(plotOutput("plotClust"))
    )
  ),
  
  server = function(input, output){
    newIris <- iris[,c(1,2)]
    
    #start with the same random centers
    centers <- matrix(c(4.5, 2, 3.3, 4), nrow=2, byrow=TRUE)
    clusters <- reactive({
      suppressWarnings(kmeans(newIris, centers = centers, 
                              iter.max = input$iteration))
    })
    
    output$plotClust <- renderPlot({
      par(mar = c(5.1, 4.1, 0, 1))
      palette(c("#E41A1C", "#377EB8", "#4DAF4A"))
      plot(newIris, col = clusters()$cluster, pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
  }
  
)
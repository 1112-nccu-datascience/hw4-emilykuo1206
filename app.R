library(shiny)
library(shinydashboard)
library(ggbiplot)
library(ggplot2)
library(factoextra)
library(dplyr)
library(FactoMineR)

# Load data
data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

# Define UI
ui <- dashboardPage(
  
  # Define dashboard header
  dashboardHeader(title = "郭彥伶 經濟碩二 110258015",titleWidth = 300),
  
  # Define dashboard sidebar
  dashboardSidebar(width = 300,
    
    # Define sidebar menu
    sidebarMenu(
      
      # Define sliderInput
      sliderInput(inputId = "n",
                  label = "Choose how many inputs to do PCA and CA:", 
                  min = 1,
                  max = 150,
                  value = 150),
      
      # Define menu items
      menuItem("PCA", icon = icon("chart-bar"),
               menuSubItem("PCA Biplot", tabName = "page1"),
               menuSubItem("PCA table", tabName = "subpage1"),
               menuSubItem("Variance Explained", tabName = "subpage2"),
               menuSubItem("Variable Contributions to PC", tabName = "subpage3")),
      menuItem("CA", tabName = "page2", icon = icon("chart-pie")),
      menuItem("iris data", tabName = "page3", icon = icon("table"))
    )
  ),
  
  # Define dashboard body
  dashboardBody(
    
    # Define tab items
    tabItems(
      
      # Define tab item for page 1
      tabItem(tabName = "page1",
              titlePanel("PCA Biplot"),
              fluidRow(
                box(selectInput("x_axis", label = h2("Select x-axis"), 
                                choices = colnames(ir.pca$rotation)),
                    selectInput("y_axis", label = h2("Select y-axis"), 
                                choices = colnames(ir.pca$rotation)),
                    width = 4),
                fluidRow(
                  box(plotOutput("plot1"), width = 7)))
      ),
      
      
      
      # Define tab item for sub-page 1
      tabItem(tabName = "subpage1",
              titlePanel("PCA table"),
              fluidRow(
                box(numericInput(inputId = "rows", label = "Number of Data Rows:", value = 25),
                    tableOutput("table1"),width = 4)
              ),
      ),
      
      # Define tab item for sub-page 2
      tabItem(tabName = "subpage2",
              titlePanel("Variance Explained"),
              fluidRow(
                box(plotOutput("plot4"), width = 6),
                box(plotOutput("plot6"), width = 6)
              )
      ),
      
      # Define tab item for sub-page 3
      tabItem(tabName = "subpage3",
              titlePanel("Variable Contributions to PC"),
              fluidRow(
                box(selectInput("PCs", label = h2("Select PC"), 
                                choices = colnames(ir.pca$rotation)),
                    width = 4),
                fluidRow(
                  box(plotOutput("plot5"), width = 7))
              )
      ),
      
      # Define tab item for page 2
      tabItem(tabName = "page2",
              titlePanel("CA Biplot"),
              fluidRow(
                box(plotOutput("plot2"), width = 6),
                box(verbatimTextOutput("plot3"), width = 6))
      ),
      
      # Define tab item for page 3
      tabItem(tabName = "page3",
              titlePanel("iris data"),
              fluidRow(
                box(radioButtons("var", "Select a Species:",
                                 choices = levels(iris$Species),
                                 selected = "setosa"),tableOutput("table2"),width = 7)
              ),
              fluidRow(
                box(verbatimTextOutput("table3"),width = 7)
              ),
      )
    )
  )
)

# Define server
server <- function(input, output,session) {
  
  # Update the x_axis and y_axis input choices based on the data
  observe({
    choices <- colnames(ir.pca$rotation)
    updateSelectInput(session, "x_axis", choices = choices)
    updateSelectInput(session, "y_axis", choices = choices)
  })
  
  # Update the y_axis input choices based on the x_axis selection
  observeEvent(input$x_axis, {
    choices <- setdiff(colnames(ir.pca$rotation), input$x_axis)
    updateSelectInput(session, "y_axis", choices = choices)
  })
  
  # Define code for page 1
  output$plot1 <- renderPlot({
    data(iris)
    log.ir <- log(iris[1:input$n, 1:4])
    ir.species <- iris[1:input$n, 5]
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    
    g <- ggbiplot(ir.pca, choices = c(which(colnames(ir.pca$rotation)==input$x_axis),which(colnames(ir.pca$rotation)==input$y_axis)), 
                  obs.scale = 1, var.scale = 1, groups = ir.species,ellipse = TRUE)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  
  # Define code for subpage 1
  output$table1 <- renderTable({
    data(iris)
    log.ir <- log(iris[1:input$n, 1:4])
    ir.species <- iris[1:input$n, 5]
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    head(ir.pca[["x"]], n = input$rows)
  })
  
  # Define code for subpage 2
  output$plot4 <- renderPlot({
    data(iris)
    log.ir <- log(iris[1:input$n, 1:4])
    ir.species <- iris[1:input$n, 5]
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    a <- fviz_eig(ir.pca,addlabels = TRUE)+
      ggtitle("PCA Scree Plot")+
      xlab("PC")
    print(a)
  })
  
  output$plot6 <- renderPlot({
    data(iris)
    log.ir <- log(iris[1:input$n, 1:4])
    ir.species <- iris[1:input$n, 5]
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    PC <- c("PC1 : 73.3% ","PC2 : 22.7% ","PC3 : 3.3% ","PC4 : 0.7% ")
    a <- fviz_eig(ir.pca,addlabels = TRUE)
    ggplot(a[["data"]]["eig"], aes(x = "", y = eig, fill = PC)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Variance Explained : Pie Chart")
  })
  
  # Define code for subpage 3
  output$plot5 <- renderPlot({
    data(iris)
    log.ir <- log(iris[1:input$n, 1:4])
    ir.species <- iris[1:input$n, 5]
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    fviz_contrib(ir.pca, choice = "var", axes = which(colnames(ir.pca$rotation)==input$PCs))
  })
  
  # Define code for page 2
  output$plot2 <- renderPlot({
    res.ca <- CA(iris[1:input$n,1:4], graph = FALSE)
    fviz_ca_biplot(res.ca)
  })
  
  output$plot3 <- renderPrint({
    res.ca <- CA(iris[1:input$n,1:4], graph = FALSE)
    summary(res.ca)
  })
  
  # Define code for page 3
  output$table2 <- renderTable({
    iris %>% filter(Species == input$var)
  })
  
  output$table3 <- renderPrint({
    summary(iris %>% filter(Species == input$var))
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)


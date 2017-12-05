#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(plyr)
library(shiny)
library(datasets)
library(codetools)
library(devtools)
data("HairEyeColor")
data_students <- as.data.frame(HairEyeColor)
data_students$Hair <- factor(data_students$Hair)
hair_color <- unique(data_students$Hair)
hair_color <- factor(c("Total", as.character(hair_color)))
students <- dim(data_students)[1]

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title and subtitle
   titlePanel("Distribution of hair and eye color and sex in students"),
   h4("The following plots show number of student by hair color, eye color and sex.
      Select the hair color and observe the distribution by eye color."),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
          selectInput("choices1", "Select hair color:", choices = hair_color)
      ),

      # Show a plot of the generated distribution
      mainPanel(
          plotOutput("bars1")
        )

    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$bars1 <- renderPlot({
    if(input$choices1 == "Total")
      data_hair <- data_students
    if(input$choices1 != "Total")
      data_hair <- filter(data_students,  data_students$Hair==input$choices1)

    data_hair.1 <- ddply(data_hair, .(Eye, Sex), summarize, total=sum(Freq, na.rm = TRUE))
    ggplot(data_hair.1, aes(x=Eye, y=total, fill=as.factor(Sex))) +
      geom_bar(stat = "identity", position = "dodge") +
      ggtitle("Total Students by Eye Color") +
      xlab("Eye Color") + ylab("Total Students") +
      guides(fill=guide_legend("Sex")) + theme_bw()
  })

}

# Run the application
shinyApp(ui = ui, server = server)


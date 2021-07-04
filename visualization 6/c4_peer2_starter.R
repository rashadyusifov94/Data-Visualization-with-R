library(shiny)
library(tidyverse)
library(plotly)
library(hrbrthemes)



#####Import Data

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))

dat<-read_csv("cces_sample_coursera.csv")



dat<- dat %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
dat<-drop_na(dat)

# Define UI for application
ui <- 
  # Application title
  navbarPage(title="My Application",
             tabPanel("Page 1",
                      sliderInput(inputId = "my_ideo",
                                  label = "Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
                                  min = 1,
                                  max = 5,
                                  value = 1),
                      
                      tabsetPanel(
                        tabPanel("Tab1",mainPanel(plotOutput("myplot1"))),
                        tabPanel("Tab2",mainPanel(plotOutput("myplot2")))
                        
                      )),
             
             tabPanel("Page 2", 
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(
                            inputId="checked_gender",
                            label="Which gender do you want to display?",
                            choices=c(1,2),
                            selected=c(1,2)
                          )
                        ),
                        mainPanel(
                          plotlyOutput("scatter")))
             ),
             
             tabPanel("Page 3",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput(inputId = "region1", label  = "Select Region", choices = dat$region,multiple = TRUE)),
                        mainPanel(
                          dataTableOutput(outputId = "regiondata"))))
             
  ) 




# Define server logic 
server <- function(input, output) {
  
  
  output$myplot1<-renderPlot(
    
    ggplot(
      filter(dat,ideo5==input$my_ideo),
      aes(x=pid7)) +
      geom_bar() + facet_wrap(~ideo5) + xlab("7 Point Party ID, 1=Very D, 7=Very R")
    
  )
  
  output$myplot2<-renderPlot(
    
    ggplot(
      filter(dat,ideo5==input$my_ideo),
      aes(x=CC18_308a)) +
      geom_bar() + facet_wrap(~ideo5) + xlab("Trump Support"))
  
  
  
  output$scatter<-renderPlotly({
    
    plot_dat<-filter(dat, gender %in% input$checked_gender)
    
    ggplot(plot_dat, aes(x=educ,y=pid7, color = factor(gender))) +
      geom_jitter(color = "black") +  geom_smooth(method = lm,) + theme(legend.position = "none") +
      scale_color_manual(values = c("blue","blue"))
  }
  )
  
  
  output$regiondata <-renderDataTable({
    
    regionfilter <- filter(dat, region %in% input$region1)
    
  }
  
  )
}


# Run the application 
shinyApp(ui = ui, server = server)


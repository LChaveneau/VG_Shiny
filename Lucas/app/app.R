#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidyr)
library(DT)
library()

df <- read_csv('vgsales.csv')
df2 <- readRDS('data_jeux.rds')

df <- df  %>% 
  mutate(Platform = as.factor(Platform)) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Genre = as.factor(Genre)) %>% 
  mutate(Publisher = as.factor(Publisher))


df2 <- df2  %>% 
  mutate(Publisher = as.factor(Publisher)) %>% 
  mutate(Annee = as.factor(Annee)) %>% 
  mutate(platform = as.factor(platform))

# Define UI for application that draws a histogram
ui <- fluidPage(  

    # Application title
    titlePanel("Video Games"),
    ## Page
    navbarPage("Page",
      tabPanel("Graphiques",
        tabsetPanel(
          tabPanel("Nombre",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("years",
                                   "Annee :",
                                   min = 1980,
                                   max = 2017,
                                   value = 2016)
                     ,
                     radioButtons("var", 
                                  label = h3("Radio buttons"),
                                  choices = list("Publisher" = "Publisher", "Genre" = "Genre"), 
                                  selected = "Genre")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("Hist")
                     )
                   )
          ),
          tabPanel("Sales", 
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("years2",
                                   "Annee :",
                                   min = 1980,
                                   max = 2017,
                                   value = 2016)
                       ,
                       radioButtons("var2", 
                                    label = h3("Radio buttons"),
                                    choices = list(
                                      "Publisher" = "Publisher", 
                                      "Genre" = "Genre", 
                                      "Video Games" = "Name", 
                                      "Platform" = "Platform"
                                      ), 
                                    selected = "Name")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                       plotOutput("SalesHist")
                     )
                   )),
          tabPanel("I have no idea", "ODOR")
        )
      ), 
      tabPanel("Recommandation",
              sidebarLayout(
                sidebarPanel(
                  radioButtons("type_recommandation",
                               label = h3("Préférence par :"),
                               choices = list("Popularité" = "Popularite", "Ventes" = "Vente"), 
                               selected = "Popularite", 
                               inline = TRUE),
                  sliderInput("years3",
                              "Annee :",
                              min = 1980,
                              max = 2017,
                              value = c(1980,2017)),
                  
                  selectInput("plateform1", label = h3("Plateform"), 
                              choices = c("--", as.list(levels(df2$Platform))),
                              selected = "--"),
                  selectInput("genre1", label = h3("Type"), 
                              choices = c("--", as.list(levels(df2$Genre))),
                              selected = "--")
                ),
                mainPanel(
                  DTOutput("Tablo")
                  )
                ),
              verbatimTextOutput('desc'),
              DTOutput("tablotest")
              ),
      tabPanel("Recherche", 
               selectizeInput("Test",
                              label = h3('Test'),
                              choices = c(
                                          "Action",
                                          "Aventure",
                                          "Puzzle",
                                          "Racing",
                                          "Educational",
                                          "Compilation",
                                          "Simulation",
                                          "Sports",
                                          "Strategie",
                                          "Role_play",
                                          "Edition_special",
                                          "DLC"),
                              multiple = TRUE,
                              options = list(dropdownParent = 'body', 
                                             maxitems = 'null', 
                                             plugins = list("remove_button"),
                                             highlight = F,
                                             closeAfterSelect = T)
                              )
               )
      ),
    theme = shinythemes::shinytheme('united')
    )
    # Sidebar with a slider input for number of bins 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe({
    input$plateform1
    
    if(input$plateform1 != '--'){
    minimum <- min(na.omit(as.numeric(as.character(df$Year[df$Platform==input$plateform1]))))
    maximum <- max(na.omit(as.numeric(as.character(df$Year[df$Platform==input$plateform1]))))
    }else{
      minimum <- 1980
      maximum <- 2017
    }
    updateSliderInput(
      inputId = "years3",
      min = minimum,
      max = maximum,
      value = c(minimum,maximum)
      )
    
  })
  
  observe({
    input$type_recommandation

    if(input$type_recommandation == "Vente"){

      updateSliderInput(
        inputId = "years3",
        min = 1980,
        max = 2017,
        value = c(1980,2017)
        )

      updateSelectInput(
        inputId = "plateform1",
        choices = c("--", as.list(levels(df$Platform))),
        selected = "--"
      )

      updateSelectInput(inputId = "genre1",
                        choices = c("--", as.list(levels(df$Genre))),
                        selected = "--")
    }
    if(input$type_recommandation == "Popularite"){
      updateSliderInput(
        inputId = "years3",
        min = 1950,
        max = 2022,
        value = c(1950,2022)
      )

      updateSelectInput(
        inputId = "plateform1",
        choices = c("--", as.list(levels(df2$platform))),
        selected = "--"
      )
      updateSelectInput(inputId = "genre1",
                        choices = c("--",
                                    "Action",
                                    "Aventure",
                                    "Puzzle",
                                    "Racing",
                                    "Educational",
                                    "Compilation",
                                    "Simulation",
                                    "Sports",
                                    "Strategie",
                                    "Role_play",
                                    "Edition_special",
                                    "DLC"),
                        selected = "--")
    }
    })
    
  variable <- reactive({switch(input$var,
                               "Publisher" = df$Publisher, 
                               "Genre" = df$Genre)
    })
  
  variable2 <- reactive({switch(input$var2,
                               "Publisher" = df$Publisher, 
                               "Genre" = df$Genre,
                               "Platform" = df$Platform,
                               "Name" = df$Name)
  })
  
    output$Hist <- renderPlot({
          fct_count(variable()[df$Year == input$years], sort = T) %>%
          slice(1:10) %>%
          ggplot() +
          aes(x=fct_reorder(f, n), y = n) +
          geom_col() +
          coord_flip()
      })

    output$SalesHist <- renderPlot({
      df %>% 
        filter(Year == input$years2) %>% 
        select(everything()) %>% 
        group_by(variable2()[df$Year == input$years2]) %>%
        summarize(Total_sales = sum(Global_Sales)) %>%
        arrange(desc(Total_sales)) %>%
        slice(1:10) %>% 
        setNames(c("Truc", "Total_sales")) %>% 
        ggplot() + 
        aes(x=fct_reorder(Truc, Total_sales), y = Total_sales) + 
        geom_col() + 
        coord_flip()
    })
    output$Tablo <- renderDT({
      ####### PAR SALES ########3
        if(input$type_recommandation == "Vente"){

        image <- paste0("<img src=\"", src="https://www.mobygames.com/images/covers/s/264995-air-hockey-android-front-cover.jpg", "\" height=\"30\" data-toggle=\"tooltip\" data-placement=\"right\" title=\"", "bijo", "\"></img>")
        
        tablo <-df %>%
          filter(as.numeric(as.character(Year)) >= input$years3[1]) %>%
          filter(as.numeric(as.character(Year)) <= input$years3[2])
        
        if(input$plateform1 != "--"){
          tablo <- tablo %>% 
            filter(Platform == input$plateform1)
          }
        if(input$genre1 != "--"){
          tablo <- tablo %>% 
            filter(Genre == input$genre1)
        }
        best_choice_tableau <- tablo %>% 
          slice(1:3) %>% 
          select("Name":"Publisher") %>% 
          mutate(picture = c(image, image, image)) %>% 
          datatable(rownames = F,
                    extensions = c('Select'),
                    selection = "single",
                    options = list(
                      select = list(style = 'os', items = 'row'), 
                      dom = 't',
                      ordering = F),
                    escape = F) 
        }
      ###############PAR POPULARITE############
    if(input$type_recommandation == "Popularite"){
      best_choice_tableau <- df2[,1:3] %>% slice(1:3) %>%
        select(everything()) %>% 
        datatable(rownames = F,
                  extensions = c('Select'),
                  selection = "single",
                  options = list(
                    select = list(style = 'os', items = 'row'), 
                    dom = 't',
                    ordering = F),
                  escape = F)
    }
      best_choice_tableau
    })
    
     output$desc <- renderPrint({
       cat(paste(input$Tablo_row_last_clicked))
       cat(paste(input$type_recommandation))
     })
     output$tablotest <- renderDT({input$Tablo})
}

# Run the application 
shinyApp(ui = ui, server = server)

# output$SalesHist <- renderPlot({
#   tablo <- df %>% 
#     filter(Year == input$years2) %>% 
#     select(everything()) %>% 
#     group_by(variable2()[df$Year == input$years2]) %>%
#     summarize(Total_sales = sum(Global_Sales))%>%
#     arrange(desc(Total_sales)) %>%
#     slice(1:10)
#   colnames(tablo) <- c("Truc", "Total_sales")
#   ggplot(tablo) + 
#     aes(x=fct_reorder(Truc, Total_sales), y = Total_sales) + 
#     geom_col() + 
#     coord_flip()
# })
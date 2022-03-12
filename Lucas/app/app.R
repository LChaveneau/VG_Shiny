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


# ## NOT RUN and TO DELETE AT THE END ->>>>>
# switch_vector <- function(x){switch(
#   x,
#   "Action" = df2$Action,
#   "Aventure" = df2$Aventure,
#   "Puzzle" = df2$Puzzle,
#   "Racing" = df2$Racing,
#   "Education" = df2$Educational,
#   "Compilation" = df2$Compilation,
#   "Simulation" = df2$Simulation,
#   "Sports" = df2$Sports,
#   "Strategie" = df2$Strategie,
#   "Role_play" = df2$Role_play,
#   "Edition_special" = df2$Edition_special,
#   "DLC" = df2$DLC)
# }
# >>>>>>

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
                  
                  selectizeInput("plateform1", 
                                 label = h3("Plateform"),
                                 choices = c(levels(df2$Platform)),
                                 multiple = TRUE,
                                 options = list(dropdownParent = 'body', 
                                                maxitems = 'null', 
                                                plugins = list("remove_button"),
                                                highlight = F,
                                                closeAfterSelect = T)
                  ),
                  selectizeInput("genre1",
                                 label = h3('Genre'),
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
                ),
                mainPanel(
                  DTOutput("Tablo")
                  )
                ),
              fluidRow(
                column(4, 
                      htmlOutput("image")
                       ),
                column(8,
                       htmlOutput('desc'),
                       verbatimTextOutput('desc2')
              )
              )
              ),
      tabPanel("Recherche", 
               selectizeInput("Test",
                              label = h3('Test'),
                              choices = c(
                                          "Action" = "Action",
                                          "Aventure" = "Aventure",
                                          "Puzzle" = "Puzzle",
                                          "Course" = "Racing",
                                          "Education" = "Educational",
                                          "Compilation" = "Compilation",
                                          "Simulation" = "Simulation",
                                          "Sports" = "Sports",
                                          "Strategie" = "Strategie",
                                          "Role play" = "Role_play",
                                          "Edition special" = "Edition_special",
                                          "DLC" = "DLC"),
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
  
  #############################################################################################
  ###################################### EVENT REACTIVE #######################################
  #############################################################################################
  
  #input$Tablo_rows_selected<- reactive(input$Tablo, {NULL})
  
  #############################################################################################
  ###################################### OBSERVE ##############################################
  #############################################################################################
  
  observe({
    input$plateform1

    if(input$type_recommandation == "Vente"){
      if(is.null(input$plateform1) == FALSE){
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
    }
    if(input$type_recommandation == "Popularite"){
      if(is.null(input$plateform1) == FALSE){
        minimum <- min(na.omit(as.numeric(as.character(df2$Annee[df2$platform==input$plateform1]))))
        maximum <- max(na.omit(as.numeric(as.character(df2$Annee[df2$platform==input$plateform1]))))
      }else{
        minimum <- 1950
        maximum <- 2022
      }
      updateSliderInput(
        inputId = "years3",
        min = minimum,
        max = maximum,
        value = c(minimum,maximum)
      )
    }
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
      
      updateSelectizeInput(
        inputId = "plateform1",
        choices = c(levels(df$Platform)))

      updateSelectizeInput(inputId = "genre1",
                        choices = c(levels(df$Genre)))
    }
    if(input$type_recommandation == "Popularite"){
      updateSliderInput(
        inputId = "years3",
        min = 1950,
        max = 2022,
        value = c(1950,2022)
      )

      updateSelectizeInput(
        inputId = "plateform1",
        choices = c(levels(df2$platform))
      )
      
      updateSelectizeInput(inputId = "genre1",
                        choices = c(
                          "Action" = "Action",
                          "Aventure" = "Aventure",
                          "Puzzle" = "Puzzle",
                          "Course" = "Racing",
                          "Education" = "Educational",
                          "Compilation" = "Compilation",
                          "Simulation" = "Simulation",
                          "Sports" = "Sports",
                          "Strategie" = "Strategie",
                          "Role play" = "Role_play",
                          "Edition special" = "Edition_special",
                          "DLC" = "DLC"))
    }
    })
  
  #############################################################################################
  ###################################### REACTIVE #############################################
  #############################################################################################
  
  tablo <- reactive({
    
    ####### PAR SALES ########
    if(input$type_recommandation == "Vente"){
      
      image <- paste0("<img src=\"", src="https://www.mobygames.com/images/covers/s/264995-air-hockey-android-front-cover.jpg", "\" height=\"30\" data-toggle=\"tooltip\" data-placement=\"right\" title=\"", "bijo", "\"></img>")
      
      tablo <- df %>%
        filter(as.numeric(as.character(Year)) >= input$years3[1]) %>%
        filter(as.numeric(as.character(Year)) <= input$years3[2])
      
      if(is.null(input$plateform1) == FALSE){
        tablo <- tablo %>% 
          filter(Platform == input$plateform1)
      }
      if(is.null(input$genre1) == FALSE){
        tablo <- tablo %>% 
          filter(Genre == input$genre1)
      }
      best_choice_tableau <- tablo %>% 
        slice(1:3) %>% 
        select("Name":"Publisher") 
        #mutate(picture = c(image, image, image)) %>% 
    }
    
    ###############PAR POPULARITE############
    if(input$type_recommandation == "Popularite"){
      
      tablo <-df2 %>%
        filter(as.numeric(as.character(Annee)) >= input$years3[1]) %>%
        filter(as.numeric(as.character(Annee)) <= input$years3[2])
      
      if(is.null(input$plateform1) == FALSE){
        tablo <- tablo %>% 
          filter(platform == input$plateform1)
      }
      
      if(is.null(input$genre1) == FALSE){
        tablo <- tablo %>% 
          filter(tablo[input$genre1] %>% rowSums() > 0)
      }
      
      best_choice_tableau <- tablo %>% 
        arrange(reviews) %>% 
        slice(1:3)
    }
    best_choice_tableau
    
  })
  
  ####################################################################################
  ####################### Reactive SWITCH ###########################################
  ####################################################################################
  
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
  
  # ###### NOT RUN AND TO DELETE AT THE END >>>>>>
  # variable_genre <- reactive({
  #   sapply(input$genre1, switch_vector)
  # }) 
  # >>>>>>>>>>
  
  ###################################################################################
  ############################## OUTPUT #############################################
  ####################################################################################
  
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
    tablo() %>% 
      select(Name, Publisher, Annee, platform) %>% 
      datatable(rownames = F,
                extensions = c('Select'),
                selection = "single",
                options = list(
                  select = list(style = 'os', items = 'row'), 
                  dom = 't',
                  ordering = F),
                escape = F)
    })
    
  output$desc <- renderText(expr = {
       # cat(paste(input$Tablo_row_last_clicked))
       # cat(paste(input$type_recommandation))
       desc <- tablo() %>% 
         slice(input$Tablo_rows_selected) %>% 
         pull(Description)
       
        paste("<h1>Description</h1>",
              "<p>", 
              desc %>% 
                str_remove("\\[edit description\\]") %>% 
                str_remove("\\[add description\\]") %>%
                str_to_upper() %>% 
                str_to_sentence() %>% 
                str_replace_all("\n", "<br>"), 
              "</p>")
       })
     output$desc2 <- renderPrint({
       print(input$Tablo_rows_selected)
     })
     
     output$image <- renderText({
       paste0("<img src=\"", src="https://www.mobygames.com/images/covers/s/264995-air-hockey-android-front-cover.jpg", "\" height=\"150\" data-toggle=\"tooltip\" data-placement=\"center\" title=\"", "bijo", "\"></img>")
     })

}

# Run the application 
shinyApp(ui = ui, server = server)

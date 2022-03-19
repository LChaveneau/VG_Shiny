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
library(fresh)
library(bslib)

boxStyle ='color:black; background-color:#DE4F53; border-radius: .1em; color:white; align:right; text-align:left; display: table-cell'
my_theme <- bs_theme(
  bg = "#e5e5e5", fg = "#0d0c0c", primary = "#dd2020",
  base_font = font_google("Press Start 2P"),
  code_font = font_google("Press Start 2P"),
  "font-size-base" = "0.55rem", "enable-rounded" = FALSE #"0.75rem"
) %>%
  bs_add_rules(
    '@import "https://unpkg.com/nes.css@latest/css/nes.min.css"'
  )


df <- read_csv('vgsales3.csv')
df2 <- readRDS('data_jeux_image.rds')
df2 <- df2 %>% 
  left_join(readRDS("data_jeux_code_html.rds"), c("Name" = "Name"))

df <- df  %>% 
  mutate(Platform = as.factor(Platform)) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Genre = as.factor(Genre)) %>% 
  mutate(Publisher = as.factor(Publisher))


df2 <- df2  %>% 
  mutate(Publisher = as.factor(Publisher)) %>% 
  mutate(Annee = as.factor(Annee)) %>% 
  mutate(platform = as.factor(platform))

df2 <- df2[!duplicated(cbind(df2$Name,
                            df2$platform)),]





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
#   "Strategie" = df2$Strategie,bslib
#   "Role_play" = df2$Role_play,
#   "Edition_special" = df2$Edition_special,
#   "DLC" = df2$DLC)
# }
# >>>>>>

# Define UI for application that draws a histogram
ui <- fluidPage(theme = my_theme,
  #includeCSS("www/TableStyle.css"),

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
                      htmlOutput("image"),
                      style = "background-color: #EAE7E6;display: table;"
                ),
                #width: auto
                #display: table
                #position: relative;
                column(8,
                       htmlOutput('desc'),
                       htmlOutput('desc2'),
                       style = boxStyle
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
    #theme = includeCSS("bootstrap.min.css")
    )
    # Sidebar with a slider input for number of bins 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #############################################################################################
  ###################################### EVENT REACTIVE #######################################
  #############################################################################################
  
  
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
        slice(1:7) #%>% 
        #mutate(picture = c(image, image, image))
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
        slice(1:7)
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
    if(input$type_recommandation == "Vente"){
      recommandation <- tablo() %>% 
        select("Name":"Publisher") %>% 
        datatable(rownames = F,
                  style = "bootstrap",
                  extensions = c('Select'),
                  selection = "single",
                  options = list(
                    select = list(style = 'os', items = 'row'), 
                    dom = 'lt',
                    ordering = F, 
                    pageLength = 3, 
                    lengthMenu = c(3, 5, 7)),
                  escape = F)
    }
    
    if(input$type_recommandation == "Popularite"){
      recommandation <- tablo() %>% 
        select(image, Name, Publisher, Annee, platform) %>% 
        ################## Affectation de la bonne taille de l'image###############
        mutate(valeur_width = image %>%  
                 str_extract("width=\"\\d+\"") %>% 
                 str_extract("\\d+") %>% 
                 as.numeric(),
               valeur_height = image %>%  
                 str_extract("height=\"\\d+\"") %>% 
                 str_extract("\\d+") %>% 
                 as.numeric()) %>% 
        mutate(image = image %>% 
                 str_replace(paste0("width=\"",valeur_width,"\""), paste0("width=\"",valeur_width * 0.5,"\"")) %>% 
                 str_replace(paste0("height=\"",valeur_height,"\""), paste0("height=\"",valeur_height * 0.5,"\""))) %>% 
        ############################################################################
        select(image, Name, Publisher, Annee, platform) %>% 
        datatable(rownames = F,
                  style = "bootstrap",
                  extensions = c('Select'),
                  selection = "single",
                  options = list(
                    select = list(style = 'os', items = 'row'), 
                    dom = 'lt',
                    ordering = F, 
                    pageLength = 3, 
                    lengthMenu = c(3, 5, 7)),
                  escape = F) %>% 
        ##### Hauteur ligne #######
        formatStyle(
          0,
          target = 'row',
          lineHeight='7'
        )
    }
    recommandation
    })
    
  output$desc <- renderText(expr = {
       # cat(paste(input$Tablo_row_last_clicked))
       # cat(paste(input$type_recommandation))
    
    if(is.null(input$Tablo_rows_selected) == FALSE){
      if(input$type_recommandation == "Popularite"){
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
      }else{
        desc <- tablo() %>% 
          slice(input$Tablo_rows_selected) %>% 
          pull(Description)
        
        paste("<h1>Description</h1>",
              "<p>", 
              desc %>% 
                str_remove_all("\\[.*?\\]") %>% 
                str_to_upper() %>% 
                str_to_sentence() %>% 
                str_replace_all("\n", "<br>") %>%
                str_remove("Gameplay"), 
              "</p>")
      }
    }
    })
     output$desc2 <- renderText({
     })
     
     output$image <- renderText({

       if(is.null(input$Tablo_rows_selected) == FALSE){
         if(input$type_recommandation == "Popularite"){
          code_html <- tablo() %>%
            slice(input$Tablo_rows_selected) %>%
            pull(code_html) %>% 
            str_replace_all("48", "25")

         }

         if(input$type_recommandation == "Vente"){
            code_html <- tablo() %>%
               slice(input$Tablo_rows_selected) %>%
               pull(tableau)
            }
         paste0(code_html)
       }
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

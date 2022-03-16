boxStyle ='color:black; background-color:#DE4F53; border-radius: .1em; color:white; align:right; text-align:left; display: table-cell;'

library(shiny)
library(tidyverse)
library(tidyr)
library(DT)
library(fresh)
library(bslib)


df <- read_csv('vgsales2.csv')
df2 <- readRDS('data_jeux_image.rds')

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
#   "Strategie" = df2$Strategie,bslib
#   "Role_play" = df2$Role_play,
#   "Edition_special" = df2$Edition_special,
#   "DLC" = df2$DLC)
# }
# >>>>>>

# Define UI for application that draws a histogram
ui <- fluidPage(bs_global_theme(),
  
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
                        )
                      ),
                      fluidRow(
                        column(4
                        ),
                        column(8,
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
  )
  #theme = includeCSS("bootstrap.min.css")
)
# Sidebar with a slider input for number of bins 

# Define server logic required to draw a histogram
server <- function(input, output) {
  bslib::bs_themer()
  
 
}

# Run the application 
shinyApp(ui = ui, server = server)

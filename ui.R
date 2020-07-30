library(dplyr)
library(shiny)
library(ggplot2)
library(wordcloud)
library(shinythemes)
library(geojsonio)
library(leaflet)
library(tidytext)

States <- geojson_read("data/ChorData.geojson", what = "sp") #Our spatial dataset containing polygons for each state 
Fatal <- read.csv('data/Fatal.csv', row.names = 1)

WordCloudText <- Fatal  %>%
    select(text = description, cause_of_death)
WordCloudText <- WordCloudText %>% unnest_tokens(word, text) %>% count(cause_of_death, word, sort = TRUE) 
WordCloudText <- WordCloudText %>% anti_join(stop_words, by = 'word') #Remove Stop Words 

Fatal$date_injury <- as.Date(Fatal$date_injury, format='%m/%d/%Y')
Fatal$subject_age <- as.integer(Fatal$subject_age)
Fatal$subject_gender <- as.factor(Fatal$subject_gender)
Fatal$subject_race <- as.factor(Fatal$subject_race)
Fatal$cause_of_death <- as.factor(Fatal$cause_of_death)

shinyUI(fluidPage(
    theme = shinytheme('darkly'),
    titlePanel('Fatal Encounters Data Visualization'),
    
    tabsetPanel(
        #### Info Tab
        tabPanel('Info',
                 column(12,
                        
                        h1('Fatal Encounters', style = 'text-align: center;'), 
                        h4('By Jacob Martin and Robert Bintu', style = 'text-align: center;'),
                        br(),
                        br(), 
                        p('This is a project that we have created for our visualizations class. It is based on the dataset that comes from FatalEncounters.org.
                          The Fatal Encounters team is a repository that collects data with the vision in mind to enable others to perform further work on it.
                          As noted from their website on what exactly the data consists of: '),
                        em('"We try to document all deaths that happen when police are present or that are caused by police: on-duty, off-duty, criminal, 
                           line-of-duty, local, federal, intentional, accidental–all of them."'),
                        br(),
                        br(), 
                        p('At the time of pulling the data on 7/14/2020, there were approximately 28,000 observations.  The data collection by the Fatal Encounters
                          team began in 2000.  It is comprised of metrics on race, gender, cause of death, the police department involved and more.  Locationally, 
                          it spans all 50 states so we felt like a geospatial visualization would be fitting.  The data was combined with population data from
                          2010 at the most recent census collection.  This was also the midpoint of the dataset so it felt fitting to use that for standardizing
                          the choropleth map for states.'), 
                        br(),
                        br(),
                        h4('Notes about Interactions'),
                        p('In order to speed up the loading speed of the application, we created it so that it will activate the mapping shapes and points upon 
                          selecting an option.  Upon selecting a tab use the settings and options in order to trigger the event which amends the plot for the 
                          data requested.'), 
                        br(),
                        br(),
                        h4('Sources'), 
                        p('Dataset: D. Brian Burghart and the entire Fatal Encounters team.'),
                        a('fatalencounters.org'),
                        br(),
                        a('https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/edit#gid=0'),
                        br(),
                        br(),
                        p("Inspiration for the Point's tab visualization from the Fatal encounters visualization team"),
                        a('https://github.com/adv-datasci/fatalencounters')
                        )  
                 
        ),
        
        #### Choropleth Tab 
        tabPanel('Choropleth',
            
            fluidRow(style = 'height: 580px', 
        
                    leafletOutput('ChorMap', width = '100%', height = '100%')
                    
                ),
            
            fluidRow(style = 'background: #48B3A1; height: auto',
                     
                column(3, style = 'color: black;', 
                       plotOutput('GenderCols', height = "100px"),
                       h3('Settings', style = 'color: black;'), 
                       checkboxInput('ChorStnd', 'Standardize', value = FALSE)
                ), 
                
                column(5, plotOutput('RaceCols', height = '300px')), 
                
                column(4, 
                       h1(textOutput('statename'), style = 'color: black'),
                       h4(textOutput('statePop'), style = 'color: black'), 
                       h4(textOutput('stateDeaths'), style = 'color: black'),
                       h4(textOutput('stateAvgAge'), style = 'color: black'),
                       h4(textOutput('stateDeathPer10'), style = 'color: black')
                )
            )
        ),
        
        #### Points Tab 
        tabPanel('Points', 
            
            fluidRow(
                
                column(12, #The column here bumps out the settings a little bit so that they aren't hugging the wall
                    # Title 
                    h3('Settings'),
                    
                    fluidRow(
                        
                        column(3, 
                            
                            # Slider input to select year range on Points Tab  
                            sliderInput('SlideYear', 
                                        label = 'Select Years',
                                        min = 2000,
                                        max = 2020,
                                        value = c(2015, 2020), 
                                        step = 1
                            )
                        ), 
                    
                        column(3, 
                            
                            # Radio button input to select how the color is determined on the map for points
                            radioButtons('ColorChoice', 
                                         label = 'Color Variable', 
                                         choices = c('Race', 'Gender', 'Cause of Death'),
                                         selected = 'Race'
                                         )
                        )
                    )
                )
            ), 
            
            hr(),
            
            fluidRow(style = 'height: 550px', 
                
                leafletOutput('PointsMap', width = '100%', height = '100%')
                
            )        
        ),
        
        #### Word Cloud Tab 
        tabPanel('WordCloud', 
                 
                fluidRow(
                    column(12, #The column here bumps out the settings a little bit so that they aren't hugging the wall
                        
                        h3('Settings'),
                        
                        column(3, 
                               selectInput(
                                   inputId = 'WordCause',
                                   label = 'Cause of Death: ',
                                   choices = c('All', unique(WordCloudText$cause_of_death)),
                                   selected = 'All'
                               ) 
                        ),
                        
                        column(3, 
                            sliderInput(
                                inputId = 'WordMax', 
                                label = 'Max # of Words', 
                                min = 100, 
                                max = 400, 
                                value = 200, 
                                step = 50
                            )
                        ), 
                        
                        column(3, 
                            sliderInput(
                                inputId = 'WordLargest', 
                                label = 'Size of largest words:', 
                                min = 4, 
                                max = 10, 
                                value = 7
                            )
                        ),
                        column(3, 
                            sliderInput(
                                inputId = 'WordSmallest', 
                                label = 'Size of smallest words:', 
                                min = 0.5, 
                                max = 2.5, 
                                value = 1
                            )
                        )
                    )
                ),
                
                hr(),
                
                fluidRow(
                    plotOutput('Cloud', height = '600px')
                )
        )
    )

))

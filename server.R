library(dplyr)
library(shiny)
library(ggplot2)
library(wordcloud)
library(shinythemes)
library(geojsonio)
library(leaflet)
library(tidytext)

# Load Data
    ## Main Data Set and Choropleth Data
States <- geojson_read("data/ChorData.geojson", what = "sp") #Our spatial dataset containing polygons for each state 
Fatal <- read.csv('data/Fatal.csv', row.names = 1)
    ## For Word Cloud Plot 
WordCloudText <- Fatal  %>%
    select(text = description, cause_of_death)
WordCloudText <- WordCloudText %>% unnest_tokens(word, text) %>% count(cause_of_death, word, sort = TRUE) 
WordCloudText <- WordCloudText %>% anti_join(stop_words, by = 'word') #Remove Stop Words 

Fatal$date_injury <- as.Date(Fatal$date_injury, format='%m/%d/%Y')
Fatal$subject_age <- as.integer(Fatal$subject_age)
Fatal$subject_gender <- as.factor(Fatal$subject_gender)
Fatal$subject_race <- as.factor(Fatal$subject_race)
Fatal$cause_of_death <- as.factor(Fatal$cause_of_death)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Code for creation of the Choropleth map 
    output$ChorMap <- renderLeaflet({
        
        ChorMap <- leaflet(States) %>%
            addTiles(
                urlTemplate = "//{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
                attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>', 
                options = tileOptions(minZoom = 4, maxZoom = 4)
            ) %>% 
            setView(lat=38.978178, lng= -96, 4)
    })
    
    # Observe to modify the Choropleth with based on if they want the states standardized
    observe({
        if (input$ChorStnd == TRUE){
            ChorDom <- States$DeathsPer10000
            bins <- c(0, 0.60, 0.80, 1, 1.20, 1.40, Inf)
            title <- 'Deaths Per 10,000'
            palette <- colorBin("YlOrRd", domain=ChorDom, bins = bins, na.color = 'transparent') 
        } else {
            bins <- c(0, 250, 500, 750, 1000, 1500, Inf)
            ChorDom <- States$Deaths
            title <- 'Total Deaths'
            palette <- colorBin("YlOrRd", domain=ChorDom, bins = bins, na.color = 'transparent')
        }
        
        leafletProxy('ChorMap') %>%
            clearShapes() %>% 
            clearControls() %>%
            addPolygons(
                data = States,
                layerId = ~name,
                fillColor = ~palette(ChorDom),
                weight = 1,
                opacity = 1,
                color = 'white',
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 4, 
                    color = 'black',
                    bringToFront = TRUE
                ),
                popup = paste0('<strong>State: </strong>', States$name, '<br>',
                               '<strong># Deaths: </strong>', format(States$Deaths, nsmall=1, big.mark=','), '<br>',
                               '<strong>2010 Population: </strong>', format(States$Pop2010, nsmall=1, big.mark=','), '<br>',
                               '<strong>Average Age of Deceased: </strong>', round(States$Avg_Age, 2), '<br>',
                               '<strong>Deaths Per 10,000 during Period: </strong>', round(States$DeathsPer10000, 3)),
                popupOptions = popupOptions(
                    autoPan = FALSE
                )
            ) %>%
                addLegend(pal = palette, values = ChorDom, title = title, position = 'bottomleft', opacity = 0.75)
    })
    
    # Events for clicking on the Choropleth Map 
    observeEvent(input$ChorMap_shape_click, { 
        event <- input$ChorMap_shape_click
        output$statename <- renderText(event$id)
        output$statePop <- renderText({ paste0('2010 Population: ', format(States$Pop2010[States$name == event$id], nsmall=1, big.mark=',')) })
        output$stateDeaths <- renderText({ paste0('Total Deaths: ', format(States$Deaths[States$name == event$id], nsmall=1, big.mark=',')) })
        output$stateAvgAge <- renderText({ paste0('Average Age of Deceased: ', format(round(States$Avg_Age[States$name == event$id], 2), nsmall=1, big.mark=',')) })
        output$stateDeathPer10 <- renderText({ paste0('Deaths per 10k Pop During Period: ', round(States$DeathsPer10000[States$name == event$id], 3)) })
        
        m <- States$Num_Males[States$name == event$id]
        f <- States$Num_Females[States$name == event$id]
        
        bl <- States$Num_Black[States$name == event$id]
        as <- States$Num_Asian[States$name == event$id]
        wh <- States$Num_White[States$name == event$id]
        hi <- States$Num_Hispanic[States$name == event$id]
        me <- States$Num_MidEastern[States$name == event$id]
        na <- States$Num_NativeAmer[States$name == event$id]
        un <- States$Num_Unknown[States$name == event$id]
        
        gens <- data.frame(Gender = c('Male', 'Female'), 
                           Count = c(m, f))
        
        race <- data.frame(Race = c('African-American/Black', 'Asian/Pacific Islander', 'European-American/White', 'Hispanic/Latino', 
                                    'Middle Eastern', 'Native American/Alaskan', 'Race Unknown'), 
                           Count = c(bl, as, wh, hi, me, na, un))
        
        gensplot <- gens %>% 
            ggplot(aes(x = Gender, y = Count)) + 
            geom_col(fill = '#3284CD', color = '#16191C', position = position_dodge(width = .75)) + 
            geom_label(aes(label=Count), hjust = -.50,  colour = '#16191C') +
            scale_y_discrete(expand = expansion(c(0, 0.2))) +
            theme(
                axis.ticks = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_text(color = '#16191C'),
                panel.grid = element_blank(),
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                plot.background = element_rect(fill = '#48B3A1', colour = '#48B3A1'),
                panel.background = element_rect(fill = '#48B3A1', colour = '#48B3A1'),
            ) + 
            coord_flip()
        
        output$GenderCols <- renderPlot(gensplot)
        
        RacePlot <- race %>% 
            ggplot(aes(x = reorder(Race, Count), y = Count)) + 
            geom_col(fill = '#3284CD', color = '#16191C') + 
            scale_y_discrete(expand = expansion(c(0, 0.2))) +
            geom_label(aes(label=Count), hjust = -.50,  colour = '#16191C') +
            theme(
                axis.ticks = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_text(color = '#16191C'),
                panel.grid = element_blank(),
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                plot.background = element_rect(fill = '#48B3A1', colour = '#48B3A1'),
                panel.background = element_rect(fill = '#48B3A1', colour = '#48B3A1'),
            ) + 
            coord_flip()
        
        output$RaceCols <- renderPlot(RacePlot)
        
    })
    
    # Code for creation of the Points Map
    output$PointsMap <- renderLeaflet({

        pal <- colorFactor('Set3', domain = Fatal$subject_race, na.color = 'transparent')
        
        PM <- leaflet(data = Fatal) %>%
            addTiles(
                urlTemplate = "//{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
                attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>', 
                options = tileOptions(minZoom = 4)
            ) %>% 
            setView(lat=38.978178, lng= -96, 4)
    })
    
    # Reactive element to watch the year selection input based on settings
    Points <- reactive({
        year_start <- input$SlideYear[1]
        year_end <- input$SlideYear[2]
        
        subset(Fatal, year >= year_start & year <= year_end)
    })
    
    # Observe to adjust points map based on settings
    observe({
        #  element to select the color input based on settings
        if (input$ColorChoice ==  'Race') {
            d <- Fatal$subject_race
            Ptitle <- 'Race'
            pal <- colorFactor('Dark2', domain = d, na.color = 'transparent')
        } else if (input$ColorChoice ==  'Gender') {
            d <- Fatal$subject_gender
            Ptitle <- 'Gender'
            colpicks <- c('#FFFFFF', '#FF6767', '#4224FF', '#67FF69')
            pal <- colorFactor(colpicks, domain = d, na.color = 'transparent')
        } else if (input$ColorChoice ==  'Cause of Death') {
            d <- Fatal$cause_of_death
            Ptitle <- 'Cause of Death'
            colpicks <- c('#6B00DC', '#C500DC', '#DC0025', '#0037B7', '#6191FF',
                          '#61FFFF', '#09B964', '#FFFD9B', '#6A4208', '#FD9907',
                          '#F5E7E0', '#9C938E', '#2D6D63', '#FFAFAF', '#9D79CF')
            pal <- colorFactor(colpicks, domain = d, na.color = 'transparent')
        } 
        
        leafletProxy('PointsMap') %>%
            clearMarkers() %>%
            clearControls() %>%
            addCircleMarkers(
                data = Fatal, 
                radius = 3, 
                lat = ~Latitude,
                lng = ~Longitude,
                color = pal(d), 
                fillOpacity = 0.7,
                stroke = FALSE, 
                layerId = ~subject_name, 
                popup = paste(paste0('<center><img src="', Fatal$photo, '" height=100px ></img></center> <br>', 
                                     '<strong>Name: </strong>', Fatal$subject_name, '<br>',
                                     '<strong>Age: </strong>', Fatal$subject_age, '<br>',
                                     '<strong>Gender: </strong>', Fatal$subject_gender, '<br>',
                                     '<strong>Incident Date: </strong>', Fatal$date_injury, '<br>',
                                     '<strong>Cause: </strong>', Fatal$cause_of_death, '<br>',
                                     '<strong>Location: </strong>', Fatal$city, ' ', Fatal$state, '<br>',
                                     '<strong>Link: </strong>', '<a href="', Fatal$link, '">', Fatal$link, '</a>', '<br>'
                ))
            ) %>%
            addLegend(pal = pal, values = d, title = Ptitle, position = 'bottomleft', opacity = 0.75)
    })
    
    
    # Word Cloud 
    options <- eventReactive(input$WordCause, {
        withProgress({
            setProgress(message = "Processing...")
            
            if (input$WordCause == 'All'){
                x <- WordCloudText %>% group_by(word) %>% summarize(n = sum(n)) %>% arrange(desc(n))
            } else {
                x <- WordCloudText %>% filter(cause_of_death == input$WordCause)
            }
            
            return(x)
        })
    })
    
    output$Cloud <- renderPlot({
        text <- options()
        wordpal <- brewer.pal(11, 'Paired')
        
        par(bg='black')
        text %>% 
            with(
                wordcloud(word,
                          n,
                          scale = c(input$WordLargest, input$WordSmallest), 
                          random.order = FALSE,
                          colors = wordpal,
                          max.words = input$WordMax,
                          
                ))
    })

})






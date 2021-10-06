# import libraries
library(shiny)
library(tidyverse)
library(facetscales)
library(ggplot2)
library(patchwork)
library(dplyr)
library(leaflet)
library(rgdal)
require(htmltools)

# so R dont abbreviate numbers
options(scipen = 1000000)

md <- read_csv("./page1_1.csv")
r <- read_csv("./page1_2.csv")
r$year <- as.numeric(r$year)
md$year <- as.numeric(md$year)
d <- filter(md, desc == "Divorces")
m <- filter(md, desc == "Marriages")
dr <- filter(r, desc == "Divorces")
mr <- filter(r, desc == "Marriages")
bc <- read_csv("./page2_1.csv")
ms <- read_csv("./page2_2.csv")
c <- read_csv("./page2_3.csv")
m1 <- read_csv("./page2_4a.csv")
m1f <- filter(m1, Gender == "female")
m1m <- filter(m1, Gender == "male")
m2 <- read_csv("./page2_4b.csv")
m2f <- filter(m2, Gender == "female")
m2m <- filter(m2, Gender == "male")
doc <- read_csv("./day_of_occurence.csv")
mst <- read_csv("./marriage_state.csv")
wm <- read_csv("country_rate.csv")

md_data <- function(yearx, descx, type){
  if (type == "r") {
    data <- filter(r, year == yearx, desc == descx)
    return(data$count)
  } else if (type == "c"){
    data <- filter(md, year == yearx, desc == descx)
    return(data$count)
  }
}

prev_t <- vector()

shinyServer(function(input, output){
  
  output$plot1 <- renderPlot({
    if (input$type1 == 1) {
      scale_y <- list(
        "Divorces" = scale_y_continuous(limits = c(40000,60000)),
        "Marriages" = scale_y_continuous(limits = c(110000, 130000))
      )

      plot1 <- ggplot(md, aes(year, count, color = desc)) +
        geom_path() +
        facet_grid_sc(rows = vars(desc), scales = list(y = scale_y)) +
        scale_x_continuous(n.breaks = 10, limits = c(2007.5, 2018.5)) +
        theme_bw() +
        scale_color_manual(values = c("Divorces" = "red", "Marriages" = "blue"), name = NULL)+
        theme(strip.text.y = element_blank()) +
        labs(title = "Marriage and Divorce Count 2008 - 2018", x = "Year", y = "Count")
    } else if (input$type1 == 2) {
      plot1 <- ggplot(r, aes(year, count, group = 1, color = desc)) +
        geom_path() +
        facet_grid(desc~.) +
        theme_bw() +
        scale_color_manual(values = c("Divorces" = "red", "Marriages" = "blue"), name = NULL)+
        theme(strip.text.y = element_blank()) +
        labs(title = "Marriage and Divorce Rate 2008 - 2018", x = "Year", y = "Rate") +
        scale_x_continuous(n.breaks = 9, limits = c(2008.5, 2018.5))
    }
    plot1
  })
  
  output$info1 <- renderText(
    if (is.null(input$click1)){
      paste("2008 - 2018 Average", "\n", "Average Count:", "\n", "  Divorces: ", round(mean(d$count)), 
            "\n", "  Marriages: ", round(mean(m$count)), "\n", "Average Rate:", "\n", "  Divorces: ", round(mean(dr$count)), 
            "\n", "  Marriages: ", round(mean(mr$count)))
    } else {
      click1 <- round(input$click1$x)
      paste(click1, "\n", click1, " Count:", "\n", "  Divorces: ", md_data(click1, "Divorces", "c"), 
            "\n", "  Marriages: ", md_data(click1, "Marriages", "c"), "\n", click1, " Rate:", "\n", "  Divorces: ", md_data(click1, "Divorces", "r"), 
            "\n", "  Marriages: ", md_data(click1, "Marriages", "r"))
    }
  )
  
  output$plot2 <- renderPlot({
    bc$per <- bc$count/sum(bc$count)
    bc$label <- scales::percent(bc$per)
    plot2 <- ggplot(bc, aes("", count, fill= Description)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_brewer() +
      theme_void() +
      labs(title="Average Marriage Count based on Birth Country 2008 - 2018", x = "", y = "") +
      geom_text(aes(label=label), position = position_stack(vjust = 0.5))
    plot2
  })
  
  output$plot3 <- renderPlot({
    ms$per <- bc$count/sum(ms$count)
    ms$label <- scales::percent(ms$per)
    plot3 <- ggplot(ms, aes("", count, fill= Description)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_brewer(palette = "Purples") +
      theme_void() +
      labs(title="Average Marriage Count based on Marital Status 2008 - 2018", x = "", y = "") +
      geom_text(aes(label=label), position = position_stack(vjust = 0.5))
    plot3
  })
  
  output$plot4 <- renderPlot({
    plot4 <- ggplot(c, aes(year, count, fill = Description)) + 
      geom_bar(stat = "identity", position = "fill") + 
      labs(y = "Percentage", title = "Marriage based on Celebrant 2008 - 2018", x = "Year") +
      scale_fill_manual(values = c("civil celebrants"="#82d193", "ministers of religion"="#a190f0")) +
      theme_minimal() +
      scale_x_continuous(n.breaks = 10)
    plot4
  })
  
  output$plot5 <- renderPlot({
    if (input$type2 == 1) {
      plot5a <- ggplot(m1m, aes(`Age Range`, count, fill = Gender)) +
        geom_bar(stat = "identity") +
        scale_y_reverse() +
        coord_flip() +
        scale_fill_manual(values = c("lightblue"), guide = FALSE) +
        theme_minimal() +
        labs(title = "First Marriage by Age", y = "Male")
      
      plot5b <- ggplot(m1f, aes(`Age Range`, count, fill = Gender)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_minimal() +
        theme(axis.title.y = element_blank(),
              axis.text.y  = element_blank(),
              axis.ticks.y = element_blank()) +
        scale_fill_manual(values = c("pink"), guide = FALSE) +
        labs(y = "Female")
      
    } else if (input$type2 == 2) {
      plot5a <- ggplot(m2m, aes(`Age Range`, count, fill = Gender)) +
        geom_bar(stat = "identity") +
        scale_y_reverse() +
        coord_flip() +
        scale_fill_manual(values = c("lightblue"), guide = FALSE) +
        theme_minimal() +
        labs(title = "Remarriage by Age", y = "Male")
      
      plot5b <- ggplot(m2f, aes(`Age Range`, count, fill = Gender)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_minimal() +
        # need to have axis blank to make it side by side
        theme(axis.title.y = element_blank(),
              axis.text.y  = element_blank(),
              axis.ticks.y = element_blank()) +
        scale_fill_manual(values = c("pink"), guide = FALSE) +
        labs(y = "Female")
    }
    # use patchwork to combine two graph together
    plot5a+plot5b
  })
  
  output$plot6 <- renderPlot({
    doc1 <- doc %>% group_by(Date) %>%
      summarise(count = mean(count)) %>%
      ungroup()
    
    plot6 <- ggplot(doc1, aes(Date, count, fill = Date)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(colours = rainbow(31), guide = FALSE) +
      scale_x_continuous(n.breaks = 31) +
      theme_minimal()
    
    plot6
  })
  
  output$plot7 <- renderPlot({
    doc2 <- doc %>% group_by(Month) %>%
      summarise(count = mean(count)) %>%
      ungroup()
    
    doc2
    
    plot7 <- ggplot(doc2, aes(Month, count, fill = Month)) +
      geom_bar(stat = "identity") +
      scale_colour_manual(values = rainbow(12)) +
      theme_minimal()
    
    plot7
  })
  
  output$plot8 <- renderPlot({
    months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    doc3 <- arrange(transform(doc,
                              Month=factor(Month,levels=months)),Month)
    
    plot8 <- ggplot(doc3) +
      geom_bar(stat = "identity", aes(Date, count, fill = Month, color = Month)) +
      facet_grid(.~Month) +
      scale_colour_manual(values = rainbow(12)) +
      theme_minimal()
    
    plot8
  })
  
  output$plot9 <- renderPlot({
    msr <- select(mst, State = state, year, count = `total marriage`)
    
    plot9 <- ggplot(msr, aes(color = State)) +
      geom_line(aes(year, count)) +
      theme_minimal() +
      labs(title = "Marriage Count by States 2008 - 2018", x = "Year", y = "Count") +
      scale_fill_brewer("Paired")+
      scale_x_continuous(n.breaks = 10)
    
    plot9
  })
  
  output$plot10 <- renderPlot({
    
      cc <- select(mst, State = state, year, count = `civil celebrants`)
      
      plot10 <- ggplot(cc, aes(color = State)) +
        geom_line(aes(year, count)) +
        theme_minimal() +
        labs(title = "Civil Celebrants Count by States 2008 - 2018", x = "Year", y = "Count")+
        scale_x_continuous(n.breaks = 10)
      
      plot10
  })
  
  output$plot11 <- renderLeaflet({
    world_spdf <- readOGR( 
      dsn= paste0(getwd(),"/world_shape_file/") , 
      layer="TM_WORLD_BORDERS_SIMPL-0.3",
      verbose=FALSE
    )
    
    wm <- select(wm, NAME = Countries, year, rates)
    wm <- wm %>% group_by(NAME) %>%
      summarise(rates = mean(rates)) %>%
      ungroup()
    
    x <- merge(world_spdf,wm, y.by = NAME)
    
    bins <- c(0, 3, 4, 5, 6, 7, 8, 10)
    pal <- colorBin("YlOrRd", domain = wm$rates, bins = bins)
    
    mypalette <- pal
    
    labels <- sprintf(
      "<strong>%s</strong>",
      x$NAME
    ) %>% lapply(htmltools::HTML)
    
    # Basic choropleth with leaflet?
    plot11 <- leaflet(x) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons( fillColor = ~mypalette(rates),
                   weight = 2,
                   opacity = 1,
                   color = "white",
                   dashArray = "3",
                   fillOpacity = 0.7,
                   highlight = highlightOptions(
                     weight = 5,
                     color = "#666",
                     dashArray = "",
                     fillOpacity = 0.7,
                     bringToFront = TRUE),
                   label = labels,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto")) %>%
      addLegend(pal = pal, values = ~rates, opacity = 0.7, title = NULL,
                                                        position = "bottomright")
    
    plot11
  })
  
  output$plot12 <- renderLeaflet({
    msr <- select(mst, State = state, year, count = `total marriage`)
    msr <- msr %>% group_by(State) %>%
      summarise(count = mean(count)) %>%
      ungroup()
    
    cc <- select(mst, State = state, year, count = `civil celebrants`)
    cc <- cc %>% group_by(State) %>%
      summarise(count = mean(count)) %>%
      ungroup()
    
    aus_map <- readOGR( 
      dsn= paste0(getwd(),"/aus_map/") , 
      layer="gadm36_AUS_1",
      verbose=FALSE
    )
    
    if (input$type3 == 1){
      numb <- select(cc, NAME_1 = State, other = count)
      mapy <- select(msr, NAME_1 = State, count)
      mapy <- merge(mapy,numb, y.by = NAME_1)
      # make it long form
      mapy$NAME_1[mapy$NAME_1 == "ACT"] <- "Australian Capital Territory"
      mapy$NAME_1[mapy$NAME_1 == "NSW"] <- "New South Wales"
      mapy$NAME_1[mapy$NAME_1 == "NT"] <- "Northern Territory"
      mapy$NAME_1[mapy$NAME_1 == "QLD"] <- "Queensland"
      mapy$NAME_1[mapy$NAME_1 == "SA"] <- "South Australia"
      mapy$NAME_1[mapy$NAME_1 == "TAS"] <- "Tasmania"
      mapy$NAME_1[mapy$NAME_1 == "VIC"] <- "Victoria"
      mapy$NAME_1[mapy$NAME_1 == "WA"] <- "Western Australia"
      
      x <- merge(aus_map,mapy, y.by = NAME_1)
      
      bins <- c(0, 800, 8840, 16880, 24920, 32960, 40000, Inf)
      pal <- colorBin("RdPu", domain = mapy$count, bins = bins)
      
      mypalette <- pal
      
      labels <- sprintf(
        "<strong>%s</strong><br/>Marriage: %g<br/>Civil Celebrants: %g",
        x$NAME_1, round(x$count), round(x$other)
      ) %>% lapply(htmltools::HTML)
      
    } else if (input$type3 == 2) {
      numb <- select(msr, NAME_1 = State, other = count)
      mapy <- select(cc, NAME_1 = State, count)
      mapy <- merge(mapy,numb, y.by = NAME_1)
      mapy$NAME_1[mapy$NAME_1 == "ACT"] <- "Australian Capital Territory"
      mapy$NAME_1[mapy$NAME_1 == "NSW"] <- "New South Wales"
      mapy$NAME_1[mapy$NAME_1 == "NT"] <- "Northern Territory"
      mapy$NAME_1[mapy$NAME_1 == "QLD"] <- "Queensland"
      mapy$NAME_1[mapy$NAME_1 == "SA"] <- "South Australia"
      mapy$NAME_1[mapy$NAME_1 == "TAS"] <- "Tasmania"
      mapy$NAME_1[mapy$NAME_1 == "VIC"] <- "Victoria"
      mapy$NAME_1[mapy$NAME_1 == "WA"] <- "Western Australia"
      
      x <- merge(aus_map,mapy, y.by = NAME_1)
      
      bins <- c(0, 700, 5960, 11220, 16480, 21740, 27000, Inf)
      pal <- colorBin("BuGn", domain = mapy$count, bins = bins)
      
      mypalette <- pal
      
      labels <- sprintf(
        "<strong>%s</strong><br/>Marriage: %g<br/>Civil Celebrants: %g",
        x$NAME_1, round(x$other), round(x$count)
      ) %>% lapply(htmltools::HTML)
      
    }
    plot12 <- leaflet(x) %>% 
      addTiles()  %>% 
      setView(lat = -25.274399, lng = 133.775131 , zoom=4.2) %>%
      addPolygons(fillColor = ~mypalette(count),
                   weight = 2,
                   opacity = 1,
                   color = "white",
                   dashArray = "3",
                   fillOpacity = 0.7,
                   highlight = highlightOptions(
                     weight = 5,
                     color = "#666",
                     dashArray = "",
                     fillOpacity = 0.7,
                     bringToFront = TRUE),
                   label = labels,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto")) %>%
      addLegend(pal = pal, values = ~count, opacity = 0.7, title = NULL,
                position = "bottomright")
    
    plot12
  })
})
















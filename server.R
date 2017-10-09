library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
sensordata <- allsensors
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]
sensordata <- sensordata[order(sensordata$PM10),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 10.0, lat = 51.0, zoom = 6)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(sensordata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(sensordata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, sensordata$PM10, breaks = 20)$breaks

  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    hist(zipsInBounds()$PM10,
         breaks = centileBreaks,
         main = "SuperZIP score (visible zips)",
         xlab = "Percentile",
         xlim = range(allsensors$PM10),
         col = '#00DD00',
         border = 'white')
  })

  get_radius = function(zoom){
    if(zoom==0) return(7)
    else if(zoom==1) return(3000)
    else if(zoom==2) return(3000)
    else if(zoom==3) return(3000)
    else if(zoom==4) return(3000)
    else if(zoom==5) return(3000)
    else if(zoom==6) return(3000)
    else if(zoom==7) return(2000)
    else if(zoom==8) return(1500)
    else if(zoom==9) return(1000)
    else if(zoom==10) return(700)
    else if(zoom==11) return(500)
    else if(zoom==12) return(250)
    else if(zoom==13) return(150)
    else if(zoom==14) return(100)
    else if(zoom==15) return(100)
    else if(zoom==16) return(70)
    else if(zoom==17) return(40)
    else return (20)
  }
  
  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    x = range(allzips$college) / 10
    print(xyplot(temp ~ PM10, data = zipsInBounds(), xlim = range(x), ylim = range(sensordata$temp)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- "adultpop"

    if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(sensordata$PM10 >= (100 - input$threshold), "yes", "no")
      pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- sensordata[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }

    if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
      radius <- ifelse(sensordata$PM10 >= (100 - input$threshold), 30000, 3000)
    } else {
      zoom <- input$map_zoom
      if(is.null(zoom)) zoom = 7
      print (zoom)
      #radius <- (sensordata[["PM10"]] / sensordata[["PM10"]] * 10000 ) / zoom
      #radius <- (sensordata[["PM10"]] / sensordata[["PM10"]] ) * get_radius(zoom) 
      radius = get_radius(zoom)
      print(radius)
    }

    leafletProxy("map", data = sensordata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~sensor_id,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    
    selectedSensor <- allsensors[allsensors$sensor_id == zipcode,]
    content <- as.character(tagList(
      tags$h4("SensorID:", as.integer(selectedSensor$sensor_id)),
      tags$strong(HTML(sprintf("%s, %s",
                               selectedSensor$country, selectedSensor$city
      ))), tags$br(),
      sprintf("Rain: %s L", as.integer(selectedSensor$rain_3h)), tags$br(),
      sprintf("Wind Speed: %s Degree: %s", round(selectedSensor$wind_speed, digits = 2), round(selectedSensor$wind_deg, digits = 2)), tags$br(),
      sprintf("Floating Average 24h: %s", as.integer(selectedSensor$PM10_average)), tags$br(),
      sprintf("PM10 Forecast: %s", as.integer(selectedSensor$PM10))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
      print(event$id)
    })
  })


  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$country)) character(0) else {
      filter(cleansensors, Country %in% input$country) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({

    df <- cleansensors %>%
      filter(
        PM10_Forecast >= input$minScore,
        PM10_Forecast <= input$maxScore,
        is.null(input$country) | Country %in% input$country,
        is.null(input$cities) | City %in% input$cities
        #is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', SensorID, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}

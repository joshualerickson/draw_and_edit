

library(leaflet)
library(leafem)
library(leaflet.extras)
library(DT)
library(tidyverse)
library(shiny)
library(shinydashboard)




draw_and_edit <- function() {

  ui <- tagList(
    fluidPage(
      fluidRow(
        column(6,DT::dataTableOutput("data_sf",width="100%", height="400px")),
        column(6,leaflet::leafletOutput("aoi")),
        tags$hr()
      ),
      fluidRow(shiny::actionButton('clear', "Clear Map"),
               shiny::actionButton('finish', "Finish")
    )
  ))




    #server
    server = function(input, output, session) {

      output$aoi <- leaflet::renderLeaflet({


        leaflet() %>% leaflet::addProviderTiles(provider = "Stamen.Watercolor") %>%  leaflet::setView(lat = 48.91167, lng = -114.90246, zoom = 4) %>%
          leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                         rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
                                         markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = TRUE),
                                         polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE),
                                         editOptions = leaflet.extras::editToolbarOptions(edit = TRUE, remove = TRUE, selectedPathOptions = TRUE,allowIntersection = TRUE))



      })


      #store the sf in a reactiveValues
      values <- shiny::reactiveValues()
      values$sf <- sf::st_sf(sf::st_sfc(crs = 4326)) %>% sf::st_as_sf()
      print(values)
      x <- reactiveValues()



      #update map with user input
      shiny::observeEvent(input$aoi_draw_new_feature, {

        feat <- input$aoi_draw_new_feature
        coords <- unlist(feat$geometry$coordinates)
        coords <- matrix(coords, ncol = 2, byrow = T)

        feature_type <- input$aoi_draw_new_feature$properties$feature_type

        if(feature_type %in% c("rectangle","polygon")){

          new_sf <- isolate(sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf())
          print(new_sf)
          shiny::isolate(values$sf <- rbind(values$sf, new_sf))
          print(values$sf)
        } else {

          new_sf <- isolate(sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf() %>% sf::st_cast("POINT"))
          shiny::isolate(values$sf <- rbind(values$sf, new_sf))

        }



        if(!all(is.na(x$df_table$id))){

          sf_rec <- reactive(values$sf %>% dplyr::mutate(id = NA, rowID = dplyr::row_number()))
          print(sf_rec())
          df <- reactive(dplyr::left_join(sf_rec() %>% dplyr::select(rowID), x$df_table, by = c('rowID')))
          print(df())

        } else {

          df <- reactive(values$sf %>% dplyr::mutate(rowID = dplyr::row_number(),id = NA))

        }

        x$df_table <- df() %>% st_drop_geometry()

        print(x$df_table)

        output$data_sf <- DT::renderDT(DT::datatable(x$df_table, editable = T))


        proxy = DT::dataTableProxy('data_sf')

        observeEvent(input$data_sf_cell_edit, {
          info = input$data_sf_cell_edit

          i = info$row
          j = info$col
          v = info$value

          x$df_table[i, j] <- DT::coerceValue(v, x$df_table[i, j])
          DT::replaceData(proxy, x$df_table, resetPaging = FALSE)  # important
          print(x$df_table)
        })


      })





      #used to stop the app via button
      observeEvent(input$finish, {

        #need this to bring the last draw in

        if(!all(is.na(x$df_table$id))){

          sf_rec <- reactive(values$sf %>% dplyr::mutate(id = NA, rowID = dplyr::row_number()))
          print(sf_rec())
          df <- reactive(dplyr::left_join(sf_rec() %>% dplyr::select(rowID), x$df_table, by = c('rowID')))
          print(df())

        } else {

          df <- reactive(values$sf %>% dplyr::mutate(id = NA, rowID = dplyr::row_number()))

        }

        x$df <- df()
        x$df_table <- df() %>% st_drop_geometry()

        maps <- shiny::reactive(x$df)

        aoi <<- maps()

        shiny::stopApp()

      })

      #set to null if clear and reset map
      shiny::observeEvent(input$clear, {

        values$sf <- NULL
        x$df_table <- NULL

        output$aoi <- leaflet::renderLeaflet({


          leaflet() %>% leaflet::addProviderTiles(provider = "Stamen.Watercolor") %>%  leaflet::setView(lat = 48.91167, lng = -114.90246, zoom = 4) %>%
            leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                           rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
                                           markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = TRUE),
                                           polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE),
                                           editOptions = leaflet.extras::editToolbarOptions(edit = TRUE, remove = TRUE, selectedPathOptions = TRUE,allowIntersection = TRUE))

        })

      })

    }

    return(runApp(shinyApp(ui,server)))

}

draw_and_edit()
mapview(aoi, zcol = 'id')

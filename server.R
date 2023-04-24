library(shiny)
function(input, output, session) {
  
  get_neighborhoods=function(subtitle) {
    unique(data$neighborhood_id[data$subtitle == subtitle])
  }
  
  observe({
    subtitle = input$subtitle
    if (is.null(subtitle) || subtitle == "Select a district") {
      
      updateSelectInput(session, "neighborhood_id", choices = c("Select a district first"))
    } else {
      
      neighborhood_id = get_neighborhoods(subtitle)
      updateSelectInput(session, "neighborhood_id", choices = c( neighborhood_id))
    }
  })
  
  user_data = reactive({
    data.frame(sq_mt_built = input$sq_mt_built,
               n_rooms = input$n_rooms,
               n_bathrooms = input$n_bathrooms,
               subtitle = input$subtitle,
               floor = input$floor,
               is_floor_under = input$is_floor_under,
               neighborhood_id = input$neighborhood_id,
               house_type_id = input$house_type_id,
               is_renewal_needed = input$is_renewal_needed,
               has_central_heating = input$has_central_heating,
               has_individual_heating = input$has_individual_heating,
               built_year = input$built_year,
               has_ac = input$has_ac,
               has_fitted_wardrobes = input$has_fitted_wardrobes,
               has_lift = input$has_lift,
               has_pool = input$has_pool,
               has_storage_room = input$has_storage_room,
               energy_certificate = input$energy_certificate,
               orientation = input$orientation,
               is_exterior = input$is_exterior,
               has_terrace = input$has_terrace,
               is_accessible = input$is_accessible,
               has_parking = input$has_parking,
               has_garden = input$has_garden,
               has_balcony = input$has_balcony,
               has_green_zones = input$has_green_zones,
               is_parking_included_in_price = input$is_parking_included_in_price) })
    

    
  predicted_price = reactive({ predict(elasticnet, user_data())})
  output$predicted_price = renderText(paste(round(predicted_price(), 3), "€"))
}

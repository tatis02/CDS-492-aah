fluidPage(
  titlePanel("House price prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("sq_mt_built", "Introduce the square meters of the house you are looking for:", min =15, max = 999 , value = 100),
      numericInput("n_rooms", "Introduce the number of rooms:", value = 3, min = 0, max = 24),
      numericInput("n_bathrooms", "Introduce the number of bathrooms:", value = 2, min = 1, max = 14),
      selectInput("subtitle", "Introduce the district you want to live in:", choices = sort(unique(data$subtitle))),
      selectInput("neighborhood_id", "Introduce the neighborhood you want to live in:", choices = "Select a district first"),
      selectInput("house_type_id", "What kind of house would you like to live in?:", choices = unique(data$house_type_id)),
      conditionalPanel(
        condition = "input.house_type_id != 'HouseType 2: Detached house'",
        selectInput("floor", "Select floor number:", choices = c(sort(unique(data$floor))))),
      
      selectInput("is_floor_under", "Is floor under?:", choices=c("True", "False")),
      selectInput("is_renewal_needed", "Is renewal needed?:", choices=c("True", "False")),
      numericInput("built_year", "Year the house was built in:", value = 2022, min =1800),
      selectInput("has_central_heating", "Has central heating?:", choices=c("True", "False")),
      selectInput("has_individual_heating", "Has individual heating?:", choices=c("True", "False")),
      selectInput("has_ac", "Do you want to have AC?:", choices=c("True", "False")),
      selectInput("has_fitted_wardrobes", "Do you want to have fitted wardrobes?:", choices=c( "True", "False")),
      selectInput("has_lift", "Do you want to have a lift?:", choices=c("True", "False")),
      selectInput("is_exterior", "Do you want it to be exterior?:", choices=c("True", "False")),
      selectInput("has_garden", "Do you want to have a garden?:", choices=c( "True", "False")),
      selectInput("has_pool", "Do you want to have a pool?:", choices=c( "True", "False")),
      selectInput("has_terrace", "Do you want to have a terrace?:", choices=c( "True", "False")),
      selectInput("has_balcony", "Do you want to have a balcony?:", choices=c( "True", "False")),
      selectInput("has_storage_room", "Do you want to have a storage room?:", choices=c( "True", "False")),
      selectInput("is_accessible", "Do you want it to be accessible?:", choices=c( "True", "False")),
      selectInput("has_green_zones", "Do you want to have a green zones?:", choices=c( "True", "False")),
      selectInput("energy_certificate", "What energy certificate would you like it to have?:", choices= sort(unique(data$energy_certificate))),
      selectInput("orientation", "Any preference in orientation:", choices=c("east", "west", "south", "north")),
      
      selectInput("has_parking", "Do you want to have parking?:", choices=c( "True", "False")),
      conditionalPanel(
        condition = "input.has_parking == 'True'",
        selectInput("is_parking_included_in_price", "Parking included in price:", choices = c("True", "False"))),
      ),
      mainPanel(
      h3("The predicted price is:"),
      br(),
      textOutput("predicted_price"))
    )
  )

server <- function(input, output, session) {
   # Return the mean mortality rate for a state  for 2000-2002
  state.mean.2000_2002 <- reactive({
    as.numeric(dplyr::filter(state_natl_death_rates,
                             State == names(state.list)[which(state.list == input$state_choice)],
                             Cause == input$death_cause,
                             Years == '2000-2002')$Crude.Rate)
  })
  
  
  # Calculate national mean mortality for 2000-2002
  national.mean.2000_2002 <- reactive({
    as.numeric(dplyr::filter(state_natl_death_rates,
                             State == 'United States',
                             Cause == input$death_cause,
                             Years == '2000-2002')$Crude.Rate)
  })
  
  #Calculate the mean mortality rate for a state  for 2015-2017
  state.mean.2015_2017 <- reactive({
    as.numeric(dplyr::filter(state_natl_death_rates,
                             State == names(state.list)[which(state.list == input$state_choice)],
                             Cause == input$death_cause,
                             Years == '2015-2017')$Crude.Rate)
  })
  
  # Calculate national mean mortality for 2015-2017
  national.mean.2015_2017 <- reactive({
    as.numeric(dplyr::filter(state_natl_death_rates,
                             State == 'United States',
                             Cause == input$death_cause,
                             Years == '2015-2017')$Crude.Rate)
  })
  
  # finds states with lowest and highest death rates and returns them
  #   and their respective rates
  #
  # Returns a list in form (lowest death rate, lowest death rate state,
  #   highest death rate, highest death rate state)
  low.high.states.2015_2017 <- reactive({
    
    grouped.data <- dplyr::filter(
      state_natl_death_rates,
      State != 'United States',
      State != 'District of Columbia',
      Cause == input$death_cause,
      Years == "2015-2017"
    )
    
    return(
      c(
        min(as.numeric(grouped.data$Crude.Rate)),
        grouped.data$State[which.min(as.numeric(grouped.data$Crude.Rate))],
        max(as.numeric(grouped.data$Crude.Rate)),
        grouped.data$State[which.max(as.numeric(grouped.data$Crude.Rate))]
      )
    )
  })
  
  # Identifying a county with the highest mortality rate in the state between 2000-2002
  #  
  # Returns a list of length 2, where the item at index 1 is the death rate and the
  #   item at index 2 is the county name
  high.rate.county.2000_2002 <- reactive({
    filtered.data <- dplyr::filter(
      cdc.data,
      state_abbr == input$state_choice,
      death_cause == input$death_cause,
      period == "2000-2002"
    )
    
    return(
      c(
        max(filtered.data$death_rate),
        filtered.data$county_name[which.max(filtered.data$death_rate)]
      )
    )
  })
  
  # Identifying a county with the highest mortality rate in the state between 2015-2017
  #  
  # Returns a list of length 2, where the item at index 1 is the death rate and the
  #   item at index 2 is the county name
  high.rate.county.2015_2017 <- reactive({
    
    filtered.data <- dplyr::filter(
      cdc.data,
      state_abbr == input$state_choice,
      death_cause == input$death_cause,
      period == "2015-2017"
    )
    
    return(
      c(
        max(filtered.data$death_rate),
        filtered.data$county_name[which.max(filtered.data$death_rate)]
      )
    )
  })
  
  # Identifying a county with the lowest mortality rate in the state between 2000-2002
  #  
  # Returns a list of length 2, where the item at index 1 is the death rate and the
  #   item at index 2 is the county name
  low.rate.county.2000_2002 <- reactive({
    
    filtered.data <- dplyr::filter(
      cdc.data,
      state_abbr == input$state_choice,
      death_cause == input$death_cause,
      period == "2000-2002"
    )
    
    return(
      c(
        min(filtered.data$death_rate),
        filtered.data$county_name[which.min(filtered.data$death_rate)]
      )
    )
  })
  
  # Identifying a county with the lowest mortality rate in the state between 2015-2017
  #  
  # Returns a list of length 2, where the item at index 1 is the death rate and the
  #   item at index 2 is the county name
  low.rate.county.2015_2017 <- reactive({
    
    filtered.data <- dplyr::filter(
      cdc.data,
      state_abbr == input$state_choice,
      death_cause == input$death_cause,
      period == "2015-2017"
    )
    
    return(
      c(
        min(filtered.data$death_rate),
        filtered.data$county_name[which.min(filtered.data$death_rate)]
      )
    )
  })
  
  
  
  
  
  
  # serv_out[["textMortFacts1"]] <- function(calc, session) {
  #   renderUI({
  #     # We reference state.list, cause.list and cause.definitions defined above
  #     
  #     tagList(
  #       tags$h5("Mean Mortality Rate for 2000-2002:", round(state.mean.2000_2002(),2)),
  #       tags$h5("Mean Mortality Rate for 2015-2017:", round(state.mean.2015_2017(),2)),
  #       tags$h5("National Mean for 2000-2002:", round(national.mean()[national.mean()$period == "2000-2002",]$death_rate,2)),
  #       tags$h5("National Mean for 2015-2017:", round(national.mean()[national.mean()$period == "2015-2017",]$death_rate,2)),
  #       tags$h5("Lowest Rate County for 2000-2002:", low.rate.county.2000_2002()),
  #       tags$h5("Lowest Rate County for 2015-2017:", low.rate.county.2015_2017()),
  #       tags$h5("National Mean for 2000-2002:", round(national.mean()[national.mean()$period == "2000-2002",]$death_rate,2)),
  #       tags$h5("National Mean for 2015-2017:", round(national.mean()[national.mean()$period == "2015-2017",]$death_rate,2))
  #     )
  #   })
  # }
  
  
  
  # ----------------------------------------------------------------------
  
  
  # 
  # update.county.fips <- function(value) {
  #   if (!is.na(value) & nchar(value) == 4) {
  #     return (
  #       paste("0", value, sep = "")
  #     )
  #   } else {
  #     return (value)
  #   }
  # }
  # 
  # 
  # 
  # 
  # 
  # output$determinant_title <- renderText({
  #   input$determinant_choice
  # })
  # 
  # output$state_title <- renderText({
  #   if (input$state_choice == "United States") {
  #     "United States"
  #   }
  #   else {
  #     names(which(state.list == input$state_choice))
  #   }
  # })
  # 
  
  
  
  output$determinants_plot4 <- renderPlot({
    
  })
  
  


  # # For a state or the US, creates the bulleted facts at the bottom of nationwide page
  # output$textMortFacts <- renderUI({
  #   if(input$state_choice == "United States") {
  #     # percent change for first bullet
  #     change_text <- "remained the same"
  #     
  #     percent_change <- round(
  #       abs(national.mean.2015_2017() - national.mean.2000_2002()) / national.mean.2000_2002() * 100,
  #       1
  #     )
  #     
  #     if (percent_change > 0) {
  #       change_text <- paste0("increased ", percent_change, "%")
  #     }
  #     else if (percent_change < 0) {
  #       change_text <- paste0("decreased ", percent_change, "%")
  #     }
  #     
  #     tagList(
  #       tags$ul(
  #         tags$li(tags$h4(paste0("Have ", change_text, " from 2000 to 2017"))),
  #         tags$li(tags$h4(paste0("Range from ", 
  #                                round(as.numeric(low.high.states.2015_2017()[1]), 1),
  #                                " per 100k people in ",
  #                                low.high.states.2015_2017()[2],
  #                                " to ",
  #                                round(as.numeric(low.high.states.2015_2017()[3]), 1),
  #                                " per 100k people in ",
  #                                low.high.states.2015_2017()[4],
  #                                " 2015-2017"))
  #         )
  #       )
  #     )
  #   }
  #   else {
  #     # percent change for first bullet
  #     change_text <- "remained the same"
  #     vals_text <- paste0("at ", round(state.mean.2015_2017(), 1), " per 100k people")
  #     
  #     percent_change <- round(
  #       (state.mean.2015_2017() - state.mean.2000_2002()) / state.mean.2000_2002() * 100,
  #       1
  #     )
  #     
  #     if (percent_change > 0) {
  #       change_text <- paste0("increased by ", abs(percent_change), "%")
  #       vals_text <- paste0("rising from ", round(state.mean.2000_2002(), 1), 
  #                           " to ", round(state.mean.2015_2017(), 1))
  #     }
  #     else if (percent_change < 0) {
  #       change_text <- paste0("decreased by ", abs(percent_change), "%")
  #       vals_text <- paste0("falling from ", round(state.mean.2000_2002(), 1), 
  #                           " to ", round(state.mean.2015_2017(), 1))
  #     }
  #     
  #     # comparison wish national average
  #     
  #     comparison_text <- "the same as"
  #     
  #     if (national.mean.2015_2017() > state.mean.2015_2017()) {
  #       comparison_text <- "lower than"
  #     }
  #     else if (national.mean.2015_2017() < state.mean.2015_2017()) {
  #       comparison_text <- "greater than"
  #     }
  #     
  #     tagList(
  #       tags$ul(
  #         #style = "font-size: 18px;",
  #         tags$li(tags$h4(paste0("Have ", change_text, " from 2000 to 2017, ", vals_text, " per 100k people"))),
  #         tags$li(tags$h4(paste0("Were ", comparison_text, " the national mean in 2015-2017 of ",
  #                                round(national.mean.2015_2017(), 2), " per 100k people"))),
  #         tags$li(tags$h4(paste0("Range from ", 
  #                                round(as.numeric(low.rate.county.2015_2017()[1]), 1),
  #                                " per 100k people in ",
  #                                low.rate.county.2015_2017()[2],
  #                                " to ",
  #                                round(as.numeric(high.rate.county.2015_2017()[1]), 1),
  #                                " per 100k people in ",
  #                                high.rate.county.2015_2017()[2],
  #                                "from 2015-2017")
  #         ))
  #       )
  #     )
  #   }
  # })
  
  
  
  
  # # Determinant Header (upper-center panel, Page 2)
  # output$textDeterminants3 <- renderUI({
  #   # We reference state.list, cause.list and cause.definitions defined above
  #   
  #   tagList(
  #     tags$h2(
  #       title="Help text for cluster distribution bar plots",
  #       paste0("Distribution of '",input$determinant_choice, "' across ", names(which(cause.list == input$death_cause)), " clusters for ", names(which(state.list == input$state_choice))), 
  #       icon("info-circle")
  #     ),
  #     NULL
  #   )
  # })
  # 
  
  
  
  # # Determinant geo Header (upper-center panel, Page 2)
  # output$textDeterminantsGeo <- renderUI({
  #   # We reference state.list, cause.list and cause.definitions defined above
  #   
  #   tagList(
  #     tags$h4(
  #       title="Geographic distribution of selected determinant across selected state",
  #       paste0("Distribution of '",input$determinant_choice, "' for ", names(which(state.list == input$state_choice))), 
  #       icon("info-circle")
  #     ),
  #     NULL
  #   )
  # })
  
  # 
  # # Mortality Rate Table
  # output$table <- renderTable(width = "100%", {
  #   rate.table <- mort.avg.cluster.ord() %>%
  #     dplyr::select(cluster, period, death_rate) %>%
  #     tidyr::spread(key = period, value = death_rate) %>% 
  #     dplyr::select(cluster, `2000-2002`, `2015-2017`)
  #   
  #   count.table <- mort.avg.cluster.ord() %>% 
  #     dplyr::select(cluster, count) %>% 
  #     base::unique()
  #   
  #   dplyr::left_join(count.table, rate.table, by = "cluster") %>%
  #     dplyr::mutate(cluster = as.character(cluster)) %>%
  #     dplyr::arrange(desc(cluster)) %>%
  #     dplyr::rename(
  #       "Trend Group" = "cluster",
  #       "Count" = "count"
  #     )
  # })
  
  
  
  # # Mortality Trend Cluster by County
  # # TODO: Replace this with a social determinant map!
  # output$geo_cluster_kmean_2 <- renderLeaflet({
  #   
  #   if(input$state_choice == "United States"){
  #     # draw.geo.cluster("US", input$death_cause, mort.cluster.ord(),
  #     #                  max(mort.cluster.ord()$cluster))
  #   }else{
  #     draw.geo.cluster(input$state_choice, input$death_cause, mort.cluster.ord(),
  #                      max(mort.cluster.ord()$cluster))
  #   }
  #   
  # })
  
  # # Mortality Rate by County Period 1
  # output$geo_mort_change1 <- renderLeaflet({
  #   if(input$state_choice == "United States"){
  #     mort.data <- dplyr::filter(
  #       cdc.data,
  #       death_cause == input$death_cause,
  #       period == "2000-2002"
  #     ) %>%
  #       dplyr::mutate(
  #         # death_rate = death_num / population * 10^5,
  #         death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
  #       ) %>%
  #       dplyr::select(county_fips, death_rate, period)
  #     
  #     geo.plot("US", input$death_cause, mort.data, "2000-2002")
  #     
  #   } else {
  #     
  #     mort.data <- dplyr::filter(
  #       cdc.data,
  #       state_abbr == input$state_choice,
  #       death_cause == input$death_cause,
  #       period == "2000-2002"
  #     ) %>%
  #       dplyr::mutate(
  #         # death_rate = death_num / population * 10^5,
  #         death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
  #       ) %>%
  #       dplyr::select(county_fips, death_rate, period)
  #     geo.plot(input$state_choice, input$death_cause, mort.data, "2000-2002")
  #   }
  #   
  # })
  # 
  
  
  
}
  
  
  shinyApp(ui = ui, server = server)
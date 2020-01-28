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

  

  
  # Gives information about county population and urbanness
  output$county_desc <- renderUI({
    
    # when app starts up there is initially no selection
    if (is.null(input$county_drop_choice)) {
      return()
    }
    
    county.data.00.02 <- dplyr::filter(
      cdc.data,
      county_name == input$county_drop_choice,
      death_cause == input$death_cause,
      state_abbr == input$state_choice,
      period == "2000-2002",
      is.na(death_rate)
    )
    county.data.15.17 <- dplyr::filter(
      cdc.data,
      county_name == input$county_drop_choice,
      death_cause == input$death_cause,
      state_abbr == input$state_choice,
      period == "2015-2017",
      is.na(death_rate)
    )
    
    if (nrow(county.data.15.17) == 0 | nrow(county.data.00.02) == 0) {
      return(
        tagList(
          tags$h5(paste0(
            "Data estimated for ", input$county_drop_choice, ", ", input$state_choice, ".")
          )
        )
      )
    }
    
    # pop change
    pop.00.02 <- county.data.00.02$population
    pop.15.17 <- county.data.15.17$population
    
    pop.change.text <- "Population has remained relatively constant since 2002"
    
    if (pop.00.02 > pop.15.17) {
      pop.change.text <- paste0("Population fell by ", 
                                formatC(pop.00.02 - pop.15.17, format="d", big.mark=","),
                                " (",
                                round((pop.00.02 - pop.15.17) / pop.00.02 * 100, 2),
                                "%) from 2002 to 2017")
    }
    if (pop.00.02 < pop.15.17) {
      pop.change.text <- paste0("Population rose by ",
                                formatC(pop.15.17 - pop.00.02, format="d", big.mark=","),
                                " (",
                                round((pop.15.17 - pop.00.02) / pop.00.02 * 100, 2),
                                "%) from 2002 to 2017")
    }
    
    # death rate change
    dr.00.02 <- county.data.00.02$death_rate
    dr.15.17 <- county.data.15.17$death_rate
    
    dr.change.text <- paste0("For ", county.data.15.17$county_name, " County, the ", tolower(input$death_cause), 
                             " mortality rate has remained relatively constant since 2002 at ",
                             round(dr.15.17, 2),
                             "per 100k people")
    
    if (dr.00.02 > dr.15.17) {
      dr.change.text <- paste0("For ", county.data.15.17$county_name, " County, the ", tolower(input$death_cause), 
                               " mortality rate fell by ",
                               round((dr.00.02 - dr.15.17) / dr.00.02 * 100, 2),
                               "% from 2002 to 2017 from ",
                               round(dr.00.02, 2), " to ",  round(dr.15.17, 2))
    }
    if (dr.00.02 < dr.15.17) {
      dr.change.text <- paste0("For ", county.data.15.17$county_name, " County, the ", tolower(input$death_cause), 
                               " mortality rate rose by ",
                               round((dr.15.17 - dr.00.02) / dr.00.02 * 100, 2),
                               "% from 2002 to 2017 from ",
                               round(dr.00.02, 2), " to ",  round(dr.15.17, 2))
    }
    
    return(
      tagList(
        # tags$h5(paste0(
        #   county.data.15.17$county_name, ", ", county.data.15.17$state_abbr,
        #   " is a ", tolower(county.data.15.17$urban_2013), " area with a population of ",
        #   formatC(pop.15.17, format="d", big.mark=","))
        # ),
        # tags$h5(pop.change.text),
        # TODO: Add determinant change!
        tags$h5(dr.change.text)
      )
    )
  })
  
  output$determinants_plot4 <- renderPlot({
    
  })
  
  # Mortality Rate Trend Line Graph
  output$mort_line <- renderPlot({
    
    if (input$state_choice == "United States"){
      
      total.data <- rbind(mort.avg.cluster.ord(), national.mean())
      total.data$cluster[total.data$cluster == 1] <- "1: Low"
      total.data$cluster[total.data$cluster == 6] <- "6: High"
      
      ggplot(
        total.data,
        aes(
          x = period, y = death_rate, 
          color = cluster, group = cluster
        )
      ) + 
        geom_line(size = 1.5) + 
        geom_point(color = "#565254", shape = 21, fill = "#f7f7f7", size = 2) + 
        # labs.line.mort(input$state_choice, input$death_cause) + 
        scale_color_manual(
          values = theme.categorical.colors.accent(max(mort.cluster.ord()$cluster))) +
        theme.line.mort() + 
        theme(legend.position = "left") + 
        ylab("Average Midlife Deaths per 100,000") +
        labs(
          fill = "Cluster", 
          color = "Cluster",
          caption = "Mortality Data: CDC WONDER Detailed Mortality\nFeature Data: County Health Rankings\nAnalysis: The Rensselaer IDEA"
        ) +
        guides(
          color = guide_legend(
            reverse = T,
            title = "Risk Group:")
        )
    } else {
      
      nclusters <- max(mort.cluster.raw()$cluster)
      total.data <- rbind(mort.avg.cluster.ord(), national.mean())
      
      if (input$state_choice == "DE") {
        
        exceptions.data <- cdc.data %>%
          dplyr::filter(death_cause == input$death_cause) %>%
          dplyr::right_join(mort.cluster.raw(), by = "county_fips")
        exceptions.data$cluster <- exceptions.data$county_name
        exceptions.data <- exceptions.data %>%
          dplyr::group_by(period, cluster) %>%
          dplyr::summarise(
            death_rate = sum(death_num) / max(sum(population), 1) * 10^5,
            count = n()
          ) %>%
          dplyr::ungroup()
        exceptions.data <- rbind(exceptions.data, national.mean())
        extras.color <- rbind(mort.avg.cluster.ord(), national.mean())
        colnames(extras.color)[2] <- "cluster.num"
        exceptions.data <- cbind(exceptions.data, extras.color$cluster.num)
        colnames(exceptions.data)[5] <- "cluster.num"
        
        
        line_plot <- ggplot(
          exceptions.data,
          aes(
            x = period, y = death_rate, 
            color = cluster.num, group = cluster.num
          )
        ) + 
          geom_line(size = 1.5) + 
          geom_point(color = "black", shape = 21, fill = "white", size = 2) + 
          # labs.line.mort(input$state_choice, input$death_cause) + 
          scale_color_manual(
            values = c(theme.categorical.colors(nclusters), "#0571b0"), labels = c("Kent", "New Castle", "Sussex", "National")) +
          theme.line.mort() + 
          theme(legend.position = "left") + 
          guides(color = guide_legend(reverse = T)) +
          labs(fill = "Counties and \n National Average", 
               color = "Counties and \n National Average",
               caption = "Mortality Data: CDC Wonder Detailed Mortality\nFeature Data: County Health Rankings\nAnalysis: The Rensselaer IDEA"
          ) + 
          ylab("Average Midlife Deaths per 100,000")
        line_plot
        
      } else if (input$state_choice == "HI") {
        
        exceptions.data <- cdc.data %>%
          dplyr::filter(death_cause == input$death_cause) %>%
          dplyr::right_join(mort.cluster.raw(), by = "county_fips")
        exceptions.data$cluster <- exceptions.data$county_name
        exceptions.data <- exceptions.data %>%
          dplyr::group_by(period, cluster) %>%
          dplyr::summarise(
            death_rate = sum(death_num) / max(sum(population), 1) * 10^5,
            count = n()
          ) %>%
          dplyr::ungroup()
        exceptions.data <- rbind(exceptions.data, national.mean())
        extras.color <- rbind(mort.avg.cluster.ord(), national.mean())
        colnames(extras.color)[2] <- "cluster.num"
        exceptions.data <- cbind(exceptions.data, extras.color$cluster.num)
        colnames(exceptions.data)[5] <- "cluster.num"
        
        line_plot <- ggplot(
          exceptions.data,
          aes(
            x = period, y = death_rate, 
            color = cluster.num, group = cluster.num
          )
        ) + 
          geom_line(size = 1.5) + 
          geom_point(color = "black", shape = 21, fill = "white", size = 2) + 
          # labs.line.mort(input$state_choice, input$death_cause) + 
          scale_color_manual(
            values = c(theme.categorical.colors(nclusters), "#0571b0"), labels = c("Kalawao", "Honolulu" , "Maui", "Hawaii", "Kauai", "National")) +
          theme.line.mort() + 
          theme(legend.position = "left") + 
          guides(color = guide_legend(reverse = T)) +
          labs(fill = "Counties and \n National Average", 
               color = "Counties and \n National Average",
               caption = "Mortality Data: CDC Wonder Detailed Mortality\nFeature Data: County Health Rankings\nAnalysis: The Rensselaer IDEA"
          ) + 
          ylab("Average Midlife deaths per 100,000")
        line_plot
        
      } else if (input$state_choice == "RI") {
        
        exceptions.data <- cdc.data %>%
          dplyr::filter(death_cause == input$death_cause) %>%
          dplyr::right_join(mort.cluster.raw(), by = "county_fips")
        exceptions.data$cluster <- exceptions.data$county_name
        exceptions.data <- exceptions.data %>%
          dplyr::group_by(period, cluster) %>%
          dplyr::summarise(
            death_rate = sum(death_num) / max(sum(population), 1) * 10^5,
            count = n()
          ) %>%
          dplyr::ungroup()
        exceptions.data <- rbind(exceptions.data, national.mean())
        extras.color <- rbind(mort.avg.cluster.ord(), national.mean())
        colnames(extras.color)[2] <- "cluster.num"
        exceptions.data <- cbind(exceptions.data, extras.color$cluster.num)
        colnames(exceptions.data)[5] <- "cluster.num"
        
        line_plot <- ggplot(
          exceptions.data,
          aes(
            x = period, y = death_rate, 
            color = cluster.num, group = cluster.num
          )
        ) + 
          geom_line(size = 1.5) + 
          geom_point(color = "black", shape = 21, fill = "white", size = 2) + 
          # labs.line.mort(input$state_choice, input$death_cause) + 
          scale_color_manual(
            values = c(theme.categorical.colors(nclusters), "#0571b0"), labels = c("Bristol", "Washington", "Newport", "Kent", "Providence", "National")) +
          theme.line.mort() + 
          theme(legend.position = "left") + 
          guides(color = guide_legend(reverse = T)) +
          labs(fill = "Counties and \n National Average", 
               color = "Counties and \n National Average",
               caption = "Mortality Data: CDC Wonder Detailed Mortality\nFeature Data: County Health Rankings\nAnalysis: The Rensselaer IDEA"
          ) + 
          ylab("Average Midlife Deaths per 100,000")
        line_plot 
        
      } else {
        
        total.data$cluster[total.data$cluster == 1] <- "1: Low"
        total.data$cluster[total.data$cluster == 2] <- "2: Medium"
        total.data$cluster[total.data$cluster == 3] <- "3: High"
        # total.data$cluster <- as_factor(total.data$cluster)
        
        # total.data$cluster_label[total.data$cluster == "1"] <- "Low"
        # total.data$cluster_label[total.data$cluster == "2"] <- "Medium"
        # total.data$cluster_label[total.data$cluster == "3"] <- "High"
        # total.data$cluster_label[total.data$cluster == "National"] <- "National"
        # total.data$cluster_label <- as_factor(total.data$cluster_label)
        
        line_plot <- ggplot(
          total.data,
          aes(
            x = period, y = death_rate, color = cluster,
            group = cluster
          )
        ) + 
          geom_line(size = 1.5) + 
          geom_point(color = "#565254", shape = 21, fill = "#f7f7f7", size = 2) + 
          # labs.line.mort(input$state_choice, input$death_cause) + 
          scale_color_manual(
            values = theme.categorical.colors.accent(nclusters)) +
          theme.line.mort() + 
          theme(legend.position = "left") + 
          guides(color = guide_legend(
            reverse = T,
            title = "Risk Group:")) +
          labs(fill = "Cluster", 
               color = "Cluster",
               caption = "Mortality Data: CDC Wonder Detailed Mortality\nFeature Data: County Health Rankings\nAnalysis: The Rensselaer IDEA"
          ) + 
          ylab("Average Midlife Deaths per 100,000") 
        
        if (is.null(county_choice())){
          line_plot
        } else {
          drop.cols <- c('county_fips')
          county_data <- cdc.countymort.mat(cdc.data, input$state_choice, county_choice(), input$death_cause)
          
          canShow <- dplyr::inner_join(county_data, cdc.original.data, by = 'county_fips') %>% 
            dplyr::filter(
              death_cause == input$death_cause
            )
          if (nrow(county_data) == 0 | any(is.na(canShow$death_rate))) {
            line_plot + xlab("period\nCould not plot county as data suppressed")
          } else {
            county_data <- county_data %>%
              dplyr::select(-drop.cols) %>%
              tidyr::gather("period", "death_rate", "2000-2002":"2015-2017") %>%
              dplyr::mutate("county" = county_choice())
            line_plot + 
              geom_line(
                mapping = aes(x = period, y = death_rate, group = county, linetype=county_choice()),
                data = county_data, color = "#565254", size = 1.3
              ) +
              geom_point(
                mapping = aes(x = period, y = death_rate),
                data = county_data, color = "#565254", shape = 21, 
                fill = "#f7f7f7", inherit.aes = FALSE, size = 2
              ) +
              scale_linetype_manual(name = "County",
                                    values = c("twodash"),
                                    guide = guide_legend(override.aes = list(color = c("#565254")))
              )
          }
        }
      }
    }
    
  },bg="transparent")
  
  generate_text <- function(name, diff_pct){
    change_text <- paste0("The mortality rate \nhas ")
    
    if (diff_pct > 0) {
      change_text <- paste0(change_text, "increased ")
    }
    else {
      change_text <- paste0(change_text, "decreased ")
    }
    
    change_text <- paste0(change_text, "in \n", name, " by ", abs(round(diff_pct,1)), "%")
    change_text
  }
  
  generate_label_data <- function(state_data, nation_data, state_begin, state_end, nation_begin, nation_end, state_x, state_y, nation_x, nation_y){
    state_data <- state_data %>% 
      mutate(label = "") %>% 
      rename(x = period)
    nation_data <- nation_data %>% 
      mutate(label = "") %>% 
      rename(x = period)
    rbind(generate_label_data_single(state_data, input$state_choice, state_begin, state_end, state_x, state_y),
          generate_label_data_single(nation_data, "United States", nation_begin, nation_end, nation_x, nation_y))
  }
  
  generate_label_data_single <- function(data, name, begin, end, label_x, label_y){
    label_data = data.frame(data)
    label_data$x[label_data$x=="2000-2002"] <- 1
    label_data$x[label_data$x=="2003-2005"] <- 2
    label_data$x[label_data$x=="2006-2008"] <- 3
    label_data$x[label_data$x=="2009-2011"] <- 4
    label_data$x[label_data$x=="2012-2014"] <- 5
    label_data$x[label_data$x=="2015-2017"] <- 6
    label_data$x <- as.numeric(as.character(label_data$x))
    n <- 10
    d_max <- 0
    for (i in 1:5){
      r1 <- label_data[label_data$x==i,]
      r2 <- label_data[label_data$x==i+1,]
      for (j in 1:n-1){
        for (d in 0:d_max){
          label_data <- rbind(label_data, data.frame("x" = i+j/n,
                                                     "death_rate" = r2$death_rate*j/n+r1$death_rate*(n-j)/n-d,
                                                     "label" = c(""),
                                                     "group" = c(name)))
          label_data <- rbind(label_data, data.frame("x" = i+j/n,
                                                     "death_rate" = r2$death_rate*j/n+r1$death_rate*(n-j)/n+d,
                                                     "label" = c(""),
                                                     "group" = c(name)))
        }
      }
    }
    mort_diff <- (end - begin) / begin * 100
    mort_text <- generate_text(name, mort_diff) 
    
    rbind(label_data,
          data.frame("x" = label_x, 
                     "death_rate" = label_y,
                     "label" = mort_text,
                     "group" = name))
  }
  
  draw_reference <- function(line_plot, l_start, l_end, r_start, r_end){
    line_plot <- draw_reference_single(line_plot, l_start, l_end, 1, l_end)
    draw_reference_single(line_plot, r_start, r_end, 6, r_start)
  }
  
  draw_reference_single <- function(line_plot, start, end, x, y){
    line_plot +
      geom_segment(aes(x='2000-2002', xend='2015-2017', y=y, yend=y),
                   color = '#565254', linetype=2) +
      geom_segment(aes(x=x, xend=x, y=start, yend=end),
                   color = '#565254', linetype=1, arrow = arrow(length=unit(0.4,"cm")))
  }
  
  add_reference_point <- function(label_data, l_start, l_end, r_start, r_end){
    label_data <- add_reference_point_single(label_data, l_start, l_end, 1, "United States")
    add_reference_point_single(label_data, r_start, r_end, 6, "United States")
  }
  
  
  add_reference_point_single <- function(label_data, start, end, x, name){
    rbind(label_data, data.frame("x" = rep(c(x), times = 6),
                                 "death_rate" = seq(start, end, length.out = 6),
                                 "label" = rep(c(""), times = 6),
                                 "group" = rep(c(name), times = 6)))
  }
  
  # Mortality Rate Trend Line Graph
  output$nation_state_infographic <- renderPlot({
    
    u <- 0.65
    v <- 1 - u
    if (is.null(input$page1_period)){
      period_choice = 6
    } else {
      period_choice = input$page1_period
    }
    
    if (input$state_choice == "United States"){
      
      nation_data <- dplyr::filter(
        cdc.data,
        death_cause == input$death_cause
      ) %>% 
        drop_na() %>%
        group_by(period) %>% 
        summarise(population = sum(population), death_num = sum(death_num)) %>%
        mutate(death_rate = death_num/population*100000, group = "United States") %>%
        select(period, death_rate, group)
      
      nation_begin <- nation_data[nation_data$period=="2000-2002",]$death_rate
      nation_end <- nation_data[nation_data$period=="2015-2017",]$death_rate
      nation_hi <- max(nation_begin, nation_end)
      nation_lo <- min(nation_begin, nation_end)
      hi <- max(nation_data$death_rate)
      lo <- min(nation_data$death_rate)
      range <- hi - lo
      
      line_plot <- ggplot(
        nation_data,
        aes(
          x = period, y = death_rate, 
          group = group, color = color
        )
      ) + 
        geom_line(size = 1.5, color = theme.cat.accent.color()) + 
        geom_point(color = "#565254", shape = 21, fill = "#f7f7f7", size = 2) + 
        theme.line.mort() + 
        theme(legend.position = "bottom", legend.title = element_blank()) + 
        ylab("Midlife deaths per 100,000") + 
        geom_segment(aes(x=period_choice, xend=period_choice, y=lo, yend=hi), color = '#38761D', linetype=2) +
        geom_polygon(data = data.frame(
          x = c(period_choice-0.1, period_choice+0.1, period_choice, period_choice-0.1), 
          y = c(hi+range*0.1, hi+range*0.1, hi, hi+range*0.1)), 
          aes(x = x, y = y), inherit.aes = FALSE, fill = '#38761D')
      
      nation_data <- nation_data %>% 
        mutate(label = "") %>% 
        rename(x = period)
      label_data <- generate_label_data_single(nation_data, "United States", nation_begin, nation_end, 1, v*nation_lo+u*nation_hi)
      label_data <- add_reference_point_single(label_data, nation_begin, nation_end, 1, "United States")
      line_plot <- draw_reference_single(line_plot, nation_begin, nation_end, 1, nation_end)
      line_plot <-line_plot +
        coord_cartesian(clip = "off") +
        geom_label_repel(data = label_data, mapping = aes(x = x, y = death_rate, label = label), 
                         fill = theme.cat.accent.color(),
                         inherit.aes = FALSE,
                         segment.colour = "#565254",
                         color = "#f7f7f7",
                         #hjust = "inward", vjust = "inward",
                         #point.padding = 0.5,
                         direction = "both",
                         xlim = c(1.5, 5.5),
                         show.legend = FALSE)
      
    } else {
      state_data <- dplyr::filter(
        cdc.data,
        state_abbr == input$state_choice,
        death_cause == input$death_cause
      ) %>% 
        drop_na() %>%
        group_by(period) %>% 
        summarise(population = sum(population), death_num = sum(death_num)) %>%
        mutate(death_rate = death_num/population*100000, group = input$state_choice) %>%
        select(period, death_rate, group)
      
      state_begin <- state_data[state_data$period=="2000-2002",]$death_rate
      state_end <- state_data[state_data$period=="2015-2017",]$death_rate
      state_hi <- max(state_begin, state_end)
      state_lo <- min(state_begin, state_end)
      
      
      nation_data <- dplyr::filter(
        cdc.data,
        death_cause == input$death_cause
      ) %>% 
        drop_na() %>%
        group_by(period) %>% 
        summarise(population = sum(population), death_num = sum(death_num)) %>%
        mutate(death_rate = death_num/population*100000, group = "United States") %>%
        select(period, death_rate, group)
      
      nation_begin <- nation_data[nation_data$period=="2000-2002",]$death_rate
      nation_end <- nation_data[nation_data$period=="2015-2017",]$death_rate
      nation_hi <- max(nation_begin, nation_end)
      nation_lo <- min(nation_begin, nation_end)
      
      data <- bind_rows(state_data, nation_data)
      hi <- max(data$death_rate)
      lo <- min(data$death_rate)
      range <- hi - lo 
      ylim <- c(lo - 0.1 * range, hi + 0.1 * range)
      
      colors <- c("placeholder" = '#D95F02', "United States"=theme.cat.accent.color())
      names(colors) <- c(input$state_choice, "United States")
      
      line_plot <- ggplot(
        data,
        aes(
          x = period, y = death_rate, 
          color = group, group = group
        )
      ) + 
        geom_line(size = 1.5) +
        geom_point(color = "#565254", shape = 21, fill = "#f7f7f7", size = 2) + 
        theme.line.mort() + 
        theme(legend.position = "bottom", legend.title = element_blank()) + 
        ylab("Midlife deaths per 100,000") +
        ylim(ylim) + 
        scale_color_manual(values = colors) + 
        geom_segment(aes(x=period_choice, xend=period_choice, y=lo, yend=hi), color = '#38761D', linetype=2) +
        geom_polygon(data = data.frame(
          x = c(period_choice-0.1, period_choice+0.1, period_choice, period_choice-0.1), 
          y = c(hi+range*0.1, hi+range*0.1, hi, hi+range*0.1)), 
          aes(x = x, y = y), inherit.aes = FALSE, fill = '#38761D')
      
      if (xor(nation_end < state_end, state_end < state_begin)){
        label_data <- generate_label_data(state_data, nation_data, state_begin, state_end, nation_begin, nation_end,
                                          1, u*state_hi+v*state_lo, 6, u*nation_lo+v*nation_hi)
        label_data <- add_reference_point(label_data, state_begin, state_end, nation_begin, nation_end)
        line_plot <- draw_reference(line_plot, state_begin, state_end, nation_begin, nation_end)
      } else {
        label_data <- generate_label_data(state_data, nation_data, state_begin, state_end, nation_begin, nation_end, 
                                          6, v*state_hi+u*state_lo, 1, v*nation_lo+u*nation_hi)
        label_data <- add_reference_point(label_data, nation_begin, nation_end, state_begin, state_end)
        line_plot <- draw_reference(line_plot, nation_begin, nation_end, state_begin, state_end)
      }
      
      line_plot <- line_plot +
        coord_cartesian(clip = "off") +
        geom_label_repel(data = label_data, 
                         mapping = aes(x = x, y = death_rate, label = label, fill = group),
                         segment.colour = "#565254",
                         color = "#f7f7f7",
                         inherit.aes = FALSE,
                         #hjust = "inward", vjust = "inward",
                         #point.padding = 0.5,
                         direction = "both",
                         xlim = c(1.5, 5.5),
                         ylim = ylim,
                         show.legend = FALSE) + 
        scale_fill_manual(values = colors)
      
      #geom_point(data = label_data, mapping = aes(x = x, y = death_rate), color = '#565254')
    }
    assign("page1_infographic", line_plot, envir = .GlobalEnv)
    line_plot
  }, bg="transparent")
  

  
  output$textMortFactsNew <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h4(
        title ="Midlife mortality rates are obtained from the CDC WONDER Detailed Mortality Online Mortality Database.  Separate crude death rates are queried  for adults 25 to 64 at the county, state, and nationwide levels for each cause of death.  Rates are not age adjusted. Unreliable or missing rates are imputed. See Project Overview for details.",
        paste0("Midlife Mortality Rate: Deaths per 100,000 for adults ages 25-64 due to ",
               names(which(cause.list == input$death_cause)), 
               " for three year periods for counties (left) and state and nation (right)."
        ), icon("info-circle")
      ),
      HTML("<h5>Data Source: CDC WONDER<br>Analysis: The Rensselaer Institute for Data Exploration and Applications 
           (<a href='http://idea.rpi.edu' target=_Blank>The Rensselaer IDEA</a>)</h5>")
      )
  })
  
  output$textInfographicTitle <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h3(
          paste0("Midlife Mortality Rates for ",
                 names(which(cause.list == input$death_cause)), 
                 " in ", 
                 location_str)
        )
      )
    }
    else {
      location_str <- names(which(state.list == input$state_choice))
      tagList(
        tags$h3(
          paste0("Midlife Mortality Rates for ",
                 names(which(cause.list == input$death_cause)), 
                 " in ", 
                 location_str,
                 " vs. United States")
        )
      )
    }
  })
  
  output$textMortFactsClosing <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h4(paste0(names(which(cause.definitions == input$death_cause))))
    )
  })
  
  # For a state or the US, creates the bulleted facts at the bottom of nationwide page
  output$textMortFacts <- renderUI({
    if(input$state_choice == "United States") {
      # percent change for first bullet
      change_text <- "remained the same"
      
      percent_change <- round(
        abs(national.mean.2015_2017() - national.mean.2000_2002()) / national.mean.2000_2002() * 100,
        1
      )
      
      if (percent_change > 0) {
        change_text <- paste0("increased ", percent_change, "%")
      }
      else if (percent_change < 0) {
        change_text <- paste0("decreased ", percent_change, "%")
      }
      
      tagList(
        tags$ul(
          tags$li(tags$h4(paste0("Have ", change_text, " from 2000 to 2017"))),
          tags$li(tags$h4(paste0("Range from ", 
                                 round(as.numeric(low.high.states.2015_2017()[1]), 1),
                                 " per 100k people in ",
                                 low.high.states.2015_2017()[2],
                                 " to ",
                                 round(as.numeric(low.high.states.2015_2017()[3]), 1),
                                 " per 100k people in ",
                                 low.high.states.2015_2017()[4],
                                 " 2015-2017"))
          )
        )
      )
    }
    else {
      # percent change for first bullet
      change_text <- "remained the same"
      vals_text <- paste0("at ", round(state.mean.2015_2017(), 1), " per 100k people")
      
      percent_change <- round(
        (state.mean.2015_2017() - state.mean.2000_2002()) / state.mean.2000_2002() * 100,
        1
      )
      
      if (percent_change > 0) {
        change_text <- paste0("increased by ", abs(percent_change), "%")
        vals_text <- paste0("rising from ", round(state.mean.2000_2002(), 1), 
                            " to ", round(state.mean.2015_2017(), 1))
      }
      else if (percent_change < 0) {
        change_text <- paste0("decreased by ", abs(percent_change), "%")
        vals_text <- paste0("falling from ", round(state.mean.2000_2002(), 1), 
                            " to ", round(state.mean.2015_2017(), 1))
      }
      
      # comparison wish national average
      
      comparison_text <- "the same as"
      
      if (national.mean.2015_2017() > state.mean.2015_2017()) {
        comparison_text <- "lower than"
      }
      else if (national.mean.2015_2017() < state.mean.2015_2017()) {
        comparison_text <- "greater than"
      }
      
      tagList(
        tags$ul(
          #style = "font-size: 18px;",
          tags$li(tags$h4(paste0("Have ", change_text, " from 2000 to 2017, ", vals_text, " per 100k people"))),
          tags$li(tags$h4(paste0("Were ", comparison_text, " the national mean in 2015-2017 of ",
                                 round(national.mean.2015_2017(), 2), " per 100k people"))),
          tags$li(tags$h4(paste0("Range from ", 
                                 round(as.numeric(low.rate.county.2015_2017()[1]), 1),
                                 " per 100k people in ",
                                 low.rate.county.2015_2017()[2],
                                 " to ",
                                 round(as.numeric(high.rate.county.2015_2017()[1]), 1),
                                 " per 100k people in ",
                                 high.rate.county.2015_2017()[2],
                                 "from 2015-2017")
          ))
        )
      )
    }
  })
  
  output$textMortFacts1 <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h5("Mean Mortality Rate for 2000-2002:", round(state.mean.2000_2002(),2)),
      tags$h5("Mean Mortality Rate for 2015-2017:", round(state.mean.2015_2017(),2)),
      tags$h5("National Mean for 2000-2002:", round(national.mean()[national.mean()$period == "2000-2002",]$death_rate,2)),
      tags$h5("National Mean for 2015-2017:", round(national.mean()[national.mean()$period == "2015-2017",]$death_rate,2)),
      tags$h5("Lowest Rate County for 2000-2002:", low.rate.county.2000_2002()),
      tags$h5("Lowest Rate County for 2015-2017:", low.rate.county.2015_2017()),
      tags$h5("National Mean for 2000-2002:", round(national.mean()[national.mean()$period == "2000-2002",]$death_rate,2)),
      tags$h5("National Mean for 2015-2017:", round(national.mean()[national.mean()$period == "2015-2017",]$death_rate,2))
    )
  })
  
  output$textNationalTitle <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h1(
        # paste0("Nationwide View: ",names(which(cause.list == input$death_cause)), " Midlife Mortality Rates Over Time")
        paste0(names(which(cause.list == input$death_cause)), " Midlife Mortality Rates Over Time")
      )
    )
  })
  
  output$textNationwideTitle <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if (is.null(input$page1_period)){
      period_choice = 6
    } else {
      period_choice = input$page1_period
    }
    
    tagList(
      tags$h3(
        paste0("Nationwide ",names(which(cause.list == input$death_cause)), " Rates for ", period.list[period_choice])
      )
    )
  })
  
  # Determinant Header (upper-right panel, Page 1)
  output$textDeterminants <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h3(
          title="Each factor is rated as Destructive, meaning that it has a positive correlation with the risk group; or Protective, meaning it has a negative correlation with the risk group. MortalityMinder shows those factors which have the highest absolute correlation with mortality risk groups For more information on the method of determining correlation please see GitHub Wiki.", 
          paste0("Factors Associated with ",names(which(cause.list == input$death_cause)), " for ", location_str),
          icon("info-circle")
        ),
        HTML("<h5>Kendall Correlation between social and economic factors and mortality risk groups. <span style='color:#f8766d'>Positively</span> (<span style='color:#00bfc4'>Negatively</span>) correlated factors indicate potential <span style='color:#f8766d'>Destructive</span> (<span style='color:#00bfc4'>Protective</span>) determinants of mortality. Click dot for details.</h5>"),
        NULL
      )
    }
    else {
      tagList(
        tags$h3(
          title="Each factor is rated as Destructive, meaning that it has a positive correlation with the risk group; or Protective, meaning it has a negative correlation with the risk group. MortalityMinder shows those factors which have the highest absolute correlation with mortality risk groups. For more information on the method of determining correlation please see GitHub Wiki.", 
          paste0("Factors Associated with ",names(which(cause.list == input$death_cause)), " for ", names(which(state.list == input$state_choice))),
          icon("info-circle")
        ),
        HTML("<h5>Kendall Correlation between social and economic factors and mortality risk groups. <span style='color:#f8766d'>Positively</span> (<span style='color:#00bfc4'>Negatively</span>) correlated factors indicate potential <span style='color:#f8766d'>Destructive</span> (<span style='color:#00bfc4'>Protective</span>) determinants of mortality. Click dot for details.</h5>"),
        NULL
      )
    }
  })

  
  
  # Cluster geo Header (Page 2 lower middle)
  output$textClusterGeo <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if (input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h3(
          title="This plot represents the geographic distribution of risk groups for the selected state.",
          paste0(names(which(cause.list == input$death_cause)), " Risk Groups for ",location_str)
          # ,icon("info-circle")
        ),
        tags$h6("A map of ",location_str," in which each county is categorized according to level of risk."),
        NULL
      )
    }
    else {
      tagList(
        tags$h3(
          title="This plot represents the geographic distribution of clusters for the selected state.",
          paste0(names(which(cause.list == input$death_cause)), " Risk Groups for ", names(which(state.list == input$state_choice)))
          # ,icon("info-circle")
        ),
        tags$h6("A map of ",names(which(state.list == input$state_choice))," in which each county is categorized according to level of risk."),
        NULL
      )
    }
  })
  

  
  
  # Determinant Header (upper-left panel, Page 2)
  output$textDeterminants2 <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h3(
          title="Each factor is rated as Destructive, meaning that it has a positive correlation with the risk group; or Protective, meaning it has a negative correlation with the risk group. MortalityMinder shows those factors which have the highest absolute correlation with mortality risk groups. For more information on the method of determining correlation please see GitHub Wiki",
          paste0("Factors Associated with ",names(which(cause.list == input$death_cause)), " for ", location_str), 
          icon("info-circle")
        ),
        HTML("<h5>Kendall Correlation between social and economic factors and mortality risk groups. <span style='color:#f8766d'>Positively</span> (<span style='color:#00bfc4'>Negatively</span>) correlated factors indicate potential <span style='color:#f8766d'>Destructive</span> (<span style='color:#00bfc4'>Protective</span>) determinants of mortality. Click dot for details.</h5>"),
        NULL
      )
    }
    else {
      tagList(
        tags$h3(
          title="Each factor is rated as Destructive, meaning that it has a positive correlation with the risk group; or Protective, meaning it has a negative correlation with the risk group. MortalityMinder shows those factors which have the highest absolute correlation with mortality risk groups. For more information on the method of determining correlation please see GitHub Wiki.",
          paste0("Factors Associated with ",names(which(cause.list == input$death_cause)), " for ", names(which(state.list == input$state_choice))), 
          icon("info-circle")
        ),
        HTML("<h5>Kendall Correlation between social and economic factors and mortality risk groups. <span style='color:#f8766d'>Positively</span> (<span style='color:#00bfc4'>Negatively</span>) correlated factors indicate potential <span style='color:#f8766d'>Destructive</span> (<span style='color:#00bfc4'>Protective</span>) determinants of mortality. Click dot for details.</h5>"),
        NULL
      )
    }
  })
  
  # Determinant Header (upper-center panel, Page 2)
  output$textDeterminants3 <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h2(
        title="Help text for cluster distribution bar plots",
        paste0("Distribution of '",input$determinant_choice, "' across ", names(which(cause.list == input$death_cause)), " clusters for ", names(which(state.list == input$state_choice))), 
        icon("info-circle")
      ),
      NULL
    )
  })
  


  
  # Determinant geo Header (upper-center panel, Page 2)
  output$textDeterminantsGeo <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h4(
        title="Geographic distribution of selected determinant across selected state",
        paste0("Distribution of '",input$determinant_choice, "' for ", names(which(state.list == input$state_choice))), 
        icon("info-circle")
      ),
      NULL
    )
  })
  
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
  
  # Mortality Trend Cluster by County
  output$geo_cluster_kmean <- renderLeaflet({
    
    if(input$state_choice == "United States"){
      kmean.us.plot(input$death_cause)
    }else{
      draw.geo.cluster(input$state_choice, input$death_cause, mort.cluster.ord(), 
                       max(mort.cluster.ord()$cluster))
    }
    
  })
  
  
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
  
  # Mortality Rate by County Period 1
  output$geo_mort_change1 <- renderLeaflet({
    if(input$state_choice == "United States"){
      mort.data <- dplyr::filter(
        cdc.data,
        death_cause == input$death_cause,
        period == "2000-2002"
      ) %>%
        dplyr::mutate(
          # death_rate = death_num / population * 10^5,
          death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
        ) %>%
        dplyr::select(county_fips, death_rate, period)
      
      geo.plot("US", input$death_cause, mort.data, "2000-2002")
      
    } else {
      
      mort.data <- dplyr::filter(
        cdc.data,
        state_abbr == input$state_choice,
        death_cause == input$death_cause,
        period == "2000-2002"
      ) %>%
        dplyr::mutate(
          # death_rate = death_num / population * 10^5,
          death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
        ) %>%
        dplyr::select(county_fips, death_rate, period)
      geo.plot(input$state_choice, input$death_cause, mort.data, "2000-2002")
    }
    
  })
  
  # Mortality Rate by County Period 2
  output$geo_mort_change2 <- renderLeaflet({
    if(input$state_choice == "United States"){
      geo.us.plot(input$death_cause, input$year_selector)
    } else{
      mort.data <- dplyr::filter(
        cdc.data,
        state_abbr == input$state_choice,
        death_cause == input$death_cause,
        period == input$year_selector
      ) %>% 
        dplyr::mutate(
          # death_rate = death_num / population * 10^5,
          death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
        ) %>%
        dplyr::select(county_fips, death_rate, period)
      
      geo.plot(input$state_choice, input$death_cause, mort.data, input$year_selector)
    }
    
  })
  

  

  
  # Kendall Correlation Between Cluster and CHR-SD
  output$page1.bar.cor1 <- renderPlot({
    
    # Sort by kendall.cor
    kendall.cor.new <- kendall.cor() %>% 
      dplyr::filter(kendall_p < 0.1) %>% 
      dplyr::arrange(desc(kendall_cor)) %>% 
      dplyr::top_n(15, abs(kendall_cor)) %>% 
      dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
    
    # # Set currently selected determinant to most correlated determinant
    # max.cor.ind = which.max(abs(kendall.cor.new$kendall_cor))
    # input$determinant_choice = kendall.cor.new[max.cor.ind, "chr_code"]
    
    #Only display the social determinants graph if there is any significant social determinant
    #Ex: New Hampshire, Delaware doesn't have any significant social determinant with p < 0.05
    if(nrow(kendall.cor.new) > 0) {
      updatePickerInput(session, "determinant_choice", selected = kendall.cor.new$chr_code[[1]])
      kendall.cor.new %>% 
        ggplot(
          aes(
            #x = reorder(chr_code, kendall_cor), 
            x = chr_code, 
            y = kendall_cor, 
            color = DIR, 
            fill = DIR)
        ) + 
        
        # Lolipop chart
        geom_point(stat = 'identity', size = 12) + 
        geom_segment(
          size = 1,
          aes(
            y = 0, 
            #x = reorder(chr_code, kendall_cor), 
            x = chr_code, 
            yend = kendall_cor, 
            #xend = reorder(chr_code, kendall_cor), 
            xend = chr_code, 
            color = DIR
          )
        ) +
        geom_text(
          aes(
            label = chr_code, 
            y = ifelse(DIR == "Protective", 0.1, -0.1),
            hjust = ifelse(DIR == "Protective", 0, 1)
          ), 
          color = "#565254", 
          size = 4
        ) +
        geom_text(
          aes(label = round(kendall_cor, 2)), 
          color = "#565254", 
          size = 3
        ) +
        
        # Coordinates
        coord_flip() + 
        scale_y_continuous(breaks = seq(-1, 1, by = .2), limits = c(-1, 1)) +
        
        # Themes
        geom_hline(yintercept = .0, linetype = "dashed") + 
        labs(
          y = "Correlation",
          x = NULL,
          fill = "Relationship",
          color = "Relationship"
        ) +
        theme_minimal() +
        theme.text() + 
        theme.background() + 
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          panel.grid.major.y = element_blank()
        ) + 
        theme(legend.position="top")
    }
    #Display something else when there are no significant SD
    else {
      
      # empty plot, then put text on it ?
      ggplot() + theme_void() +
        geom_text(aes(x = 0, y = 0, label="There are no significant social determinants."))
      
    }
  }, bg = "transparent")
  
  draw_border <- function(plot.name, border){
    proxy <- leafletProxy(plot.name)
    #remove any previously highlighted polygon
    proxy %>% clearGroup("highlighted_polygon")
    
    #add a slightly thicker red polygon on top of the selected one
    proxy %>% addPolylines(stroke = TRUE, 
                           weight = 4,
                           color="black",
                           data = border,
                           group="highlighted_polygon",
                           dashArray = "4 2 4")
  }
  
  highlight_county <- function(event){
    county_name <- sub(event$id, pattern = " [[:alpha:]]*$", replacement = "")
    
    county_indices <- which(state_map@data$NAME %in% c(county_name))
    
    if (length(county_indices) == 0){
      for (current_polygons in state_map@polygons){
        for (current_polygon in current_polygons@Polygons){
          current_coords <- current_polygon@coords
          if (sp::point.in.polygon(c(event$lng), c(event$lat), current_coords[,1], current_coords[,2])){
            assign("county_polygon", current_polygons, envir = .GlobalEnv)
            break
          }
        }
      }
    }else if (length(county_indices) == 1){
      assign("county_polygon", state_map@polygons[[county_indices[[1]]]], envir = .GlobalEnv)
    } else {
      for (index in county_indices){
        current_polygon <- state_map@polygons[[index]]
        current_coords <- current_polygon@Polygons[[1]]@coords
        if (sp::point.in.polygon(c(event$lng), c(event$lat), current_coords[,1], current_coords[,2])){
          assign("county_polygon", current_polygon, envir = .GlobalEnv)
          break
        }
      }
    }
    draw_border("geo_cluster_kmean", county_polygon)
    draw_border("geo_mort_change2", county_polygon)
    draw_border("determinants_plot5", county_polygon)
  }
  
  # click on geo cluster map shows county data on mort_line
  observe({
    event <- input$geo_cluster_kmean_shape_click
    if (is.null(event))
      return()
    highlight_county(event)
    county_choice(event$id)
    updatePickerInput(session, "county_drop_choice", selected = gsub(" County", "", event$id))
  })
  
  observe({
    event <- input$geo_mort_change2_shape_click
    if (is.null(event))
      return()
    highlight_county(event)
    county_choice(event$id)
    updatePickerInput(session, "county_drop_choice", selected = gsub(" County", "", event$id))
  })
  
  observe({
    event <- input$determinants_plot5_shape_click
    if (is.null(event))
      return()
    highlight_county(event)
    county_choice(paste0(event$id," County"))
    updatePickerInput(session, "county_drop_choice", selected = event$id)
  })
  
  observe({
    county_name <- sub(county_choice(), pattern = " [[:alpha:]]*$", replacement = "")
    req(county_name)
    county_indices <- which(state_map@data$NAME %in% c(county_name))
    
    if (length(county_indices) != 1){
      all.county = state_map@data$NAME
      highest.score = - Inf
      polygon = NULL
      for (index in seq(1, length(all.county))){
        county = all.county[[index]]
        curr.score =  stringdist::stringsim(county, county_name)
        if (curr.score > highest.score){
          highest.score = curr.score
          polygon = state_map@polygons[[index]]
        }
      }
    } else {
      polygon <- state_map@polygons[[county_indices[[1]]]] 
    }
    
    draw_border("geo_cluster_kmean", polygon)
    draw_border("geo_mort_change2", polygon)
    draw_border("determinants_plot5", polygon)
  })
  
  observe({
    event <- input$determinants_plot3_click
    req(event)
    
    geo.namemap$county_fips <- with_options(c(scipen = 999), str_pad(geo.namemap$county_fips, 5, pad = "0"))
    geo.namemap <- geo.namemap[geo.namemap$state_abbr != "HI",]
    geo.namemap <- rbind(geo.namemap, c("Hawaii", "HI", "15", "Hawaii", "15001"), c("Hawaii", "HI", "15", "Honolulu", "15003"), c("Hawaii", "HI", "15", "Kalawao", "15005"), c("Hawaii", "HI", "15", "Kauai", "15007"), c("Hawaii", "HI", "15", "Maui", "15009"))
    sd.code = chr.namemap.inv.2019[input$determinant_choice, "code"]
    sd.select <- chr.data.2019 %>% 
      dplyr::select(county_fips, VAR = sd.code) %>% 
      dplyr::right_join(mort.cluster.ord(), by = "county_fips") %>% 
      dplyr::inner_join(geo.namemap, by = "county_fips") %>%
      tidyr::drop_na()
    
    
    data <- dplyr::filter(
      cdc.data,
      period == "2015-2017", 
      death_cause == input$death_cause
    ) %>% 
      dplyr::select(county_fips, death_rate) %>% 
      dplyr::inner_join(sd.select, by = "county_fips") %>% 
      tidyr::drop_na()
    
    
    point <- nearPoints(data, event, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    county_name = point$county_name
    county_choice(paste0(county_name, " County"))
    
    updatePickerInput(session, "county_drop_choice", selected = point$county_name)
    
  })
  
  # click on bar plot triggers page change
  observe({
    req(input$page1_bar_plot_click) # Same as if-not-NULL
    click <- input$page1_bar_plot_click
    
    js$nextpage()
    
    point <- nearPoints(kendall_cor_new, click, threshold = 50, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    updatePickerInput(session, "determinant_choice", selected = point$chr_code)
  })
  
  # click on bar plot triggers page change
  observe({
    req(input$page2_bar_plot_click) # Same as if-not-NULL
    click <- input$page2_bar_plot_click
    
    point <- nearPoints(kendall_cor_new, click, threshold = 50, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    updatePickerInput(session, "determinant_choice", selected = point$chr_code)
  })
  }

shinyApp(ui = ui, server = server)

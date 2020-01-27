# Date: 2019/8/1
# Contributors: 
#   UI: Shengjin Li (prototype)
#   Server: Yuxuan Wang (prototype)
#   Visualizations: Ziyi Wang (prototype)
#   Fall 2019 Coding Team: Ross DeVito, Jose Figueroa, Xinyu Gu, Taras Kaminsky, Lilian Ngweta, 
#             Jiangshan Lin, Farukh Saidmuratov, Christina Van Hal, Ziyi Wang, Hongrui Zhang
#   Advisors: Kristin Bennett, John Erickson, Karan Bhanot
#   The Rensselaer Institute for Data Exploration & Applications (IDEA)

source("Source.R")
deps <- list("topojson.min.js", 
             htmlDependency(name = "d3-scale-chromatic",
                            version = "1.3.3",
                            src = list(href = "https://d3js.org/"),
                            script = "d3-scale-chromatic.v1.min.js")
)
#-----------------
state.list <- state.abb
names(state.list) <- state.name
state.list <- append(state.list, "United States", after = 0)

# Cause list with Assault
# cause.list <- c("All Cause" = "All Cause", "Deaths by Assault"="Assault","Cancer Deaths"="Cancer","Cardiovascular Disease"="Cardiovascular","Deaths of Despair"="Despair")
# cause.definitions <- c("\"Deaths of Despair\" are deaths due to suicide, overdose, substance abuse and poisonings"="Despair",
#                        "\"Deaths by Assault\" are deaths caused by injuries inflicted by another person with intent to injure or kill, by any means"="Assault",
#                        "\"Cardiovascular Disease\" are deaths due to diseases of the circulatory systems such as heart disease and stroke"="Cardiovascular",
#                        "\"Cancer Deaths\" are deaths due to cancer and neoplasm"="Cancer")

# Cause list with Assault commented out
cause.list <- c("All Cause" = "All Cause","Cancer"="Cancer","Cardiovascular Disease"="Cardiovascular","Deaths of Despair"="Despair")
cause.definitions <- c("\"Deaths of Despair\" are deaths due to suicide, overdose, substance abuse and poisonings"="Despair",
                       "\"Cardiovascular Disease\" are deaths due to diseases of the circulatory systems such as heart disease and stroke"="Cardiovascular",
                       "\"Cancer\" refers to deaths due to cancer and neoplasm"="Cancer")

period.list <- c("2000-2002","2003-2005","2006-2008","2009-2011","2012-2014","2015-2017")

n.clusters.state = 3
n.clusters.nation = 6
jscode <- "shinyjs.nextpage = function(){$('.fp-next').click();}"

ui_list <- list()

ui_list[["Page1"]] <- fluidPage(
  
  ##################### CSS Imports #####################  
  
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("nextpage")),
  tags$head(includeCSS("custom_no_scroll.css")),
  tags$head(includeCSS("jquery-ui.min.css")),
  tags$head(includeCSS("fullpage.css")),
  tags$head(includeCSS("geoattr.css")),
  tags$head(
    tags$script(src="jquery-3.4.1.min.js"),
    tags$script("$.noConflict(true);")),
  
  ##################### NAV BAR #####################
  tags$div(
    class = "navbar",
    
    tags$div(
      class = "title",
      tags$h1(
        "MortalityMinder")
    ),
    tags$div(
      class = "input",
      tags$h3(id = "input_text2", "State:"),
      pickerInput(
        inputId = "p1_state_choice",
        label = h4("State"), 
        choices = state.list,
        selected = "OH",
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          "dropup-auto" = FALSE,
          `max-options` = 1
        )      
      ),
      tags$h3(id = "input_text1", "Cause of Death:"),
      pickerInput(
        inputId = "p1_death_cause",
        label = h4("Cause of Death"),
        choices = cause.list,
        selected = "Despair",
        choicesOpt = list(
          subtext = c(" "," "," ","Self-Harm and some other causes"),
          "dropup-auto" = FALSE
        )
      )
      
    )
    
    
    
    
  ),
  
  tags$div(
    id = "fullpage",
    tags$div(
      class = "section s1",
      
      ##################### PAGE 1, NATIONWIDE ANALYSIS #####################
      
      tags$div(
        class = "slide",
        tags$div(
          class = "nav_bar_blank"
        ),
        # Div tag functions as outter "shell" to pull from fullpage.css
        # Each page is a row, of columns, of rows, etc.
        
        fluidRow(
          class = "page page1", # National Map Page
          uiOutput("national_map"),
          column(3, 
                 class="page1_col page1_col1", 
                 tags$div(
                   class = "page1_col1_heading",
                   htmlOutput("page1_main_header")
                 ),
                 tags$h4("MortalityMinder analyzes trends of premature death in the United States which are caused by:\n"),
                 tags$ul(
                   tags$li("All Causes"),
                   tags$li("Cancer"),
                   tags$li("Deaths of Despair"),
                   tags$li("Cardiovascular Disease")
                   # tags$li(tags$h4("Assault Deaths"))
                 ), # End List
                 tags$h4("MortalityMinder is an interactive presentation that examines county-level factors associated with midlife mortality trends.\n"), 
                 HTML("<h4>Choose <b>State</b> and <b>Cause of Death</b> on the menu bar at the top of the page(and <b>Risk Factor</b> on Factor View page) to see how mortality rates in the selected state and the United States have changed from 2000 to 2017.</h4>"), 
                 tags$br(),
                 tags$img(
                   class="IDEA_Logo_Wrapper2",
                   width = "80%",
                   src="RPIlogo.png",
                   alt = "Institute of Data Exploration and Applications")
                 
          ), # End Column 1
          tags$div(
            class = "vl"
          ),
          column(8,
                 fluidRow(
                   class = "page1_col page1_col2_top",
                   tags$div(
                     class = "page1_title",
                     uiOutput("textNationalTitle"),
                     uiOutput("textMortFactsClosing")
                   )
                 ), # End of inner FluidRow (Column 2 top)
                 
                 fluidRow(class="page1_col page1_col2_middle",
                          fluidRow(
                            tags$ul(
                              class = "ul_period",
                              tags$button(
                                id = "first_period",
                                class = "period_text",
                                "2000-02"
                              ),
                              tags$button(
                                id = "second_period",
                                class = "period_text",
                                "2003-05"
                              ),
                              tags$button(
                                id = "third_period",
                                class = "period_text",
                                "2006-08"
                              ),
                              tags$button(
                                id = "forth_period",
                                class = "period_text",
                                "2009-11"
                              ),
                              tags$button(
                                id = "fifth_period",
                                class = "period_text",
                                "2012-14"
                              ),
                              tags$button(
                                id = "sixth_period",
                                class = "period_text",
                                style= "background-color: #565254; color: #f7f7f7;",
                                "2015-17"
                              )
                            ) # End List of buttons
                          ), # End Button Functionality
                          fluidRow(
                            class="page1_col2_graphics_row",
                            column(6,
                                   class = "page1_col page1_col2_middle_left",
                                   # tags$h3("National Plot Title"),
                                   tags$div(class = "page1_title",
                                            uiOutput("textNationwideTitle")
                                   ), 
                                   tags$div(class="NationalMapContainer",
                                            style="position:relative;width: 100%;left: 0;",
                                            tags$img(
                                              id = "national_map_new",
                                              class = "landing_page_map",
                                              src = "Despair/1.png",
                                              alt = "US National map plotting deaths of despair at the county level."
                                            )
                                   ) # End of Image DIV container
                            ), # End of Middle inner Column
                            column(6,
                                   class = "page1_col page1_col2_middle_right",
                                   tags$div(class = "page1_title",
                                            uiOutput("textInfographicTitle")
                                   ),
                                   tags$div(class = "nation_state_infographic",
                                            plotOutput("nation_state_infographic")
                                   )
                            )
                          )
                 )
                 , # End of inner Fluid Row (Column 2 Middle)
                 fluidRow(
                   class = "page1_col page1_col2_bottom",
                   uiOutput("textMortFactsNew")
                   
                 ) # Close inner FluidRow (Column 2 Bottom)
          ) #Close Column 2
        ) #Close Outter Row (National Map Page)
        
      ) # Close div tag "slide"
    )
  )
)
ui_list[["Page2"]] <- fluidPage(
  
  ##################### CSS Imports #####################  
  
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("nextpage")),
  tags$head(includeCSS("custom_no_scroll.css")),
  tags$head(includeCSS("jquery-ui.min.css")),
  tags$head(includeCSS("fullpage.css")),
  tags$head(includeCSS("geoattr.css")),
  tags$head(
    tags$script(src="jquery-3.4.1.min.js"),
    tags$script("$.noConflict(true);")),
  
  ##################### NAV BAR #####################
  tags$div(
    class = "navbar",
    
    tags$div(
      class = "title",
      tags$h1(
        "MortalityMinder")
    ),
    tags$div(
      class = "input",
      tags$h3(id = "input_text2", "State:"),
      pickerInput(
        inputId = "p2_state_choice",
        label = h4("State"), 
        choices = state.list,
        selected = NULL,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          "dropup-auto" = FALSE,
          `max-options` = 1
        )      
      ),
      tags$h3(id = "input_text1", "Cause of Death:"),
      pickerInput(
        inputId = "p2_death_cause",
        label = h4("Cause of Death"),
        choices = cause.list,
        selected = "Despair",
        choicesOpt = list(
          subtext = c(" "," "," ","Self-Harm and some other causes"),
          "dropup-auto" = FALSE
        )
      ),
      tags$script(src = "jquery-ui.min.js"),
      tags$script(src = "fullpage.js"),
      tags$script(src = "jquery.ba-outside-events.js"),
      includeScript(path = "myscript.js")
      
    ),
    
    
    ##################### PAGE 2, INDIVIDUAL STATE ANALYSIS #####################
    
    
    
    tags$div(
      class = "slide",
      tags$div(
        class = "nav_bar_blank"
      ),
      fluidRow(
        class = "page page2",
        column(7,
               class="page2_col page2_col1",
               fluidRow(
                 class="page2_col page2_col1_top",
                 
                 column(4,
                        class = "page2_col page2_col1_top_left",
                        tags$div(
                          class = "page2_col1_heading",
                          htmlOutput("page2_main_header")
                        ),
                        uiOutput("textDescription")
                        
                 ), # End of inner Column (Column 1 Top Left)
                 column(8,
                        class = "page2_col page2_col1_top_right",
                        tags$div(
                          class="page2_col1_top_right_title",
                          uiOutput("textMortRates")
                        ), # End of title div container
                        radioButtons("year_selector",
                                     #label = "Click on time period to select state map for that period",
                                     label = NULL,
                                     selected = "2015-2017",
                                     choiceNames = c("2000-02", "2003-05", "2006-08", "2009-11", "2012-14", "2015-17"),
                                     choiceValues = c("2000-2002", "2003-2005", "2006-2008", "2009-2011", "2012-2014", "2015-2017"),
                                     inline = TRUE),
                        leafletOutput("geo_mort_change2",width="82%",height="75%")
                 ) # End of inner Column (Column 1 top right)
                 
               ), # End of inner FluidRow (Column1 Top)
               tags$div(
                 class = "hr"
               ),
               fluidRow(
                 class = "page2_col page2_col1_bot",
                 column(5,
                        class = "page2_col page2_col1_bot_left",
                        tags$div(
                          class="page2_col1_bot_left_title",
                          uiOutput("textClusterGeo")
                        ), # End of title div container
                        leafletOutput("geo_cluster_kmean",width="82%",height="75%")
                 ), # End of inner Column (Bottom Left)
                 column(5, 
                        class = "page2_col page2_col1_bot_right", 
                        tags$div(
                          class="page2_col1_bot_right_title",
                          uiOutput("textDeathTrends")
                        ), # End of title div container
                        plotOutput("mort_line",width="100%",height="70%")
                 ) # End of inner Column (Bottom Right)
                 
               ) #End of inner fluidRow (Column 1 Bottom)
        ), # End of Column 1
        column(3,
               class = "page2_col page2_col2",
               tags$div(
                 class = "page2_col2_title",
                 uiOutput("textDeterminants")
               ), # End of title container
               
               tags$div(
                 class = "page2_col2_plot",
                 plotOutput("page1.bar.cor1",width="100%",height="100%", 
                            # hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                            hover = hoverOpts("plot_hover"),
                            click = clickOpts("page1_bar_plot_click")),
                 uiOutput("hover_info")
               ) # End of plot div container
        ) # End of Column 2
      )# End of FluidRow (Page1, State Analysis) 
    ), # End of slide div tag
    tags$script(src = "jquery-ui.min.js"),
    tags$script(src = "fullpage.js"),
    tags$script(src = "jquery.ba-outside-events.js"),
    includeScript(path = "myscript.js")
    
  )
)


ui_list[["Page3"]] <- fluidPage(
  ##################### CSS Imports #####################  
  
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("nextpage")),
  tags$head(includeCSS("custom_no_scroll.css")),
  tags$head(includeCSS("jquery-ui.min.css")),
  tags$head(includeCSS("fullpage.css")),
  tags$head(includeCSS("geoattr.css")),
  tags$head(
    tags$script(src="jquery-3.4.1.min.js"),
    tags$script("$.noConflict(true);")),
  
  ##################### NAV BAR #####################
  tags$div(
    class = "navbar",
    
    tags$div(
      class = "title",
      tags$h1(
        "MortalityMinder")
    ),
    tags$div(
      class = "input",
      tags$h3(id = "input_text2", "State:"),
      pickerInput(
        inputId = "p3_state_choice",
        label = h4("State"), 
        choices = state.list,
        selected = NULL,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          "dropup-auto" = FALSE,
          `max-options` = 1
        )      
      ),
      tags$h3(id = "input_text1", "Cause of Death:"),
      pickerInput(
        inputId = "p3_death_cause",
        label = h4("Cause of Death"),
        choices = cause.list,
        selected = "Despair",
        choicesOpt = list(
          subtext = c(" "," "," ","Self-Harm and some other causes"),
          "dropup-auto" = FALSE
        )
      ),
      tags$script(src = "jquery-ui.min.js"),
      tags$script(src = "fullpage.js"),
      tags$script(src = "jquery.ba-outside-events.js"),
      includeScript(path = "myscript.js")
      
    ),
    
    ##################### PAGE 3, INDIVIDUAL DETERMINANT ANALYSIS #####################
    
    tags$div(
      class = "slide",
      tags$div(
        class = "nav_bar_blank"
      ),
      fluidRow(
        class = "page page3",
        column(4,
               class = "page3_col page3_col3",
               fluidRow(
                 class = "page3_col3_top",
                 tags$div(
                   class = "page3_col1_heading",
                   htmlOutput("page3_main_header")
                 ),
                 tags$div(
                   tags$div(
                     class = "prompt_text",
                     "Select a factor:"              
                   ),
                   pickerInput(
                     inputId = "determinant_choice",
                     label = "Selected Determinant: ",
                     choices = str_sort(chr.namemap.2019[intersect(colnames(chr.data.2019), rownames(chr.namemap.2019)),]),
                     selected = "Socio-Economic",
                     width = "100%",
                     inline = TRUE,
                     options = list(
                       `live-search` = TRUE,
                       "dropup-auto" = TRUE
                     ) # End of Options
                   ) # End of pickerInput
                 ), # End of pickerInput container
                 
                 tags$br(),
                 tags$h4(htmlOutput("determinant_text")),
                 tags$h5(uiOutput("determinant_link")),
                 tags$h5(htmlOutput("determinant_original_source")),
                 tags$h5(htmlOutput("determinant_corr")),
                 tags$h5(htmlOutput("determinant_dir"))
               ),# End of Column 3 top
               tags$br(),
               fluidRow(
                 class = "page3_col3_bot",
                 tags$div(
                   tags$div(
                     class = "prompt_text",
                     uiOutput("textCountyPrompt")              
                   ),
                   uiOutput("county_selector")
                 ), # End of pickerInput container
                 leafletOutput("determinants_plot5", width="82%",height="75%"),
                 tags$div(
                   class="data_source_footnote",
                   HTML("<h6 style='text-align: right;'>Mortality Data: CDC Wonder Detailed Mortality<br>Feature Data: County Health Rankings<br>Analysis: The Rensselaer IDEA</h6>")
                 ),
                 fluidRow(
                   class = "page3_col3_county_desc",
                   uiOutput("county_desc")
                 )
               ) # End of inner Column 3 bottom
               
        ), # End Column 1
        
        tags$div(
          class = "vl"
        ),
        column(4,
               class = "page3_col page3_col2",
               
               fluidRow(
                 class = "page3_col2_top",
                 uiOutput("textBoxplotTitle"),
                 plotOutput("determinants_plot2",height="70%")
               ), #End of Column 2 Top
               
               #tags$div(class = "hr"),
               
               fluidRow(
                 class = "page3_col2_bot",
                 style = "position: relative",
                 uiOutput("textScatterplotTitle"),
                 uiOutput("determinants_plot3_county_name"),
                 plotOutput("determinants_plot3",height="80%",
                            click = clickOpts("determinants_plot3_click"), hover = hoverOpts("determinants_plot3_hover"))
               ) # End of Column 2 Bottom
        ), # End of Column 2
        tags$div(
          class = "vl"
        ),
        column(4,
               class = "page3_col page3_col1",
               tags$div(
                 class = "col1_title",
                 uiOutput("textDeterminants2")
               ), # End title div container
               plotOutput("determinants_plot1", height = "95%", width = "100%",
                          click = clickOpts("page2_bar_plot_click"))
        )
        # End of Column 3
      ) # End of Fluid Row
    ), # End of Page 3
    tags$script(src = "jquery-ui.min.js"),
    tags$script(src = "fullpage.js"),
    tags$script(src = "jquery.ba-outside-events.js"),
    includeScript(path = "myscript.js")
  )
)

ui_list[["Page4"]] <- fluidPage(
  
  ##################### CSS Imports #####################  
  
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("nextpage")),
  tags$head(includeCSS("custom_no_scroll.css")),
  tags$head(includeCSS("jquery-ui.min.css")),
  tags$head(includeCSS("fullpage.css")),
  tags$head(includeCSS("geoattr.css")),
  tags$head(
    tags$script(src="jquery-3.4.1.min.js"),
    tags$script("$.noConflict(true);")),
  
  ##################### NAV BAR #####################
  tags$div(
    class = "navbar",
    
    tags$div(
      class = "title",
      tags$h1(
        "MortalityMinder")
    ),
    tags$div(
      class = "input",
      tags$h3(id = "input_text2", "State:"),
      pickerInput(
        inputId = "p4_state_choice",
        label = h4("State"), 
        choices = state.list,
        selected = NULL,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          "dropup-auto" = FALSE,
          `max-options` = 1
        )      
      ),
      tags$h3(id = "input_text1", "Cause of Death:"),
      pickerInput(
        inputId = "p4_death_cause",
        label = h4("Cause of Death"),
        choices = cause.list,
        selected = "Despair",
        choicesOpt = list(
          subtext = c(" "," "," ","Self-Harm and some other causes"),
          "dropup-auto" = FALSE
        )
      ),
      tags$script(src = "jquery-ui.min.js"),
      tags$script(src = "fullpage.js"),
      tags$script(src = "jquery.ba-outside-events.js"),
      includeScript(path = "myscript.js")
      
    ),
    ##################### PAGE 4, ABOUT PAGE #####################
    
    tags$div(
      class = "slide",
      tags$div(
        class = "nav_bar_blank"
      ),
      fluidRow(
        class = "page page4",
        column(4, tags$h4("ABOUT MORTALITYMINDER",align="center"),
               fluidRow(
                 column(11, 
                        HTML("<h5>The goal of MortalityMinder (MM)  is to enable healthcare researchers, providers, 
                             payers, and policy makers to gain actionable insights into how, where, 
                             and why midlife mortality rates are rising in the United States (US).  
                             </h5>"),
                        tags$ul(
                          tags$li(HTML("Explores mortality trends for midlife adults ages 25-64 across the United 
                                       States from 2000 to 2017 using data from <a href=\"https://wonder.cdc.gov/mcd.html\" 
                                       target=\"_blank\">CDC WONDER</a>, the definitive source of US mortality data.")
                          ),
                          tags$li(
                            HTML("Identifies social and economic factors associated with increased mortality trends at the 
                                 county-level for each state and the nation obtained from <a href=\"https://www.countyhealthrankings.org/\"
                                 target=\"_blank\">County Health Rankings (CHR)</a>, an aggregate of county-level data from 20 sources 
                                 curated by the Robert Wood Johnson Foundation.")
                            ),
                          tags$li("Addresses factors including health behaviors, clinical care, education, employment, social supports, 
                                  community safety and physical environment domains."),
                          tags$li(
                            "Interactively visualizes potential determinants and their impact on mortality trends."
                          ),
                          tags$li(
                            "Investigates deaths due to All Causes, Cancer Deaths, Cardiovascular Deaths, Deaths of Despair (suicide, self harm and overdose)."
                          ),
                          tags$li(HTML(
                            "Publicly-accessible, freely available, easily maintained, and readily extensible 
                            <a href='https://github.com/TheRensselaerIDEA/MortalityMinder/' target=_blank>open source</a> web tool.")
                          )
                          ),
                        HTML("<h5>Limitation: Associated factors are correlated to midlife mortality rates. Further investigation is needed to see if they actually cause changes in mortality rate.</h5>")
                          ),
                 column(11, tags$h4("DOWNLOAD SOURCE DATA",align="center"),
                        fluidRow(downloadButton("downloadCDCData", "Mortality Data", class = "dbutton")), ##tags$br(),
                        fluidRow(downloadButton("downloadCHRData", "Factor Data", class = "dbutton")), ##tags$br(),
                        fluidRow(downloadButton("downloadFactorDesc", "Factor Descriptions", class = "dbutton")), ##tags$br(),
                        tags$h4("DOWNLOAD CURRENT RESULTS",align="center"), 
                        fluidRow(downloadButton("downloadClusters", "Current State Clusters", class = "dbutton")), ##tags$br(),
                        fluidRow(downloadButton("downloadClusterTime", "Current State Clusters Through Time", class = "dbutton")), ##tags$br(),
                        fluidRow(downloadButton("downloadCorr", "Current Factor Correlations", class = "dbutton"))
                 ),
                 column(11,
                        tags$img(
                          class="IDEA_Logo_Wrapper3",
                          src="no_mobile.png",
                          alt = "Do not use with mobile devices"),
                        tags$h6("MortalityMinder has been optimized for laptop and large-screen use. Use with mobile devices is not recommended.")
                 )
                 ) # Close row
        ), #close column
        column(4, tags$h4("INNOVATION",align="center"),   offset=1,
               fluidRow(
                 column(11, tags$h5("MortalityMinder (MM) dramatically illustrates  midlife mortality rate increases reported in  (Wolf and Schoomaker, 
                                    JAMA 2019), while providing greater insight into community-level variations and their associated 
                                    factors to help determine remedies."), 
                        HTML("<h5>Using authoritative data from the CDC and other sources, MM is designed to help health policy decision makers in the 
                             public and private sectors identify and address unmet healthcare needs, healthcare costs, and healthcare utilization.</h5>"),
                        HTML("<h5>Innovative analysis divides counties into <b>risk groups</b> for visualization and correlation analysis using K-Means clustering and Kendall correlation.</h5>"),
                        HTML("<h5>For each State and Cause of Death, MM dynamically creates three analysis and visualization infographics:</h5>"),
                        tags$ul(
                          tags$li(HTML("<b>National View</b> reveals midlife mortality rates  through time and compares state and national trends.")),
                          tags$li(HTML("<b>State View</b> categorizes counties into risk groups based on their midlife mortality rates over time. 
                                       The app determines correlations of factors to risk groups and  visualizes the most significant protective 
                                       and destructive factors. ")),
                          tags$li(HTML("<b>Factor View</b> enables users to explore individual factors including their relation to the selected cause at 
                                       a county level for each state and the distribution of those factors within each state.")),
                          tags$li(HTML("Selecting 'United States' for <b>State</b> initiates nationwide analysis."))
                          )),
                 
                 column(11, tags$h4("INSIGHTS",align="center"), 
                        HTML("<h5>MortalityMinder provides a compelling and engaging tool to investigate the social and economic determinants of mortality. MM:</h5>"), 
                        tags$ul(
                          tags$li("Documents the disturbing rise in midlife Deaths of Despair due to suicide, overdose, and self-harm and other 
                                  national/regional increases in midlife mortality rates due to All Causes, Cancer, and Cardiovascular Disease."),
                          tags$li("Highlights potential social determinants through statistical analysis of factors associated with disparities 
                                  in regional trends in midlife mortality rates."),
                          tags$li("Provides county-level confirmation of trends and hypothesized causes."),
                          tags$li("Yields insights that can be used to create region-specific interventions and best practices to meet unmet healthcare needs."),
                          tags$li("Enables rigorous analysis of potential determinants of health by local, state, and national healthcare 
                                  organizations to support development of programs, policies, and procedures to improve longevity.  ")
                          ) # End of list
                          ) 
                        ), 
               fluidRow(class="IDEA_Logo_Wrapper",
                        tags$img(
                          class="Idea_Logo",
                          src="IDEA_logo_500.png", 
                          width="100%", 
                          style="bottom: 0; left: 0;",
                          alt = "Institute of Data Exploration and Applications"
                        )
               )
                        ), # Close column
        column(4,
               column(11, tags$h4("IMPLEMENTATION AND DEPLOYMENT",align="center"), 
                      HTML("<h5>MortalityMinder is an open-source R project freely available with full documentation via a 
                           <a href='https://github.com/TheRensselaerIDEA/MortalityMinder/', target=_blank>GitHub repository</a>.</h5>"),
                      tags$ul(
                        tags$li("R was chosen for its powerful environment for statistical computing and graphics using standard packages. "), 
                        tags$li(HTML("MM utilizes the <a href='https://shiny.rstudio.com/' target='_blank'>R Shiny</a> and 
                                     <a href='https://github.com/alvarotrigo/fullPage.js' target='_blank'>FullPage Javascript</a>
                                     frameworks  for web interactivity.")),
                        tags$li("Source data preparation is documented on the GitHub Wiki. Data Loader scripts enable new data sources 
                                and preparations to be easily incorporated. Data may be downloaded under 'DOWNLOAD SOURCE DATA'."),
                        tags$li("Missing county mortality rates are imputed using state-wide rates and Amelia R Package."),
                        tags$li("MM can be run from the public web locations or installed locally."), 
                        tags$li("Code is easily customized, extended, and maintained. The app continuously evolves in an agile framework
                                to incorporate user feedback and introductions of new data streams, analyses, visualization, and health 
                                care problems."),
                        tags$li("App design based on formal usability study of 20+ users and recommendations from our advisory board of 
                                healthcare and design professionals."),
                        tags$li("The innovative visualizations and analytics in MortalityMinder can be adapted into other applications 
                                or formats by using the provided code and data.")
                        )
                        ),
               column(11, tags$h4("ACKNOWLEDGEMENTS", align = "center"), 
                      tags$h5("MortalityMinder was created by undergraduate and graduate students in the Health Analytics Challenge Lab at Rensselaer Polytechnic Institute with generous support from the United Health Foundation and the Rensselaer Institute for Data Exploration and Applications (IDEA). MortalityMinder was directed by Kristin P. Bennett and John S. Erickson."),
                      tags$h5("The MortalityMinder Team would like to thank our advisory board, including Ms. Anne Yau, United Health Foundation; Dr. Dan Fabius, Continuum Health; Ms. Melissa Kamal, New York State Department of Health; and Dr. Tom White, Capital District Physicians' Health Plan (CDPHP).")
               ),
               column(11, tags$h4("LINKS", align = "center"), 
                      HTML("<h5><a href='https://github.com/TheRensselaerIDEA/MortalityMinder/' target=_blank>MortalityMinder GitHub Repository (public)</a><br>
                           <a href='https://github.com/TheRensselaerIDEA/MortalityMinder/wiki' target=_blank>MortalityMinder GitHub Wiki (public)</a><br>
                           <a href='https://bit.ly/mortalityminder_video_final' target=_blank>MortalityMinder Video</a><br>
                           <a href='http://bit.ly/mortalityminder_slides' target=_blank>MortalityMinder Overview Slides</a><br><br>
                           Please send questions and comments about MortalityMinder to: <a href='mailto:erickj4@rpi.edu' target=_blank>erickj4@rpi.edu</a><br>
                           Suggest improvements and report bugs on <a href='https://github.com/TheRensselaerIDEA/MortalityMinder/issues' target=_blank>GitHub</a></h5>")
                      )
                      )
        # Close inner fluidRow
                      )
               ) # Close outter fluidRow
               ), # Close Page 4
  tags$script(src = "jquery-ui.min.js"),
  tags$script(src = "fullpage.js"),
  tags$script(src = "jquery.ba-outside-events.js"),
  includeScript(path = "myscript.js")
                        )




# ##################### Server Code #####################

serv_calc <- list()

# This calc is for synchronizing the state nav bar selection across pages as
# well as create a universal state_choice value. Each observeEvent checks for
# a picker to change, then it updates each other page's picker
serv_calc[[1]] <- function(calc, session) {
  # if(!exists("calc$state_choice")) {
  #   calc$state_choice <- "OH"
  #  }
}  

serv_calc[[2]] <- function(calc, session) {
  observeEvent(calc$p1_state_choice, {
    calc$state_choice <- calc$p1_state_choice
    
  })
  observeEvent(calc$p2_state_choice, {
    calc$state_choice <- calc$p2_state_choice
  })
  observeEvent(calc$p3_state_choice, {
    calc$state_choice <- calc$p3_state_choice
  })
  observeEvent(calc$p4_state_choice, {
    calc$state_choice <- calc$p4_state_choice
  })
  
  observeEvent(calc$state_choice,{
    updatePickerInput(session,"p1_state_choice", select = calc$state_choice)
    updatePickerInput(session, "p2_state_choice", select = calc$state_choice)
    updatePickerInput(session, "p3_state_choice", select = calc$state_choice)
    updatePickerInput(session, "p4_state_choice", select = calc$state_choice)
    
  })
  
  
}

serv_calc[[3]] <- function(calc, session) {
  observeEvent(calc$p1_death_cause, {
    calc$death_cause <- calc$p1_death_cause
    
  })
  observeEvent(calc$p2_death_cause, {
    calc$death_cause <- calc$p2_death_cause
    
  })
  observeEvent(calc$p3_death_cause, {
    calc$death_cause <- calc$p3_death_cause
    
  })
  observeEvent(calc$p4_death_cause, {
    calc$death_cause <- calc$p4_death_cause
    
  })
  
  observe({
    #calc$state_choice
    updatePickerInput(session,"p1_death_cause", select = calc$death_cause)
    updatePickerInput(session, "p2_death_cause", select = calc$death_cause)
    updatePickerInput(session, "p3_death_cause", select = calc$death_cause)
    updatePickerInput(session, "p4_death_cause", select = calc$death_cause)
  })
  
  
}

#Extracting the national mean
serv_calc[[4]] <- function(calc, session) {
  calc$determinant.url <- reactive({
    return(as.character(
      SocialDeterminants[SocialDeterminants$Name == calc$determinant_choice,]$"URL"))
  })
}


serv_calc[[5]] <- function(calc, session) {
  calc$determinant.source <- reactive({
    return(as.character(
      SocialDeterminants[SocialDeterminants$Name == calc$determinant_choice,]$"Source"))
  })
  
  calc$determinant.source_url <- reactive({
    return(as.character(
      SocialDeterminants[SocialDeterminants$Name == calc$determinant_choice,]$"Source_url"))
  })
}

serv_calc[[6]] <- function(calc, session) {
  calc$county_choice <- reactiveVal()
}
serv_calc[[7]] <- function(calc, session) {
  calc$mort.rate <- reactive({
    calc$county_choice(NULL)
    assign("county_polygon", NULL, envir = .GlobalEnv)
    assign("page1_period_choice", 6, envir = .GlobalEnv)
    if(calc$state_choice == "United States"){
      cdc.data %>% dplyr::filter(
        death_cause == calc$death_cause,
        #state_abbr == input$state_choice,
        period == "2015-2017"
      ) %>%
        dplyr::mutate(
          # death_rate = death_num / population * 10^5
          #death_rate = cut(death_rate, bin.geo.mort("Despair"))
        ) %>%
        dplyr::select(county_fips, death_rate)
    }else {
      assign("state_map", readRDS(paste("../shape_files/", calc$state_choice, ".Rds", sep = "")), envir = .GlobalEnv)
      cdc.data %>% dplyr::filter(
        death_cause == calc$death_cause,
        state_abbr == calc$state_choice,
        period == "2015-2017"
      ) %>%
        dplyr::mutate(
          # death_rate = death_num / population * 10^5
          #death_rate = cut(death_rate, bin.geo.mort("Despair"))
        ) %>%
        dplyr::select(county_fips, death_rate)
    }
  })
}


# get unfiltered kendal cors
serv_calc[[8]] <- function(calc, session) {
  calc$kendall.cor <- reactive({
    
    calc$kendall.cor.new <- calc$mort.rate() %>%
      dplyr::mutate(VAR = death_rate) %>%
      kendall.func(chr.data.2019) %>%
      dplyr::mutate(
        DIR = dplyr::if_else(
          kendall_cor <= 0,
          "Protective",
          "Destructive"
        ),
        chr_code = chr.namemap.2019[chr_code, 1]
      ) %>% na.omit()
    
  })
}

#Extracting the national mean
serv_calc[[9]] <- function(calc, session) {
  national.mean <- reactive({
    switch(calc$death_cause,
           "Despair" = {
             death_rate <- c(28.929453, 33.665595, 37.821445, 40.081486, 43.900063, 55.084642)
           },
           "Assault" = {
             death_rate <- c(6.750937, 6.729051, 6.687417, 5.934990, 5.915201, 6.999898)
           },
           "Cancer" = {
             death_rate <- c(107.637100, 107.638200, 106.628310, 106.949100, 105.219690, 101.169700)
           },
           "Cardiovascular" = {
             death_rate <- c(96.830591, 95.807343, 92.915303, 90.702418, 91.232679, 93.598232)
           },
           "All Cause" = {
             death_rate <- c(366.07178, 373.10366, 373.65807, 373.40143, 379.60383, 395.93077)
           })
    
    
    nation.dataframe <- data.frame(
      period = c("2000-2002", "2003-2005", "2006-2008", "2009-2011", "2012-2014", "2015-2017"),
      cluster = rep("National", 6),
      death_rate,
      count = rep(NA, 6))
  })
}



serv_out <- list()

serv_out[["page1_main_header"]] <- function(calc, session) {
  renderUI({
    if (calc$state_choice == "United States") {
      tags$h3(
        paste0("Nationwide View: What are the trends in midlife mortality rates for ", names(which(cause.list == calc$death_cause)), " across the United States?")
      )
    } else {
      tags$h3(
        paste0("Nationwide View: What are the trends in midlife mortality rates for ", names(which(cause.list == calc$death_cause)), " across the United States and in ",
               names(which(state.list == calc$state_choice)), "?")
      )
    }
    
  })
}

serv_out[["page2_main_header"]] <- function(calc, session) {
  renderUI({
    if (calc$state_choice == "United States") {
      location_str = "the United States"
    } else {
      location_str = names(which(state.list == calc$state_choice))
    }
    tags$h3(
      paste0("State View: How do midlife mortality rates for ", names(which(cause.list == calc$death_cause)), " vary by county across ", location_str, " and why?")
    )
  })
}

serv_out[["page3_main_header"]] <- function(calc, session) {
  renderUI({
    if (calc$state_choice == "United States") {
      location_str = "the United States"
    } else {
      location_str = names(which(state.list == calc$state_choice))
    }
    tags$h3(
      paste0("Factor View: How are county-level social and economic factors associated with midlife mortality rates for ", names(which(cause.list == calc$death_cause)), " in ", location_str,"?")
    )
  })
}

# Textual description box (upper-left panel, Page 1)
serv_out[["textDescription"]] <- function(calc, session) {
  renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h5(paste0(names(which(cause.definitions == calc$death_cause)))),
      HTML("<h5>In this analysis, counties that share similar midlife mortality rate trends are categorized into <b>risk groups</b>.</h5>"),
      HTML("<h5>The <b>upper map</b> to the right shows the <b>midlife mortality rates</b> of the counties over time. The <b>lower map</b> on the left shows the <b>risk group</b> of each county. The <b>line graph</b> below compares the average mortality rates per year for each risk group  with the national mean (blue)."),
      HTML("<h5><b>Darker colors</b> indicate increased midlife mortality risk. <b>Hover</b> to see information and definitions. <b>Click on maps</b> to see county names and mortality rates. <b>Zoom maps</b> with buttons or mouse."),
      NULL
    )
  })
}

serv_out[["textMortFactsTitle"]] <- function(calc, session) {
  renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    if(calc$state_choice == "United States") {
      location_str <- "the United States"
    }
    else {
      location_str <- names(which(state.list == calc$state_choice))
    }
    tagList(
      tags$h3(
        paste0("Midlife Mortality Rates for ",
               names(which(cause.list == calc$death_cause)),
               " in ",
               location_str,
               ":")
      )
    )
  })
}

serv_out[["determinant_text"]] <- function(calc, session) {
  renderUI({
    reason_text <- ""
    if (!is.na(SocialDeterminants[SocialDeterminants$Name == calc$determinant_choice,]$"Reason")) {
      reason_text <- as.character(
        SocialDeterminants[SocialDeterminants$Name == calc$determinant_choice,]$"Reason"
      )
    }
    
    tagList(
      tags$h3(
        paste0("DEFINITION: ", as.character(
          SocialDeterminants[SocialDeterminants$Name == calc$determinant_choice,]$"Definitions")
        )),
      tags$h5(paste0("EXPLANATION: ",reason_text))
    )
  })
}


serv_out[["determinant_link"]] <- function(calc, session) {
  renderUI({
    tagList(tags$h5(
      "Text Source: ",
      tags$a(
        "County Health Rankings",
        href = calc$determinant.url(),
        style = "color: #00bfc4;",
        target="_blank"
      )
    )
    )
  })
}



serv_out[["determinant_original_source"]] <- function(calc, session) {
  renderUI({
    tagList(tags$h5(
      "Data Source: ",
      tags$a(
        calc$determinant.source(),
        href = calc$determinant.source_url(),
        style = "color: #00bfc4;",
        target="_blank"
      )
    )
    )
  })
}


# Mortality Rates Header (Page 2 lower middle)
serv_out[["textMortRates"]] <- function(calc, session) {
  renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(calc$state_choice == "United States") {
      location_str <- "the United States"
      tagList(tags$h3(
        title="This plot represents the distribution of midlife mortality rates (ages 25-64) for the selected state.",
        paste0(names(which(cause.list == calc$death_cause)), " Midlife Mortality Rates for ",
               location_str, " for ", calc$year_selector)
        # , icon("info-circle")
      ),
      tags$h6("The geographic distribution of midlife mortality rates (ages 25-64) for ",paste(location_str,".",sep = "")),
      NULL
      )
    }
    else {
      tagList(
        tags$h3(
          title="This plot represents the distribution of midlife mortality rates (ages 25-64) for the selected state.",
          # paste0("State View: ",names(which(cause.list == input$death_cause)), " Midlife Mortality Rates for ", names(which(state.list == input$state_choice))," for ",input$year_selector)
          paste0(names(which(cause.list == calc$death_cause)), " Midlife Mortality Rates for ", names(which(state.list == calc$state_choice))," for ",calc$year_selector)
        ),
        tags$h6("The geographic distribution of midlife mortality rates (ages 25-64) for ",paste(names(which(state.list == calc$state_choice)),".",sep="")),
        NULL
      )
    }
  })
}

# Death Trends Header (Page 2 lower middle)
serv_out[["textDeathTrends"]] <- function(calc, session) {
  renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(calc$state_choice == "United States") {
      location_str <- "the United States"
      tagList(
        tags$h3(
          title="This plot represents the average midlife death trends for each risk group. The blue line represents the national average.  Click on a map to see the line for a specific county. If a state has 6 or fewer counties, the average for each county is shown.",
          paste0(names(which(cause.list == calc$death_cause)), " Trends for Risk Groups across ", location_str)
          # , icon("info-circle")
        ),
        tags$h6("The average midlife death trends for each risk group conpared with the national average (in blue)."),
        NULL
      )
    }
    else {
      tagList(
        tags$h3(
          title="This plot represents the average midlife death trends for each risk group. The blue line represents the national average.  Click on a map to see the line for a specific county. If a state has 6 or fewer counties, the average for each county is shown.",
          paste0(names(which(cause.list == calc$death_cause)), " Trends for Risk Groups across ", names(which(state.list == calc$state_choice)))
          # , icon("info-circle")
        ),
        tags$h6("The average midlife death trends for each risk group compared with the national average (in blue). Click on any map to see the trend for a specific county."),
        NULL
      )
    }
  })
}


serv_out[["determinant_corr"]] <-function(calc, session) {
  renderText({
    if (nrow(calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]) == 0) {
      return("")
    }

    if (calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]$kendall_cor >= 0) {
      return(paste0("Kendall Correlation with ",
                    calc$death_cause,
                    " mortality: <span style=\"color:	#f8766d\"> <strong> ",
                    round(calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]$kendall_cor, 4),
                    "</strong> </span>"))
    }
    else {
      return(paste0("Kendal Correlation with ",
                    calc$death_cause,
                    " mortality: <span style=\"color: #00bfc4\"> <strong>",
                    round(calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]$kendall_cor, 4),
                    "</strong> </span>"))
    }
  })
}

serv_out[["determinant_dir"]] <-function(calc, session) {
  renderText({
    if (nrow(calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]) == 0) {
      return("")
    }
    
    if (calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]$kendall_p > .05) {
      return(paste0("<strong>No</strong> statistically significant ",
                    " relationship with mortality (p-value = ",
                    signif(calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]$kendall_p, 2),
                    ")"))
    }
    else if (calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]$kendall_cor >= 0) {
      return(paste0("Statistically significant <strong> <span style=\"color:	#f8766d\">",
                    tolower(as.character(calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]$DIR)),
                    "</span> </strong> relationship with mortality (p-value = ",
                    signif(calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]$kendall_p, 2),
                    ")"))
    }
    else {
      return(paste0("Statistically significant <strong> <span style=\"color:	#00bfc4\">",
                    tolower(as.character(calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]$DIR)),
                    "</span> </strong> relationship with mortality (p-value = ",
                    signif(calc$kendall.cor()[calc$kendall.cor()$chr_code == calc$determinant_choice,]$kendall_p, 2),
                    ")"))
    }
  })
}


# ----------------------------------------------------------------------
#   # Functions for data download
#   
# Outputs cdc.unimputed.data as a csv
serv_out[["downloadCDCData"]] <- function(calc, session) {
  downloadHandler(
    filename = function() {
      "cdc_data.csv"
    },
    content = function(file) {
      write.csv(cdc.unimputed.data, file, row.names = FALSE)
    }
  )
}

# Outputs chr.data.2019 as a csv
serv_out[["downloadCHRData"]] <- function(calc, session) {
  downloadHandler(
    filename = function() {
      "chr_data_2019.csv"
    },
    content = function(file) {
      write.csv(chr.data.2019, file, row.names = FALSE)
    }
  )
}

# Outputs chr.namemap.2019 as a csv
serv_out[["downloadFactorDesc"]] <- function(calc, session) {
  downloadHandler(
    filename = function() {
      "chr_data_desc.csv"
    },
    content = function(file) {
      write.csv(SocialDeterminants, file, row.names = FALSE)
    }
  )
}

# Outputs mort.cluster.ord as a csv
serv_out[["downloadClusters"]] <- function(calc, session) {
  downloadHandler(
    filename = function() {
      paste0(calc$state_choice, "_", calc$death_cause, "_clusters.csv")
    },
    content = function(file) {
      write.csv( mort.cluster.ord(), file, row.names = FALSE)
    }
  )
}

# Outputs mort.avg.cluster.ord as a csv
serv_out[["downloadClusterTime"]] <- function(calc, session) {
  downloadHandler(
    filename = function() {
      paste0(calc$state_choice, "_", calc$death_cause, "_clusters_time_series.csv")
    },
    content = function(file) {
      write.csv( mort.avg.cluster.ord(), file, row.names = FALSE)
    }
  )
}

# Outputs kendall.cor as a csv
serv_out[["downloadCorr"]] <- function(calc, session) {
  downloadHandler(
    filename = function() {
      paste0(calc$state_choice, "_", calc$death_cause, "_", calc$determinant_choice , "_correlations.csv")
    },
    content = function(file) {
      write.csv( kendall.cor(), file, row.names = FALSE)
    }
  )
}




mwsApp(ui_list, serv_calc, serv_out)
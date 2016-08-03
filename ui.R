library(shiny)

#shinyUI(pageWithSidebar(
shinyUI(navbarPage(strong("Severe Weather Events in the U.S."),
                   tabPanel("APPLICATION",
      # headerPanel("Severe Weather Events in the U.S."),
      sidebarPanel(
            checkboxInput("fin", "Financial Losses"),
            conditionalPanel(
                  condition = "input.fin == true",
                  radioButtons(inputId = "FinDamage", label = "Type of Damage",
                            choices = c("Crop" = "crop", "Property" = "prop", "Crop + Property" = "finagg"))
            ),
            checkboxInput("pop", "Population Affected"),
            conditionalPanel(
                  condition = "input.pop == true",
                  radioButtons(inputId = "PopAffected", label = "Type of Damage",
                               choices = c("Injuries" = "inj", "Fatalities" = "fat", "Injuries + Fatalities" = "popagg"))
            ),
            selectInput("Aggregation", "Level of Aggergation", c("National" = "national", "State" = "state")),
            conditionalPanel(
                  condition = "input.Aggregation == 'state'",
                   textInput(inputId = "state", label = "State",
                      placeholder = "Type in state abbreviation (e.g. CA)")
            ),
            selectInput("Peril", "Type of Peril",
                        c("All Perils"  = "all", "Flood" = "FLOOD", "Hail" = "HAIL", "Hurricane" = "HURRICANE",
                          "Tornado" = "TORNADO", "Storm Surge" = "STORM SURGE")),
            actionButton("submitButton", "Submit")
            
      ),
      mainPanel(
            h3('Geographical Aggregation'),
            textOutput('Aggregation'),
            h3('Financial Damage [USD] (Peril Selected)'),
            textOutput('FinDmg_peril'),
            textOutput('FinDmg'),
            h3('Population Affected (Peril Selected)'),
            textOutput('PopAff_peril'),
            textOutput('PopAff'),
            # h3('Peril Rank'),
            # textOutput('PerilRank'),
            conditionalPanel(
                  condition = "input.Aggregation == 'state'",
                  h3('State Rank (All perils)'),
                  h4('Financial Damage :: Population Affected'),
                  textOutput('StateRank')
            ),
            conditionalPanel(
                  condition = "input.Aggregation != 'state'",
                  h3("Financial Damage (All perils)"),
                  textOutput('txt_plot1'),
                  htmlOutput("plot1"),
                  h3("Population Affected (All perils)"),
                  textOutput('txt_plot2'),
                  htmlOutput("plot2")
            ),
            conditionalPanel(
                  condition = "input.Aggregation == 'state'",
                  h3("Financial Damage (All perils)"),
                  plotOutput("plot3"),
                  h3("Population Affected (All perils)"),
                  plotOutput("plot4")
            )
      )
                   ),
      tabPanel("DOCUMENTATION",
               
               h1("Open App"),
               p("Open the application by navigating to the APPLICATION panel. 
                 The following interface will welcome you."),
               img(src = "fig0.png", height = 200),
               
               
               h1("User Input"),
               p("Insert the user input in the left-side panel illustrated in the following figure"),
               strong("CLICK the Submit Button after input is inserted."), p(""),
               img(src = "fig1.png", height = 200),
               
               h2("Metrics"),
               p("Choose the metrics in which you would like to see the damage due to events, i.e."), 
               strong("(1) Financial loss"), p("for crops, property or combined; or"),
               strong("(2) Population affected"), p("in terms of injuries, fatalities or combined."),
               img(src = "fig2.png", height = 400),
               
               h2("Geographic Aggregation"),
               p("Choose the level of aggregation of results, i.e."), strong("(1) National"), p(""),  
               strong("(2) State"), p(""),
               img(src = "fig3.png", height = 100), p(""), 
               strong("NOTE: In the case of state, provide the state abbreviation capitalized"),
               img(src = "fig4.png", height = 150),
               
               h2("Type of Peril"),
               p("Select the type of peril from the drop-down list"),
               img(src = "fig5.png", height = 200),
               
               h1("Results"),
               p("Results are displayed in the right-hand panel:"),
               p("(1) upper part showing general information and number resutls, while"),
               p("(2) lower part showing plots of results of interest for ALL PERILS only."),
               
               h2("Example: Country-level Results"),
               h4("General information and values"),
               p("Financial damage and population affected are shown for the entire nation and the peril 
                              selected."),
               img(src = "res3.png", height = 200),
               h4("Figures"),
               p("The figures show interactive maps with the financial losses and population affected for
                              all states."),
               strong("NOTE: the plots are shown for all perils irrespective of the peril selected."), p(""),
               img(src = "res4.png", height = 400),
               h2("Example: State-level Results"),
               h4("General information and values"),
               p("Financial damage and population affected are shown for the state chosen and the peril 
                              selected. State rank shows the rank of the state chosen with respect to financial damage and 
                              population affected, among the other 49 states."),
               img(src = "res1.png", height = 200),
               h4("Figures"),
               p("The figures show bar charts of financial damage and population affected for the first 
                              three most-affected states and the state chosen."),
               strong("NOTE: the plots are shown for all perils irrespective of the peril selected."), p(""),
               img(src = "res2.png", height = 400)
               
               )
))
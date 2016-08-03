library(shiny)
suppressPackageStartupMessages(library(googleVis))

# load data
noaa <- read.csv("./data/StormData_subset.csv")
mapping <- read.csv("./data/StateMapping.csv", header = TRUE)

# clean data 
noaa_new <- noaa
noaa_new["dmg_prop"] <- 0
noaa_new["dmg_crop"] <- 0
noaa_new["prop_factor"] <- 1
noaa_new["crop_factor"] <- 1

noaa_new$prop_factor[noaa_new$PROPDMGEXP == "k" | noaa_new$PROPDMGEXP == "K"] = 1000
noaa_new$prop_factor[noaa_new$PROPDMGEXP == "m" | noaa_new$PROPDMGEXP == "M"] = 1000000
noaa_new$prop_factor[noaa_new$PROPDMGEXP == "b" | noaa_new$PROPDMGEXP == "B"] = 1000000000

noaa_new$crop_factor[noaa_new$CROPDMGEXP == "k" | noaa_new$CROPDMGEXP == "K"] = 1000
noaa_new$crop_factor[noaa_new$CROPDMGEXP == "m" | noaa_new$CROPDMGEXP == "M"] = 1000000
noaa_new$crop_factor[noaa_new$CROPDMGEXP == "b" | noaa_new$CROPDMGEXP == "B"] = 1000000000

noaa_new$dmg_prop <- noaa_new$prop_factor*noaa_new$PROPDMG
noaa_new$dmg_crop <- noaa_new$crop_factor*noaa_new$CROPDMG

# calulate damage by peril, by state
noaa_fat_state <- with(noaa_new, aggregate(FATALITIES, list(Peril = EVTYPE, State = STATE),
                                           FUN = "sum"))
noaa_inj_state <- with(noaa_new, aggregate(INJURIES, list(Peril = EVTYPE, State = STATE),
                                           FUN = "sum"))
noaa_crop_state <- with(noaa_new, aggregate(dmg_crop, list(Peril = EVTYPE, State = STATE),
                                            FUN = "sum"))
noaa_prop_state <- with(noaa_new, aggregate(dmg_prop, list(Peril = EVTYPE, State = STATE),
                                            FUN = "sum"))

noaa_fat <- with(noaa_new, aggregate(FATALITIES, list(Peril = EVTYPE),FUN = "sum"))
noaa_inj <- with(noaa_new, aggregate(INJURIES, list(Peril = EVTYPE),FUN = "sum"))
noaa_crop <- with(noaa_new, aggregate(dmg_crop, list(Peril = EVTYPE),FUN = "sum"))
noaa_prop <- with(noaa_new, aggregate(dmg_prop, list(Peril = EVTYPE),FUN = "sum"))

noaa_crop_map <- with(noaa_new, aggregate(dmg_crop, list(State = STATE), FUN = "sum"))
noaa_fat_map <- with(noaa_new, aggregate(FATALITIES, list(State = STATE), FUN = "sum"))
noaa_inj_map <- with(noaa_new, aggregate(INJURIES, list(State = STATE), FUN = "sum"))
noaa_prop_map <- with(noaa_new, aggregate(dmg_prop, list(State = STATE), FUN = "sum"))

crop_map <- merge(noaa_crop_map, mapping, by.x = "State", by.y = "Abbrev")
damage_map <- merge(noaa_prop_map, crop_map, by = "State") # x.y = crop; x.x. = prop 
damage_map["total"] <- damage_map$x.x + damage_map$x.y

inj_map <- merge(noaa_inj_map, mapping, by.x = "State", by.y = "Abbrev")
population_map <- merge(noaa_fat_map, inj_map, by = "State") # x.y = inj; x.x. = fat 
population_map["total"] <- population_map$x.x + population_map$x.y

colnames(damage_map) <- c("State","Property", "Crop", "Name", "Total_Loss")
colnames(population_map) <- c("State","Fatalities", "Injuries", "Name", "Total_Population")

damage_map_order <- damage_map[with(damage_map, 
                                    order(Total_Loss, decreasing = TRUE)), ]
population_map_order <- population_map[with(population_map, 
                                            order(Total_Population, decreasing = TRUE)), ]

dmg_matrix <- t(as.matrix(damage_map_order[,seq(1,3)]))
pop_matrix <- t(as.matrix(population_map_order[,seq(1,3)]))

shinyServer(
      function(input, output){
            # output$Documentation <- renderText({"sdadasd"})
            
            values <- reactiveValues(index = 0, dindex = 0, vect_index = c(0,0,0,0,0), vect_dindex = c(0,0,0,0,0))
            # values <- reactiveValues(dindex = 0)
            
            Rindex <- reactive({
                  if (input$Aggregation == 'state' & input$submitButton > 0){
                        values$index + which(population_map_order$State == input$state)
                  }
            })
            Rdindex <- reactive({
                  if (input$Aggregation == 'state' & input$submitButton > 0){
                        values$dindex + which(damage_map_order$State == input$state)
                  }
            })
                  
            output$StateRank <- renderText(
                  if (input$submitButton > 0){
                        paste0(as.character(isolate(Rdindex())),"  ::  ",as.character(isolate(Rindex())))
                        })
            
            output$Aggregation <- renderText({
                  if (input$submitButton > 0){
                  if (input$Aggregation == 'state') {
                        paste0("State: ",input$state)    
                  }
                  else {
                        "National"
                  }
                        
            }})
            
            output$FinDmg_peril <- renderText({
                  if (input$FinDamage == 'crop' & input$fin){
                        paste0("Crop financial losses from: ", input$Peril)
                  }
                  else if (input$FinDamage == 'prop' & input$fin){
                        paste0("Property financial losses from: ", input$Peril)
                  }
                  else if (input$FinDamage == 'finagg' & input$fin){
                        paste0("Crop and property financial losses from: ", input$Peril)
                  }
            })
            output$FinDmg <- renderText({
                  # State, Each Peril
                  if (input$Aggregation == 'state' & input$FinDamage == 'crop' & input$fin &
                      input$Peril != 'all' & input$submitButton > 0) {
                        as.character(noaa_crop_state$x[noaa_crop_state$State == input$state &
                                                             noaa_crop_state$Peril == input$Peril])
                  }
                  else if (input$Aggregation == 'state' & input$FinDamage == 'prop' & input$fin &
                           input$Peril != 'all' & input$submitButton > 0){
                        as.character(noaa_prop_state$x[noaa_prop_state$State == input$state &
                                                             noaa_prop_state$Peril == input$Peril])
                  }
                  else if (input$Aggregation == 'state' & input$FinDamage == 'finagg' & input$fin &
                           input$Peril != 'all' & input$submitButton > 0){
                        as.character(noaa_crop_state$x[noaa_crop_state$State == input$state &
                                                             noaa_crop_state$Peril == input$Peril]+
                                     noaa_prop_state$x[noaa_prop_state$State == input$state &
                                                                   noaa_prop_state$Peril == input$Peril])
                  }
                  # State, All Perils
                  else if (input$Aggregation == 'state' & input$FinDamage == 'crop' & input$fin &
                           input$Peril == 'all' & input$submitButton > 0) {
                        as.character(sum(noaa_crop_state$x[noaa_crop_state$State == input$state]))
                  }
                  else if (input$Aggregation == 'state' & input$FinDamage == 'prop' & input$fin &
                           input$Peril == 'all' & input$submitButton > 0){
                        as.character(sum(noaa_prop_state$x[noaa_prop_state$State == input$state]))
                  }
                  else if (input$Aggregation == 'state' & input$FinDamage == 'finagg' & input$fin &
                           input$Peril == 'all' & input$submitButton > 0){
                        as.character(sum(noaa_crop_state$x[noaa_crop_state$State == input$state])+
                                           sum(noaa_prop_state$x[noaa_prop_state$State == input$state]))
                  }
                  # National, Each Peril
                  else if (input$Aggregation == 'national' & input$FinDamage == 'crop' & input$fin &
                           input$Peril != 'all' & input$submitButton > 0) {
                        as.character(noaa_crop$x[noaa_crop$Peril == input$Peril])
                  }
                  else if (input$Aggregation == 'national' & input$FinDamage == 'prop' & input$fin &
                           input$Peril != 'all' & input$submitButton > 0){
                        as.character(noaa_prop$x[noaa_prop$Peril == input$Peril])
                  }
                  else if (input$Aggregation == 'national' & input$FinDamage == 'finagg' & input$fin &
                           input$Peril != 'all' & input$submitButton > 0){
                        as.character(noaa_crop$x[noaa_crop$Peril == input$Peril]+noaa_prop$x[noaa_prop$Peril == input$Peril])
                  }
                  # National, All Perils
                  else if (input$Aggregation == 'national' & input$FinDamage == 'crop' & input$fin &
                           input$Peril == 'all' & input$submitButton > 0) {
                        as.character(sum(noaa_crop$x))
                  }
                  else if (input$Aggregation == 'national' & input$FinDamage == 'prop' & input$fin &
                           input$Peril == 'all' & input$submitButton > 0){
                        as.character(sum(noaa_prop$x))
                  }
                  else if (input$Aggregation == 'national' & input$FinDamage == 'finagg' & input$fin &
                           input$Peril == 'all' & input$submitButton > 0){
                        as.character(sum(noaa_crop$x)+sum(noaa_prop$x))
                  }
            })
            
            output$PopAff_peril <- renderText({
                  if (input$PopAffected == 'inj' & input$pop){
                        paste0("Injuries from: ", input$Peril)
                  }
                  else if (input$PopAffected == 'prop' & input$pop){
                        paste0("Fatalities from: ", input$Peril)
                  }
                  else if (input$PopAffected == 'finagg' & input$pop){
                        paste0("Injuries and fatalities from: ", input$Peril)
                  }
            })
            output$PopAff <- renderText({
                  # State, Each Peril
                  if (input$Aggregation == 'state' & input$PopAffected == 'inj' & input$pop &
                      input$Peril != 'all' & input$submitButton > 0) {
                        as.character(noaa_inj_state$x[noaa_inj_state$State == input$state &
                                                             noaa_inj_state$Peril == input$Peril])
                  }
                  else if (input$Aggregation == 'state' & input$PopAffected == 'fat' & input$pop &
                           input$Peril != 'all' & input$submitButton > 0){
                        as.character(noaa_fat_state$x[noaa_fat_state$State == input$state &
                                                             noaa_fat_state$Peril == input$Peril])
                  }
                  else if (input$Aggregation == 'state' & input$PopAffected == 'popagg' & input$pop &
                           input$Peril != 'all' & input$submitButton > 0){
                        as.character(noaa_inj_state$x[noaa_inj_state$State == input$state &
                                                             noaa_inj_state$Peril == input$Peril]+
                                           noaa_fat_state$x[noaa_fat_state$State == input$state &
                                                                   noaa_fat_state$Peril == input$Peril])
                  }
                  # State, All Perils
                  else if (input$Aggregation == 'state' & input$PopAffected == 'inj' & input$pop &
                           input$Peril == 'all' & input$submitButton > 0) {
                        as.character(sum(noaa_inj_state$x[noaa_inj_state$State == input$state]))
                  }
                  else if (input$Aggregation == 'state' & input$PopAffected == 'fat' & input$pop &
                           input$Peril == 'all' & input$submitButton > 0){
                        as.character(sum(noaa_fat_state$x[noaa_fat_state$State == input$state]))
                  }
                  else if (input$Aggregation == 'state' & input$PopAffected == 'popagg' & input$pop &
                           input$Peril == 'all' & input$submitButton > 0){
                        as.character(sum(noaa_inj_state$x[noaa_inj_state$State == input$state])+
                                           sum(noaa_fat_state$x[noaa_fat_state$State == input$state]))
                  }
                  # National, Each Peril
                  else if (input$Aggregation == 'national' & input$PopAffected == 'inj' & input$pop &
                           input$Peril != 'all' & input$submitButton > 0) {
                        as.character(noaa_inj$x[noaa_inj$Peril == input$Peril])
                  }
                  else if (input$Aggregation == 'national' & input$PopAffected == 'fat' & input$pop &
                           input$Peril != 'all' & input$submitButton > 0){
                        as.character(noaa_fat$x[noaa_fat$Peril == input$Peril])
                  }
                  else if (input$Aggregation == 'national' & input$PopAffected == 'popagg' & input$pop &
                           input$Peril != 'all' & input$submitButton > 0){
                        as.character(noaa_inj$x[noaa_inj$Peril == input$Peril]+noaa_fat$x[noaa_fat$Peril == input$Peril])
                  }
                  # National, All Perils
                  else if (input$Aggregation == 'national' & input$PopAffected == 'inj' & input$pop &
                           input$Peril == 'all' & input$submitButton > 0) {
                        as.character(sum(noaa_inj$x))
                  }
                  else if (input$Aggregation == 'national' & input$PopAffected == 'fat' & input$pop &
                           input$Peril == 'all' & input$submitButton > 0){
                        as.character(sum(noaa_fat$x))
                  }
                  else if (input$Aggregation == 'national' & input$PopAffected == 'popagg' & input$pop &
                           input$Peril == 'all' & input$submitButton > 0){
                        as.character(sum(noaa_inj$x)+sum(noaa_fat$x))
                  }
            })
            output$txt_plot1 <- renderText({
                  if (input$submitButton > 0 & input$Aggregation == 'national' & input$fin){
                        if (input$FinDamage == 'crop'){
                              "Crop Financial Damage from All Perils"
                        }
                        else if (input$FinDamage == 'prop'){
                              "Property Financial Damage from All Perils"
                        }
                        else if (input$FinDamage == 'finagg'){
                              "Crop and Property Financial Damage from All Perils"
                        }
                  }
            })
            output$plot1 <- renderGvis({
                  if (input$submitButton > 0 & input$Aggregation == 'national' & input$FinDamage == 'crop' & input$fin){
                        # plot(pC)
                        gvisGeoChart(damage_map, "Name", "Crop",options=list(region="US", displayMode="regions", 
                                                                             resolution="provinces",
                                                                             width=600, height=400,colorAxis="{colors:['green', 'blue']}"))
                  }
                  else if (input$submitButton > 0 & input$Aggregation == 'national' & input$FinDamage == 'prop' & input$fin){
                        # plot(pP)
                        gvisGeoChart(damage_map, "Name", "Property",options=list(region="US", displayMode="regions", 
                                                                                 resolution="provinces",
                                                                                 width=600, height=400,colorAxis="{colors:['green', 'blue']}"))
                  }
                  else if (input$submitButton > 0 & input$Aggregation == 'national' & input$FinDamage == 'finagg' & input$fin){
                        # plot(pDT)
                        gvisGeoChart(damage_map, "Name", "Total_Loss",options=list(region="US", displayMode="regions", 
                                                                                   resolution="provinces",
                                                                                   width=600, height=400,colorAxis="{colors:['green', 'blue']}"))
                  }
            })
            output$txt_plot2 <- renderText({
                  if (input$submitButton > 0 & input$Aggregation == 'national' & input$fin){
                        if (input$PopAffected == 'inj'){
                              "Number of Injuries from All Perils"
                        }
                        else if (input$PopAffected == 'fat'){
                              "Number of Fatalities from All Perils"
                        }
                        else if (input$PopAffected == 'popagg'){
                              "Number of Injuries and Fatalities Combined from All Perils"
                        }
                  }
            })
            output$plot2 <- renderGvis({
                  if (input$submitButton > 0 & input$Aggregation == 'national' & input$PopAffected == 'inj' & input$pop){
                        # plot(pI)
                        gvisGeoChart(population_map, "Name", "Injuries",options=list(region="US", displayMode="regions", 
                                                                                     resolution="provinces",
                                                                                     width=600, height=400,colorAxis="{colors:['green', 'blue']}"))
                  }
                  else if (input$submitButton > 0 & input$Aggregation == 'national' & input$PopAffected == 'fat' & input$pop) {
                        # plot(pF)
                        gvisGeoChart(population_map, "Name", "Fatalities",options=list(region="US", displayMode="regions", 
                                                                                       resolution="provinces",
                                                                                       width=600, height=400,colorAxis="{colors:['green', 'blue']}"))
                  }
                  else if (input$submitButton > 0 & input$Aggregation == 'national' & input$PopAffected == 'popagg' & input$pop) {
                        # plot(pPT)
                        gvisGeoChart(population_map, "Name", "Total_Population",options=list(region="US", displayMode="regions", 
                                                                                             resolution="provinces",
                                                                                             width=600, height=400,colorAxis="{colors:['green', 'blue']}"))
                  }
            })
            output$plot3 <- renderPlot({
                  if (input$submitButton > 0 & input$Aggregation == 'state' & input$fin){
                        barplot(dmg_matrix[c(2,3),c(1,2,3,isolate(Rdindex()))],cex.names = 0.75, cex.axis = 0.75,
                                main="Selected State vs. 3-most Damaged States",
                                xlab="State", ylab="Financial Loss [USD]",
                                col=c("green","blue"),names.arg = dmg_matrix[1,c(1,2,3,isolate(Rdindex()))],
                                legend = rownames(dmg_matrix[c(2,3),]))
                  }
                              })
            output$plot4 <- renderPlot({
                  if (input$submitButton > 0 & input$Aggregation == 'state' & input$pop){
                        barplot(pop_matrix[c(2,3),c(1,2,3,isolate(Rindex()))],cex.names = 0.75, cex.axis = 0.75,
                                main="Selected State vs. 3-most Affected States",
                                xlab="State", ylab="Number of People Affected",
                                col=c("green","blue"),names.arg = pop_matrix[1,c(1,2,3,isolate(Rindex()))],
                                legend = rownames(pop_matrix[c(2,3),]))
                  }
            })
      })


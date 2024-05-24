library(shiny)
library(rhandsontable)
library(lubridate)
library(tidyverse)
library(stringr)

#####
#UI
#####
ui <- fluidPage(
  titlePanel("Daily Analysis"), 
  
  sidebarLayout(position = "left",
                sidebarPanel(fileInput('file1', 'Choose CSV File',
                                       accept=c('text/csv', 
                                                'text/comma-separated-values,text/plain', 
                                                '.csv')),
                             #sliderInput(inputId = "Volume_visit", "Volume Per Visit", min = 0, max = 100, value = 20), 
                             sliderInput(inputId = "Conc", "Concentration of Nectar", 
                                         min = 0, max = 100, value = 17), #this needs to be an input where the user can enter in the volume per visit
                             #sliderInput(inputId = "dur", "Min duration for poke", 
                              #           min=0, max=1000, value=200),
                             sliderInput(inputId = "durlong", "Min duration for visits considered to be 'long'", 
                                         min=2000, max=10000, value=3000),
                             sliderInput(inputId = "pumpconv", "Number of pump steps per microLitre of liquid", 
                                         min=0, max=0.5, value= 0.324)
                ),
                mainPanel(# Output 1
                  h3("Volumes and Energy Consumed"),
                  plotOutput("Rewards"),
                  # Output 2
                  h3("Total Volume Consumed (mL)"),
                  textOutput("Total_Volume"),
                  # Output 3
                  h3("Total visits to the flowers"),
                  plotOutput("Vis_flowers"),
                  # Output 4
                  h3("Proportion of long visits"),
                  rHandsontableOutput("Vis_long"),
                  # Output 5
                  h3("Visit durations"),
                  plotOutput("Vis_dur"),
                  #Output 6
                  h3("Proportion of wrongly unrewarded visits"),
                  rHandsontableOutput("Wrong_unrew"),
                  # Output 7
                  h3("Duration of light-barrier events"),
                  plotOutput("Light_bar"),
                  #Output 8
                  h3("Proportion of total events that were just light-barrier events"),
                  rHandsontableOutput("Light_bar_prop"),
                  #Output 9
                  h3("Errors in the light-barrier event durations"),
                  rHandsontableOutput("Light_errors"),
                  # Output 10
                  h3("Pump Refills"),
                  rHandsontableOutput("refills"),
                  # Output 11 
                  h3("Choice behaviour in the training phase"), 
                  plotOutput("Training"), 
                  # Output 12
                  h3("Overview of the main experimental phase"),
                  plotOutput("Mainoverview"), 
                  # Output 13
                  h3("Number of expected blocks vs observed blocks"), 
                  plotOutput("Mainblocks"), 
                  # Output 14
                  h3("Proportion of blocks that are not 10 minutes long"), 
                  rHandsontableOutput("Mainblockdurations"), 
                  # Output 15 
                  h3("Blocks longer than 600 seconds"), 
                  rHandsontableOutput("Mainlongblocks"), 
                  # Output 16
                  h3("Alternation of rewarding blocks"), 
                  plotOutput("Alternation"), 
                  # Output 17
                  h3("Proportion of all blocks that were improperly rewarded"), 
                  rHandsontableOutput("Mainallblockrewards"), 
                  # Output 18
                  h3("Proportion of proper blocks that were improperly rewarded"), 
                  rHandsontableOutput("Mainproperblockrewards")
                )
  )
)

######
#Server
######
server <- function(input, output) {
  
  dataplus <- reactive({
    
  inFile <- input$file1
    if (is.null(inFile))
      return(NULL)

#####
#Selection and Preparation of the CSV file for Analysis
#####
#Selection of data table
  #read csv file with raw data
  data_table <- read.csv2(inFile$datapath, sep = ";", dec = ".", header = TRUE,
                          fileEncoding = "UTF-16LE", as.is = TRUE, row.names = NULL) 
  
  # setwd("/Users/shambhavi/Google Drive/Experiments & Data/Timing - Berlin_2021 (backup)/Main/raw_data")
  # getwd()
  # #
  # # setting the folder
  # data_table <- read.csv2("Timing_complete-21.11.28.csv", sep = ";", dec = ".", header = TRUE,
  #                         fileEncoding = "UTF-16LE", as.is = TRUE, row.names = NULL)

   mydata <- data_table
  # getting the experiment data 
  firstrow <- max(which(data_table$SystemMsg == "start")) + 1 
  lastrow <- nrow(data_table) 
  # selecting the experiment data from the bats 
  data_table <- data_table %>% 
    slice(firstrow:lastrow) 
  
  # substituting . with ,
  data_table$DateTime <- sub(",", ".", data_table$DateTime)
  
  # converting DateTime to the correct format
  data_table$DateTime <- as.POSIXct(as.numeric(data_table$DateTime) * (60 * 60 * 24),
                                    origin = "1899-12-30", tz = "GMT")
  
  # sorting visits chronologically 
  data_table <- data_table %>% 
    arrange (DateTime) %>% 
    filter(str_detect(unitLabel, "Test", negate = TRUE))
  
  })
  output$table <- renderTable({
    dataplus()
  })
  
#####
#Calculation of calories consumed and volume of reward consumed
#####
#Note the rewarded visits
output$Rewards <- renderPlot({
  mydata <- dataplus()
#Calculation of total volumes consumed
  Rewards <- mydata %>% 
    #Remove those visits made by a wand
    filter(str_detect(IdLabel, "Bat"), 
           #getting only the visits that were rewarded with some nectar
           !is.na(reinforce1value), 
           # removing the negative sense1durations
           sense1duration > 0) %>% 
    group_by(IdLabel) %>% 
    summarise(reward_vol = sum(reinforce1value)*input$pumpconv) %>% 
    #calculating the number of kJ consumed
    mutate(kJ = 15.96/1000000*reward_vol*(0.05298*input$Conc^2+9.56955*input$Conc+3.32727), 
         reward_vol = reward_vol/1000)

  # Output 1
  Rewards %>% 
  ggplot(aes(IdLabel, kJ, color = IdLabel, label = reward_vol)) + 
    geom_hline(yintercept = 25, linetype = 2) +
    geom_text() + 
    xlab("Bat")
})
  
output$Total_Volume <- renderText({
  mydata <- dataplus()
  # Output 2
  Total_Volume <- mydata %>% 
    #Remove those visits made by a wand
    filter(str_detect(IdLabel, "Bat"), 
           # getting the reward values 
           !is.na(reinforce1value), 
           # removing the negative sense1durations
           sense1duration > 0) %>%
    group_by(IdLabel) %>% 
    # calculating the total amount 
    summarise(reward_vol = sum(reinforce1value)*input$pumpconv) %>% 
    ungroup()
  
  Total_Volume <- sum(Total_Volume$reward_vol)/1000
  })

#####
#Calculation of Visit Parameters 
#####

#calculating total number of visits

output$Vis_flowers <- renderPlot({
  mydata <- dataplus()
  Vis_flowers <- mydata %>% 
    # filtering out the bats
    filter(str_detect(IdLabel, "Bat"), 
           # getting the reward values 
           !is.na(reinforce1value),
           eventDuration < 60000, 
           # removing the negative sense1durations
           sense1duration > 0, 
           str_detect(unitLabel,"Cond")) %>% 
    #Selecting the visits to the flowers
    select(IdLabel, unitLabel) %>% 
    mutate(Flower = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>%
    group_by(IdLabel, Flower) %>% 
    summarise(Visits = n())
  
  #final table for visit parameters
  colnames(Vis_flowers) <- c("IdLabel", "Flower", "Total_Visits") 
  
  # Output 3
  # plotting it out
  Vis_flowers %>% 
  ggplot(aes(Flower, Total_Visits, label = Total_Visits, fill = IdLabel)) + 
    geom_bar(stat = "identity", color = "black") + 
    geom_text(vjust = -0.5) +
    ylab("Number of visits") + 
    scale_x_continuous(breaks = seq(0,12,1)) +
    theme_bw()
})

output$Vis_long <- renderRHandsontable ({
  mydata <- dataplus()
  Vis_long <- mydata %>% 
    filter(str_detect(IdLabel, "Bat"), 
           # getting the reward values
           !is.na(reinforce1value), 
           eventDuration < 60000, 
           # removing the negative sense1durations
           sense1duration > 0, 
           str_detect(unitLabel,"Cond")) %>% 
    select (IdLabel, unitLabel, eventDuration) %>% #Selecting the visits to the flowers
    mutate(Flower = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>%
    group_by(Flower) %>% 
    # summarise(Visits = n()) %>% 
    select(IdLabel, unitLabel, eventDuration, Flower) %>% 
    mutate(Cage_number = case_when(Flower == 1 | Flower == 2 ~ 1, 
                                   Flower == 3 | Flower == 4 ~ 2, 
                                   Flower == 5 | Flower == 6 ~ 3, 
                                   Flower == 7 | Flower == 8 ~ 4, 
                                   Flower == 9 | Flower == 10 ~ 5, 
                                   Flower == 11 | Flower == 12 ~ 6), 
           is.long = ifelse(eventDuration > 3000, 1, 0)) %>% 
    group_by(Cage_number, IdLabel) %>% 
  #calculating proportion of long visits and the length of the longest visit
    summarise(proplong = sum(is.long)/n(), 
              maxduration = max(eventDuration)/1000, 
              eventDuration = n())
  
  names(Vis_long)[5]<-"Total_Visits"
  names(Vis_long)[1]<-"Cage Number" 
  
  #final table for visit parameters
  Vis_long <- Vis_long[,c(2,1,5,3,4)]
  
  # Output 4
  # make the table
  rhandsontable(Vis_long, rowHeaders = NULL) %>% 
    hot_heatmap()
})

output$Vis_dur <- renderPlot({
  mydata <- dataplus()
  Vis_dur <- mydata %>% 
    mutate(sense1duration = ifelse(str_detect(unitLabel, "RFID"), 0, sense1duration)) %>%  
    filter(str_detect(unitLabel, "CondMod") |
             str_detect(unitLabel, "RFID"), 
           # removing the negative sense1durations
            sense1duration >= 0) %>%
    mutate(event = case_when(str_detect(unitLabel, "CondMod") ~ "CondMod", 
                             str_detect(unitLabel, "RFID") ~ "RFID"), 
           event = as.factor(event), 
           eventDuration = eventDuration/1000) 
  
 # Output 5 
  Vis_dur %>% 
    ggplot(aes(as.numeric(eventDuration))) +
    geom_density(aes(color = event)) +
    facet_wrap( IdLabel ~., scales = 'free') +
    scale_x_log10() +
    theme_bw() +
    xlab("Visit durations [s]")
})

#Calculating the number of unrewarded visits
output$Wrong_unrew <- renderRHandsontable({
  mydata <- dataplus()
  Wrong_unrew <- mydata %>%
    filter(str_detect(unitLabel, "CondMod"), 
           str_detect(IdLabel, "Bat"),
           # removing the negative sense1durations
           sense1duration > 0, 
           SystemMsg != "inactive") %>% 
    mutate(correct = ifelse(outLabel == "positive", 1, 0), 
           Flower = as.integer(str_extract(unitLabel, "[0-9]+"))) %>%
    ungroup() %>% 
    group_by(IdLabel, Flower) %>% 
    summarise(prop_unrew = 1 - mean(correct))
  
  colnames(Wrong_unrew) <- c("Bat", "Flower number", "Proportion of wrongly unrewarded visits")
  
  #Output 6
  rhandsontable(Wrong_unrew, rowHeaders = NULL) %>% 
  hot_heatmap()
  
})

#####
#Light-barrier events and errors
#####

output$Light_bar <- renderPlot({
  mydata <- dataplus()
  
  Light_bar <- mydata %>% 
    filter(# removing the negative sense1durations
      sense1duration > 0) %>% 
    mutate(light_event = ifelse(str_detect(unitLabel, "P"), 1, 0), 
           Flower = as.integer(str_extract(unitLabel, "[0-9]+")), 
           Flower = as.character(Flower), 
           Cage_number = as.character(case_when(Flower == 1 | Flower == 2 ~ 1, 
                                                Flower == 3 | Flower == 4 ~ 2, 
                                                Flower == 5 | Flower == 6 ~ 3, 
                                                Flower == 7 | Flower == 8 ~ 4, 
                                                Flower == 9 | Flower == 10 ~ 5, 
                                                Flower == 11 | Flower == 12 ~ 6)), 
           sense1duration = as.numeric(sense1duration)/1000)
  # Output 7  
  Light_bar %>% 
    filter(light_event == 1) %>% 
    ggplot() +
    geom_jitter(aes(x = Cage_number, y = sense1duration, color = Flower)) + 
    xlab("Cage number") + 
    ylab("Duration of the light-barrier event [s]") + 
    theme_bw()
  
})

output$Light_bar_prop <- renderRHandsontable({
  
  mydata <- dataplus()
  
  Light_bar_prop <- mydata %>% 
    filter(# removing the negative sense1durations
      sense1duration > 0) %>% 
    mutate(light_event = ifelse(str_detect(unitLabel, "P"), 1, 0), 
           Flower = as.integer(str_extract(unitLabel, "[0-9]+")), 
           Flower = as.character(Flower), 
           Cage_number = as.character(case_when(Flower == 1 | Flower == 2 ~ 1, 
                                                Flower == 3 | Flower == 4 ~ 2, 
                                                Flower == 5 | Flower == 6 ~ 3, 
                                                Flower == 7 | Flower == 8 ~ 4, 
                                                Flower == 9 | Flower == 10 ~ 5, 
                                                Flower == 11 | Flower == 12 ~ 6)), 
           sense1duration = as.numeric(sense1duration), 
           sense1duration = ifelse(!is.na(sense1duration), sense1duration/1000, sense1duration)) %>%
    group_by(Cage_number, Flower) %>% 
    summarise(prop_lightevents = (mean(light_event))) %>% 
    filter(!is.na(Cage_number))
  
  colnames(Light_bar_prop) <- c("Cage number", "Flower number", "Proportion of LS events out of all events")
  
  #Output 8
  rhandsontable(Light_bar_prop, rowHeaders = NULL) %>% 
    hot_heatmap()
  
})


output$Light_errors <- renderRHandsontable({
  
  mydata <- dataplus()
  
  Light_errors <- mydata %>% 
    mutate(sense1duration = replace_na(sense1duration, 0),
           Flower = as.integer(str_extract(unitLabel, "[0-9]+")), 
           Flower = as.character(Flower), 
           Cage_number = as.character(case_when(Flower == 1 | Flower == 2 ~ 1, 
                                                Flower == 3 | Flower == 4 ~ 2, 
                                                Flower == 5 | Flower == 6 ~ 3, 
                                                Flower == 7 | Flower == 8 ~ 4, 
                                                Flower == 9 | Flower == 10 ~ 5, 
                                                Flower == 11 | Flower == 12 ~ 6)), 
           sense1duration = as.numeric(sense1duration)) %>% 
    filter(sense1duration < 0) %>% 
    select(DateTime, Cage_number, Flower, sense1duration)
  
  
  colnames(Light_errors) <- c("Time stamp", "Cage number", "Flower number", "Wrong sense1duration")
  
  #Output 9
  rhandsontable(Light_errors, rowHeaders = NULL) %>% 
    hot_heatmap()
})  


#####
#Number of Refill Events
#####

#Output 9
output$refills <- renderRHandsontable({
  mydata <- dataplus()
  refills <- mydata %>% 
    filter(str_detect(SystemMsg, "start pump")) %>% 
    summarise(Events = n())
  
  rhandsontable(refills, rowHeaders = NULL) %>% 
    hot_heatmap()
})

#####
#Training phase
#####

output$Training <- renderPlot ({
  mydata <- dataplus()
  Training <- mydata %>% 
    filter(str_detect(IdLabel, "Bat"), 
           #Insert variable for reinforcement
           !is.na(reinforce1value)) %>% 
    mutate(Exp_stage = ifelse(str_detect(outFuncLabel, "Initial|Forced|Free"), "Training", "Main")) %>% 
    filter(Exp_stage == "Training")
  
  if (dim(Training)[1] != 0) {
    Training_counts <- Training %>%
      mutate(
        Flower = as.numeric(str_extract(unitLabel, "[0-9]+")), # making a column for flower number
        Phase = str_remove_all(outFuncLabel, c("out1|out2")),  # making a column for experiment phase
        # making phase a factor
        vis_vol = round(reinforce1value * input$pumpconv, digits = 0), # making a column for the reward volume
        Flower = as.character(Flower)
      ) %>%
      group_by(IdLabel) %>%
      mutate(count = n()) %>%
      #filter(count > 250) %>% # filtering out those bats that did not complete the training day
      select(-count) %>%
      filter(outLabel == "positive") %>%
      mutate(phase_count = ifelse(Phase != lag(Phase), 1, 0), 
             phase_count = replace_na(phase_count, 0), 
             phase_count = cumsum(phase_count)) %>%  
      #Phase = factor(Phase, levels = c("Initial", "Forced1", "Free1", "Forced2", "Free2"))) %>%
      group_by(IdLabel, Phase, phase_count, Flower, vis_vol) %>%
      summarise(unitLabel = n()) %>% 
      filter(!is.na(Phase))
    
    # creating a look-up table to properly number the phases - can probably be done in a cleaner way
    
    phase_numbers <- data.frame(phase_count = seq(0, 16), 
                                ind_phase_count = c(0, rep(1:4, each = 4)))
    
    Training_counts <- left_join(Training_counts, phase_numbers, by = "phase_count") %>% 
      unite ("Phase", c(2,7), sep = " - ") %>% 
      mutate(Phase = factor(Phase, levels = c("Initial - 0", "Forced1 - 1", "Free1 - 1", "Forced2 - 1", "Free2 - 1", 
                                              "Forced1 - 2", "Free1 - 2", "Forced2 - 2", "Free2 - 2", 
                                              "Forced1 - 3", "Free1 - 3", "Forced2 - 3", "Free2 - 3", 
                                              "Forced1 - 4", "Free1 - 4", "Forced2 - 4", "Free2 - 4")))
    
    Viscount <- Training_counts %>% 
      group_by(IdLabel) %>%
      summarise(sumvis = sum(unitLabel)) %>%
      ungroup() %>%
      summarise(meanvis = mean(sumvis), 
                sdvis = sd(sumvis)) 
    Meanvis <- Viscount %>% pull(meanvis)
    Sdvis <- Viscount %>% pull(sdvis)
    rm(Viscount)
    
    # Output 11
    Training_counts %>% 
      filter(str_detect(Phase, "Forced", negate = TRUE)) %>% 
      ggplot() + 
      geom_bar(
        aes(x = as.factor(Phase), y = unitLabel, fill = as.factor(vis_vol), group = Flower), 
        stat='identity', position = 'dodge'
      ) +
      scale_fill_manual(values = c("skyblue","royalblue", "blue")) +
      labs(fill = "Volume of reward") + 
      geom_text(
        aes(x = as.factor(Phase), y = unitLabel, label = unitLabel, group = Flower, fontface = "bold"),
        position = position_dodge(width = 1),
        vjust = -0.6, size = 2
      ) +
      facet_grid (IdLabel~., scales = "free_x") +
      theme_bw() +
      scale_y_continuous(limits = c(0, 55)) +
      xlab("Experiment Phase") + 
      ylab("Number of Visits") +
      theme(axis.title = element_text(size = 10), 
            axis.text.x = element_text(angle = 45, size = 8, vjust = 1.1, hjust = 1), 
            strip.text.x = element_text(size = 9, margin = margin(0.08,0,0.08,0, "cm")), 
            legend.title = element_text(size=10),
            legend.text = element_text(size=10), 
            legend.position = "bottom")
    
    
  } else {
    text = paste("There is no animal in the training phase")
    ggplot() + 
      annotate("text", x = 4, y = 25, size=8, label = text) + 
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }
  
})

#####
# Overview of main experimental phase 
#####

output$Mainoverview <- renderPlot ({
  mydata <- dataplus()
  #mydata <- data_table
  # creating a look-up table to sort out which animals are in the main experimental phase 
  stages <- mydata %>% 
    filter(SystemMsg == "start block nbr") %>% 
    select(IdLabel, SystemMsg) %>% 
    distinct() %>% 
    mutate(main = ifelse(SystemMsg == "start block nbr", 1, 0)) %>% 
    filter(main == 1) %>% 
    select(-SystemMsg)
  
  Main <- left_join(mydata, stages, by = "IdLabel") %>% 
    filter(main == 1) %>% 
    select(-main)
  
  if (dim(Main)[1] != 0) {
    
  mydata <- dataplus()
  #mydata <- data_table
  blocks <- Main %>% 
    # filter just the bats in the lockdown stage
    #filter(Substage == "Lockdown") %>% 
    group_by(IdLabel) %>% 
    # adding a column for the block number
    mutate(block_num = ifelse(SystemMsg == "start block nbr", MsgValue1, NA)) %>% 
    fill(block_num) %>% 
    # correcting it and adding 1
    mutate(block_num = as.numeric(block_num) + 1) %>% 
    group_by(IdLabel, block_num) %>% 
    # adding a column to note the occurrence of a lockdown
    mutate(lockdown = ifelse(SystemMsg == "block until the end", 1, 
                             ifelse(SystemMsg == "start block nbr", 0, NA))) %>% 
    fill(lockdown) %>% 
    ungroup() %>% 
    # grouping properly
    group_by(IdLabel, block_num) %>% 
    # calculating the time that has passed within a block for each one of the visits
    mutate(block_time = as.numeric(difftime(DateTime, min(DateTime), units = "secs"))) %>% 
    # removing the NAs
    filter(!is.na(block_num)) 
  
  # making a separate table to calculate the actual total duration of the blocks 
  block_dur <- blocks %>% 
    # figuring out the first occurrence of a new block, 
    # even one where the bats didn't make a visit
    mutate(event_num = 1:n()) %>% 
    filter(event_num == 1) %>% 
    select(DateTime, IdLabel, block_num, event_num) %>% 
    filter(!is.na(block_num)) %>% 
    ungroup() %>% 
    group_by(IdLabel) %>% 
    # calculating the time difference between the various blocks in seconds, 
    # the actual block durations
    mutate(block_dur = as.numeric(difftime(lead(DateTime), DateTime, units = "secs"))) %>% 
    select(IdLabel, block_num, block_dur) %>%
    filter(!is.na(block_dur))
  
  ld_order_sum <- blocks %>% 
    # making a column to note the start of the lockdown
    mutate(lockdown_start = ifelse(SystemMsg == "block until the end", block_time, 0)) %>% 
    select(IdLabel, block_num, lockdown, lockdown_start, lockdown_start) %>% 
    filter(!is.na(block_num)) %>%
    mutate(block_num = as.numeric(block_num)) %>% 
    distinct() %>% 
    group_by(IdLabel, block_num) %>% 
    # summarising so that there is only one row per block and that row notes whether 
    # a lockdown has occurred and when that lockdown started
    summarise(lockdown = sum(lockdown), 
              lockdown_start = sum(lockdown_start))
  
  last_vis_block <- blocks %>% 
    group_by(IdLabel, block_num) %>% 
    filter(str_detect(unitLabel, "Cond") & !is.na(reinforce1value)) %>% 
    mutate(rewarded_vis = 1:n()) %>% 
    filter(rewarded_vis == max(rewarded_vis), 
           # marking the last rewarded visit that happens only after 301 seconds 
           # to allow for visits immediately before the start of the non-rewarding phase
           block_time > 301) %>% 
    select(IdLabel, block_num, block_time) 
  
  # now putting the tables together so all the information is in one table
  
  lockdown_blocks <- left_join(ld_order_sum, block_dur, by = c("IdLabel", "block_num")) %>% 
    mutate(lockdown_dur = ifelse(lockdown_start > 0, block_dur - lockdown_start, 0))
  
  lockdown_blocks <- left_join(lockdown_blocks, last_vis_block, by = c("IdLabel", "block_num")) %>% 
    rename(last_rew_vis = block_time) %>% 
    mutate(last_rew_vis = replace_na(last_rew_vis, 0))

# couple of sanity checks 
# check1 <- lockdown_blocks %>% 
#   filter(lockdown_dur < 0)
# check2 <- lockdown_blocks %>% 
#   filter(lockdown_start == block_dur)

# plotting it out 

# Output 12

  lockdown_blocks %>% 
    # flipping the lockdown start and block duration so that the colouring is 
    # unambiguous
    mutate(helper = lockdown_start, 
           lockdown_start = ifelse(lockdown_start > 0, block_dur, lockdown_start), 
           block_dur = ifelse(lockdown_start > 0, helper, block_dur)) %>% 
    select(-helper) %>% 
    ggplot() + 
    geom_col(aes(block_num, lockdown_start),  fill = "blue") + 
    geom_col(aes(block_num, block_dur), fill = "pink") + 
    geom_col(aes(block_num, last_rew_vis), fill = "green") + 
    scale_x_continuous(breaks = seq(0, 80, by = 5)) +
    geom_hline(aes(yintercept = 150), linetype = "dotted") +
    geom_hline(aes(yintercept = 300), linetype = "dotted") + 
    geom_hline(aes(yintercept = 600), linetype = "dashed") + 
    facet_grid(IdLabel~., scales = "free") +
    xlab("Block number") + 
    ylab("Block and lockdown duration [s]") + 
    theme_bw()

} else {
  text = paste("There is no animal in the Main phase")
  ggplot() + 
    annotate("text", x = 4, y = 25, size=8, label = text) + 
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
}

})

######
#Number of blocks executed properly in the main experimental phase 
######
output$Mainblocks <- renderPlot ({
  mydata <- dataplus()
  stages <- mydata %>% 
    filter(SystemMsg == "start block nbr") %>% 
    select(IdLabel, SystemMsg) %>% 
    distinct() %>% 
    mutate(main = ifelse(SystemMsg == "start block nbr", 1, 0)) %>% 
    filter(main == 1) %>% 
    select(-SystemMsg)
  
  Main <- left_join(mydata, stages, by = "IdLabel") %>% 
    filter(main == 1) %>% 
    select(-main)
  
  if (dim(Main)[1] != 0) {
    
    mydata <- dataplus()
    Main <- mydata %>% 
      mutate(DateTime = parse_date_time(DateTime, orders = "YmdHMS"))
    
    ind_times <- Main %>%
      filter(SystemMsg == "start block nbr") %>%
      group_by(IdLabel) %>%
      summarise(start_time = min(DateTime),
                end_time = max(DateTime)) %>%
      select(IdLabel, start_time, end_time)
    
    alldays <- Main %>%
      left_join(ind_times) %>%
      group_by(IdLabel) %>%
      filter(str_detect(unitLabel, "Cond")) %>%
      mutate(difftime = DateTime - start_time,
             num_difftime = as.numeric(difftime)/60)
    observed_blocks <- Main %>%
      group_by(IdLabel) %>%
      filter(SystemMsg == "start block nbr") %>%
      summarise(observed_blocks = n())
  
  
  # Output 13
  
  ind_times %>%
    mutate(total_time = end_time - start_time,
           expected_blocks = ceiling(total_time / dminutes(10))) %>%
    left_join(observed_blocks) %>%
    ggplot(aes(expected_blocks, observed_blocks)) +
    geom_point() +
    geom_abline() + 
    xlab("Expected number of blocks") + 
    ylab("Observed number of blocks") + 
    theme_bw()
  
  } else {
    text = paste("There is no animal in the main experiment phase")
    ggplot() +
      annotate("text", x = 4, y = 25, size = 7, label = text) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }
})

######
#Duration of blocks in the main experimental phase
######
  
output$Mainblockdurations <- renderRHandsontable ({
  mydata <- dataplus()
  stages <- mydata %>% 
    filter(SystemMsg == "start block nbr") %>% 
    select(IdLabel, SystemMsg) %>% 
    distinct() %>% 
    mutate(main = ifelse(SystemMsg == "start block nbr", 1, 0)) %>% 
    filter(main == 1) %>% 
    select(-SystemMsg)
  
  Main <- left_join(mydata, stages, by = "IdLabel") %>% 
    filter(main == 1) %>% 
    select(-main)
  
  if (dim(Main)[1] != 0) {
    
    mydata <- dataplus()
    Main <- mydata
  # this shows the actual duration of the blocks
  blocks <- Main %>%
    mutate(DateTime = parse_date_time(DateTime, orders = "YmdHMS")) %>% 
    group_by(IdLabel) %>%
    filter(SystemMsg == "start block nbr") %>%
    mutate(block_timespan = lead(DateTime) - DateTime) %>%
    #expected 10 min, in reality close, but variable
    arrange(IdLabel)
  
  # observed lockdown numbers
  observed_lockdowns <- Main %>%
    group_by(IdLabel) %>%
    filter(SystemMsg == "block until the end") %>%
    summarise(observed_lockdowns = n())
  
  #expected lockdown number can be obtained from the parameter file
  # proportion of blocks that are not 10 min
  
  # Output 14
  
  prop_10 <- blocks %>%
    mutate(block_timespan = as.numeric(block_timespan)) %>%
    group_by(IdLabel) %>%
    filter(!is.na(block_timespan)) %>% 
    summarise(`Proportion of 10 min blocks` = sum(block_timespan == 10, na.rm = TRUE)/n(),
              `Proportion of 10 min ± 3 s blocks` = sum(between(block_timespan, 9.95, 10.05), na.rm = TRUE)/n())
  
  rhandsontable(prop_10, rowHeaders = NULL) %>% 
    hot_heatmap()
  
  } else {
    text = paste("There is no animal in the main experiment phase")
    ggplot() +
      annotate("text", x = 4, y = 25, size = 7, label = text) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }
  
})

######
# Long blocks 
######

output$Mainlongblocks <- renderRHandsontable ({
  mydata <- dataplus()
  stages <- mydata %>% 
    filter(SystemMsg == "start block nbr") %>% 
    select(IdLabel, SystemMsg) %>% 
    distinct() %>% 
    mutate(main = ifelse(SystemMsg == "start block nbr", 1, 0)) %>% 
    filter(main == 1) %>% 
    select(-SystemMsg)
  
  Main <- left_join(mydata, stages, by = "IdLabel") %>% 
    filter(main == 1) %>% 
    select(-main)
  
  if (dim(Main)[1] != 0) {
    
    long_blocks <- mydata %>% 
      # filter just the bats in the lockdown stage
      #filter(Substage == "Lockdown") %>% 
      group_by(IdLabel) %>% 
      # adding a column for the block number
      mutate(block_num = ifelse(SystemMsg == "start block nbr", MsgValue1, NA)) %>% 
      fill(block_num) %>% 
      # correcting it and adding 1
      mutate(block_num = as.numeric(block_num) + 1) %>% 
      group_by(IdLabel, block_num) %>% 
      # adding a column to note the occurrence of a lockdown
      mutate(lockdown = ifelse(SystemMsg == "block until the end", 1, 
                               ifelse(SystemMsg == "start block nbr", 0, NA))) %>% 
      fill(lockdown) %>% 
      ungroup() %>% 
      # grouping properly
      group_by(IdLabel, block_num) %>% 
      # calculating the time that has passed within a block for each one of the visits
      mutate(block_time = as.numeric(difftime(DateTime, min(DateTime), units = "secs"))) %>% 
      # removing the NAs
      filter(!is.na(block_num)) %>% 
      # figuring out the first occurrence of a new block, 
      # even one where the bats didn't make a visit
      mutate(event_num = 1:n()) %>% 
      filter(event_num == 1) %>% 
      select(DateTime, IdLabel, block_num, event_num) %>% 
      filter(!is.na(block_num)) %>% 
      ungroup() %>% 
      group_by(IdLabel) %>% 
      # calculating the time difference between the various blocks in seconds, 
      # the actual block durations
      mutate(block_dur = as.numeric(difftime(lead(DateTime), DateTime, units = "secs"))) %>% 
      select(IdLabel, block_num, block_dur) %>%
      filter(!is.na(block_dur), 
             block_dur > 600.5) %>% 
      arrange(IdLabel, block_num)
    
    if (dim(long_blocks)[1] != 0){
      
      # Output 15
      
      rhandsontable(long_blocks, rowHeaders = NULL) %>% 
        hot_heatmap()
      
    } else {
      text = paste("All the blocks are 600 ± 5 s long")
      ggplot() +
        annotate("text", x = 4, y = 25, size = 7, label = text) +
        theme_bw() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())
    }
    
  } else {
    text = paste("There is no animal in the main experiment phase")
    ggplot() +
      annotate("text", x = 4, y = 25, size = 7, label = text) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }
  
})

######
# Sequence of rewarding flowers 
######

output$Alternation <- renderPlot ({
  mydata <- dataplus()
  #mydata <- data_table
  stages <- mydata %>% 
    filter(SystemMsg == "start block nbr") %>% 
    select(IdLabel, SystemMsg) %>% 
    distinct() %>% 
    mutate(main = ifelse(SystemMsg == "start block nbr", 1, 0)) %>% 
    filter(main == 1) %>% 
    select(-SystemMsg)
  
  Main <- left_join(mydata, stages, by = "IdLabel") %>% 
    filter(main == 1) %>% 
    select(-main)
  
  if (dim(Main)[1] != 0) {
    
    mydata <- dataplus()
    
    Alternation <- mydata %>% 
      # filter just the bats in the lockdown stage
      #filter(Substage == "Lockdown") %>% 
      group_by(IdLabel) %>% 
      # adding a column for the block number
      mutate(block_num = ifelse(SystemMsg == "start block nbr", MsgValue1, NA)) %>% 
      fill(block_num) %>% 
      # correcting it and adding 1
      mutate(block_num = as.numeric(block_num) + 1) %>% 
      group_by(IdLabel, block_num) %>% 
      filter(str_detect(unitLabel, "CondMod"), 
             !is.na(block_num)) %>% 
      mutate(rewarded = ifelse(is.na(reinforce1value), "unrewarded", "rewarded"), 
             Flower = ifelse(as.numeric(str_extract(unitLabel, "[0-9]+")) %% 2 == 1, 
                      "odd", "even"), 
             block_time = as.numeric(difftime(DateTime, min(DateTime), units = "secs"))) %>% 
      filter(block_time <= 300) %>% 
      select(IdLabel, unitLabel, block_num, Flower, rewarded) %>% 
      filter(rewarded == "rewarded") %>% 
      distinct() %>% 
      mutate(block_even_odd = ifelse(block_num %% 2 == 1, "Odd-numbered block", "Even-numbered block"))
    
      # Output 16
      
    Alternation %>% 
      ggplot() + 
      geom_point(aes(block_num, Flower,, color = Flower, group = Flower), size = 0.5) + 
      scale_x_continuous(breaks = seq(0,72,10)) + 
      facet_grid(IdLabel~block_even_odd, scales = 'free') + 
      ylab("Rewarding flower") + 
      xlab("Block number") + 
      theme_bw() + 
      theme(legend.position = "none")
    
  } else {
    text = paste("There is no animal in the main experiment phase")
    ggplot() +
      annotate("text", x = 4, y = 25, size = 7, label = text) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }
  
})

######
#All blocks improperly rewarded in the main experimental phase
######

output$Mainallblockrewards <- renderRHandsontable ({
  mydata <- dataplus()
  stages <- mydata %>% 
    filter(SystemMsg == "start block nbr") %>% 
    select(IdLabel, SystemMsg) %>% 
    distinct() %>% 
    mutate(main = ifelse(SystemMsg == "start block nbr", 1, 0)) %>% 
    filter(main == 1) %>% 
    select(-SystemMsg)
  
  Main <- left_join(mydata, stages, by = "IdLabel") %>% 
    filter(main == 1) %>% 
    select(-main)
  
  if (dim(Main)[1] != 0) {
  
    mydata <- dataplus()
    Main <- mydata %>%
    group_by(IdLabel) %>%
    mutate(block = ifelse(SystemMsg == "start block nbr", 1, 0),
           DateTime = parse_date_time(DateTime, orders = "YmdHMS"), 
           start_time_block = if_else(block == 1, as.POSIXct(DateTime), NA_POSIXct_),
           block = cumsum(block),
           rewarded = ifelse(!is.na(reinforce1value), 1, 0),
           lockdown = ifelse(SystemMsg == "block until the end", 1, 0),
           lockdown = cumsum(lockdown)) %>%
    fill(start_time_block) %>%
    mutate(running_time_block = DateTime - start_time_block,
           is_halftime = ifelse(running_time_block > minutes(5), 1, 0))
    
    proper_blocks <- Main %>%
      mutate(DateTime = parse_date_time(DateTime, orders = "YmdHMS")) %>% 
      group_by(IdLabel) %>%
      filter(SystemMsg == "start block nbr") %>%
      mutate(block_timespan = lead(DateTime) - DateTime) %>%
      #expected 10 min, in reality close, but variable
      arrange(IdLabel) %>%
      mutate(block = as.numeric(MsgValue1) + 1,
             proper_block = ifelse(block_timespan == minutes(10), TRUE, FALSE)) %>%
      select(IdLabel, block, block_timespan, proper_block)
    
  # Output 17

  unproperly_rewarded <- Main %>%
    filter(is_halftime == 1, str_detect(unitLabel, "Cond")) %>%
    group_by(IdLabel) %>%
    summarise(prop_unproperly_rewarded = sum(rewarded == 1)/n()) %>%
    arrange(desc(prop_unproperly_rewarded)) %>%
    rename(`Proportion of all blocks rewarded improperly` = prop_unproperly_rewarded)

  rhandsontable(unproperly_rewarded, rowHeaders = NULL) %>%
    hot_heatmap()
  
  } else {
    text = paste("There is no animal in the main experiment phase")
    ggplot() +
      annotate("text", x = 4, y = 25, size = 7, label = text) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }

})

######
#Proper blocks improperly rewarded in the main experimental phase
######

output$Mainproperblockrewards <- renderRHandsontable ({
  mydata <- dataplus()
  stages <- mydata %>% 
    filter(SystemMsg == "start block nbr") %>% 
    select(IdLabel, SystemMsg) %>% 
    distinct() %>% 
    mutate(main = ifelse(SystemMsg == "start block nbr", 1, 0)) %>% 
    filter(main == 1) %>% 
    select(-SystemMsg)
  
  Main <- left_join(mydata, stages, by = "IdLabel") %>% 
    filter(main == 1) %>% 
    select(-main)
  
  if (dim(Main)[1] != 0) {
    
    mydata <- dataplus()
    Main <- mydata %>%
      group_by(IdLabel) %>%
      mutate(block = ifelse(SystemMsg == "start block nbr", 1, 0),
             DateTime = parse_date_time(DateTime, orders = "YmdHMS"), 
             start_time_block = if_else(block == 1, as.POSIXct(DateTime), NA_POSIXct_),
             block = cumsum(block),
             rewarded = ifelse(!is.na(reinforce1value), 1, 0),
             lockdown = ifelse(SystemMsg == "block until the end", 1, 0),
             lockdown = cumsum(lockdown)) %>%
      fill(start_time_block) %>%
      mutate(running_time_block = DateTime - start_time_block,
             is_halftime = ifelse(running_time_block > minutes(5), 1, 0))
    
    proper_blocks <- Main %>%
      mutate(DateTime = parse_date_time(DateTime, orders = "YmdHMS")) %>% 
      group_by(IdLabel) %>%
      filter(SystemMsg == "start block nbr") %>%
      mutate(block_timespan = lead(DateTime) - DateTime) %>%
      #expected 10 min, in reality close, but variable
      arrange(IdLabel) %>%
      mutate(block = as.numeric(MsgValue1) + 1,
             proper_block = ifelse(block_timespan == minutes(10), TRUE, FALSE)) %>%
      select(IdLabel, block, block_timespan, proper_block)
  
    # Output 17

    # filter for proper blocks first
    unproperly_rewarded_propblock <- Main %>%
      left_join(proper_blocks) %>%
      filter(proper_block == TRUE, is_halftime == 1, str_detect(unitLabel, "Cond")) %>%
      group_by(IdLabel) %>%
      summarise(prop_unproperly_rewarded = sum(rewarded == 1)/n()) %>%
      arrange(desc(prop_unproperly_rewarded)) %>%
      rename(`Proportion of proper blocks rewarded improperly` = prop_unproperly_rewarded)

    rhandsontable(unproperly_rewarded_propblock, rowHeaders = NULL) %>%
      hot_heatmap()
    
  } else {
    text = paste("There is no animal in the main experiment phase")
    ggplot() +
      annotate("text", x = 4, y = 25, size = 7, label = text) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }
  
})

}
shinyApp(ui = ui, server = server)
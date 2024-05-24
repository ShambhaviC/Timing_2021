# Timing_2021
Raw data, code and files related to the experiment done on whether bats use timing or another strategy to anticipate a future event

The folders "analysis/data/configuration_files" contain the Excel files that were used to execute the experimental program with the desired reward schedule using PhenoSoft Control.

The folders "analysis/data/raw_data" contain CSV files with raw data from the experimental runs of bats inside flight-cages with two artificial flowers each, i.e., nectar-dispensing devices. The files were produced by the software PhenoSoft Control. 

The folders "analysis/data/meta_data" contains the following input CSV files: "ConditionsTiming.csv" and "MasterTableTiming.csv". These files are necessary for the R scripts that stitch the raw data together and analyse them. 
  
The folder "analysis/data/processed_data" contains the following CSV files: "Training.csv" which is the set of data from the training part of the experiment from the PhenoSoft software combined with the meta data from the folder "analysis/data/meta_data" and "analysis/data/meta_data"; "Main.csv" which is the set of data from the main part of the experiment from the PhenoSoft software combined with the meta data from the folder "analysis/data/meta_data" and "analysis/data/meta_data"; "Pump_data.csv" which is the set of data about the start and end times of the pump-refilling activity, derived from the raw data in "Main.csv" and "Training.csv". 

The folder "analysis/R" contains the following R scripts: load.R whose outputs are the processed CSV files, saved in the folder "analysis/data/processed_data"; shiny_gen_timing.R which is a shiny app that was used for the daily analysis of the raw data during the experiments to ensure that the protocol had worked as intended and the bats had drunk enough nectar.

The folder "analysis/images" contains the following images, which are read into the RMarkdownfile: "operant_wall.png"; "no_access_1.png"; "no_access_2.png"; "baseline.png"

The RMarkdown file with the complete text of the chapter and the complete code for the analysis of the processed CSV files in the folder "analysis/data/processed_data" is found in the folder "analysis". 

## 1. Content of configuration files

These files were written to execute the experimental schedule for each day of the experiment in the software PhenoSoft Control. 

## 2. Content of raw files

|Column label |Type     |Description |
|-------------|---------|------------|
|DateTime     |-        |The astronomical date and time for each event of the experiment|
|IdRFID       |-        |RFID number of a single bat, place-holders here as the RFID devices were not used for this experiment|
|IdLabel			|-        |Short unique identifying label for each bat|
|unitLabel		|-        |Code identifying which reward-dispensing device ('flower') was activated during an event|
|             |CondMod  |Detections of both a transponder number and an infra-red beam interruption, identified as a nose-poke|
|             |pumpBeh  |Events relating to states of the syringe and its refilling algorithm|
|             |exp      |Events related to the programmed reward schedule, clarified in **SystemMsg**|
|eventDuration|-        |Duration of event in milliseconds|
|sense1duration|-       |Total duration of the infra-red beam interruption|
|sense1Events|-         |Number of interruptions of infra-red beam. When such events happen fast enough (less than 200ms apart) these are registered as a single event, but the number of such short interruptions is given here|
|senseRFIDrecords|-     |Number of times the transponder number was detected|
|reinforce1value|-		  |Reward (in pump step units, delivered by a stepper motor syringe pump filled with sugar-water)|
|reinforce1Total|-		  |Sum of duration of the infra-red beam interruption and RFID transponder detection|
|reinforce1Account|-		|Duration of the RFID transponder detection|
|outFuncLabel |-  	    |Label indicating which 'flower' delivered a reward in response to a nose-poke|
|outLabel     |-        |'positive' indicates the delivery of a reward|
|SystemMsg    |-        |Contains the volume of the fluctuating option at the time-points when the bats made visits to the fluctuating option, in units of pump steps|
|MsgValue1    |-        |Events in the experimental schedule|
|MsgValue2    |-        |The contents of this column are not relevant to this experiment|
|MSgValue3    |-        |The contents of this column are not relevant to this experiment|

## 3. Content of "ConditionsTiming.csv"

This file is user-generated, providing relevant experimental information not present in the raw files.

|Column label|	Description|
|------------|-------------|
|Day		     |Number of each day of each stage of the experiment|
|IdRFID		   |RFID number of a single bat, place-holders here as the RFID devices were not used for this experiment|
|IdLabel     |Short unique identifying label for each bat|
|Cage        |Flower number|
|Discard     |Binary value of 1 or 0 indicating whether the bat should be excluded from analysis|
|Stage       |Stage of the experiment, either Training or Main|
|Substage    |Substage of the experiment stage: Baseline or Experimental in the Main experiment|
|Notes       |Any additional notes|


## 4. Content of "MasterTableTiming.csv"

This file is user-generated and allows mapping the raw csv files to the respective experimental days.

|Column label|	Description|
|------------|-------------|
|Day         |Number of experimental day starting from the first day to the last sequentially|
|Path        |Path of the raw csv file corresponding to the day|
|Comments    |This column contains any relevant notes about the raw data file|

## 5. Content of "Training.csv" file

This file is the output of the load.R script which processes the folder of raw csv files, with further information supplied by "ConditionsSubjectiveMean", "MasterTableSubjectiveMean", "ConditionsObjectiveMean" and "MasterTableObjectiveMean" csv files. It contains the data from the training days of the experiment

|Column label |Type     |Description |
|-------------|---------|------------|
|Day		      |-        |Number of experimental day starting from the first day to the last sequentially|
|DateTime     |-        |The astronomical date and time for each event of the experiment|
|IdRFID       |-        |RFID number of a single bat, place-holders here as the RFID devices were not used for this experiment|
|IdLabel			|-        |Short unique identifying label for each bat|
|unitLabel		|-        |Code identifying which reward-dispensing device ('flower') was activated during an event|
|             |RFID     |Detections of a transponder number|
|             |CondMod  |Detections of both a transponder number and an infra-red beam interruption, identified as a nose-poke|
|             |pumpBeh  |Events relating to states of the syringe and its refilling algorithm|
|             |exp      |Events related to the programmed reward schedule, clarified in **SystemMsg**|
|eventDuration|-        |Duration of event in milliseconds|
|sense1duration|-       |Total duration of the infra-red beam interruption|
|reinforce1value|-		  |Reward (in pump step units, delivered by a stepper motor syringe pump filled with sugar-water)|
|reinforce1Account|-		|Duration of the RFID transponder detection|
|outFuncLabel |-  	    |Label indicating which 'flower' delivered a reward in response to a nose-poke|
|outLabel     |-        |'positive' indicates the delivery of a reward|
|SystemMsg    |-        |Contains the volume of the fluctuating option at the time-points when the bats made visits to the fluctuating option, in units of pump steps|
|MsgValue1    |-        |Events in the experimental schedule|
|IdLabel      |-        |Short unique identifying label for each bat|
|Cage         |-        ||
|IdLabel      |-        ||
|Discard      |-        ||
|Stage      |-        ||
|Substage     |-        ||
|Notes  |-        ||


## 6. Content of "Main.csv" file
This file is the output of the load.R script which processes the folder of raw csv files, with further information supplied by "ConditionsSubjectiveMean", "MasterTableSubjectiveMean", "ConditionsObjectiveMean" and "MasterTableObjectiveMean" csv files. It contains the data from the main experimental days of the experiment. 

|Column label |Type     |Description |
|-------------|---------|------------|
|Day		      |-        |Number of experimental day starting from the first day to the last sequentially|
|DateTime     |-        |The astronomical date and time for each event of the experiment|
|IdLabel      |-        |Short unique identifying label for each bat|
|unitLabel		|-        |Code identifying which reward-dispensing device ('flower') was activated during an event|
|eventDuration|-        |Duration of event in milliseconds|
|reinforce1value|-      |Duration of event in milliseconds|
|outFuncLabel|-         |Label indicating which 'flower' delivered a reward in response to a nose-poke|
|outLabel	   |-         |Contents of this column are irrelevant for this experiment|
|SystemMsg	 |-         |Contents of this column are irrelevant for this experiment|
|Cohortday   |-         |Number of experimental day starting from the first day to the last sequentially|
|Period      |-         |Period of the sine wave in hours|
|Amplitude   |-         |Amplitude of the sine wave in units of pump steps|
|Disp        |-         |Displacement above 0 of the sine wave output in units of pump steps|
|Rel         |-         |Binary 1 or 0 indicating whether a particular flower was responsive to a bat|
|Cond        |-         |This column indicated the stage of the experiment, Training or "Rateofchange"|
|Reversal    |-         |Binary 1 or 0 indicating whether a particular day was the first night or reversal night of a condition|
|Cohort      |-         |Number of each of cohort of 6 bats run together|
|choice      |-         |Binary TRUE or FALSE to indicate if the choice is a true choice|
|Experiment  |-         |This column indicates whether the bats experienced which level of the richness treatment|
|sine_steps  |-         |Number of pump steps of the fluctuating reward at each recorded time-point|
|sine_vol    |-         |Volume of the fluctuating reward in microLitres at each recorded time-point|
|vis_vol     |-         |Volume of the reward obtained by the bats in microLitres at each recorded time-point|
|timediff    |-         |Time elapsed in seconds since the first visit made for each of the bats|
|chosen      |-         |Binary column containing the choice made by the bats converted to the fixed output in microLitres and the peak of the fluctuating output in microLitres|
|Tracking    |-         |This column contains information about whether a bat was reversal responsive or not|


## 7. Content of "Pump_data.csv" file

|Column label |Type     |Description |
|-------------|---------|------------|
|Day		      |-        |Number of experimental day starting from the first day to the last sequentially|
|DateTime     |-        |The astronomical date and time for each event of the experiment|
|IdLabel      |-        |This column is irrelevant to this file|
|IdLabel      |-        |This column is irrelevant to this file|
|unitLabel		|-        |This column contains the label indicating that the pump was activated in this time|
|eventDuration|-        |This column is irrelevant to this file|
|reinforce1value|-      |This column is irrelevant to this file|
|outFuncLabel|-         |This column is irrelevant to this file|
|outLabel	   |-         |This column is irrelevant to this file|
|SystemMsg	 |-         |This column containst the label indicating whether the pump behaviour started or ended at this time point|
|MsgValue1   |-         |This column is irrelevant to this file|
|Loc         |-         |This column is irrelevant to this file|
|Cohortday   |-         |This column is irrelevant to this file|
|Period      |-         |This column is irrelevant to this file|
|Amplitude   |-         |This column is irrelevant to this file|
|Disp        |-         |This column is irrelevant to this file|
|Rel         |-         |This column is irrelevant to this file|
|Discard     |-         |This column is irrelevant to this file|
|Cond        |-         |This column is irrelevant to this file|
|Reversal    |-         |This column is irrelevant to this file|
|Cohort      |-         |This column is irrelevant to this file|
|Cohort      |-         |This column is irrelevant to this file|
|choice      |-         |This column indicates that these events were not choices made by the bats|
|Experiment  |-         |This column indicates whether the bats experienced which level of the richness treatment|

For further information contact: shambhavic21@gmail.com

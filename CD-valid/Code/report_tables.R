
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(haven)))
suppressWarnings(suppressMessages(library(caret)))
suppressWarnings(suppressMessages(library(rio)))
suppressWarnings(suppressMessages(library(htmltools)))
suppressWarnings(library(DT))
suppressWarnings(library(crosstalk))
suppressWarnings(library(stringr))
suppressWarnings(suppressMessages(library(parameters)))


source("settings.R")


pretest<- import_list(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Pretest/", 
                             "Belgium", 
                             "/", 
                             "Belgium", 
                             ".xlsx"))
df<- import_list(paste0(path2eu, "/EU-S Data/eu-data-validation/CD-valid/Outcomes/Full Fieldwork/", 
                        "Belgium", 
                        "/", 
                        "Belgium", 
                        ".xlsx"))


#Joining pretest flag column and separating expert and population TPS
pre_thr<- pretest$tps_comparisson%>%
  select(GPP_Variable_Name, TPS_Variable_Name, Flag)%>%
  rename("Pretest Flag" = "Flag")
pre_thr$`Pretest Flag`<- ifelse(pre_thr$`Pretest Flag` == "green", "Green", ifelse(pre_thr$`Pretest Flag` == "yellow", "Yellow", "Red"))

pre_time<- pretest$time_changes%>%
  filter(warning != "Not enough info")%>%
  select(variable, warning)%>%
  rename("Pretest Flag" = "warning")
pre_time$`Pretest Flag`<- ifelse(pre_time$`Pretest Flag` == "Green light", "Green", ifelse(pre_time$`Pretest Flag` == "Yellow light", "Yellow", "Red"))

threshold1<- suppressMessages(suppressWarnings(left_join(df$tps_comparisson, pre_thr)))%>% distinct()
threshold1$`Pretest Flag`<- ifelse(is.na(threshold1$`Pretest Flag`), "Not Evaluated", threshold1$`Pretest Flag`)
time<- suppressMessages(suppressWarnings(left_join(df$time_changes%>%
                                                     filter(warning != "Not enough info"), pre_time)))%>% distinct()
time$`Pretest Flag`<- ifelse(is.na(time$`Pretest Flag`), "Not Evaluated", time$`Pretest Flag`)

experts<- threshold1%>%
  filter(Type_Survey == "expert")%>%
  drop_na()
population<- threshold1%>%
  filter(Type_Survey == "population")%>%
  drop_na()

# Threshold Analysis for population polls
popflags<- population%>%
  group_by(Pillar)%>%
  count(Flag)
popflagssub<- population%>%
  group_by(Sub_Pillar)%>%
  count(Flag)

#Threshold analysis for expert surveys
expflags<- experts%>%
  group_by(Pillar)%>%
  count(Flag)
expflagssub<- experts%>%
  group_by(Sub_Pillar)%>%
  count(Flag)

# Time change analysis
timeflags<- time%>%
  group_by(pillar)%>%
  count(warning)
timeflagssub<- time%>%
  group_by(subpillar)%>%
  count(warning)

pop_output<- popflags%>%
  pivot_wider(names_from = Flag, values_from = n)

pop_output[is.na(pop_output)] <- 0

if (!("red" %in% colnames(pop_output))){
  
  pop_output$red<- rep(0, nrow(pop_output))
  
} 


pop_output<- pop_output%>%
  mutate(sum = green + red)%>%
  mutate(perc = paste0(round(red/sum*100, digits = 1), "%"))

pop_output$Label<- ifelse(pop_output$red >= pop_output$sum/2, "Significantly Different", "Consistent")

pop_output<- pop_output%>%
  select(Pillar, green, red, perc, Label)

colnames(pop_output)<- c("Pillar", "Green Flags", "Red Flags", "Percent Red Flags", "Label")


dir<- population
dir$Direction<- ifelse(dir$GPP_datapoint-dir$TPS_datapoint>0, "Positive", ifelse(dir$GPP_datapoint-dir$TPS_datapoint<0, "Negative", "No Change"))

dir<- dir%>%
  group_by(Sub_Pillar, Direction, Flag)%>%
  summarise(c = n(), .groups = 'drop')

dirvec<- c()

for (p in sort(unique(population$Sub_Pillar))){
  
  pillar<- dir%>%
    filter(Sub_Pillar == p)
  
  r<- pillar%>%
    filter(Flag == "red")
  rs<- sum(r$c)
  
  if (nrow(pillar)==1 & pillar$Flag[[1]] == "red"){
    d<- pillar$Direction[[1]]
    dirvec<- c(dirvec, paste0("Potential ", d,  " trend"))
    
  } else if (nrow(pillar)==1 & pillar$Flag[[1]] == "green"){
    dirvec<- c(dirvec, "No change")
    
  }else if (rs/sum(pillar$c)>=.75){
    
    if (nrow(r)== 1){
      d<- pillar$Direction[[1]]
      dirvec<- c(dirvec, paste0("Potential ", d,  " trend"))
      
    } else{
      dirvec<- c(dirvec, "Potential Mixed trends")
    }
    
  } else {
    dirvec<- c(dirvec, "No change")
  }
  
}

pop_output_sub<- popflagssub%>%
  pivot_wider(names_from = Flag, values_from = n)

pop_output_sub[is.na(pop_output_sub)] <- 0

if (!("red" %in% colnames(pop_output_sub))){
  
  pop_output_sub$red<- rep(0, nrow(pop_output_sub))
} 

pop_output_sub<- pop_output_sub%>%
  mutate(sum = green + red )%>%
  mutate(perc = paste0(round(red/sum*100, digits = 1), "%"))  

pop_output_sub$Label<- dirvec

pop_output_sub<- pop_output_sub%>%
  select(Sub_Pillar, green, red, perc, Label)

colnames(pop_output_sub)<- c("Sub-Pillar", "Green Flags", "Red Flags", "Percent Red Flags", "Label")


exp_output<- expflags%>%
  pivot_wider(names_from = Flag, values_from = n)

exp_output[is.na(exp_output)] <- 0

exp_output<- exp_output%>%
  mutate(sum = green + red )%>%
  mutate(perc = paste0(round(red/sum*100, digits = 1), "%"))

exp_output$Label<- ifelse(exp_output$red >= exp_output$sum/2, "Significantly Different", "Consistent")

exp_output<- exp_output%>%
  select(Pillar, green, red, perc, Label)

colnames(exp_output)<- c("Pillar", "Green Flags", "Red Flags", "Percent Red Flags", "Label")



dir<- experts
dir$Direction<- ifelse(dir$GPP_datapoint-dir$TPS_datapoint>0, "Positive", ifelse(dir$GPP_datapoint-dir$TPS_datapoint<0, "Negative", "No Change"))

dir<- dir%>%
  group_by(Sub_Pillar, Direction, Flag)%>%
  summarise(c = n(), .groups = 'drop')

dirvec<- c()

for (p in sort(unique(experts$Sub_Pillar))){
  
  pillar<- dir%>%
    filter(Sub_Pillar == p)
  
  r<- pillar%>%
    filter(Flag == "red")
  rs<- sum(r$c)
  
  if (nrow(pillar)==1 & pillar$Flag[[1]] == "red"){
    d<- pillar$Direction[[1]]
    dirvec<- c(dirvec, paste0("Potential ", d,  " trend"))
    
  } else if (nrow(pillar)==1 & pillar$Flag[[1]] == "green"){
    dirvec<- c(dirvec, "No change")
    
  } else if (rs/sum(pillar$c)>=.75){
    
    if (nrow(r)== 1){
      d<- pillar$Direction[[1]]
      dirvec<- c(dirvec, paste0("Potential ", d,  " trend"))
      
    } else{
      dirvec<- c(dirvec, "Potential Mixed trends")
    }
    
  } else {
    dirvec<- c(dirvec, "No change")
  }
  
}


exp_output_sub<- expflagssub%>%
  pivot_wider(names_from = Flag, values_from = n)

exp_output_sub[is.na(exp_output_sub)] <- 0

exp_output_sub<- exp_output_sub%>%
  mutate(sum = green + red )%>%
  mutate(perc = paste0(round(red/sum*100, digits = 1), "%"))

exp_output_sub$Label<- dirvec

exp_output_sub<- exp_output_sub%>%
  select(Sub_Pillar, green, red, perc, Label)

colnames(exp_output_sub)<- c("Sub-Pillar", "Green Flags", "Red Flags", "Percent Red Flags", "Label")




time_output<- timeflags%>%
  pivot_wider(names_from = warning, values_from = n)

if (!("Red light" %in% colnames(time_output))){
  
  time_output$`Red light`<- rep(0, nrow(time_output))
}

time_output[is.na(time_output)] <- 0

time_output<- time_output%>%
  mutate(sum = `Green light` + `Red light`)%>%
  mutate(perc = paste0(round(`Red light`/sum*100, digits = 1), "%"))

time_output$Label<- ifelse(time_output$`Red light` >= time_output$sum/2, "Significantly Different", "Consistent")

time_output<- time_output%>%
  select(pillar, `Green light`, `Red light`, perc, Label)

colnames(time_output)<- c("Pillar", "Green Flags", "Red Flags", "Percent Red Flags", "Label")


dir<- time%>%
  group_by(subpillar, warning, direction)%>%
  summarise(c = n(), .groups = 'drop')

dirvec<- c()

for (p in sort(unique(time$subpillar))){
  
  pillar<- dir%>%
    filter(subpillar == p)
  
  r<- pillar%>%
    filter(warning == "Red light")
  rs<- sum(r$c)
  
  if (nrow(pillar)==1 & pillar$warning[[1]] == "Red light"){
    d<- gsub(" .*", "", pillar$direction[[1]])
    dirvec<- c(dirvec, paste0("Potential ", d,  " trend"))
    
  } else if (nrow(pillar)==1 & pillar$warning[[1]] == "Green light"){
    dirvec<- c(dirvec, "No change")
    
  } else if (rs/sum(pillar$c)>=.75){
    
    if (nrow(r)== 1){
      d<- gsub(" .*", "", pillar$direction[[1]])
      dirvec<- c(dirvec, paste0("Potential ", d,  " trend"))
      
    } else{
      dirvec<- c(dirvec, "Potential Mixed trends")
    }
    
  } else {
    dirvec<- c(dirvec, "No change")
  }
  
}


time_output_sub<- timeflagssub%>%
  pivot_wider(names_from = warning, values_from = n)

if (!("Red light" %in% colnames(time_output_sub))){
  
  time_output_sub$`Red light`<- rep(0, nrow(time_output_sub))
}

time_output_sub[is.na(time_output_sub)] <- 0

time_output_sub<- time_output_sub%>%
  mutate(sum = `Green light` + `Red light`)%>%
  mutate(perc = paste0(round(`Red light`/sum*100, digits = 1), "%"))

time_output_sub$Label<- dirvec

time_output_sub<- time_output_sub%>%
  select(subpillar, `Green light`, `Red light`, perc, Label)

colnames(time_output_sub)<- c("Sub-Pillar", "Green Flags", "Red Flags", "Percent Red Flags", "Label")

pop<- population %>%
  select(Sub_Pillar, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, `Pretest Flag`,  Pillar )%>%
  arrange(Sub_Pillar)
pop$Direction<- ifelse(pop$GPP_datapoint-pop$TPS_datapoint>0, "Positive", ifelse(pop$GPP_datapoint-pop$TPS_datapoint<0, "Negative", "No Change"))
pop$Flag<- ifelse(pop$Flag == "red", "Red", ifelse(pop$Flag == "green", "Green", NA))

pop<- pop%>%
  select(Sub_Pillar, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, Direction, `Pretest Flag`,  Pillar )

colnames(pop)<- c("Sub Pillar", "GPP Indicator", "Flag", "GPP Question",  "TPS Question", "Match Level", "TPS Source", "TPS Year", "GPP Score", "TPS Score", "Difference", "Direction", "Pretest Flag", "Pillar")



expt<- experts%>%
  select(Sub_Pillar, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, `Pretest Flag`,  Pillar )%>%
  arrange(Sub_Pillar)
expt$Direction<- ifelse(expt$GPP_datapoint-expt$TPS_datapoint>0, "Positive", ifelse(expt$GPP_datapoint-expt$TPS_datapoint<0, "Negative", "No Change"))
expt$Flag<- ifelse(expt$Flag == "red", "Red", ifelse(expt$Flag == "green", "Green", NA))

expt<- expt%>%
  select(Sub_Pillar, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, Direction, `Pretest Flag`,  Pillar )

colnames(expt)<- c("Sub Pillar", "GPP Indicator", "Flag", "GPP Question",  "TPS Question", "Match Level", "TPS Source", "TPS Year", "GPP Score", "TPS Score", "Difference", "Direction", "Pretest Flag", "Pillar")


tm<- time%>%
  select(subpillar, `2023  EU Questionnaire`, warning, Description, prev_year, current_score, previous_score, ttestResult,  direction,  `Pretest Flag`, pillar )%>%
  arrange(subpillar)

colnames(tm)<- c("Sub Pillar", "GPP Indicator", "Flag", "GPP Question", "Comparison Year", "Current Score", "Previous Score", "P value",  "Direction", "Pretest Flag", "Pillar" )

tm$Flag<- ifelse(tm$Flag == "Red light", "Red", ifelse(tm$Flag == "Green light", "Green", NA))


tps<- threshold1%>%
  select(Sub_Pillar, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, Type_Survey, `Pretest Flag`, Pillar )%>%
  arrange(Sub_Pillar)
tps$Direction<- ifelse(tps$GPP_datapoint-tps$TPS_datapoint>0, "Positive", ifelse(tps$GPP_datapoint-tps$TPS_datapoint<0, "Negative", "No Change"))
tps$Flag<- ifelse(tps$Flag == "red", "Red", ifelse(tps$Flag == "green", "Green", NA))

tps<- tps%>%
  select(Sub_Pillar, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, Type_Survey, Direction, `Pretest Flag`, Pillar )
colnames(tps)<- c("Sub Pillar", "GPP Indicator", "Flag", "GPP Question",  "TPS Question", "Match Level", "TPS Source", "TPS Year", "GPP Score", "TPS Score", "Difference", "Survey Type", "Direction", "Pretest Flag", "Pillar")


### National Level


# spmatches <- str_extract_all(par, "[0-9]\\.[0-9]{1,2}:")
# spmatches <- unlist(spmatches)
# popsum <- pop[grepl(paste(spmatches, collapse="|"), pop$`Sub Pillar`), ]
# popsum <- popsum%>%
#   select(-Pillar)
# ttsum  <- tm[grepl(paste(spmatches, collapse="|"), tm$`Sub Pillar`), ]
# ttsum  <- ttsum%>%
#   select(-Pillar)
# expsum <- expt[grepl(paste(spmatches, collapse="|"), expt$`Sub Pillar`), ]
# expsum <- expsum%>%
#   select(-Pillar)




### NUTS Level

thresholdnuts1<-df$tps_nuts%>% distinct()

timenuts<- df$time_nuts%>% distinct()

expertsnuts<- thresholdnuts1%>%
  filter(Type_Survey == "expert")%>%
  drop_na()
populationnuts<- thresholdnuts1%>%
  filter(Type_Survey == "population")%>%
  drop_na()

popnuts<- populationnuts%>%
  select(Sub_Pillar, NUTS, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, Pillar )%>%
  arrange(Sub_Pillar)
popnuts$Direction<- ifelse(popnuts$GPP_datapoint-popnuts$TPS_datapoint>0, "Positive", ifelse(popnuts$GPP_datapoint-popnuts$TPS_datapoint<0, "Negative", "No Change"))
popnuts$Flag<- ifelse(popnuts$Flag == "red", "Red", ifelse(popnuts$Flag == "green", "Green", NA))

popnuts<- popnuts%>%
  select(Sub_Pillar, NUTS, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, Direction, Pillar )

colnames(popnuts)<- c("Sub Pillar", "NUTS", "GPP Indicator", "Flag", "GPP Question",  "TPS Question", "Match Level", "TPS Source", "TPS Year", "GPP Score", "TPS Country Score", "Difference", "Direction", "Pillar")



exptnuts<- expertsnuts%>%
  select(Sub_Pillar, NUTS, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference,  Pillar )%>%
  arrange(Sub_Pillar)
exptnuts$Direction<- ifelse(exptnuts$GPP_datapoint-exptnuts$TPS_datapoint>0, "Positive", ifelse(exptnuts$GPP_datapoint-exptnuts$TPS_datapoint<0, "Negative", "No Change"))
exptnuts$Flag<- ifelse(exptnuts$Flag == "red", "Red", ifelse(exptnuts$Flag == "green", "Green", NA))

exptnuts<- exptnuts%>%
  select(Sub_Pillar, NUTS, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, Direction,  Pillar )

colnames(exptnuts)<- c("Sub Pillar", "NUTS", "GPP Indicator", "Flag", "GPP Question",  "TPS Question", "Match Level", "TPS Source", "TPS Year", "GPP Score", "TPS Country Score", "Difference", "Direction", "Pillar")


tmnuts<- timenuts%>%
  select(subpillar, nuts, `2023  EU Questionnaire`, warning, Description, prev_year, current_score, previous_score, ttestResult,  direction, pillar )%>%
  arrange(subpillar)
tmnuts$Difference<- abs(tmnuts$current_score- tmnuts$previous_score)

tmnuts<- tmnuts%>%
  select(subpillar, nuts, `2023  EU Questionnaire`, warning, Description, prev_year, current_score, previous_score, Difference,  direction, pillar )

colnames(tmnuts)<- c("Sub Pillar", "NUTS", "GPP Indicator", "Flag", "GPP Question", "Comparison Year", "Current Score", "Previous Country Score", "Difference",  "Direction", "Pillar" )

tmnuts$Flag<- ifelse(tmnuts$Flag == "Red light", "Red", ifelse(tmnuts$Flag == "Green light", "Green", NA))


tpsnuts<- thresholdnuts1%>%
  select(Sub_Pillar, NUTS, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, Type_Survey, Pillar )%>%
  arrange(Sub_Pillar)
tpsnuts$Direction<- ifelse(tpsnuts$GPP_datapoint-tpsnuts$TPS_datapoint>0, "Positive", ifelse(tpsnuts$GPP_datapoint-tpsnuts$TPS_datapoint<0, "Negative", "No Change"))
tpsnuts$Flag<- ifelse(tpsnuts$Flag == "red", "Red", ifelse(tpsnuts$Flag == "green", "Green", NA))

tpsnuts<- tpsnuts%>%
  select(Sub_Pillar, NUTS, `2023  EU Questionnaire`, Flag, Description, TPS_Question, Match, TPS_Source, TPS_Year, GPP_datapoint, TPS_datapoint, Difference, Type_Survey, Direction, Pillar )
colnames(tpsnuts)<- c("Sub Pillar", "NUTS", "GPP Indicator", "Flag", "GPP Question",  "TPS Question", "Match Level", "TPS Source", "TPS Year", "GPP Score", "TPS Country Score", "Difference", "Survey Type", "Direction", "Pillar")




### GPP Over Time


library(openxlsx)


xl_lst <- list('time_output' = time_output, 'time_output_sub' = time_output_sub, "pop_output" = pop_output, 
               "pop_output_sub"= pop_output_sub, "exp_output"= exp_output, "exp_output_sub"= exp_output_sub, 
               "tm"= tm, "pop"=pop, "expt" = expt, "tps"= tps, "tmnuts"= tmnuts, "popnuts"=popnuts, 
               "exptnuts" = exptnuts, "tpsnuts"= tpsnuts)

write.xlsx(xl_lst, file = "filename.xlsx")



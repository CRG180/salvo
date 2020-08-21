in_data<-read.csv("data_inputs.csv")
in_data$outcome_china<-rep("NA",nrow(in_data))
in_data$outcome_us<-rep("NA",nrow(in_data))
in_data$iter <- rep("NA",nrow(in_data))



for(i in 1:nrow(in_data)){
# Number of Ships, Weapons Systms
china_platform <- in_data$china_platform[i]
us_platform <- in_data$us_offense[i]

# Staying number of hits it can take from enemy
china_staying<- in_data$china_staying_power[i]
us_staying<-in_data$us_staying_power[i]

# Defense stregnth in num missiles/platform
china_defense <- in_data$china_defense[i]
us_defense <-in_data$us_defense[i]

# offense stregnth in num missiles/platform
china_offense <- in_data$china_offense[i]
us_offense <-in_data$us_offense[i]

first_salvo <- in_data$first_salvo[i]

# Scouting ablility 
china_scouting<-in_data$china_scouting[i]
us_scouting <-in_data$us_scouting[i]

# readiness ablility 
china_ready<-in_data$china_ready[i]
us_ready <-in_data$us_ready[i]



iter = 0
while(us_platform > 0 && china_platform > 0 ){
iter = iter +1  
if(first_salvo=="china"){
#delta us
delta_us <-((china_scouting * china_offense*china_platform)-(us_ready * us_defense*us_platform))/us_staying
#update platform count
us_platform <- us_platform- delta_us


## Delta China 
delta_china<- ((us_scouting * us_offense*us_platform)-(china_ready * china_defense*china_platform))/china_staying
# update platform number 
china_platform <- china_platform-delta_china

} # end for if us salvo first

if(first_salvo=="us"){
  ## Delta China 
  delta_china<- ((us_scouting*us_offense*us_platform)-(china_ready * china_defense*china_platform))/china_staying
  # update platform number 
  china_platform <- china_platform-delta_china
  
  #delta us
  delta_us <-((china_scouting *china_offense*china_platform)-(us_ready * us_defense*us_platform))/us_staying
  #update platform count
  us_platform <- us_platform- delta_us
  
} #end if for china salvo first

if(first_salvo=="tie"){
  print("I don't know")
}# end tie salvo 
} # end while loop   

### Outfile 
in_data$outcome_china[i]<-china_platform  # the number of ships that c
in_data$outcome_us[i]<-us_platform
in_data$iter[i] <- iter

} # end for loop 

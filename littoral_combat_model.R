in_data<-read.csv("lit_input.csv")
in_data$outcome_land_based_force<-rep("NA",nrow(in_data))
in_data$outcome_sea_based_force<-rep("NA",nrow(in_data))
in_data$FER<-rep("NA",nrow(in_data))
in_data$FER_first_salvo<-rep("NA",nrow(in_data))
in_data$iter <- rep("NA",nrow(in_data))



for(i in 1:nrow(in_data)){
  # Number of Ships, Weapons Systems
  sea_based_force <- in_data$sea_based_force[i]
  land_based_force <- in_data$land_based_force[i]
  
  # Staying number of hits it can take from enemy
  sea_based_staying<- in_data$sea_based_staying[i]
  #land_based_staying<-in_data$land_based_staying[i]
  
  # Defense strength in num missiles/platform
  sea_based_defense <- in_data$sea_based_defense[i]
  #land_based_defense <-in_data$land_based_defense[i]
  
  #defensive Readiness of the sea based Force
  sea_based_readiness <- in_data$sea_based_readiness[i]
  
  # direct fire strength in num missiles/platform
  sea_based_direct_fire <- in_data$sea_based_direct_fire[i]
  land_based_direct_fire<-in_data$land_based_direct_fire[i]
  
  #who fires first
  first_salvo <- in_data$first_salvo[i]
  
  # Scouting targeting
  sea_based_targeting<-in_data$sea_based_targeting[i]
  land_based_targeting <-in_data$land_based_targeting[i]
  
  # readiness ability 
  sea_based_deception<-in_data$sea_based_deception[i]
  #land_based_deception <-in_data$land_based_deception[i]
  
  
  
  iter = 0
  while(sea_based_force > 0 && land_based_force > 0 ){
    iter = iter +1  
    if(first_salvo=="land"){
   
    delta_sea <- ((land_based_targeting*sea_based_deception*land_based_direct_fire*land_based_force)-
      (sea_based_readiness*sea_based_defense*sea_based_force))/sea_based_direct_fire
    
    sea_based_force <- sea_based_force-delta_sea
      
      
    delta_land <- sea_based_targeting * sea_based_direct_fire *sea_based_force
    land_based_force- land_based_force - delta_land
    if(iter == 1){
      in_data$FER_first_salvo[i]<-(sea_based_force/in_data$sea_based_force[i])/(land_based_force/in_data$land_based_force[i])
    }
      
    } # end for if us salvo first
    
    if(first_salvo=="sea"){
      delta_land <- sea_based_targeting * sea_based_direct_fire *sea_based_force
      land_based_force<- land_based_force - delta_land
      
      delta_sea <- ((land_based_targeting*sea_based_deception*land_based_direct_fire*land_based_force)-
                      (sea_based_readiness*sea_based_defense*sea_based_force))/sea_based_direct_fire
      sea_based_force <- sea_based_force-delta_sea
      if(iter == 1){ # don't think that works correct not even sure the purpose of FER does not account for salva
        in_data$FER_first_salvo[i]<-(sea_based_force/in_data$sea_based_force[i])/(land_based_force/in_data$land_based_force[i])
      }
      
    } #end if for china salvo first
    
    if(first_salvo=="tie"){
      print("I don't know")
    }# end tie salvo 
  } # end while loop   
  
  ### Outfile 
  in_data$outcome_land_based_force[i]<-land_based_force  
  in_data$outcome_sea_based_force[i]<-sea_based_force
  in_data$iter[i] <- iter
  in_data$FER[i]<-(sea_based_force/in_data$sea_based_force[i])/(land_based_force/in_data$land_based_force[i])
  
} # end for loop 
write.csv(in_data, "lit_output.csv")

library(data.table)
#Importing large dataset
participant_checkin = fread("participant_checkin_2.csv")
participant_checkin = participant_checkin[, -1]

#Importing smaller dataset
library(readr)
Club_site = read_csv("Club_site.csv")

library(readr)
Customer_agreement = read_csv("Customer_Agreement.csv")
Customer_agreement = Customer_agreement[, -1]

#---------------------------------------

#Adding integer values to gym types
for(i in 1:nrow(Club_site)){
  if(Club_site[i, 9] == "Active"){
    Club_site[i, 10] = 1
  }
  else if(Club_site[i, 9] == "Sport"){
    Club_site[i, 10] = 2
  }
  else if(Club_site[i, 9] == "SuperSport"){
    Club_site[i, 10] = 3
  }
  else if(Club_site[i, 9] == "UltraSport"){
    Club_site[i, 10] = 4
  }
  else{
    Club_site[i, 9] == 0
  }
}

Club_site$club_type_id = as.integer(Club_site$club_type_id) #converiting columns from character to integer 
participant_checkin$club_type_id = as.integer(Club_site$club_type_id)  

#-----------------------------------------

#Using new ID numbers for dataset
club_nums = participant_checkin$club_num
club_types = participant_checkin$club_num
club_src_nums = Club_site[, c(1, 10)]

for(i in 1:nrow(club_src_nums)){
  indexes = which(club_nums %in% club_src_nums$club_src_num[i]) #Find index for each club
  for(j in 1:length(indexes))
    club_types[indexes[j]] = club_src_nums$club_type_id[i] #Find club type and add to column
}

#-------------------------------------------

#Simplifying Participant ID

participant_checkin$simple_participant_id = 0
simple_participant_id = participant_checkin$simple_participant_id
participant_id_list = participant_checkin$participant_id


participant_id = participant_checkin$participant_id[1] #starting from first ID
integer = 1
for(i in 1:(nrow(participant_checkin))){
  if(participant_id_list[i] == participant_id){
    simple_participant_id[i] = integer #if ID same as previous ID, enter same simplified ID
  }
  else{
    integer = integer + 1
    participant_id = participant_id_list[i]
    simple_participant_id[i] = integer #if different, iterate to next ID
  }
}

participant_checkin$simple_participant_id = simple_participant_id

#-------------------------------------------

#Finding Max gym type to reveal probable membership type

#install.packages("tidyverse")
library(tidyverse)

setkey(participant_checkin, simple_participant_id)


participant = 1
participant_membership = c()
for(i in 1:72339){ #72339 different particpants
  participant = participant_checkin[simple_participant_id == i] #Subset for each particpant
  participant_club_types = participant$club_types #Find column of gym types they attended
  ultra_sport_check = which(participant_club_types %in% 4) #Find indices of ultra-sport attendances
  super_sport_check = which(participant_club_types %in% 3) #Find indices of super-sport attendances
  if(length(ultra_sport_check > 0)){ #If attended an ultra-sport - assume they have utlra-sport membership
    participant$membership = 4
  }
  else if(length(super_sport_check > 0)){ #If attnded a super-sport and not ultra-sport, assume super-sport membership
    participant$membership = 3
  }
  else{
    participant$membership = 2 #Else have standard Sport membership (no active option)
  }
  participant_membership = c(participant_membership, participant$membership)
}

participant_checkin$membership_type = participant_membership

#----------------------------------------------------
#Finding Duration of Membership
library(lubridate)

start_dates = Customer_agreement$agreement_sign_dt

#Start Dates
new_start_dates_1 = mdy_hm(start_dates) #Clean dates of mdy_hm type (majority) - leaves mdy types blank (NA)
new_start_dates_2 = mdy(start_dates) #Clean dates of mdy type (minority) - leaves mdy_hm types blank (NA)
new_start_dates = as.Date(new_start_dates_1) #Convert mdy_hm type to mdy type

indices = which(new_start_dates %in% NA)
for(i in 1:(length(indices))){
  new_start_dates_1[indices[i]] = new_start_dates_2[indices[i]] #Fill missing values
}

Customer_agreement$cleaned_agreement_sign_dt = new_start_dates

#End Dates
end_dates = Customer_agreement$termination_date

new_end_dates_1 = mdy_hm(end_dates) #Clean dates of mdy_hm type (majority) - leaves mdy types blank (NA)
new_end_dates_2 = mdy(end_dates) #Clean dates of mdy type (minority) - leaves mdy_hm types blank (NA)

indices = which(new_end_dates %in% NA)
for(i in 1:(length(indices))){
  new_end_dates_1[indices[i]] = new_end_dates_2[indices[i]] #Fill missing values
}

new_end_dates = as.Date(new_end_dates_1) #Convert mdy_hm type to mdy type
Customer_agreement$cleaned_termination_date = new_end_dates

#----------------------------------------------
# Finding first and final checkin time

checkin_times = participant_checkin$checkin_datetime_localtime
max_checkin = max(checkin_times)
min_checkin = min(checkin_times)

signup_dates = Customer_agreement$cleaned_agreement_sign_dt
max_signup = max(signup_dates)
min_signup = min(signup_dates)


#-------------------------------------------------
#Finding Length of Contracts
start_dates = Customer_agreement$cleaned_agreement_sign_dt
end_dates = Customer_agreement$cleaned_termination_date

date_diff = c()
final_date = "2019-02-17" #Final attendance entry in dataset
for(i in 1:(length(start_dates))){
  if(is.na(end_dates[i])){
    date_diff = c(date_diff, difftime(final_date, start_dates[i]))
  }
  else{
    date_diff = c(date_diff, difftime(end_dates[i], start_dates[i]))
  }
}

Customer_agreement$duration_of_membership = date_diff

Customer_agreement$duration_of_membership_month = (Customer_agreement$duration_of_membership)/30.4375

date_diff = Customer_agreement$duration_of_membership
date_diff_2 = date_diff
start_date_cutoff = "2006-12-31"
for(i in 1:(length(start_dates))){
  if(start_dates[i] < start_date_cutoff){
    if(is.na(end_dates[i])){
      date_diff_2[i] = difftime(final_date, start_date_cutoff)
    }
    else{
      date_diff_2[i] = difftime(end_dates[i], start_date_cutoff)
    }
  }
}

Customer_agreement$duration_of_membership_cutoff = date_diff_2
Customer_agreement$duration_of_membership_month_cutoff = (Customer_agreement$duration_of_membership_cutoff)/30.4375

#--------------------------------------------------
#Finding Average Attendance for dataset
library(plyr)

Customer_agreement_core = Customer_agreement[which(Customer_agreement$agreement_type_id == 17),] #Membership core only

participant_ids = Customer_agreement_core$participant_id
participant_checkin_ids = participant_checkin$participant_id

table = table(participant_checkin_ids)
attendance_frequencies = as.data.frame(table)
frequencies = attendance_frequencies$Freq
frequencies_ids = attendance_frequencies$participant_checkin_ids

total_attendance = participant_ids
for(i in 1:length(participant_ids)){
  index = which(frequencies_ids %in% participant_ids[i])
  if(length(index)!= 0){
    total_attendance[i] = frequencies[index]
  }
  else{
    total_attendance[i] = 0
  }
}

Customer_agreement_core$total_attendance = total_attendance
total_attendance = as.numeric(total_attendance) 
total_attendance_value = sum(total_attendance)

library(dplyr)
Customer_agreement_core_2 = distinct(Customer_agreement_core)

duplicates = duplicated(participant_ids)
duplicates_index = which(duplicates == TRUE)
CA_start_dates = Customer_agreement_core$cleaned_agreement_sign_dt
for(i in 1:length(duplicates_index)){
  specific_duplicate_indices = which(partipant_ids)
  if(Customer_agreement){
    
  }
}
Customer_agreement_core_2 = Customer_agreement_core[-duplicates_index,]
total_attendance = as.numeric(Customer_agreement_core_2$total_attendance)
total_attendance_value = sum(total_attendance)

Customer_agreement_core_2 = 

attendance = attendance_frequencies$Freq
max_attendance = max(attendance)



#Save Files
write.csv(participant_checkin, "participant_checkin_2.csv")

write.csv(Customer_agreement, "Customer_agreement.csv")

write.csv(Customer_agreement_core, "Customer_agreement_core.csv")


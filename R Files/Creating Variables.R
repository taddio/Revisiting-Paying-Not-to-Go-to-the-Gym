#Creating New Variables
#--------------------------------------------------------------------------------

library(data.table)
#Importing large dataset
participant_checkin = fread("participant_checkin_2.csv")

#Importing smaller dataset
library(readr)
Club_site = read.csv("Club_site.csv")

Customer_agreement = read_csv("Customer_Agreement.csv")

Gym_customer = read_csv("Gym_Customer.csv")

#---------------------------------------

#Adding integer values to gym types - Gym types manually added using data found online
Club_site$club_type_id = 0

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
    Club_site[i, 10] == 0
  }
}

#---------------------------------------------------------
#Adding club types to participant checkin
participant_checkin$club_num = 0
for(i in 1:nrow(Club_site)){
  participant_checkin$club_num[which(participant_checkin$club_num == Club_site$club_src_num[i])]] = Club_site$club_type_id[i]
}
#----------------------------------------------------
#Finding Duration of Membership
library(lubridate)

start_dates = Customer_agreement$agreement_sign_dt

#Start Dates - start dates are formated in two different ways (mdy_hm and mdy)

new_start_dates_1 = mdy_hm(start_dates) #Clean dates of mdy_hm type (majority) - leaves mdy types blank (NA)
new_start_dates_2 = mdy(start_dates) #Clean dates of mdy type (minority) - leaves mdy_hm types blank (NA)
new_start_dates = as.Date(new_start_dates_1) #Convert mdy_hm type to mdy type

indices = which(new_start_dates %in% NA)
for(i in 1:(length(indices))){
  new_start_dates_1[indices[i]] = new_start_dates_2[indices[i]] #Fill missing values
}

Customer_agreement$cleaned_agreement_sign_dt = new_start_dates

#End Dates - same format issues as start dates

end_dates = Customer_agreement$termination_date

new_end_dates_1 = mdy_hm(end_dates) #Clean dates of mdy_hm type (majority) - leaves mdy types blank (NA)
new_end_dates_2 = mdy(end_dates) #Clean dates of mdy type (minority) - leaves mdy_hm types blank (NA)

indices = which(new_end_dates %in% NA)
for(i in 1:(length(indices))){
  new_end_dates_1[indices[i]] = new_end_dates_2[indices[i]] #Fill missing values
}

new_end_dates = as.Date(new_end_dates_1) #Convert mdy_hm type to mdy type
Customer_agreement$cleaned_termination_date = new_end_dates

#-------------------------------------------------
#Finding Length of Contracts
start_dates = Customer_agreement$cleaned_agreement_sign_dt
end_dates = Customer_agreement$cleaned_termination_date

date_diff = c()
final_date = "2019-02-17" #Final attendance entry in dataset - or artificial cutoff as desired
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
start_date_cutoff = "2006-12-31" #
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

participant_ids = Customer_agreement_core$participant_id #List of unique ids found in Customer Agreement
participant_checkin_ids = participant_checkin$participant_id #List of IDs from checkin data

table = table(participant_checkin_ids) #Tabulates checkin data into unique IDs and frequency (number of checkins)
attendance_frequencies = as.data.frame(table) #Convert table to data frame
frequencies = attendance_frequencies$Freq #extract frequencies column
frequencies_ids = attendance_frequencies$participant_checkin_ids #extract unique IDs

duplicates = duplicated(participant_ids)
duplicates_index = which(duplicates == TRUE)
customer_agreement_statuses = Customer_agreement_core$agreement_status_id
specific_duplicate_indices = which(duplicates == TRUE & customer_agreement_statuses != 3)

checked = c()
for(i in 1:length(duplicates_index)){
  index = which(participant_ids == participant_ids[duplicates_index[i]])
  if(length(index) > 2){
    if(length(which(checked == index[1])) == 0){
      checked = c(checked, index[1])
    }
  }
}

check_duplicates_table = Customer_agreement_core[, c(1,5)]
check_duplicates_table = check_duplicates_table[order(Customer_agreement_core$agreement_status_id),]

Customer_agreement_core_clean = Customer_agreement_core[-specific_duplicate_indices, ]
total_attendances = sum(Customer_agreement_core_clean$total_attendance)

duplicates_2 = duplicated(Customer_agreement_core_clean$participant_id)
duplicates_index_2 = which(duplicates_2 == TRUE)

attendance = attendance_frequencies$Freq
max_attendance = max(attendance)


#---------------------------------------------------------------------------------------
#Order customer_agreement dataset

Customer_agreement_ordered = Customer_agreement[order(Customer_agreement$participant_id,  
                                                      Customer_agreement$agreement_type_id,
                                                      Customer_agreement$agreement_status_id),]

#-------------------------------------------------------------------------------------
#Counting different agreement types

Customer_agreement_16 = Customer_agreement[which(Customer_agreement$agreement_type_id == 16),] #Upgrade only
Customer_agreement_17 = Customer_agreement[which(Customer_agreement$agreement_type_id == 17),] #Membership core only
Customer_agreement_19 = Customer_agreement[which(Customer_agreement$agreement_type_id == 19),] #Privelege only
Customer_agreement_20 = Customer_agreement[which(Customer_agreement$agreement_type_id == 20),] #Sessions only

library(plyr)
table_16 = table(Customer_agreement_16$agreement_status_id)
print(table_16)

table_17 = table(Customer_agreement_17$agreement_status_id)
print(table_17)

table_19 = table(Customer_agreement_19$agreement_status_id)
print(table_19)

table_20 = table(Customer_agreement_20$agreement_status_id)
print(table_20)

#----------------------------------------------------------------------------------------
#Adding club source numbers to Gym customer csv

club_site_numbers = Club_site[, c(1,2)]
club_site_numbers = club_site_numbers[order(club_site_numbers$club_id),]

Gym_customer = Gym_customer[order(Gym_customer$club_of_enrollment), ]

#--------------------------------------------------------------------------------------------
#Finding number of attendances at each club
club_checkins = participant_checkin$club_num
club_nums = Club_site$club_src_num
club_attendences = c()
for(i in 1:length(club_nums)){
  indices = which(club_checkins == club_nums[i])
  club_attendences = c(club_attendences, length(indices))
}

Club_site$total_attendance = club_attendences


#-------------------------------------------------------------------------------------------
#Excluding Corporate Offices

Club_site = Club_site[-which(Club_site$club_src_num %in% c(1, 300, 400, 500)),]

#Excluding Zero Attendance Clubs

Club_site = Club_site[-which(Club_site$total_attendance == 0), ]


#OLD CODE SECTION - USED METHOD THAT WAS NOT GOOD - See 'Forming Subset and Analysis'








#--------------------------------------------------------------
#Excluding Clubs which are not open for entire 2010-2017 period

checkin_club_numbers = participant_checkin$club_num
clubs = Club_site$club_src_num


#finding minimum dates
min_dates = c()

for(i in 1:length(clubs)){
  subset = participant_checkin[which(checkin_club_numbers == clubs[i]),]
  subset_dates = subset$checkin_datetime_localtime
  min = subset_dates[1]
  for(j in 1:length(subset_dates)){
    if(subset_dates[j] < min){
      min_2 = min
      min = subset_dates[j]
    }
  }
  min_dates = c(min_dates, min)
}

print(min_dates)


#finding maximum dates
max_dates = c()

for(i in 1:length(clubs)){
  subset = participant_checkin[which(checkin_club_numbers == clubs[i]),]
  subset_dates = subset$checkin_datetime_localtime
  max = subset_dates[1]
  for(j in 1:length(subset_dates)){
    if(subset_dates[j] > max){
      max = subset_dates[j]
    }
  }
  max_dates = c(max_dates, max)
}

#Attach to Club Site

Club_site$max_checkin_date = max_dates
Club_site$min_checkin_date = min_dates

#Exclude clubs before min cutoff

Club_site = Club_site[which(Club_site$min_checkin_date < "2010-01-01 00:00:00-08:00"),]

Club_site = Club_site[which(Club_site$max_checkin_date > "2017-01-01 00:00:00-08:00"),]

#Exclude participants with attendances NOT at these clubs


subsetted_club_nums = Club_site$club_src_num
excluded_club_nums = clubs[-which(clubs %in% subsetted_club_nums)]
participants = unique(participant_checkin$participant_id)
participant_checkin_ids = participant_checkin$participant_id
excluded_participants = c()

for(i in 1:length(participants)){
  subset = participant_checkin[which(participant_checkin_ids == participants[i]), ]
  subset_club_nums = subset$club_num
  unique_clubs = unique(subset_club_nums)
  if(length(which(excluded_club_nums %in% unique_clubs)) > 0){
    excluded_participants = c(excluded_participants, participants[i])
  }
}


participant_checkin_ids = participant_checkin$participant_id
subset_participant_checkin = participant_checkin[-which(participant_checkin_ids %in% excluded_participants), ]


subset_checkin_times = subset_participant_checkin$checkin_datetime_localtime

subset_participant_checkin_2 = subset_participant_checkin[which(subset_checkin_times > "2010-01-01 00:00:00-08:00"), ]
subset_participant_checkin_2 = subset_participant_checkin[which(subset_checkin_times < "2017-01-01 00:00:00-08:00"), ]

subset_participant_checkin_ids = unique(subset_participant_checkin_2$participant_id)

write.csv(subset_participant_checkin_2, "clean_subset_participant_checkin.csv")

subset_customer_agreement = Customer_agreement[which(Customer_agreement$participant_id %in% 
                                                       subset_participant_checkin_ids), ]
subset_customer_agreement = subset_customer_agreement[which(subset_customer_agreement$agreement_type_id 
                                                            %in% c(16,17)), ]

write.csv(subset_customer_agreement, "semi_clean_customer_agreement.csv")


subset_participant_checkin_3 = subset_participant_checkin_2[which(subset_participant_checkin_2$participant_id
                                                                  %in% subset_customer_agreement$participant_id), ]

write.csv(subset_participant_checkin_3, "clean_subset_participant_checkin.csv")
library(plyr)

member_table = table(subset_customer_agreement$participant_id)
member_table = as.data.frame(member_table)

member_table_issues = member_table[which(member_table$Freq > 1), ]

subset_customer_agreement = read_csv("semi_clean_customer_agreement.csv")
subset_customer_agreement = subset_customer_agreement[, -2]

unique_subset_members = unique(subset_customer_agreement$participant_id)

subset_customer_agreement = subset_customer_agreement[-which(subset_customer_agreement$cleaned_agreement_sign_dt
                                                             > "2017-01-01"), ]

subset_customer_agreement = subset_customer_agreement[-which(subset_customer_agreement$cleaned_termination_date
                                                             < "2010-01-01"), ]

subset_customer_agreement_ids = subset_customer_agreement$participant_id
subset_customer_agreement_types = subset_customer_agreement$agreement_type_id
subset_customer_agreement_statuses = subset_customer_agreement$agreement_status_id
ids = c()

for(i in 1:length(subset_customer_agreement_types)){
  if(subset_customer_agreement_types[i] == 17 & (subset_customer_agreement_statuses[i] == 3 | 
                                                 subset_customer_agreement_statuses[i] == 5)){
    ids = c(ids, subset_customer_agreement_ids[i])
  }
}

subset_customer_agreement = subset_customer_agreement[which(subset_customer_agreement$participant_id
                                                            %in% ids), ]

subset_customer_agreement = subset_customer_agreement[-which(subset_customer_agreement$agreement_status_id %in% 
                                                               c(6, 8, 27, 28, 30, 31, 32, 33, 34, 35, 36)), ]

exclude_unknown_clubs = subset_participant_checkin_3[which(subset_participant_checkin_3$club_num
                                                           == "Unknown"), ]
unknown_clubs_members = unique(exclude_unknown_clubs$participant_id)

subset_customer_agreement = subset_customer_agreement[-which(subset_customer_agreement$participant_id %in% 
                                                               unknown_clubs_members), ]

subset_participant_checkin_3 = subset_participant_checkin_3[which(subset_participant_checkin_3$participant_id
                                                                  %in% subset_customer_agreement$participant_id), ]

unique_subset_members = unique(subset_customer_agreement$participant_id)

#--------------------------------------------------------------------------
#Finding Length of Contracts
start_dates = subset_customer_agreement$cleaned_agreement_sign_dt #Use sub of customer agreements of your choosing
end_dates = subset_customer_agreement$cleaned_termination_date

date_diff = c()
final_date = "2017-01-01" #Final attendance entry in dataset - censor date (can be changed)
for(i in 1:(length(start_dates))){
  if(is.na(end_dates[i])){ #Will find difference between start start and 
    date_diff = c(date_diff, difftime(final_date, start_dates[i]))
  }
  else{
    date_diff = c(date_diff, difftime(end_dates[i], start_dates[i]))
  }
}

subset_customer_agreement$duration_of_membership = date_diff

subset_customer_agreement$duration_of_membership_month = (subset_customer_agreement$duration_of_membership)/30.4375

art_cutoff = subset_customer_agreement$cleaned_termination_date #artificial cut-off - censores terminations dates after final_date
for(i in 1:length(art_cutoff)){
  if(is.na(art_cutoff[i]) | art_cutoff[i] > "2017-01-01"){
    art_cutoff[i] = "2017-01-01"
  }
}

subset_customer_agreement$art_cutoff = art_cutoff

date_diff = subset_customer_agreement$duration_of_membership
date_diff_2 = date_diff
end_date_cutoff = "2010-01-01"
for(i in 1:(length(start_dates))){
  if(start_dates[i] < start_date_cutoff){
    date_diff_2[i] = difftime(art_cutoff[i], start_date_cutoff)
  }
  else{
    date_diff_2[i] = difftime(art_cutoff[i], start_dates[i])
  }
}

subset_customer_agreement$duration_of_membership_cutoff = date_diff_2
subset_customer_agreement$duration_of_membership_month_cutoff = (subset_customer_agreement$duration_of_membership_cutoff)/30.4375

subset_17_customer_agreement = subset_customer_agreement[which(subset_customer_agreement$agreement_type_id == 17), ]

subset_participant_checkin_4 = subset_participant_checkin_3[which(subset_participant_checkin_3$participant_id
                                                                  %in% subset_17_customer_agreement$participant_id), ]

subset_membership_start = subset_17_customer_agreement$cleaned_agreement_sign_dt
subset_membership_end = subset_17_customer_agreement$cleaned_termination_date
participant_ids = subset_17_customer_agreement$participant_id
attendance = c()
for(i in 1:length(participant_ids)){
  if(length(which(participant_ids == participant_ids[i])) > 1){
    subset = subset_participant_checkin_3[which(subset_participant_checkin_3$participant_id == participant_ids[i])]
    attendance = c(attendance, length(which(subset$checkin_datetime_localtime > subset_17_customer_agreement[i, 7] 
                                            & subset$checkin_datetime_localtime < subset_17_customer_agreement[i, 8])))
  }
  else{
    attendance = c(attendance, length(which(subset_participant_checkin_3$participant_id == participant_ids[i]))) 
  }
}

subset_17_customer_agreement$attendance = attendance
subset_17_customer_agreement = subset_17_customer_agreement[which(subset_17_customer_agreement$attendance > 0), ]

subset_17_customer_agreement$average_attendance = subset_17_customer_agreement$attendance/subset_17_customer_agreement$duration_of_membership_month_cutoff
sum(subset_17_customer_agreement$average_attendance)/7986
sum(subset_17_customer_agreement$average_attendance)/8283

attendance_table = table(subset_participant_checkin_3$participant_id) #Tabulates checkin data into unique IDs and frequency (number of checkins)
attendance_frequencies = as.data.frame(attendance_table) #Convert table to data frame
frequencies = attendance_frequencies$Freq #extract frequencies column
frequencies_ids = attendance_frequencies$participant_checkin_ids #extract unique IDs

#-------------------------------------------------------------------------------------------------------
#Finding start and end dates after subsetting data

checkin_times = participant_checkin$checkin_datetime_localtime
max_checkin = max(checkin_times)
min_checkin = min(checkin_times)

signup_dates = Customer_agreement$cleaned_agreement_sign_dt
max_signup = max(signup_dates)
min_signup = min(signup_dates)


#Save Files
write.csv(participant_checkin, "participant_checkin_2.csv")
write.csv(participant_checkin_subset_1, "participant_checkin_subset_1.csv")
write.csv(participant_checkin_subset_2, "participant_checkin_subset_2.csv")

write.csv(Customer_agreement, "Customer_agreement_subset_5.csv")

write.csv(Customer_agreement_core, "Customer_agreement_core.csv")

write.csv(Customer_agreement_16, "Customer_agreement_16.csv")

write.csv(Club_site, "Club_site_with_attendances.csv")
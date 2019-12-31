#Extracting Subset

#-------------------------------------------------------------------------------------------
#Reading Data

library(data.table)
#Importing large dataset
participant_checkin = fread("participant_checkin_2.csv") #Read checkin data

library(readr)
Club_site = read_csv("Club_site.csv") #Read Club Site data
Club_site_expanded = read_csv("Club_site_expanded.csv")

Customer_agreement = read.csv("Customer_Agreement.csv") #Read Customer Agreement Data
Customer_agreement = read_csv("Customer_agreement_subset_3.csv") #Read Customer Agreement Data

Gym_customer = read_csv("Gym_Customer.csv") #Read Gym Customer data

Registration = read_csv("B. Registration.csv")

#--------------------------------------------------------------------------------------------
#Finding number of attendances at each club
club_checkins = participant_checkin$club_num
club_nums = Club_site_expanded$club_src_num
club_attendences = c()
for(i in 1:length(club_nums)){ #For every club, find number of checkins in participant checkin data using which()
  indices = which(club_checkins == club_nums[i])
  club_attendences = c(club_attendences, length(indices))
}
Club_site_expanded$total_attendance = club_attendences

#Participant checkins reduced to 11664382 after excluding office checkins, unknown checkins, or checkins at clubs not in club site
sum(club_attendences)

#-------------------------------------------------------------------------------------------
#Excluding Corporate Offices

Club_site_1 = Club_site_expanded[-which(Club_site$club_src_num %in% c(1, 300, 400, 500)),] #Exclude corporate offices

#Excluding Zero Attendance Clubs

Club_site_2 = Club_site_expanded[-which(Club_site_expanded$total_attendance == 0), ] #Exclude those with zero attendance

#Clubs reduced to 446 after excluding those with 0 attendances

#--------------------------------------------------------------
#ESubset Checkin to 2010-2016 period

#Subsetting checkin
checkin_times = participant_checkin$checkin_datetime_localtime

participant_checkin_2 = participant_checkin[which(checkin_times > "2010-01-01 00:00:00-08:00"), ]
participant_checkin_2 = participant_checkin_2[which(checkin_times < "2017-01-01 00:00:00-08:00"), ]

#7,787,522 attendances between 2010-2017

#Counting attendances of each person
attendance_table = table(participant_checkin_2$participant_id) #Creates tables for each participant and associated frequencies
attendance_table = as.data.frame(attendance_table) #Create Dataframe

gym_member_subset = attendance_table

#-----------------------------------------------------------------------------------------------------------
#Creating new set

#Finding duration of memberships - type 17 (core memberships around which everything else is based)
Customer_agreement_types = Customer_agreement$agreement_type_id
Customer_agreement_statuses = Customer_agreement$agreement_status_id
Customer_agreement_ids = Customer_agreement$participant_id
indices = c()

for(i in 1:length(Customer_agreement_types)){
  if(Customer_agreement_types[i] == 17 & (Customer_agreement_statuses[i] == 3 | 
                                          Customer_agreement_statuses[i] == 5)){ #Agreement 3 are active, agreement 5 are terminations
    indices = c(indices, i) #Collect list of indices for agreements - can be done more efficiently using which() function
  }
}

core_customer_agreements = Customer_agreement[indices, ] #Subset customer agreements, leaving only core memberships that are active or terminated
core_customer_agreements = core_customer_agreements[, -c(8:11)]


#Cleaning dates - includes code to make sure they are all the same variable type (Date - since package for finding differences requires date variable type)
core_customer_agreements$start_date = as.Date("2010-01-01", "%Y-%m-%d") #Initialised with default min cutoff
core_customer_agreements$start_date = as.Date(core_customer_agreements$start_date, "%Y-%m-%d")
core_customer_agreements$end_date = as.Date("2017-01-01", "%Y-%m-%d") #Initialised with defaul max cutoff
core_customer_agreements$end_date = as.Date(core_customer_agreements$end_date, "%Y-%m-%d") 

core_customer_agreements$cleaned_agreement_sign_dt = 
  as.Date(core_customer_agreements$cleaned_agreement_sign_dt , "%Y-%m-%d") 

start_dates = as.Date(core_customer_agreements$cleaned_agreement_sign_dt)
start_dates2 = start_dates
end_dates = as.Date(core_customer_agreements$cleaned_termination_date)
end_dates2 = end_dates

#Censors membership start and end dates based on cutoff defined below
final_date = "2017-01-01" 
start_date = "2010-01-01"
new_dates = core_customer_agreements$start_date #new_dates = new start dates with censoring
for(i in 1:(length(start_dates))){
  if((start_dates[i] > start_date) & (start_dates[i] < final_date)){
    new_dates[i] = start_dates2[i]
  }
  else if(start_dates[i] > start_date){
    new_dates[i] = final_date
  }
}

new_dates_2 = core_customer_agreements$end_date #new_dates_2 = new end dates withe censoring
for(i in 1:(length(end_dates))){
  if(is.na(end_dates[i])){
    new_dates_2[i] = final_date
  }
  else if((end_dates[i] > start_date) & (end_dates[i] < final_date)){
    new_dates_2[i] = end_dates2[i]
  }
  else if(end_dates[i] < start_date){
    new_dates_2[i] = start_date
  }
  else{
    new_dates_2[i] = final_date
  }
}

core_customer_agreements$start_date = new_dates
core_customer_agreements$end_date = new_dates_2

core_customer_agreements$date_diff = difftime(core_customer_agreements$end_date, core_customer_agreements$start_date,
                                              units = "days") #Finds difference in time, in days, between start date and end dates for whole vector

core_customer_agreements$date_diff_month = core_customer_agreements$date_diff/30.44 #30.44 average lenth of a month


#Finding duration of memberships - core type 16 

#note algorithms same as above but used on upgrade data
Customer_agreement_types = Customer_agreement$agreement_type_id
Customer_agreement_statuses = Customer_agreement$agreement_status_id
Customer_agreement_ids = Customer_agreement$participant_id
indices = c()

for(i in 1:length(Customer_agreement_types)){
  if(Customer_agreement_types[i] == 16 & (Customer_agreement_statuses[i] == 3 | 
                                          Customer_agreement_statuses[i] == 5)){ #subset using only active or terminated types (most)
    indices = c(indices, i)
  }
}

upgrade_customer_agreements = Customer_agreement[indices, ]
upgrade_customer_agreements = upgrade_customer_agreements[, -c(8:11)]

upgrade_customer_agreements$start_date = as.Date("2010-01-01", "%Y-%m-%d")
upgrade_customer_agreements$start_date = as.Date(upgrade_customer_agreements$start_date, "%Y-%m-%d")
upgrade_customer_agreements$end_date = as.Date("2017-01-01", "%Y-%m-%d")
upgrade_customer_agreements$end_date = as.Date(upgrade_customer_agreements$end_date, "%Y-%m-%d")

upgrade_customer_agreements$cleaned_agreement_sign_dt = 
  as.Date(upgrade_customer_agreements$cleaned_agreement_sign_dt , "%Y-%m-%d")

start_dates = as.Date(upgrade_customer_agreements$cleaned_agreement_sign_dt)
start_dates2 = start_dates
end_dates = as.Date(upgrade_customer_agreements$cleaned_termination_date)
end_dates2 = end_dates

final_date = "2017-01-01" 
start_date = "2010-01-01"
new_dates = upgrade_customer_agreements$start_date

for(i in 1:(length(start_dates))){
  if((start_dates[i] > start_date) & (start_dates[i] < final_date)){
    new_dates[i] = start_dates2[i]
  }
  else if(start_dates[i] > start_date){
    new_dates[i] = final_date
  }
}
new_dates

new_dates_2 = upgrade_customer_agreements$end_date
for(i in 1:(length(end_dates))){
  if(is.na(end_dates[i])){
    new_dates_2[i] = final_date
  }
  else if((end_dates[i] > start_date) & (end_dates[i] < final_date)){
    new_dates_2[i] = end_dates2[i]
  }
  else if(end_dates[i] < start_date){
    new_dates_2[i] = start_date
  }
  else{
    new_dates_2[i] = final_date
  }
}

upgrade_customer_agreements$start_date = new_dates
upgrade_customer_agreements$end_date = new_dates_2

upgrade_customer_agreements$date_diff = difftime(upgrade_customer_agreements$end_date, 
                                                 upgrade_customer_agreements$start_date,
                                                 units = "days")

upgrade_customer_agreements$date_diff_month = upgrade_customer_agreements$date_diff/30.44

#Exclude participants with attendances no attedances in range 

#NOTE
#
#
#WILL CHANGE THIS - SHOULD GIVE PARTICIPANTS WITH NO ATTENDANCES AN ATTENDANCE OF '1' FOR BETTER ESTIMATION 
#
#AS IS, WILL BIAS TOWARDS EXTREME VALUES

core_customer_agreements_2 = core_customer_agreements[-which(core_customer_agreements$date_diff == 0), ] #Remove agreements that are censored at either end, or start and end on the same day

#Use table from subset of checkin data to check attendance numbers - find total attendance numbers
attendances = c()
attendance_frequencies = attendance_table$Freq
attendance_pid = attendance_table$Var1
participants = core_customer_agreements_2$participant_id
for(i in 1:length(participants)){
  index = which(attendance_pid %in% participants[i])
  if(length(index) > 0){
    attendances = c(attendances, attendance_frequencies[index])
  }
  else{
    attendances = c(attendances, 0)
  }
}

core_customer_agreements_2$total_attendance = attendances 

core_customer_agreements_2$average_attendance = (attendances)/as.numeric(core_customer_agreements_2$date_diff_month)

sum(core_customer_agreements_2$average_attendance)/25481

#Finding number of attendances
p_ids = core_customer_agreements_2$participant_id
participant_checkin_ids = participant_checkin_2$participant_id
p_start_dates = core_customer_agreements_2$start_date
p_end_dates = core_customer_agreements_2$end_date
participant_checkin_times = as.Date(participant_checkin_2$checkin_datetime_localtime , "%Y-%m-%d")
attendance = core_customer_agreements_2$total_attendance
for(i in 1:length(p_ids)){
  subset_indices = which(participant_checkin_ids == p_ids[i])
  subset_ids = participant_checkin_ids[subset_indices]
  subset_checkin_times = participant_checkin_times[subset_indices]
  subset_ids = subset_ids[which(subset_checkin_times > p_start_dates[i] & subset_checkin_times < p_end_dates[i])]
  attendance[i] = length(subset_ids)
}

core_customer_agreements_2$total_attendance = attendance
core_customer_agreements_2$average_attendance = (attendance)/as.numeric(core_customer_agreements_2$date_diff_month)

#Descriptive stats and Histograms for attendances 
#-----------------------------------------------------------------

#Descriptive stats for attendances
average_monthly_attendance = mean(core_customer_agreements_2$average_attendance) #N equals 25481
median_monthly_attendance = median(core_customer_agreements_2$average_attendance)
sd_monthly_attendance = sd(core_customer_agreements_2$average_attendance)

# add a normal distribution line in histogram
hist(core_customer_agreements_2$average_attendance, breaks = 47, freq=FALSE, col="gray", 
     xlab="Average Monthly Gym Attendance", main="Average Monthly Gym Attendance Distribution")
curve(dnorm(x, mean=mean(core_customer_agreements_2$average_attendance),
            sd(core_customer_agreements_2$average_attendance)), add=TRUE, col="red") #line

# add a normal distribution line in histogram
hist(core_customer_agreements_2$average_attendance[which(core_customer_agreements_2$date_diff_month > 1)], #excluding memberships less than 1 month in length
     breaks = 47, freq=FALSE, col="gray", 
     xlab="Average Monthly Gym Attendance", main="Average Monthly Gym Attendance Distribution")
curve(dnorm(x, mean=mean(core_customer_agreements_2$average_attendance),
            sd(core_customer_agreements_2$average_attendance)), add=TRUE, col="red") #line

mean(core_customer_agreements_2$average_attendance[which(core_customer_agreements_2$date_diff_month > 1)])

#-----------------------------------------------------------------------------------------------
#Finding Average cost of attendance

#Initialising variables with defaults
core_customer_agreements_2$upgrade_start_date = as.Date("2017-01-01", "%Y-%m-%d")
core_customer_agreements_2$upgrade_end_date = as.Date("2017-01-01", "%Y-%m-%d")
core_customer_agreements_2$upgrade_boolean = 0
core_customer_agreements_2$upgrade_duration = 0
core_customer_agreements_2$upgrate_duration_months = 0
core_customer_agreements_2$upgrade_duration_perc = core_customes_agreements_2$upgrade_duration/core_customer_agreements_2$date_diff

upgrade_members = unique(upgrade_customer_agreements$participant_id)

#Add upgrade boolean to customer agreement sheet
participant_ids = core_customer_agreements_2$participant_id
for(i in 1:length(core_customer_agreements_2$upgrade_boolean)){
  if(participant_ids[i] %in% upgrade_members){
    core_customer_agreements_2$upgrade_boolean[i] = 1
  }
}

#
upgrade_members_boolean = core_customer_agreements_2$upgrade_boolean
upgrade_participant_ids = upgrade_customer_agreements$participant_id
date_diff = upgrade_customer_agreements$date_diff
upgrade_duration_all = core_customer_agreements_2$upgrade_duration
for(i in 1:length(upgrade_members_boolean)){
  upgrade_duration = 0
  if(upgrade_members_boolean[i] == 1){
    indices = which(upgrade_participant_ids == participant_ids[i])
    for(j in 1:length(indices)){
      upgrade_duration = upgrade_duration + date_diff[indices[j]]
    }
    upgrade_duration_all[i] = upgrade_duration
  }
}

#Finding number of participants who attended only a single club
participant_ids = unique(core_customer_agreements_2$participant_id)
participant_checkin_ids = participant_checkin_2$participant_id
single_club = c()
for(i in 1:length(participant_ids)){
  unique_clubs = unique(participant_checkin_2[which(participant_checkin_ids == participant_ids[i]), club_num])
  if(length(unique_clubs) == 1){
    single_club = c(single_club, participant_ids[i])
  }
}

single_club #N = 4769 only attended a single club, rest of sample attended multiple clubs

upgrade_customer_agreements_2 = upgrade_customer_agreements[which(upgrade_customer_agreements$date_diff > 0), ] #remove upgrades with 0 day durations (may be due to censoring)

upgrade_customer_agreements_2 = upgrade_customer_agreements_2[-which(duplicated(upgrade_customer_agreements_2) == TRUE), ] #Remove any duplicates

upgrade_customer_agreements_3 = upgrade_customer_agreements_2[-which(upgrade_customer_agreements_2$participant_id
                                                                     %in% two_plus_upgrades), ] 

#two_plus_upgrades variable generation appears lost... just checked for individuals with more than two upgrade entries
#likely I didn't use it - as no reason to exclude upgrade memberships with more than 2 entries

#Estimation of upgrade durations
#
#THIS NEEDS WORK - NOT HAPPY WITH HOW THIS IS DONE
#
#Shouldn't have big effect on results, but I think this is inaccurate. Trouble with overlapping upgrades
upgrade_participant_ids = upgrade_customer_agreements_3$participant_id
participant_ids = core_customer_agreements_2$participant_id
for(i in 1:length(upgrade_participant_ids)){
  index = which(participant_ids == upgrade_participant_ids[i])
  core_customer_agreements_2[index, "upgrade_duration"] = upgrade_customer_agreements_3$date_diff[i]
}

core_customer_agreements_2$upgrate_duration_months = core_customer_agreements_2$upgrade_duration/30.4375

#Initialising variables for total cost estimation
total_cost = rep(0, 25481)
membership_durations = core_customer_agreements_2$date_diff_month
participant_ids = core_customer_agreements_2$participant_id
participant_checkin_ids = participant_checkin_2$participant_id
gym_customer_ids = Gym_customer$participant_id
club_src_nums = Club_site_expanded$club_src_num[-which(is.na(Club_site_expanded$monthly_attendance_cost))]
monthly_cost = Club_site_expanded$monthly_attendance_cost[-which(is.na(Club_site_expanded$monthly_attendance_cost))]
tax = Club_site_expanded$tax[-which(is.na(Club_site_expanded$monthly_attendance_cost))]

#TOTAL COST ESTIMATION ALGORITHM - See diagram for more info
for(i in 1:length(participant_ids)){
  if(membership_durations[i] > 2){
    club_nums = unique(participant_checkin_2$club_num[which(participant_checkin_ids == participant_ids[i])])
    max_monthly_attendance_cost = 0
    temp_max_monthly_attendance_cost = 0
    if(length(club_nums) > 0){
      for(j in 1:length(club_nums)){
        if(club_nums[j] %in% club_src_nums){
          temp_max_monthly_attendance_cost = monthly_cost[which(club_src_nums == club_nums[j])] + tax[which(club_src_nums == club_nums[j])]
          club_index = which(club_src_nums %in% club_nums[j])
          if(temp_max_monthly_attendance_cost > max_monthly_attendance_cost){
            max_monthly_attendance_cost = temp_max_monthly_attendance_cost
          }
        }
        else if(36.99 > max_monthly_attendance_cost){
          max_monthly_attendance_cost = 36.99
        }
      } 
    }
    else{
      max_monthly_attendance_cost = 36.99
    }
    
    if(participant_ids[i] %in% upgrade_customer_agreements_3$participant_id){
      max_monthly_attendance_cost = max_monthly_attendance_cost - 10*(1-(as.numeric(core_customer_agreements_2[i, "upgrade_duration"]))/as.numeric((core_customer_agreements_2[i, "date_diff"])))
    }
    
    total_individual_cost = (max_monthly_attendance_cost * ceiling(as.numeric(membership_durations[i]))) + (ceiling((as.numeric(membership_durations[i])) - 3)%/%12)*49.99
    total_cost[i] = total_individual_cost
  }
  else{
    total_cost[i] = 0
  }
}

core_customer_agreements_2$total_cost = total_cost
core_customer_agreements_2$average_monthly_cost = total_cost/ceiling(as.numeric(core_customer_agreements_2$date_diff_month))
core_customer_agreements_2$average_cost_attendance = (core_customer_agreements_2$average_monthly_cost)/(core_customer_agreements_2$average_attendance)

average_cost_attendances = core_customer_agreements_2$average_cost_attendance[-which(core_customer_agreements_2$average_cost_attendance %in% c(Inf, 0, NaN))] #N = 23855


#Descriptive Stats for average cost of attendance
#------------------------------------------------------------------------
quantile(average_cost_attendances, c(.05, .95)) 

mean(average_cost_attendances)
median(average_cost_attendances)

mean(average_cost_attendances[which(average_cost_attendances > 2.080549 & average_cost_attendances < 1036.474110)])

var(average_cost_attendances)

sd(average_cost_attendances)

#Plotting Results
hist(average_cost_attendances, breaks = 2500, freq=TRUE, col="gray", 
     xlim=c(0, 100),
     xlab="Average Cost per Gym Attendance ($)", 
     main="Average Cost per Gym Attendance")
curve(dnorm(x, mean=mean(average_cost_attendances),
            sd(average_cost_attendances)), add=TRUE, col="red") #line


#-------------------------------------------------------------------------

#Daily Pass costs

core_customer_agreements_3 = core_customer_agreements_2[-which(core_customer_agreements_2$average_cost_attendance %in% c(Inf, 0, NaN)), ]
participant_ids = core_customer_agreements_3$participant_id
day_pass_costs_average = rep(0, 23855)
participant_checkin_ids = participant_checkin_2$participant_id
participant_checkin_clubs = participant_checkin_2$club_types
start_dates = core_customer_agreements_3$start_date
end_dates = core_customer_agreements_3$end_date
participant_checkin_times = as.Date(participant_checkin_2$checkin_datetime_localtime , "%Y-%m-%d")

for(i in 1:length(participant_ids)){
  club_attendances = participant_checkin_clubs[which(participant_checkin_ids == participant_ids[i] &
                                                       participant_checkin_times > start_dates[i] &
                                                       participant_checkin_times < end_dates[i])]
  if(length(club_attendances) > 0){
    if("Unknown" %in% club_attendances | "500" %in% club_attendances){
      day_pass_costs_average[i] = NA
    }
    else{
      club_sum = sum(as.numeric(club_attendances))
      total_day_pass_cost = (length(club_attendances)*10) + club_sum*5
      day_pass_costs_average[i] = (total_day_pass_cost)/(length(club_attendances))
    }
  }
}
max(day_pass_costs_average[which(day_pass_costs_average > 30)])

day_pass_costs_average_noNA = day_pass_costs_average[-which(is.na(day_pass_costs_average) == TRUE)]
max(day_pass_costs_average_noNA)

core_customer_agreements_3$day_pass_costs = day_pass_costs_average
core_customer_agreements_3$day_pass_comparison = 0
core_customer_agreements_3$day_pass_comparison[which(is.na(core_customer_agreements_3$day_pass_costs))] = NA
core_customer_agreements_3$day_pass_comparison[which(core_customer_agreements_3$average_cost_attendance_2 > 
                                                       core_customer_agreements_3$day_pass_costs)] = 1

sum(core_customer_agreements_3$day_pass_comparison)
sum(core_customer_agreements_3$day_pass_comparison[-which(is.na(core_customer_agreements_3$day_pass_comparison))])

which(core_customer_agreements_3$day_pass_comparison %in% c(0, 1))

hist(core_customer_agreements_3$day_pass_costs[-which(is.na(core_customer_agreements_3$day_pass_costs))], breaks = c(15,20,25,30), freq=TRUE, col="gray", 
     xlim=c(0, 100),
     xlab="Average Day Pass Cost per Gym Attendance ($)", 
     main="Average Day Pass Cost per Gym Attendance")
curve(dnorm(x, mean=mean(average_cost_attendances),
            sd(average_cost_attendances)), add=TRUE, col="red") #line

mean(core_customer_agreements_3$day_pass_costs[-which(is.na(core_customer_agreements_3$day_pass_costs))])
median(core_customer_agreements_3$day_pass_costs[-which(is.na(core_customer_agreements_3$day_pass_costs))])
var(customer_agreements_2$day_pass_costs[-which(is.na(customer_agreements_2$day_pass_costs))])
sqrt(var(customer_agreements_2$day_pass_costs[-which(is.na(customer_agreements_2$day_pass_costs))]))

mean(core_customer_agreements_2$average_monthly_cost[which(core_customer_agreements_2$date_diff_month > 2)])
median(core_customer_agreements_2$average_monthly_cost[which(core_customer_agreements_2$date_diff_month > 2)])
max(core_customer_agreements_2$average_monthly_cost[which(core_customer_agreements_2$date_diff_month > 2)])
var(core_customer_agreements_2$average_monthly_cost[which(core_customer_agreements_2$date_diff_month > 2)])
sqrt(var(core_customer_agreements_2$average_monthly_cost[which(core_customer_agreements_2$date_diff_month > 2)]))


customer_agreements_2 = read_csv("Customer_agreement_subset_2.csv")

core_customer_agreements_3 = customer_agreements_2[-which(is.na(core_customer_agreements_3$day_pass_costs)), ]

#CREATING PANEL OF HISTOGRAMS

par(mfcol = c(2, 2))
hist(core_customer_agreements_3$average_attendance, breaks = 47, freq=TRUE, col="gray", 
     ylim = c(0, 5000),
     xlab="Average Monthly Gym Attendance", main="Average Monthly Gym Attendance")

hist(core_customer_agreements_3$average_monthly_cost_2, breaks = 20, freq=TRUE, col="gray", 
     xlim=c(0, 100),
     ylim = c(0, 5000),
     xlab="Average Monthly Cost ($)", 
     main="Average Monthly Cost")

hist(core_customer_agreements_3$average_cost_attendance_2, breaks = 1750, freq=TRUE, col="gray", 
     xlim=c(0, 100),
     ylim = c(0, 5000),
     xlab="Average Cost per Gym Attendance ($)", 
     main="Average Cost per Gym Attendance")

hist(core_customer_agreements_3$day_pass_costs, breaks = c(15,20,25,30), freq=TRUE, col="gray", 
     xlim=c(0, 100),
     ylim = c(0, 10000),
     xlab="Average Day Pass Cost per Gym Attendance ($)", 
     main="Average Day Pass Cost")

#Descriptive statistics
mean(core_customer_agreements_3$average_attendance)
median(core_customer_agreements_3$average_attendance)
max(core_customer_agreements_3$average_attendance)
min(core_customer_agreements_3$average_attendance)
var(core_customer_agreements_3$average_attendance)
sqrt(var(core_customer_agreements_3$average_attendance))

mean(core_customer_agreements_3$average_monthly_cost_2)
median(core_customer_agreements_3$average_monthly_cost_2)
max(core_customer_agreements_3$average_monthly_cost_2)
min(core_customer_agreements_3$average_monthly_cost_2)
var(core_customer_agreements_3$average_monthly_cost_2)
sqrt(var(core_customer_agreements_3$average_monthly_cost_2))


mean(core_customer_agreements_3$average_cost_attendance_2)
median(core_customer_agreements_3$average_cost_attendance_2)
max(core_customer_agreements_3$average_cost_attendance_2)
min(core_customer_agreements_3$average_cost_attendance_2)
var(core_customer_agreements_3$average_cost_attendance_2)
sqrt(var(core_customer_agreements_3$average_cost_attendance_2))

quantile(core_customer_agreements_3$average_cost_attendance_2, c(.1, .9)) 

membership_durations = core_customer_agreements_3$date_diff
upgrade_durations = core_customer_agreements_3$upgrade_duration

for(i in 1:length(core_customer_agreements_3$participant_id)){
  if(upgrade_durations[i] > membership_durations[i]){
    core_customer_agreements_3$upgrade_duration[i] = membership_durations[i]
  }
}

total_cost = rep(0, 17938)
membership_durations = core_customer_agreements_3$date_diff_month
participant_ids = core_customer_agreements_3$participant_id
participant_checkin_ids = participant_checkin_2$participant_id
gym_customer_ids = Gym_customer$participant_id
club_src_nums = Club_site_expanded$club_src_num[-which(is.na(Club_site_expanded$monthly_attendance_cost))]
monthly_cost = Club_site_expanded$monthly_attendance_cost[-which(is.na(Club_site_expanded$monthly_attendance_cost))]
tax = Club_site_expanded$tax[-which(is.na(Club_site_expanded$monthly_attendance_cost))]

for(i in 1:length(participant_ids)){
  club_nums = unique(participant_checkin_2$club_num[which(participant_checkin_ids == participant_ids[i])])
  max_monthly_attendance_cost = 0
  temp_max_monthly_attendance_cost = 0
  if(length(club_nums) > 0){
    for(j in 1:length(club_nums)){
      if(club_nums[j] %in% club_src_nums){
        temp_max_monthly_attendance_cost = monthly_cost[which(club_src_nums == club_nums[j])] + tax[which(club_src_nums == club_nums[j])]
        club_index = which(club_src_nums %in% club_nums[j])
        if(temp_max_monthly_attendance_cost > max_monthly_attendance_cost){
          max_monthly_attendance_cost = temp_max_monthly_attendance_cost
        }
      }
      else if(36.99 > max_monthly_attendance_cost){
        max_monthly_attendance_cost = 36.99
      }
    } 
    
    if(core_customer_agreements_3$upgrade_duration[i] > 0){
      max_monthly_attendance_cost = max_monthly_attendance_cost - 10*(1-(as.numeric(core_customer_agreements_3[i, "upgrade_duration"]))/as.numeric((core_customer_agreements_3[i, "date_diff"])))
    }
    
    total_individual_cost = (max_monthly_attendance_cost * ceiling(as.numeric(membership_durations[i]))) + (ceiling((as.numeric(membership_durations[i])) - 3)%/%12)*49.99
    total_cost[i] = total_individual_cost
  }
}

max(total_cost/core_customer_agreements_3$date_diff_month)
which((total_cost/core_customer_agreements_3$date_diff_month) == 125.182)

core_customer_agreements_3$average_monthly_cost_2 = total_cost/ceiling(core_customer_agreements_3$date_diff_month)
core_customer_agreements_3$average_cost_attendance_2 = (core_customer_agreements_3$average_monthly_cost_2)/(core_customer_agreements_3$average_attendance)


#--------------------------------------------------------------------
#Cancellation Lags
customer_agreements_3 = read_csv("Customer_agreement_subset_3.csv")
customer_agreements_3 = customer_agreements_3[, -c(1,2)]

participant_checkin_2 = participant_checkin[which(participant_checkin$participant_id %in% customer_agreements_3$participant_id
                                                  & participant_checkin$checkin_datetime_localtime < "2017-01-01"
                                                  & participant_checkin$checkin_datetime_localtime > "2010-01-01")]

cancel_participant_ids = customer_agreements_3$participant_id[which(!is.na(customer_agreements_3$cleaned_termination_date)
                                                                    & customer_agreements_3$cleaned_termination_date < "2017-01-01")]
difftime = rep(0, length(cancel_participant_ids))
cost_of_lag = rep(0, length(cancel_participant_ids))

for(i in 1:length(cancel_participant_ids)){
  termination_date = max(customer_agreements_3$cleaned_termination_date[which(customer_agreements_3$participant_id == cancel_participant_ids[i] & 
                                                                                !is.na(customer_agreements_3$cleaned_termination_date))])
  
  subset_checkins = format(strptime(participant_checkin_2$checkin_datetime_localtime[which(participant_checkin_2$participant_id == cancel_participant_ids[i])],
                                    "%Y-%m-%d %H:%M:%S-%H:%M"), format = "%Y-%m-%d")
  subset_checkins = subset_checkins[which(subset_checkins < termination_date)]
  
  if(length(subset_checkins) == 0){
    subset_checkins = format(strptime(participant_checkin_2$checkin_datetime_localtime[which(participant_checkin_2$participant_id == cancel_participant_ids[i])],
                                      "%Y-%m-%d %H:%M:%S+%H:%M"), format = "%Y-%m-%d")
    subset_checkins = subset_checkins[which(subset_checkins < termination_date)]
  }
  difftime_days = difftime(termination_date, max(subset_checkins),
                           units = "days")
  difftime_months = difftime_days/30.44
  difftime[i] = difftime_months
  
  cost_of_lag[i] = floor(difftime[i]) * customer_agreements_3$average_monthly_cost_2[which(customer_agreements_3$participant_id == cancel_participant_ids[i] & 
                                                                                             customer_agreements_3$cleaned_termination_date == termination_date)]
}


customer_agreements_3$cancel_lag[which(customer_agreements_3$end_date == "2017-01-01")] = NA
indices = which(!is.na(customer_agreements_3$cleaned_termination_date) &
                  customer_agreements_3$cleaned_termination_date < "2017-01-01")
for(i in 1:length(cancel_participant_ids)){
  customer_agreements_3$cancel_lag[indices[i]] = difftime[i]
  customer_agreements_3$lag_cost[indices[i]] = cost_of_lag[i]
}

customer_agreements_3$lag_cost[which(is.na(customer_agreements_3$cancel_lag))] = NA

length(cost_of_lag[which(is.na(cost_of_lag))])

difftime = difftime[-which(is.na(difftime))]

#Cancellation Lags Analysis

mean(difftime)
mean(difftime)
median(difftime)
var(difftime)
sqrt(var(difftime))
quantile(difftime, c(0.25, 0.75))
mean(difftime[which(difftime < 24)])
max(difftime)
min(difftime)
length(difftime[which(difftime > 1)])

hist(difftime, breaks = 500, freq=TRUE, col="gray", 
     xlim = c(0, 2),
     xlab="Duration from last attendance to termination (months)", 
     main="Duration from Last Attendance to Termination")

hist(difftime, breaks = 100, freq=TRUE, col="gray", 
     xlim = c(0, 85),
     xlab="Duration from last attendance to termination (months)", 
     main="Duration from Last Attendance to Termination")

cost_of_lag = cost_of_lag[-which(is.na(cost_of_lag))]

mean(cost_of_lag)
median(cost_of_lag)
var(cost_of_lag)
sqrt(var(cost_of_lag))
quantile(cost_of_lag, c(0.25, 0.75))
max(cost_of_lag)
min(cost_of_lag)

hist(cost_of_lag, breaks = 1000, freq=FALSE, col="gray",
     xlim = c(30, 1000),
     ylim = c(0, 0.006),
     xlab="Cost of Termination Lag ($)", 
     main="Cost of Termination Lags")
lines(density(cost_of_lag, adjust=2), col="red")   # add another "smoother" density

for(i in 1:length(cancel_participant_ids)){
  customer_agreements_3[which(customer_agreements_3$participant_id == cancel_participant_ids[i]), "cancel_lag"] = difftime[i]
}

customer_agreements_3$cancel_lag[which(is.na(customer_agreements_3$cleaned_termination_date))] = NA
#--------------------------------------------------------------------------------
#Declining Monthly Attendance

customer_agreements_4 = customer_agreements_3[which(customer_agreements_3$cleaned_agreement_sign_dt >= "2010-01-01"),]

customer_agreements_4$attendance_month_1 = NA
customer_agreements_4$attendance_month_2 = NA
customer_agreements_4$attendance_month_3 = NA
customer_agreements_4$attendance_month_4 = NA
customer_agreements_4$attendance_month_5 = NA
customer_agreements_4$attendance_month_6 = NA
customer_agreements_4$attendance_month_7 = NA
customer_agreements_4$attendance_month_8 = NA
customer_agreements_4$attendance_month_9 = NA
customer_agreements_4$attendance_month_10 = NA
customer_agreements_4$attendance_month_11 = NA
customer_agreements_4$attendance_month_12 = NA

participant_checkin_2 = participant_checkin_2[which(participant_checkin_2$checkin_datetime_localtime < "2017-01-01"
                                                    & participant_checkin_2$checkin_datetime_localtime >= "2010-01-01"),]


participant_checkin_2$clean_date = format(strptime(participant_checkin_2$checkin_datetime_localtime,
                                                   "%Y-%m-%d %H:%M:%S-%H:%M"), format = "%Y-%m-%d")
participant_checkin_2$clean_date = as.Date(participant_checkin_2$clean_date)

participant_ids = customer_agreements_4$participant_id

library(lubridate)

for(i in 1:length(participant_ids)){
  subset = participant_checkin_2$clean_date[which(participant_checkin_2$participant_id == participant_ids[i])]
  durations = c(customer_agreements_3$date_diff_month[i], 12)
  max_duration = floor(min(durations))
  date_1 = as.Date(customer_agreements_4$start_date[which(customer_agreements_4$participant_id == participant_ids[i])])
  date_2 = date_1
  for(j in 1:max_duration){
    month(date_2) = month(date_1) + 1
    customer_agreements_4[i, paste0("attendance_month_", as.character(j))] = 
      length(subset[which(subset > date_1 & subset < date_2)])
    date_1 = date_2
  }
}

#Declining monthly attendance analysis

median(customer_agreements_4$attendance_month_1)
median(customer_agreements_4$attendance_month_2)
median(customer_agreements_4$attendance_month_3[-which(is.na(customer_agreements_4$attendance_month_3))])
median(customer_agreements_4$attendance_month_4[-which(is.na(customer_agreements_4$attendance_month_4))])
median(customer_agreements_4$attendance_month_5[-which(is.na(customer_agreements_4$attendance_month_5))])
median(customer_agreements_4$attendance_month_6[-which(is.na(customer_agreements_4$attendance_month_6))])
median(customer_agreements_4$attendance_month_7[-which(is.na(customer_agreements_4$attendance_month_7))])
median(customer_agreements_4$attendance_month_8[-which(is.na(customer_agreements_4$attendance_month_8))])
median(customer_agreements_4$attendance_month_9[-which(is.na(customer_agreements_4$attendance_month_9))])
median(customer_agreements_4$attendance_month_10[-which(is.na(customer_agreements_4$attendance_month_10))])
median(customer_agreements_4$attendance_month_11[-which(is.na(customer_agreements_4$attendance_month_11))])
median(customer_agreements_4$attendance_month_12[-which(is.na(customer_agreements_4$attendance_month_12))])

mean(customer_agreements_4$average_monthly_cost_2)
mean(customer_agreements_4$average_monthly_cost_2)
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_3))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_4))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_5))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_6))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_7))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_8))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_9))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_10))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_11))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_12))])


mean(customer_agreements_4$average_monthly_cost_2)
mean(customer_agreements_4$average_monthly_cost_2)
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_3))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_4))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_5))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_6))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_7))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_8))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_9))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_10))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_11))])
mean(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_12))])


sd(customer_agreements_4$average_monthly_cost_2)
sd(customer_agreements_4$average_monthly_cost_2)
sd(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_3))])
sd(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_4))])
sd(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_5))])
sd(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_6))])
sd(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_7))])
sd(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_8))])
sd(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_9))])
sd(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_10))])
sd(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_11))])
sd(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_12))])


length(customer_agreements_4$average_monthly_cost_2)
length(customer_agreements_4$average_monthly_cost_2)
length(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_3))])
length(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_4))])
length(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_5))])
length(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_6))])
length(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_7))])
length(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_8))])
length(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_9))])
length(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_10))])
length(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_11))])
length(customer_agreements_4$average_monthly_cost_2[-which(is.na(customer_agreements_4$attendance_month_12))])

customer_agreements_4$price_attendance_month_1 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_1)
customer_agreements_4$price_attendance_month_2 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_2)
customer_agreements_4$price_attendance_month_3 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_3)
customer_agreements_4$price_attendance_month_4 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_4)
customer_agreements_4$price_attendance_month_5 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_5)
customer_agreements_4$price_attendance_month_6 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_6)
customer_agreements_4$price_attendance_month_7 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_7)
customer_agreements_4$price_attendance_month_8 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_8)
customer_agreements_4$price_attendance_month_9 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_9)
customer_agreements_4$price_attendance_month_10 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_10)
customer_agreements_4$price_attendance_month_11 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_11)
customer_agreements_4$price_attendance_month_12 = (customer_agreements_4$average_monthly_cost_2)/(customer_agreements_4$attendance_month_12)

customer_agreements_4$price_attendance_month_1[which(customer_agreements_4$price_attendance_month_1 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_1 %in% c(Inf))]
customer_agreements_4$price_attendance_month_2[which(customer_agreements_4$price_attendance_month_2 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_2 %in% c(Inf))]
customer_agreements_4$price_attendance_month_3[which(customer_agreements_4$price_attendance_month_3 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_3 %in% c(Inf))]
customer_agreements_4$price_attendance_month_4[which(customer_agreements_4$price_attendance_month_4 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_4 %in% c(Inf))]
customer_agreements_4$price_attendance_month_5[which(customer_agreements_4$price_attendance_month_5 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_5 %in% c(Inf))]
customer_agreements_4$price_attendance_month_6[which(customer_agreements_4$price_attendance_month_6 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_6 %in% c(Inf))]
customer_agreements_4$price_attendance_month_7[which(customer_agreements_4$price_attendance_month_7 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_7 %in% c(Inf))]
customer_agreements_4$price_attendance_month_8[which(customer_agreements_4$price_attendance_month_8 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_8 %in% c(Inf))]
customer_agreements_4$price_attendance_month_9[which(customer_agreements_4$price_attendance_month_9 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_9 %in% c(Inf))]
customer_agreements_4$price_attendance_month_10[which(customer_agreements_4$price_attendance_month_10 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_10 %in% c(Inf))]
customer_agreements_4$price_attendance_month_11[which(customer_agreements_4$price_attendance_month_11 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_11 %in% c(Inf))]
customer_agreements_4$price_attendance_month_12[which(customer_agreements_4$price_attendance_month_12 %in% c(Inf))] = customer_agreements_4$average_monthly_cost_2[which(customer_agreements_4$price_attendance_month_12 %in% c(Inf))]

mean(customer_agreements_4$attendance_month_1)
mean(customer_agreements_4$attendance_month_2)
mean(customer_agreements_4$attendance_month_3[-which(is.na(customer_agreements_4$attendance_month_3))])
mean(customer_agreements_4$attendance_month_4[-which(is.na(customer_agreements_4$attendance_month_4))])
mean(customer_agreements_4$attendance_month_5[-which(is.na(customer_agreements_4$attendance_month_5))])
mean(customer_agreements_4$attendance_month_6[-which(is.na(customer_agreements_4$attendance_month_6))])
mean(customer_agreements_4$attendance_month_7[-which(is.na(customer_agreements_4$attendance_month_7))])
mean(customer_agreements_4$attendance_month_8[-which(is.na(customer_agreements_4$attendance_month_8))])
mean(customer_agreements_4$attendance_month_9[-which(is.na(customer_agreements_4$attendance_month_9))])
mean(customer_agreements_4$attendance_month_10[-which(is.na(customer_agreements_4$attendance_month_10))])
mean(customer_agreements_4$attendance_month_11[-which(is.na(customer_agreements_4$attendance_month_11))])
mean(customer_agreements_4$attendance_month_12[-which(is.na(customer_agreements_4$attendance_month_12))])


#-------------------------------------------------------------------------------------------------------------
#Cancellation Lag and Monthly Attendance

dataset = customer_agreements_4[which(!is.na(customer_agreements_4$cancel_lag)),]
regressor = lm(formula = log(floor(cancel_lag)+1) ~ log(price_attendance_month_1 + 1),
               data = dataset)
summary(regressor)

library(ggplot2)
ggplot() +
  geom_point(aes(x = log(dataset$price_attendance_month_1 + 1), y = log(floor(dataset$cancel_lag) + 1)),
             colour = 'red') +
  geom_line(aes(x = log(dataset$price_attendance_month_1 + 1), y = predict(regressor)),
            colour = 'blue', size = 1.5) +
  ggtitle('Cancel Lag vs. Price per Attendance Month 1') +
  xlab('Log( Price per Attendance Month 1 ($) + 1)') +
  ylab('Log( Cancel Lag (months) + 1 )')

ggplot(regressor) + 
  geom_point(aes(x=.fitted, y=.stdresid))

hist(log(dataset$cancel_lag), breaks = 100, freq=TRUE, col="gray", 
     xlab="Duration from last attendance to termination (months)", 
     main="Duration from Last Attendance to Termination")

corr <- cor.test(x=log(dataset$price_attendance_month_1 + 1), 
                 y=log(dataset$cancel_lag + 1), method = 'pearson')

corr <- cor.test(x=log(dataset$price_attendance_month_4[which(!is.na(dataset$price_attendance_month_4))] + 1), 
                 y=log(dataset$cancel_lag[which(!is.na(dataset$attendance_month_4))] + 1), method = 'pearson')

dataset = customer_agreements_4[which(!is.na(customer_agreements_4$cancel_lag) &
                                        !is.na(customer_agreements_4$price_attendance_month_4)),]
regressor = lm(formula = log(floor(cancel_lag)+1) ~ log(price_attendance_month_4 + 1),
               data = dataset)
summary(regressor)

library(ggplot2)
ggplot() +
  geom_point(aes(x = log(dataset$price_attendance_month_4 + 1), y = log(floor(dataset$cancel_lag) + 1)),
             colour = 'red') +
  geom_line(aes(x = log(dataset$price_attendance_month_4 + 1), y = predict(regressor)),
            colour = 'blue', size = 1.5) +
  ggtitle('Cancel Lag vs. Price per Attendance Month 4') +
  xlab('Log( Average Price Per Attendance ($) + 1 )') +
  ylab('Log( Cancel Lag (months) + 1 )')

ggplot(regressor) + 
  geom_point(aes(x=.fitted, y=.stdresid))

#-----------------------------------------------------------------------------------------------------
#Gender of Gym Members

Customer_agreement$gender = "I"

for(i in 1:nrow(Customer_agreement)){
  gender = Gym_customer$gender[which(Gym_customer$participant_id == Customer_agreement$participant_id[i])]
  gender_predict = Gym_customer$gender_predict[which(Gym_customer$participant_id == Customer_agreement$participant_id[i])]
  if(gender == "I"){
    if(!is.na(gender_predict)){
      Customer_agreement$gender[i] = gender_predict
    }
  }
  else{
    Customer_agreement$gender[i] = gender
  }
}

Customer_agreement$gender[which(Customer_agreement$gender == "N")] = "I"

library(plyr)
genders = table(Customer_agreement$gender)
gender = as.data.frame(genders)

#---------------------------------------------------------------------------------------
#Age at sign-up
Registration = read_csv("B. Registration.csv")

Registration$date = format(strptime(Registration$signup_datetime_localtime,
                                    "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")

Customer_agreement$age = 0

for(i in 1:nrow(Customer_agreement)){
  age = Registration$age[which(Registration$participant_id == Customer_agreement$participant_id[i])]
  Customer_agreement$age[i] = age
}

length(which(Customer_agreement$age != 0))
mean(Customer_agreement$age[which(Customer_agreement$age != 0)])
sd(Customer_agreement$age[which(Customer_agreement$age != 0)])

Customer_agreement$diff_age = 0
for(i in 1:nrow(Customer_agreement)){
  index = which(Registration$participant_id == Customer_agreement$participant_id[i])
  diff_age = difftime(Registration$date[index], Customer_agreement$cleaned_agreement_sign_dt[i])
  Customer_agreement$diff_age[i] = diff_age
}
Customer_agreement$diff_age = Customer_agreement$diff_age/365.25

Customer_agreement$age_signup = 0
Customer_agreement$age_signup = Customer_agreement$age - Customer_agreement$diff_age

mean(Customer_agreement$age_signup[which(Customer_agreement$age_signup != 0)])
sd(Customer_agreement$age_signup[which(Customer_agreement$age_signup != 0)])

hist(Customer_agreement$age_signup[which(Customer_agreement$age_signup != 0)], breaks = 50, freq=FALSE, col="gray", 
     xlab="Age", main="Age Distribution")
curve(dnorm(x, mean=mean(Customer_agreement$age_signup[which(Customer_agreement$age_signup != 0)]),
            sd(Customer_agreement$age_signup[which(Customer_agreement$age_signup != 0)])), add=TRUE, col="red") #line

hist(Customer_agreement$age[which(Customer_agreement$age != 0)], breaks = 50, freq=FALSE, col="gray", 
     xlab="Age", main="Age Distribution")
curve(dnorm(x, mean=mean(Customer_agreement$age[which(Customer_agreement$age != 0)]),
            sd(Customer_agreement$age[which(Customer_agreement$age != 0)])), add=TRUE, col="red") #line

median(Customer_agreement$age_signup[which(Customer_agreement$age_signup != 0)])
median(Customer_agreement$age[which(Customer_agreement$age != 0)])


#Save Files
write.csv(participant_checkin, "participant_checkin_2.csv")
write.csv(participant_checkin_subset_1, "participant_checkin_subset_1.csv")
write.csv(participant_checkin_2, "participant_checkin_subset_2010_2017.csv")



write.csv(Customer_agreement, "Customer_agreement.csv")

write.csv(Customer_agreement_core, "Customer_agreement_core.csv")

write.csv(core_customer_agreements_2, "Customer_agreement_subset.csv")

write.csv(core_customer_agreements_3, "Customer_agreement_subset_3.csv")

write.csv(customer_agreements_4, "Customer_agreement_subset_4.csv")

write.csv(upgrade_customer_agreements, "Upgrade_customer_agreements.csv")

write.csv(Club_site, "Club_site_with_attendances.csv")

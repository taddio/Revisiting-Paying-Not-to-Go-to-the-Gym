#---------------------------------------------------------------------------------------------
#Making Caltech Attendance Subset

#Caltech Gym Data

#Import Dataset
library(data.table)
#Importing large dataset
participant_checkin = fread("Lunch2018_3.csv") #Read checkin data

checkin_data = fread("checkin_data.csv") #Read Club Site data

checkin_data_2 = fread("checkin_data_clean_dates.csv") 

anastasia_data_subset = fread("chanlder_gym_combined_v3.csv")


#Cleaning Dates
library(lubridate)

clean_date_time = format(strptime(checkin_data$`Date/Time`, "%m/%e/%y %I:%M %p"), format = "%Y-%m-%d %H:%M:%S")
clean_date_time_2 = format(strptime(checkin_data$`Date/Time`, "%D %H:%M"), format = "%Y-%m-%d %H:%M:%S")

checkin_data$clean_date_time = clean_date_time

checkin_data_sorted = checkin_data[order(`UID (transformed)`, clean_date_time)]
checkin_data_sorted = checkin_data_sorted[-which(is.na(checkin_data_sorted$`UID (transformed)`))]


anastasia_data_subset$Date = format(strptime(anastasia_data_subset$Date, "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
anastasia_data_subset$Lunch_time = format(strptime(anastasia_data_subset$DateTime, "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")

anastasia_data_subset$chandler_lunch = 1
anastasia_data_subset$gym_attend = 0
anastasia_data_subset$number_gym_attend = 0
anastasia_data_subset$gym_time = NA
anastasia_data_subset$gym_time_2 = NA
anastasia_data_subset$gym_time_3 = NA

anastasia_data_subset = anastasia_data_subset[order(Cust_ID, DateTime)]

checkin_data_sorted$Date = format(strptime(checkin_data_sorted$clean_date_time, "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
checkin_data_sorted$Date = format(checkin_data_sorted$Date, "%Y-%m-%d")
checkin_data_sorted$Time = format(strptime(checkin_data_sorted$clean_date_time, "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")
checkin_data_sorted$Day_of_week = weekdays(as.Date(checkin_data_sorted$Date))

UIDs = unique(checkin_data_sorted$`UID (transformed)`)
UIDS_chandler = unique(anastasia_data_subset$Cust_ID)

UIDs = UIDs[which(UIDs %in% UIDS_chandler)]

anastasia_data_subset$Date = format(anastasia_data_subset$Date, "%Y-%m-%d")
anastasia_data_subset_2 = anastasia_data_subset
anastasia_data_subset_2 = anastasia_data_subset_2[, -2]

for(i in 1:length(UIDs)){
  subset_gym_indices = which(checkin_data_sorted$`UID (transformed)` == UIDs[i])
  subset = checkin_data_sorted[subset_gym_indices, ]
  subset_chandler_indices = which(anastasia_data_subset$Cust_ID == UIDs[i])
  subset_chandler_date = anastasia_data_subset[subset_chandler_indices, ]
  
  if(length(which(subset$Date %in% subset_chandler_date$Date)) > 0){
    gym_dates_in_chandler_indices = which(subset$Date %in% subset_chandler_date$Date)
    
    subset_gym_dates_in_chandler = subset$Date[gym_dates_in_chandler_indices]
    subset_gym_time_in_chandler = subset$Time[gym_dates_in_chandler_indices]
    unique_subset_gym_dates_in_chandler = unique(subset_gym_dates_in_chandler)
    
    subset_gym_dates_not_chandler = subset$Date[-gym_dates_in_chandler_indices]
    subset_gym_time_not_chandler = subset$Time[-gym_dates_in_chandler_indices]
  }
  else{
    subset_gym_dates_not_chandler = subset$Date
    subset_gym_time_not_chandler = subset$Time
  }
  
  unique_subset_gym_dates_not_chandler = unique(subset_gym_dates_not_chandler)
  gender = unique(subset_chandler_date$Sex)
  person_type = unique(subset_chandler_date$Person_Type)
  
  days_of_week = subset$Day_of_week
  
  
  if(length(unique_subset_gym_dates_in_chandler) > 0){
    for(j in 1:length(unique_subset_gym_dates_in_chandler)){
      indices = which(anastasia_data_subset_2$Cust_ID == UIDs[i] & anastasia_data_subset_2$Date == unique_subset_gym_dates_in_chandler[j])
      anastasia_data_subset_2$gym_attend[indices] = 1
      anastasia_data_subset_2$number_gym_attend[indices] = length(which(subset_gym_dates_in_chandler == unique_subset_gym_dates_in_chandler[j]))
      anastasia_data_subset_2$gym_time[indices] = min(subset_gym_time_in_chandler[which(subset_gym_dates_in_chandler == unique_subset_gym_dates_in_chandler[j])])
    }
  }
  
  if(length(unique_subset_gym_dates_not_chandler) > 0){
    for(h in 1:length(unique_subset_gym_dates_not_chandler)){
      Date = unique_subset_gym_dates_not_chandler[h]
      NewRow = data.table(V1 = 0, Cust_ID = UIDs[i],
                          Person_Type = person_type, Sex = gender,
                          Store = NA, Sequence = NA, Quantity = NA, Product = NA, Department = NA, Prod_Num = NA, 
                          Price = NA, Date = Date, Total_Minutes = NA, Day_of_Week = days_of_week[h], Meal_Category = NA, 
                          Health_Score = NA, Last_Visit_Days = NA, Time_Lag_Min = NA, Frequency = NA, Lunch_time = NA, 
                          chandler_lunch = 0, gym_attend = 1, 
                          gym_time = min(subset_gym_time_not_chandler[which(subset_gym_dates_not_chandler == Date)]), 
                          number_gym_attend = length(which(subset_gym_dates_not_chandler == unique_subset_gym_dates_not_chandler[h])))
      list = list(anastasia_data_subset_2, NewRow)
      anastasia_data_subset_2 = rbindlist(list, use.names = TRUE)
    }
  }
}

for(i in 1:length(UIDs)){
  subset_gym_indices = which(checkin_data_sorted$`UID (transformed)` == UIDs[i])
  subset = checkin_data_sorted[subset_gym_indices, ]
  subset_chandler_indices = which(anastasia_data_subset$Cust_ID == UIDs[i])
  subset_chandler_date = anastasia_data_subset[subset_chandler_indices, ]
  unique_subset_gym_dates_in_chandler = subset_chandler_indices[0]
  
  if(length(which(subset$Date %in% subset_chandler_date$Date)) > 0){
    gym_dates_in_chandler_indices = which(subset$Date %in% subset_chandler_date$Date)
    
    subset_gym_dates_in_chandler = subset$Date[gym_dates_in_chandler_indices]
    subset_gym_time_in_chandler = subset$Time[gym_dates_in_chandler_indices]
    unique_subset_gym_dates_in_chandler = unique(subset_gym_dates_in_chandler)
    
    subset_gym_dates_not_chandler = subset$Date[-gym_dates_in_chandler_indices]
    subset_gym_time_not_chandler = subset$Time[-gym_dates_in_chandler_indices]
  }
  
  
  if(length(unique_subset_gym_dates_in_chandler) > 0){
    for(j in 1:length(unique_subset_gym_dates_in_chandler)){
      indices = which(anastasia_data_subset_2$Cust_ID == UIDs[i] & anastasia_data_subset_2$Date == unique_subset_gym_dates_in_chandler[j])
      anastasia_data_subset_2$gym_attend[indices] = 1
      anastasia_data_subset_2$number_gym_attend[indices] = length(which(subset_gym_dates_in_chandler == unique_subset_gym_dates_in_chandler[j]))
      anastasia_data_subset_2$gym_time[indices] = min(subset_gym_time_in_chandler[which(subset_gym_dates_in_chandler == unique_subset_gym_dates_in_chandler[j])])
    }
  }
  
}

anastasia_data_subset_3$gym_time[which(anastasia_data_subset_2$gym_attend == 0)] = NA
anastasia_data_subset_3$number_gym_attend[which(anastasia_data_subset_2$gym_attend == 0)] = NA

length(unique(data$Cust_ID))
length(unique(data$Cust_ID[which(data$gym_attend == 1)]))
length(unique(data$Cust_ID[which(data$number_gym_attend > 4)]))

#------------------------------------------------------------------------------------------------------------
#Dataset two
# - Exclude attendances close together
# - Add attendance times


new_subset = anastasia_data_subset[which(anastasia_data_subset$number_gym_attend > 1), ]
new_subset_indices = which(anastasia_data_subset$number_gym_attend > 1)
new_checkin_subset = checkin_data_sorted[checkin_data_sorted$`UID (transformed)` %in% unique(new_subset$Cust_ID)]

new_subset$six_eight = 0
new_subset$eight_ten = 0
new_subset$ten_twelve = 0
new_subset$twelve_fourteen = 0
new_subset$fourteen_sixteen = 0
new_subset$sixteen_eighteen = 0
new_subset$eighteen_twenty = 0
new_subset$twentytwo_twenty = 0

anastasia_data_subset_2 = anastasia_data_subset

Cust_ids = unique(new_subset$Cust_ID)
checkin_dates_subset = new_checkin_subset$Date
checkin_times_subset = new_checkin_subset$Time
for(i in 1:length(Cust_ids)){
  joint_subset = new_subset[which(new_subset$Cust_ID == Cust_ids[i])]
  checkin_subset = new_checkin_subset[which(new_checkin_subset$`UID (transformed)` == Cust_ids[i] & new_checkin_subset$Date %in% unique(joint_subset$Date)),]
  subset_dates = checkin_subset$Date
  for(j in 1:length(subset_dates)){
    subsubset_times = checkin_subset$Time[which(subset_dates == subset_dates[j])]
    
    new_subset[which(new_subset$Cust_ID == Cust_ids[i] & new_subset$Date == subset_dates[j]), "six_eight"] = length(which(subsubset_times >= "06:00:00" & subsubset_times < "08:00:00"))
    
    new_subset[which(new_subset$Cust_ID == Cust_ids[i] & new_subset$Date == subset_dates[j]), "eight_ten"] = length(which(subsubset_times >= "08:00:00" & subsubset_times < "10:00:00"))
    
    new_subset[which(new_subset$Cust_ID == Cust_ids[i] & new_subset$Date == subset_dates[j]), "ten_twelve"] = length(which(subsubset_times >= "10:00:00" & subsubset_times < "12:00:00"))
    
    new_subset[which(new_subset$Cust_ID == Cust_ids[i] & new_subset$Date == subset_dates[j]), "twelve_fourteen"] = length(which(subsubset_times >= "12:00:00" & subsubset_times < "14:00:00"))
    
    new_subset[which(new_subset$Cust_ID == Cust_ids[i] & new_subset$Date == subset_dates[j]), "fourteen_sixteen"] = length(which(subsubset_times >= "14:00:00" & subsubset_times < "16:00:00"))
    
    new_subset[which(new_subset$Cust_ID == Cust_ids[i] & new_subset$Date == subset_dates[j]), "sixteen_eighteen"] = length(which(subsubset_times >= "16:00:00" & subsubset_times < "18:00:00"))
    
    new_subset[which(new_subset$Cust_ID == Cust_ids[i] & new_subset$Date == subset_dates[j]), "eighteen_twenty"] = length(which(subsubset_times >= "18:00:00" & subsubset_times < "20:00:00"))
    
    new_subset[which(new_subset$Cust_ID == Cust_ids[i] & new_subset$Date == subset_dates[j]), 'twentytwo_twenty'] = length(which(subsubset_times >= "20:00:00" & subsubset_times < "22:00:00"))
  }
}

new_subset$number_gym_attend_2 = 0
for(i in 1:length(new_subset$number_gym_attend)){
  new_gym_attend = 0
  row = new_subset[i,]
  if(row$six_eight > 0){
    new_gym_attend = new_gym_attend + 1
  }
  if(row$eight_ten > 0){
    new_gym_attend = new_gym_attend + 1
  }
  if(row$ten_twelve > 0){
    new_gym_attend = new_gym_attend + 1
  }
  if(row$twelve_fourteen > 0){
    new_gym_attend = new_gym_attend + 1
  }
  if(row$fourteen_sixteen > 0){
    new_gym_attend = new_gym_attend + 1
  }
  if(row$sixteen_eighteen > 0){
    new_gym_attend = new_gym_attend + 1
  }
  if(row$eighteen_twenty > 0){
    new_gym_attend = new_gym_attend + 1
  }
  if(row$twentytwo_twenty > 0){
    new_gym_attend = new_gym_attend + 1
  }
  new_subset[i, "number_gym_attend_2"] = new_gym_attend
}

new_subset[which(gym_time >= "22:00:00"), "number_gym_attend_2"] = 1


new_subset_2 = new_subset[which(new_subset$number_gym_attend_2 > 1), ]
cust_ids_greater_two = unique(new_subset_2$Cust_ID)

install.packages("chron")
library(lubridate)
new_attend_list = c()

for(i in 1:length(cust_ids_greater_two)){
  subset_checkins = checkin_data_sorted[which(checkin_data_sorted$`UID (transformed)` == cust_ids_greater_two[i] & checkin_data_sorted$Date %in% new_subset_2$Date),]
  unique_dates = unique(subset_checkins$Date[which(subset_checkins$Date %in% new_subset_2$Date)])
  for(j in 1:length(unique_dates)){
    number_checkin = new_subset$number_gym_attend_2[which(new_subset$Cust_ID == cust_ids_greater_two[i] &
                                                            new_subset$Date == unique_dates[j])]
    if(length(number_checkin != 0)){
      if(number_checkin[1] == 2){
        time = subset_checkins$Time[which(subset_checkins$Date == unique_dates[j])]
        time = chron(times. = time, format = c(times = "h:m:s"))
        if((time[2]-time[1]) > "02:00:00"){
          new_attend = 2
        }
        else{
          new_attend = 1
        }
        new_subset_2$number_gym_attend_2[which(new_subset_2$Cust_ID == cust_ids_greater_two[i] &
                                                 new_subset_2$Date == unique_dates[j])] = new_attend
      }
      else if(number_checkin[1] == 3){
        time = subset_checkins$Time[which(subset_checkins$Date == unique_dates[j])]
        time = chron(times = time, format = c(times = "h:m:s"))
        difftime_1 = time[3] - time[1]
        difftime_2 = time[2] - time[1]
        difftime_3 = time[3] - time[2]
        if(difftime_1 > "02:00:00" & difftime_2 > "02:00:00" & difftime_3 > "02:00:00"){
          new_attend = 3
        }
        else if(difftime_1 > "02:00:00" & (difftime_2 < "02:00:00" | difftime_1 < "02:00:00")){
          new_attend = 2
        }
        else if(difftime_1 < "02:00:00"){
          new_attend = 1
        }
        new_subset_2$number_gym_attend_2[which(new_subset_2$Cust_ID == cust_ids_greater_two[i] &
                                                 new_subset_2$Date == unique_dates[j])] = new_attend
      }
    } 
  }
}


new_subset$number_gym_attend_2[which(new_subset$Cust_ID == 152777)] = 3
new_subset$number_gym_attend_2[which(new_subset$Cust_ID == 155007)] = 3

new_subset_2 = read.csv("manual_chandler_addition.csv")

cust_ids = new_subset$Cust_ID
for(i in 1:length(cust_ids)){
  anastasia_data_subset$number_gym_attend[which(anastasia_data_subset$Cust_ID == cust_ids[i] &
                                                  anastasia_data_subset$Date == new_subset$Date[i])] = new_subset$number_gym_attend_2[i]
}

cust_ids = new_subset_2$Cust_ID
for(i in 1:length(cust_ids)){
  anastasia_data_subset$number_gym_attend[which(anastasia_data_subset$Cust_ID == cust_ids[i] &
                                                  anastasia_data_subset$Date == new_subset_2$Date[i])] = new_subset_2$number_gym_attend_2[i]
}

anastasia_data_subset$gym_time_2 = NA
anastasia_data_subset$gym_time_3 = NA
anastasia_data_subset$gym_time_4 = NA

anastasia_data_subset$gym_time_4 = as.character(anastasia_data_subset$gym_time_3)

new_subset = anastasia_data_subset[which(!is.na(anastasia_data_subset$gym_attend))]
new_subset = anastasia_data_subset[which(anastasia_data_subset$number_gym_attend > 1)]

cust_ids = new_subset$Cust_ID
dates = new_subset$Date
gym_attend = new_subset$number_gym_attend
for(i in 1:length(cust_ids)){
  checkin_subset = chron(times. = checkin_data_sorted$Time[which(checkin_data_sorted$`UID (transformed)` == cust_ids[i] &
                                                                   checkin_data_sorted$Date == dates[i])], format = c(times = "h:m:s"))
  time_1 = checkin_subset[1]
  if(gym_attend[i] >= 2){
    for(j in 2:length(checkin_subset)){
      time_2 = checkin_subset[j]
      if(time_2 - time_1 > "02:00:00" ){
        index = j + 1
        new_subset$gym_time_2[i] = as.character(time_2)
        break
      }
    }
  }
  if(gym_attend[i] >= 3){
    for(h in index:length(checkin_subset)){
      time_3 = checkin_subset[h]
      if(time_3 - time_2 > "02:00:00"){
        new_subset$gym_time_3[i] = as.character(time_3)
        break
      }
    }
  }
}

new_subset$gym_time_4[which(new_subset$Cust_ID == 155262 & new_subset$Date == "2018-10-13")] = "19:09:00"

cust_ids = new_subset$Cust_ID
dates = new_subset$Date
gym_attend = new_subset$number_gym_attend

for(i in 1:length(cust_ids)){
  if(gym_attend[i] >= 2){
    anastasia_data_subset$gym_time_2[which(anastasia_data_subset$Cust_ID == cust_ids[i] & 
                                             anastasia_data_subset$Date == dates[i])] = new_subset$gym_time_2[i]
  }
  if(gym_attend[i] >= 3){
    anastasia_data_subset$gym_time_3[which(anastasia_data_subset$Cust_ID == cust_ids[i] & 
                                             anastasia_data_subset$Date == dates[i])] = new_subset$gym_time_3[i]
  }
  if(gym_attend[i] == 4){
    anastasia_data_subset$gym_time_4[which(anastasia_data_subset$Cust_ID == cust_ids[i] & 
                                             anastasia_data_subset$Date == dates[i])] = new_subset$gym_time_4[i]
  }
}

#--------------------------------------------------------------------------------------------------------------
#New Dataset

Customer_Dataset = as.data.table(unique(anastasia_data_subset$Cust_ID))
Customer_Dataset$Gender = "M"

anastasia_data_subset = new_combined

cust_ids = Customer_Dataset$Cust_ID
for(i in 1:length(cust_ids)){
  subset = anastasia_data_subset$Sex[which(anastasia_data_subset$Cust_ID == cust_ids[i])]
  Customer_Dataset$Gender[i] = subset[1]
}

cust_ids = Customer_Dataset$Cust_ID
for(i in 1:length(cust_ids)){
  subset = anastasia_data_subset$Person_Type[which(anastasia_data_subset$Cust_ID == cust_ids[i])]
  Customer_Dataset$Person_Type[i] = subset[1]
}

Customer_Dataset$Total_Gym_Attendance = 0

#Total Gym Attendance each individual
cust_ids = Customer_Dataset$Cust_ID
for(i in 1:length(cust_ids)){
  subset = checkin_data_sorted$`UID (transformed)`[which(checkin_data_sorted$`UID (transformed)` == cust_ids[i])]
  Customer_Dataset$Total_Gym_Attendance[i] = length(subset)
}

unique_types = unique(Customer_Dataset$Person_Type)

colnames(Customer_Dataset)[colnames(Customer_Dataset)=="Chandler_Lunch_Days"] = "Chandler_Lunch_Items"

Customer_Dataset$Total_Chandler_Days = 0

anastasia_data_subset_no_pending = anastasia_data_subset[-which(anastasia_data_subset$Department == "PENDING")]
anastasia_data_subset_with_pending = anastasia_data_subset

anastasia_data_subset = anastasia_data_subset_no_pending


#Total Chandler Items Bought each Individual
cust_ids = Customer_Dataset$Cust_ID
for(i in 1:length(cust_ids)){
  subset = anastasia_data_subset$chandler_lunch[which(anastasia_data_subset$Cust_ID == cust_ids[i])]
  Customer_Dataset$Total_Chandler_Lunch[i] = sum(subset)
}

#Total Days Attending Chandler - generating variable
for(i in 1:length(cust_ids)){
  subset = anastasia_data_subset$Date[which(anastasia_data_subset$Cust_ID == cust_ids[i] & 
                                              anastasia_data_subset$chandler_lunch == 1)]
  Customer_Dataset$Chandler_Lunch_Days[i] = length(unique(subset))
}

#Total Days Attending Gym - generating variable
for(i in 1:length(cust_ids)){
  subset = anastasia_data_subset$Date[which(anastasia_data_subset$Cust_ID == cust_ids[i] &
                                              anastasia_data_subset$gym_attend == 1)]
  Customer_Dataset$Gym_Days[i] = length(unique(subset))
}

#Total Days Attending Lunch + Gym - generating variable
for(i in 1:length(cust_ids)){
  subset = anastasia_data_subset$Date[which(anastasia_data_subset$Cust_ID == cust_ids[i] &
                                              anastasia_data_subset$gym_attend == 1 &
                                              anastasia_data_subset$chandler_lunch == 1)]
  Customer_Dataset$Gym_Lunch_Days[i] = length(unique(subset))
}

#Total Days Attending Lunch + no Gym - generating variable
for(i in 1:length(cust_ids)){
  subset = anastasia_data_subset$Date[which(anastasia_data_subset$Cust_ID == cust_ids[i] &
                                              anastasia_data_subset$gym_attend == 0 &
                                              anastasia_data_subset$chandler_lunch == 1)]
  Customer_Dataset$no_Gym_Lunch_Days[i] = length(unique(subset))
}

#Total Days Attending no Lunch + Gym - generating variable
for(i in 1:length(cust_ids)){
  subset = anastasia_data_subset$Date[which(anastasia_data_subset$Cust_ID == cust_ids[i] &
                                              anastasia_data_subset$gym_attend == 1 &
                                              anastasia_data_subset$chandler_lunch == 0)]
  Customer_Dataset$Gym_no_Lunch_Days[i] = length(unique(subset))
}


#Total Gym Attendance
for(i in 1:length(unique_types)){ 
  print(sum(Customer_Dataset$Total_Gym_Attendance[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}

sum(Customer_Dataset$Total_Gym_Attendance)

#Average Gym Attendance
for(i in 1:length(unique_types)){ 
  print(mean(Customer_Dataset$Total_Gym_Attendance[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
mean(Customer_Dataset$Total_Gym_Attendance)

#Total Chandler Items Bought per type
for(i in 1:length(unique_types)){
  print(sum(Customer_Dataset$Total_Chandler_Lunch[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
sum(Customer_Dataset$Total_Chandler_Lunch)

#Average Chandler Items Bought per type
for(i in 1:length(unique_types)){
  print(mean(Customer_Dataset$Total_Chandler_Lunch[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
mean(Customer_Dataset$Total_Chandler_Lunch)

#Total Days Attending Chandler per Type
for(i in 1:length(unique_types)){
  print(sum(Customer_Dataset$Chandler_Lunch_Days[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
sum(Customer_Dataset$Chandler_Lunch_Days)

#Average Days Attending Chandler
for(i in 1:length(unique_types)){
  print(mean(Customer_Dataset$Chandler_Lunch_Days[which(Customer_Dataset$Person_Type == unique_types[i])]))
}
mean(Customer_Dataset$Chandler_Lunch_Days)

#Total Gym Days Per Type
for(i in 1:length(unique_types)){
  print(sum(Customer_Dataset$Gym_Days[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
sum(Customer_Dataset$Gym_Days)

#Average Gym Days Per Type
for(i in 1:length(unique_types)){
  print(mean(Customer_Dataset$Gym_Days[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
mean(Customer_Dataset$Gym_Days)

#Total Gym + Lunch Days Per Type
for(i in 1:length(unique_types)){
  print(sum(Customer_Dataset$Gym_Lunch_Days[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
sum(Customer_Dataset$Gym_Lunch_Days)

#Average Gym + Lunch Days Per Type
for(i in 1:length(unique_types)){
  print(mean(Customer_Dataset$Gym_Lunch_Days[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
mean(Customer_Dataset$Gym_Lunch_Days)

#Total Gym + no Lunch Days Per Type
for(i in 1:length(unique_types)){
  print(sum(Customer_Dataset$Gym_no_Lunch_Days[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
sum(Customer_Dataset$Gym_no_Lunch_Days)

#Average Gym + no Lunch Days Per Type
for(i in 1:length(unique_types)){
  print(mean(Customer_Dataset$Gym_no_Lunch_Days[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
mean(Customer_Dataset$Gym_no_Lunch_Days)

#Total no Gym + Lunch Days Per Type
for(i in 1:length(unique_types)){
  print(sum(Customer_Dataset$no_Gym_Lunch_Days[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}
sum(Customer_Dataset$no_Gym_Lunch_Days)

#Average no Gym + Lunch Days Per Type
for(i in 1:length(unique_types)){
  print(mean(Customer_Dataset$no_Gym_Lunch_Days[which(Customer_Dataset$Person_Type == unique_types[i])])) 
}

mean(Customer_Dataset$no_Gym_Lunch_Days)


sum(Customer_Dataset$Gym_no_Lunch_Days)

#---------------------------------------------------------------------------------------------
#Check for double counting of pending
pending_indices = which(anastasia_data_subset_with_pending$Department == "PENDING")

cust_ids = anastasia_data_subset_with_pending$Cust_ID
double_count_indices = c()
for(i in 1:length(pending_indices)){
  subset = anastasia_data_subset_with_pending$Cust_ID[which(anastasia_data_subset_with_pending$Cust_ID == cust_ids[pending_indices[i]]
                                                            & anastasia_data_subset_with_pending$Product == anastasia_data_subset_with_pending$Product[pending_indices[i]]
                                                            & anastasia_data_subset_with_pending$Date == anastasia_data_subset_with_pending$Date[pending_indices[i]]
                                                            & anastasia_data_subset_with_pending$lunch_time == anastasia_data_subset_with_pending$lunch_time[pending_indices[i]])]
  if(length(subset) > 1){
    double_count_indices = c(double_count_indices, pending_indices[i])
  }
}

anastasia_data_subset = anastasia_data_subset_with_pending[-double_count_indices, ]

#------------------------------------------------------------------------------
#Fixing undergrad datetimes

undergrad_checkin_subset = participant_checkin[which(participant_checkin$Person_Type == "Undergraduate"),]
undergrad_checkin_subset$Date = format(strptime(undergrad_checkin_subset$Date, "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
undergrad_checkin_subset$lunch_time = format(strptime(undergrad_checkin_subset$DateTime, "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")

undergrad_gym_checkins = anastasia_data_subset[which(anastasia_data_subset$Person_Type == "Undergraduate" &
                                                       anastasia_data_subset$gym_attend == 1),]

undergrad_checkin_subset$chandler_lunch = 1
undergrad_checkin_subset$gym_attend = 0
undergrad_checkin_subset$number_gym_attend = 0
undergrad_checkin_subset$gym_time = NA
undergrad_checkin_subset$gym_time_2 = NA
undergrad_checkin_subset$gym_time_3 = NA
undergrad_checkin_subset$gym_time_4 = NA

cust_ids = undergrad_gym_checkins$Cust_ID
undergrad_gym_checkins_2 = undergrad_gym_checkins
remove_indices = c()

for(i in 1:length(cust_ids)){
  subset_chandler_dates = undergrad_checkin_subset$Date[which(undergrad_checkin_subset$Cust_ID == cust_ids[i])]
  if(undergrad_gym_checkins$Date[i] %in% subset_chandler_dates){
    index = which(undergrad_checkin_subset$Cust_ID == cust_ids[i] &
                    undergrad_checkin_subset$Date == undergrad_gym_checkins$Date[i])
    
    undergrad_checkin_subset$gym_attend[index] = 1
    undergrad_checkin_subset$gym_time[index] = undergrad_gym_checkins$gym_time[i]
    undergrad_checkin_subset$gym_time_2[index] = undergrad_gym_checkins$gym_time_2[i]
    undergrad_checkin_subset$gym_time_3[index] = undergrad_gym_checkins$gym_time_3[i]
    undergrad_checkin_subset$gym_time_4[index] = undergrad_gym_checkins$gym_time_4[i]
    
    remove_indices = c(remove_indices, i)
  }
}

cust_ids = undergrad_checkin_subset$Cust_ID
for(i in 103977:length(cust_ids)){
  if(!is.na(undergrad_checkin_subset$gym_time[i])){
    undergrad_checkin_subset$number_gym_attend[i] = 1
  }
  if(!is.na(undergrad_checkin_subset$gym_time_2[i])){
    undergrad_checkin_subset$number_gym_attend[i] = 2
  }
  if(!is.na(undergrad_checkin_subset$gym_time_3[i])){
    undergrad_checkin_subset$number_gym_attend[i] = 3
  }
}

undergrad_gym_checkins_2 = undergrad_gym_checkins[-remove_indices,]
undergrad_gym_checkins_2 = undergrad_gym_checkins_2[, -c(1,2)]
colnames(undergrad_gym_checkins_2)[colnames(undergrad_gym_checkins_2)=="Lunch_time"] <- "lunch_time"
undergrad_gym_checkins_2$lunch_time = NA

undergrad_combined = rbindlist(list(undergrad_checkin_subset, undergrad_gym_checkins_2), use.names = TRUE)

anastasia_data_subset_2 = anastasia_data_subset[-which(anastasia_data_subset$Person_Type == "Undergraduate"), ]

colnames(anastasia_data_subset_2)[colnames(anastasia_data_subset_2)=="Lunch_time"] <- "lunch_time"
new_combined = rbindlist(list(anastasia_data_subset_2, undergrad_combined), use.names = TRUE)

new_combined = new_combined[order(new_combined$Cust_ID, new_combined$Date, new_combined$lunch_time),]



#-----------------------------------------------------------------------------
#Save Files

write.csv(anastasia_data_subset, "chandler_gym_combined.csv")

write.csv(new_combined, "chandler_gym_combined_v2.csv")

write.csv(anastasia_data_subset, "chanlder_gym_combined_v3.csv")

sum(anastasia_data_subset$gym_attend)

write.csv(new_subset_2, "manual_chandler_addition.csv")

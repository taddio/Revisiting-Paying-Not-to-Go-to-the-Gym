
#-------------------------------------------------------------------------------------------
#Reading Data
library(readr)

Customer_agreement = read_csv("Customer_agreement_subset_5.csv") #Read Customer Agreement Data
Customer_agreement = Customer_agreement[,-c(1,2)]

Customer_agreement = Customer_agreement[which(Customer_agreement$cleaned_agreement_sign_dt < "2016-01-01"),]
Customer_agreement = read_csv("Customer_agreement_subset_5.csv") #Read Customer Agreement Data

#Variable with first month

library(lubridate)

Customer_agreement$Sign_up_month = ""

Months = c("January", "Februray", "March", "April", "May", "June", "July",
           "August", "September", "October", "November", "December")

for(i in 1:nrow(Customer_agreement)){
  Customer_agreement$Sign_up_month[i] = Months[
    month(as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[i], format="%d/%m/%Y"))]
}

month_attendance = as.data.frame(Months)
month_attendance$sign_ups = 0
month_attendance$mean_attendance_month_1 = 0
month_attendance$mean_attendance_month_2 = 0
month_attendance$mean_attendance_month_3 = 0
month_attendance$mean_attendance_month_4 = 0
month_attendance$mean_attendance_month_5 = 0
month_attendance$mean_attendance_month_6 = 0
month_attendance$mean_attendance_month_7 = 0
month_attendance$mean_attendance_month_8 = 0
month_attendance$mean_attendance_month_9 = 0
month_attendance$mean_attendance_month_10 = 0
month_attendance$mean_attendance_month_11 = 0
month_attendance$mean_attendance_month_12 = 0

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  month_attendance$mean_attendance_month_1[i] =
    mean(Customer_agreement$attendance_month_1[indices])
  month_attendance$sign_ups[i] = length(indices)
}

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_2[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_2[i] =
    mean(attendance)
}

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_3[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_3[i] =
    mean(attendance)
}

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_4[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_4[i] =
    mean(attendance)
}

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_5[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_5[i] =
    mean(attendance)
}

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_6[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_6[i] =
    mean(attendance)
}

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_7[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_7[i] =
    mean(attendance)
}
for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_8[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_8[i] =
    mean(attendance)
}

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_9[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_9[i] =
    mean(attendance)
}

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_10[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_10[i] =
    mean(attendance)
}

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_11[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_11[i] =
    mean(attendance)
}

for(i in 1:12){
  indices = which(Customer_agreement$Sign_up_month == Months[i])
  attendance = Customer_agreement$attendance_month_12[indices]
  attendance[is.na(attendance)] = 0
  month_attendance$mean_attendance_month_12[i] =
    mean(attendance)
}

month_attendance$one_month_ratio = month_attendance$mean_attendance_month_2/month_attendance$mean_attendance_month_1
month_attendance$three_month_ratio = month_attendance$mean_attendance_month_4/month_attendance$mean_attendance_month_1
month_attendance$six_month_ratio = month_attendance$mean_attendance_month_7/month_attendance$mean_attendance_month_1
month_attendance$nine_month_ratio = month_attendance$mean_attendance_month_10/month_attendance$mean_attendance_month_1

#--------------------------------------------------------------------------------------
#Plots
library(ggplot2)
# Basic barplot
plot = ggplot(aes(x = Months, y = three_month_ratio), data = month_attendance) + 
  geom_bar(fill = "dark blue", stat = "identity") + 
  scale_x_discrete(limits= month_attendance$Months)
plot

plot = ggplot(aes(x = Months, y = mean_attendance_month_1), data = month_attendance) + 
  geom_bar(fill = "dark blue", stat = "identity") + 
  scale_x_discrete(limits= month_attendance$Months)
plot

plot(month_attendance$sign_ups)

write.csv(month_attendance, "month_attendance.csv")

#-------------------------------------------------------------------------------------
#2 week difference at sign-up

#Reading Data
library(readr)

Customer_agreement = read_csv("Customer_agreement_subset_5.csv") #Read Customer Agreement Data
Customer_agreement = Customer_agreement[,-c(1,2)]

biweek_attendance = read_csv("biweek_attendance.csv")

#Variable with first month

library(lubridate)

Customer_agreement$Sign_up_biweek = ""

Biweeks = c("Jan1", "Jan2", "Feb1", "Feb2", "Mar1", "Mar2", "Apr1", "Apr2",
            "May1", "May2", "Jun1", "Jun2", "Jul1", "Jul2", "Aug1", "Aug2",
            "Sep1", "Sep2", "Oct1", "Oct2", "Nov1", "Nov2", "Dec1", "Dec2")

for(i in 1:nrow(Customer_agreement)){
  date = as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[i], format="%d/%m/%Y")
  if(day(date) <= 15 & month(date) != 2){
    Customer_agreement$Sign_up_biweek[i] = Biweeks[2*month(date) - 1]
  }
  else if(day(date) <= 14 & month(date) == 2){
    Customer_agreement$Sign_up_biweek[i] = Biweeks[2*month(date) - 1]
  }
  else{
    Customer_agreement$Sign_up_biweek[i] = Biweeks[2*month(date)]
  }
}

biweek_attendance = as.data.frame(Biweeks)
biweek_attendance$sign_ups = 0

for(i in 1:24){
  biweek_attendance$sign_ups[i] = length(which(Customer_agreement$Sign_up_biweek == Biweeks[i]))
}

for(i in 1:12){
  biweek_attendance$new_column = 0
  new_name = paste("month_attendance_", as.character(i), sep = "")
  colnames(biweek_attendance)[colnames(biweek_attendance)=="new_column"] = new_name
}

for(i in 1:12){
  biweek_attendance$new_column = 0
  new_name = paste("median_attendance_", as.character(i), sep = "")
  colnames(biweek_attendance)[colnames(biweek_attendance)=="new_column"] = new_name
}

for(i in 1:12){
  biweek_attendance$new_column = 0
  new_name = paste("members_", as.character(i), sep = "")
  colnames(biweek_attendance)[colnames(biweek_attendance)=="new_column"] = new_name
}

for(i in 1:12){
  for(j in 1:24){
    indices = which(Customer_agreement$Sign_up_biweek == Biweeks[j])
    attendance = Customer_agreement[indices, 27+i]
    attendance[is.na(attendance)] = 0
    names(attendance) = c("attendance")
    biweek_attendance[j, 2+i] = mean(attendance$attendance)
  }
}

for(i in 1:12){
  for(j in 1:24){
    indices = which(Customer_agreement$Sign_up_biweek == Biweeks[j])
    attendance = Customer_agreement[indices, 27+i]
    attendance[is.na(attendance)] = 0
    names(attendance) = c("attendance")
    biweek_attendance[j, 26+i] = median(attendance$attendance)
  }
}

for(i in 1:12){
  for(j in 1:24){
    indices = which(Customer_agreement$Sign_up_biweek == Biweeks[j])
    attendance = Customer_agreement[indices, 27+i]
    biweek_attendance[j, 14+i] = length(which(is.na(attendance) == FALSE))
  }
}
colnames(biweek_attendance)[colnames(biweek_attendance_1)=="members_12"] = "half_month"


library(tidyr)
library(ggplot2)

biweek_attendance_1 = biweek_attendance[, c(1, 3:14)]
biweek_attendance_1 = biweek_attendance[, -1]
biweek_attendance_1$half_month = c(1:24)
colnames(biweek_attendance)[colnames(biweek_attendance_1)=="sign_ups"] = "half_month"

biweek_attendance_1$Biweeks = c(1:24)
data_long = gather(biweek_attendance_1, month, median_monthly_attendance, 
                   median_attendance_1:median_attendance_12, factor_key = TRUE)

data_long$Biweeks = with(data_long, reorder(Biweeks))

data_long$Biweeks = c(1:24)
  
p = ggplot(data_long, aes(x=month, y=mean_monthly_attendance, group=Biweeks, order = Biweeks)) +
  geom_line(aes(color=Biweeks)) +
  geom_point(aes(color=Biweeks))
p


#------------------------------------------------------
#Thanksgiving attendance

select_dates_attendance_list = c("Thanksgiving", "Post-Christmas", "Post-New-Year", "March-random", "June-random", "November-random")

select_dates_attendance = as.data.frame(select_dates_attendance_list)
select_dates_attendance$sign_ups = 0

Customer_agreement$select_dates = ""

for(i in 1:nrow(Customer_agreement)){
date = as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[i], format="%d/%m/%Y")
if(day(date) %in% c(25:29) & month(date) == 11 & year(date) == 2010){
  Customer_agreement$select_dates[i] = select_dates_attendance_list[1]
}
else if(day(date) %in% c(24:28) & month(date) == 11 & year(date) == 2011){
  Customer_agreement$select_dates[i] = select_dates_attendance_list[1]
}
else if(day(date) %in% c(22:26) & month(date) == 11 & year(date) == 2012){
  Customer_agreement$select_dates[i] = select_dates_attendance_list[1]
}
else if(day(date) %in% c(28:30) & month(date) == 11 & year(date) == 2013){
  Customer_agreement$select_dates[i] = select_dates_attendance_list[1]
}
else if(day(date) %in% c(1:2) & month(date) == 12 & year(date) == 2013){
  Customer_agreement$select_dates[i] = select_dates_attendance_list[1]
}
else if(day(date) %in% c(27:30) & month(date) == 11 & year(date) == 2014){
  Customer_agreement$select_dates[i] = select_dates_attendance_list[1]
}
else if(day(date) == 1 & month(date) == 12 & year(date) == 2014){
  Customer_agreement$select_dates[i] = select_dates_attendance_list[1]
}
else if(day(date) %in% c(26:30) & month(date) == 11 & year(date) == 2015){
  Customer_agreement$select_dates[i] = select_dates_attendance_list[1]
}
}


for(i in 1:nrow(Customer_agreement)){
  date = as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[i], format="%d/%m/%Y")
  if(day(date) %in% c(27:31) & month(date) == 12 & year(date) %in% c(2010:2016)){
    Customer_agreement$select_dates[i] = select_dates_attendance_list[2]
  }
}
  
for(i in 1:nrow(Customer_agreement)){
  date = as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[i], format="%d/%m/%Y")
  if(day(date) %in% c(1:5) & month(date) == 1 & year(date) %in% c(2010:2016)){
    Customer_agreement$select_dates[i] = select_dates_attendance_list[3]
  }
}

for(i in 1:nrow(Customer_agreement)){
  date = as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[i], format="%d/%m/%Y")
  if(day(date) %in% c(26:30) & month(date) == 3 & year(date) %in% c(2010:2016)){
    Customer_agreement$select_dates[i] = select_dates_attendance_list[4]
  }
}

for(i in 1:nrow(Customer_agreement)){
  date = as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[i], format="%d/%m/%Y")
  if(day(date) %in% c(11:15) & month(date) == 6 & year(date) %in% c(2010:2016)){
    Customer_agreement$select_dates[i] = select_dates_attendance_list[5]
  }
}

for(i in 1:nrow(Customer_agreement)){
  date = as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[i], format="%d/%m/%Y")
  if(day(date) %in% c(11:15) & month(date) == 9 & year(date) %in% c(2010:2016)){
    Customer_agreement$select_dates[i] = select_dates_attendance_list[6]
  }
}

#Sign ups

for(i in 1:length(select_dates_attendance_list)){
  select_dates_attendance$sign_ups[i] = length(which(Customer_agreement$select_dates == 
                                                    select_dates_attendance_list[i]))
}


#Set up columns

for(i in 1:12){
  select_dates_attendance$new_column = 0
  new_name = paste("month_attendance_", as.character(i), sep = "")
  colnames(select_dates_attendance)[colnames(select_dates_attendance)=="new_column"] = new_name
}

for(i in 1:12){
  select_dates_attendance$new_column = 0
  new_name = paste("median_attendance_", as.character(i), sep = "")
  colnames(select_dates_attendance)[colnames(select_dates_attendance)=="new_column"] = new_name
}

for(i in 1:12){
  select_dates_attendance$new_column = 0
  new_name = paste("members_", as.character(i), sep = "")
  colnames(select_dates_attendance)[colnames(select_dates_attendance)=="new_column"] = new_name
}

for(i in 1:12){
  select_dates_attendance$new_column = 0
  new_name = paste("month_attendance_variance", as.character(i), sep = "")
  colnames(select_dates_attendance)[colnames(select_dates_attendance)=="new_column"] = new_name
}


#Filling in months 

for(i in 1:12){
  for(j in 1:6){
    indices = which(Customer_agreement$select_dates == select_dates_attendance_list[j])
    attendance = Customer_agreement[indices, 27+i]
    attendance[is.na(attendance)] = 0
    names(attendance) = c("attendance")
    select_dates_attendance[j, 2+i] = mean(attendance$attendance)
  }
}

for(i in 1:12){
  for(j in 1:6){
    indices = which(Customer_agreement$select_dates == select_dates_attendance_list[j])
    attendance = Customer_agreement[indices, 27+i]
    attendance[is.na(attendance)] = 0
    names(attendance) = c("attendance")
    select_dates_attendance[j, 14+i] = median(attendance$attendance)
  }
}

for(i in 1:12){
  for(j in 1:6){
    indices = which(Customer_agreement$select_dates == select_dates_attendance_list[j])
    attendance = Customer_agreement[indices, 27+i]
    select_dates_attendance[j, 26+i] = length(which(is.na(attendance) == FALSE))
  }
}

for(i in 1:12){
    indices = which(Customer_agreement$select_dates == select_dates_attendance_list[1])
    attendance = Customer_agreement[indices, 27+i]
    attendance[is.na(attendance)] = 0
    names(attendance) = c("attendance")
    select_dates_attendance[1, 1+i] = var(attendance$attendance)
  }

#------------------------------------------------------
#5-day attendance

five_day_attendance = read_csv("select_dates_attendance.csv")

five_day_attendance_list = c()

Customer_agreement$five_day_period = ""

#Variable names setup
for(i in 1:73){
  index_1 = (i-1)*5 + 1
  index_2 = index_1 + 4
  five_day_attendance_list = c(five_day_attendance_list, paste(as.character(index_1), as.character(index_2), sep = "-") )
}

five_day_attendance_dataframe = as.data.frame(five_day_attendance_list)

#Labelling customer agreements by signup date
library(lubridate)

Customer_agreement$five_day_period = ""

for(i in 1:73){
  index_1 = (i-1)*5 + 1
  index_2 = index_2 + 4
  for(j in 1:nrow(Customer_agreement)){
    date = as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[j], format="%d/%m/%Y")
    if(yday(date) %in% c(index_1:index_2) & year(date) %in% c(2010, 2011, 2013, 2014, 2015)){
      Customer_agreement$five_day_period[j] = five_day_attendance_list[i]
    }
    else if(index_1 < 60 & yday(date) %in% c(index_1:index_2) & year(date) == 2012 & yday(date) != 60){
      Customer_agreement$five_day_period[j] = five_day_attendance_list[i]
    }
    else if(index_1 >= 60 & yday(date) %in% c((index_1+1):(index_2+1)) & year(date) == 2012){
      Customer_agreement$five_day_period[j] = five_day_attendance_list[i]
    }
  }
}

#Set up columns

for(i in 1:12){
  five_day_attendance_dataframe$new_column = 0
  new_name = paste("month_attendance_", as.character(i), sep = "")
  colnames(five_day_attendance_dataframe)[colnames(five_day_attendance_dataframe)=="new_column"] = new_name
}

for(i in 1:12){
  five_day_attendance_dataframe$new_column = 0
  new_name = paste("median_attendance_", as.character(i), sep = "")
  colnames(five_day_attendance_dataframe)[colnames(five_day_attendance_dataframe)=="new_column"] = new_name
}

for(i in 1:12){
  five_day_attendance_dataframe$new_column = 0
  new_name = paste("members_", as.character(i), sep = "")
  colnames(five_day_attendance_dataframe)[colnames(five_day_attendance_dataframe)=="new_column"] = new_name
}

for(i in 1:12){
  five_day_attendance_dataframe$new_column = 0
  new_name = paste("month_attendance_variance", as.character(i), sep = "")
  colnames(five_day_attendance_dataframe)[colnames(five_day_attendance_dataframe)=="new_column"] = new_name
}

#Filling in months 

for(i in 1:12){
  for(j in 1:73){
    indices = which(Customer_agreement$five_day_period == five_day_attendance_list[j])
    attendance = Customer_agreement[indices, 27+i]
    attendance[is.na(attendance)] = 0
    names(attendance) = c("attendance")
    five_day_attendance_dataframe[j, 1+i] = mean(attendance$attendance)
  }
}

for(i in 1:12){
  for(j in 1:73){
    indices = which(Customer_agreement$five_day_period == five_day_attendance_list[j])
    attendance = Customer_agreement[indices, 27+i]
    attendance[is.na(attendance)] = 0
    names(attendance) = c("attendance")
    five_day_attendance_dataframe[j, 13+i] = median(attendance$attendance)
  }
}

for(i in 1:12){
  for(j in 1:73){
    indices = which(Customer_agreement$five_day_period == five_day_attendance_list[j])
    attendance = Customer_agreement[indices, 27+i]
    five_day_attendance_dataframe[j, 25+i] = length(which(is.na(attendance) == FALSE))
  }
}

for(i in 1:12){
  for(j in 1:73){
    indices = which(Customer_agreement$five_day_period == five_day_attendance_list[j])
    attendance = Customer_agreement[indices, 27+i]
    attendance[is.na(attendance)] = 0
    names(attendance) = c("attendance")
    five_day_attendance_dataframe[j, 37+i] = var(attendance$attendance)
  }
}

for(i in 1:12){
  attendance = Customer_agreement[, 27+i]
  five_day_attendance_dataframe[75, 25+i] = length(which(is.na(attendance) == FALSE))
  attendance[is.na(attendance)] = 0
  names(attendance) = c("attendance")
  five_day_attendance_dataframe[75, 1+i] = mean(attendance$attendance)
  five_day_attendance_dataframe[75, 13+i] = median(attendance$attendance)
  five_day_attendance_dataframe[75, 37+i] = var(attendance$attendance)
}

attition = read_csv("attrition_and_attendance.csv")

colnames(attition)[colnames(attition)=="Mean Attendance"] = "mean_attendance"

for(i in 1:73){
  attition$new_column = 0
  new_name = five_day_attendance_list[i]
  colnames(attition)[colnames(attition)=="new_column"] = new_name
}

for(i in 1:73){
  for(j in 1:10){
    index = i + (j-1)*73
    attrition[index,i+4] = 1
  }
}

attition$month = 0
for(i in 1:10){
  index_1 = (i-1)*73 + 1
  index_2 = index_1 + 72
  attition$month[index_1:index_2] = i
}

attrition = attrition[, -c(78:90)]

Months = c("January", "Februray", "March", "April", "May", "June", "July",
           "August", "September", "October", "November", "December")

for(i in 1:12){
  attrition$new_column = 0
  new_name = Months[i]
  colnames(attrition)[colnames(attrition)=="new_column"] = new_name
}

for(i in 1:730){
  attrition[i, (78 + attrition$month[i])] = 1
}

attrition = read_csv("attrition_data_5_days.csv")

reg = lm(formula = attrition ~ . - attrition_rate - mean_attendance - `1-5` - X1,
         data = attrition)

summary(reg)

reg2 = lm(formula = attrition_rate ~ . - attrition - mean_attendance - `1-5` - X1,
         data = attrition)

summary(reg2)

reg2 = lm(formula = attrition_rate ~ . - attrition - mean_attendance - 1 - X1,
          data = attrition)

summary(reg2)

reg3 = lm(formula = mean_attendance ~ . - attrition_rate - attrition - X1 - `1-5`,
          data = attrition)

summary(reg3)


reg3 = lm(formula = mean_attendance ~ . - attrition_rate - attrition - X1 - 1,
          data = attrition)

summary(reg3)


reg4 = lm(formula = mean_attendance ~ . - attrition,
          data = attition)

summary(reg4)



install.packages("stargazer")
library(stargazer)

stargazer(reg, title = "Attrition on Lag Mean Attendance, Monthly Dummies and Time Trend", align=TRUE)

#mean_attrition
mean_attrition = read_csv("mean_attrition.csv")

hist(mean_attrition$mean_monthly_attrition, breaks = 40, freq=FALSE, col="gray", 
     xlab="Mean Attrition", main="Mean Attrition")
curve(dnorm(x, mean=3.447945205, 2.109148153), add=TRUE, col="red")

#signups and attrition
signup = read_csv("sign-ups-five-days.csv")
signup = signup[-c(74:75),]

hist(signup$`sign-ups`, breaks = 40, freq = FALSE, col = "gray",
     xlab = "Sign Ups", main = "Sign ups by 5 day period")
curve(dnorm(x, mean = mean(signup$`sign-ups`), sd(signup$`sign-ups`)), add=TRUE, col="red")
abline(v = mean(signup$`sign-ups`), lty = 2) 
abline(v = mean(signup$`sign-ups`)+sd(signup$`sign-ups`), lty = 2)
abline(v = mean(signup$`sign-ups`)-sd(signup$`sign-ups`), lty = 2)
abline(v = mean(signup$`sign-ups`)+2*sd(signup$`sign-ups`), lty = 2)
abline(v = mean(signup$`sign-ups`)-2*sd(signup$`sign-ups`), lty = 2)

mean(mean_attrition$mean_monthly_attrition[which(signup$`sign-ups` < mean(signup$`sign-ups`)-2*sd(signup$`sign-ups`))])
mean(mean_attrition$mean_monthly_attrition[which(signup$`sign-ups` > mean(signup$`sign-ups`)-2*sd(signup$`sign-ups`) &
                                                   signup$`sign-ups` < mean(signup$`sign-ups`)-1*sd(signup$`sign-ups`))])
mean(mean_attrition$mean_monthly_attrition[which(signup$`sign-ups` > mean(signup$`sign-ups`)-1*sd(signup$`sign-ups`) &
                                                   signup$`sign-ups` < mean(signup$`sign-ups`))])
mean(mean_attrition$mean_monthly_attrition[which(signup$`sign-ups` > mean(signup$`sign-ups`) &
                                                   signup$`sign-ups` < mean(signup$`sign-ups`)+1*sd(signup$`sign-ups`))])
mean(mean_attrition$mean_monthly_attrition[which(signup$`sign-ups` > mean(signup$`sign-ups`)+1*sd(signup$`sign-ups`) &
                                                   signup$`sign-ups` < mean(signup$`sign-ups`)+2*sd(signup$`sign-ups`))])
mean(mean_attrition$mean_monthly_attrition[which(signup$`sign-ups` > mean(signup$`sign-ups`)+2*sd(signup$`sign-ups`))])


mean_attrition$mean_monthly_attrition_rate = 0
for(i in 1:73){
  column = five_day_attendance_list[i]
  mean_attrition$mean_monthly_attrition_rate[i] = mean(attition$attrition_rate[which(attition[,3+i] == 1)])
}

mean(mean_attrition$mean_monthly_attrition_rate[which(signup$`sign-ups` < mean(signup$`sign-ups`)-2*sd(signup$`sign-ups`))])
mean(mean_attrition$mean_monthly_attrition_rate[which(signup$`sign-ups` > mean(signup$`sign-ups`)-2*sd(signup$`sign-ups`) &
                                                   signup$`sign-ups` < mean(signup$`sign-ups`)-1*sd(signup$`sign-ups`))])
mean(mean_attrition$mean_monthly_attrition_rate[which(signup$`sign-ups` > mean(signup$`sign-ups`)-1*sd(signup$`sign-ups`) &
                                                   signup$`sign-ups` < mean(signup$`sign-ups`))])
mean(mean_attrition$mean_monthly_attrition_rate[which(signup$`sign-ups` > mean(signup$`sign-ups`) &
                                                   signup$`sign-ups` < mean(signup$`sign-ups`)+1*sd(signup$`sign-ups`))])
mean(mean_attrition$mean_monthly_attrition_rate[which(signup$`sign-ups` > mean(signup$`sign-ups`)+1*sd(signup$`sign-ups`) &
                                                   signup$`sign-ups` < mean(signup$`sign-ups`)+2*sd(signup$`sign-ups`))])
mean(mean_attrition$mean_monthly_attrition_rate[which(signup$`sign-ups` > mean(signup$`sign-ups`)+2*sd(signup$`sign-ups`))])


#reg attrition on sign ups

signup$mean_monthly_attendance = 0
for(i in 1:73){
  column = five_day_attendance_list[i]
  signup$mean_monthly_attendance[i] = mean(attition$attrition_rate[which(attition[,3+i] == 1)])
}

signup$attrition_Rate = mean_attrition$mean_monthly_attrition_rate
signup$attrition = mean_attrition$mean_monthly_attrition

reg_attrition_rate = lm(formula = attrition_Rate ~ `sign-ups`,
          data = signup)

summary(reg_attrition_rate)

reg_attrition = lm(formula = attrition ~ `sign-ups`,
                   data = signup)

summary(reg_attrition)

reg_attendance = lm(formula = mean_monthly_attendance ~ `sign-ups`,
                    data = signup)

summary(reg_attendance)

#Month 3 jump

list2 = c()
for(i in 1:nrow(five_day_attendance_dataframe)){ 
  if(five_day_attendance_dataframe$month_attendance_3[i] - five_day_attendance_dataframe$month_attendance_2[i] > 0){
    list2 = c(list2, i)
  }
}


#Variance

signup$variance_ratio = 0

for(i in 1:nrow(signup)){
  signup$variance_ratio[i] = (five_day_attendance_dataframe$month_attendance_variance1[i])/(five_day_attendance_dataframe$month_attendance_variance12[i])
}

reg_variance= lm(formula = variance_ratio ~ `sign-ups`,
                    data = signup)

summary(reg_variance)

length(which(!is.na(Customer_agreement$termination_date)))


#Attrition Data
library(readr)

signup = read_csv("sign_up_data.csv")
attrition = read_csv("attrition_data_5_days.csv")
Customer_agreement_signup = read_csv("Customer_agreement_subset_5.csv")

#-------------------------------------------------------
#Save Dataset
write.csv(biweek_attendance, "biweek_attendance.csv")

write.csv(select_dates_attendance, "select_dates_attendance.csv")

write.csv(five_day_attendance_dataframe, "five_day_attendance_2.csv")

write.csv(signup, "sign_up_data.csv")

write.csv(attition, "attrition_data_5_days.csv")

write.csv(surive_15_data, "diss1.csv")
write.csv(surive_15_data_2, "diss2.csv")
write.csv(surive_15_data_3, "diss3.csv")
write.csv(surive_15_data_4, "diss4.csv")
write.csv(surive_15_data_5, "diss5.csv")
write.csv(surive_15_data_6, "diss6.csv")

write.csv(demographics, "zip_code_demographics.csv")

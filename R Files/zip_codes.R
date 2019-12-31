#Finding ZIP codes for gym members

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

which(Gym_customer$club_of_access != Gym_customer$club_of_enrollment)

participant_checkin_2 = participant_checkin_2[which(participant_checkin_2$participant_id %in% participant_ids),]
participant_checkin_2 = participant_checkin_2[which(participant_checkin_2$checkin_datetime_localtime > "2010-01-01 00:00:00-08:00"), ]
participant_checkin_2 = participant_checkin_2[which(participant_checkin_2$checkin_datetime_localtime < "2017-01-01 00:00:00-08:00"), ]

#--------------------------------------------------------------------------------------------------------------
#Demographics

#Age
Customer_agreement$age = 0

for(i in 1:nrow(Customer_agreement)){
  age = Registration$age[which(Registration$participant_id == Customer_agreement$participant_id[i])]
  Customer_agreement$age[i] = age
}

Customer_agreement$diff_age = 0
for(i in 1:nrow(Customer_agreement)){
  index = which(Registration$participant_id == Customer_agreement$participant_id[i])
  diff_age = difftime(Registration$date[index], Customer_agreement$cleaned_agreement_sign_dt[i])
  Customer_agreement$diff_age[i] = diff_age
}
Customer_agreement$diff_age = Customer_agreement$diff_age/365.25

Customer_agreement$age_signup = 0
Customer_agreement$age_signup = Customer_agreement$age - Customer_agreement$diff_age

Customer_agreement$age_square = Customer_agreement$age ** 2

classifier = glm(formula = day_pass_comparison ~ income + high_school + bachelors + grad + age + age_square + income*bachelors + income*grad,
                 family = binomial(link = "probit"),
                 data = Customer_agreement_2)

summary(classifier)

#Gender

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

#Race

Customer_agreement$white = 0
Customer_agreement$black = 0
Customer_agreement$asian = 0
Customer_agreement$native = 0
Customer_agreement$multi = 0
Customer_agreement$hispanic = 0


for(i in 1:nrow(Customer_agreement)){
  race = c()
  pid = which(Gym_customer$participant_id == Customer_agreement$participant_id[i])[1]
  
  race = c(race, Gym_customer$probwhite[pid])
  race = c(race, Gym_customer$probblack[pid])
  race = c(race, Gym_customer$probasian[pid])
  race = c(race, Gym_customer$probnative[pid])
  race = c(race, Gym_customer$probmulti[pid])
  race = c(race, Gym_customer$probhisp[pid])
  
  race[which(race == "(S)")] = 0
  index = which(race == max(race))
  Customer_agreement[i, 33 + index] = 1
}

length(which(Customer_agreement$participant_id %in% Gym_customer$participant_id))

length(which(Customer_agreement$white == 0 &
               Customer_agreement$black == 0 &
               Customer_agreement$asian == 0 &
               Customer_agreement$native == 0 &
               Customer_agreement$multi == 0 &
               Customer_agreement$hispanic == 0))

#Month of Sign-up

Customer_agreement$Sign_up_month = 0

for(i in 1:nrow(Customer_agreement)){
  Customer_agreement$Sign_up_month[i] = 
    month(as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[i], format="%d/%m/%Y"))
}

Customer_agreement$Sign_up_jan = 0
Customer_agreement$Sign_up_feb = 0
Customer_agreement$Sign_up_mar = 0
Customer_agreement$Sign_up_apr = 0
Customer_agreement$Sign_up_may = 0
Customer_agreement$Sign_up_jun = 0
Customer_agreement$Sign_up_jul = 0
Customer_agreement$Sign_up_aug = 0
Customer_agreement$Sign_up_sep = 0
Customer_agreement$Sign_up_oct = 0
Customer_agreement$Sign_up_nov = 0
Customer_agreement$Sign_up_dec = 0

for(i in 1:nrow(Customer_agreement)){
  sign_up_month = Customer_agreement$Sign_up_month[i]
  Customer_agreement[i, (40 + sign_up_month)] = 1
}

#Year of Sign-up
Customer_agreement$Sign_up_year = 0

for(i in 1:nrow(Customer_agreement)){
  Customer_agreement$Sign_up_year[i] = 
    year(as.POSIXlt(Customer_agreement$cleaned_agreement_sign_dt[i], format="%d/%m/%Y"))
}

Customer_agreement$Sign_up_2010 = 0
Customer_agreement$Sign_up_2011 = 0
Customer_agreement$Sign_up_2012 = 0
Customer_agreement$Sign_up_2013 = 0
Customer_agreement$Sign_up_2014 = 0
Customer_agreement$Sign_up_2015 = 0
Customer_agreement$Sign_up_2016 = 0

for(i in 1:nrow(Customer_agreement)){
  sign_up_year = Customer_agreement$Sign_up_year[i] - 2009
  Customer_agreement[i, (53 + sign_up_year)] = 1
}

#Postal
Customer_agreement$postal = 0

for(i in 1:nrow(Customer_agreement)){
  Customer_agreement$postal[i] = Gym_customer$customer_postal[which(Gym_customer$participant_id == Customer_agreement$participant_id[i])]
}

#-------------------------------------------------------------------------------------------------------------------
#Income and Education via Zip Code

#zip code demographic data

demographics = read_csv("zip_code_data_2016.csv")

demographics = demographics[which(demographics$`Zip Code` %in% Customer_agreement$postal),]

Customer_agreement$income = 0
Customer_agreement$high_school = 0
Customer_agreement$bachelors = 0

for(i in 1:nrow(Customer_agreement)){
  if(Customer_agreement$postal[i] %in% demographics$`Zip Code`){
    index = which(demographics$`Zip Code` == Customer_agreement$postal[i])
    Customer_agreement$income[i] = demographics$median_income[index]
    Customer_agreement$high_school[i] = demographics$highschool[index]
    Customer_agreement$bachelors[i] = demographics$graduate[index]
  }
}

length(which(Customer_agreement$income == 0))



#---------------------------------------------------------------------------------------------------------
#Analysis


#Survive 15 1 

surive_15_data = Customer_agreement[which(Customer_agreement$start_date < "2015-09-30"),]

surive_15_data = surive_15_data[which(surive_15_data$gender != "I"), ]

surive_15_data$female = 0

surive_15_data$female[which(surive_15_data$gender == "M")] = 1

surive_15_data$cancel_boolean = 1

surive_15_data$cancel_boolean[is.na(surive_15_data$cancel_lag)] = 0

surive_15_data_2 = surive_15_data[which(surive_15_data$cleaned_agreement_sign_dt >= "2010-01-01"), ]

classifier = glm(formula = cancel_boolean  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   upgrade_boolean,
                 family = binomial(link = "probit"),
                 data = surive_15_data)

summary(classifier)


classifier = glm(formula = cancel_boolean  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean,
                 family = binomial(link = "probit"),
                 data = surive_15_data_2)

summary(classifier)


classifier = glm(formula = cancel_boolean  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean,
                 family = binomial(link = "probit"),
                 data = surive_15_data)

summary(classifier)

#Survive 15 + Race

surive_15_data_3 = surive_15_data[-which(surive_15_data$white == 0 &
                                           surive_15_data$black == 0 &
                                           surive_15_data$asian == 0 &
                                           surive_15_data$native == 0 &
                                           surive_15_data$multi == 0 &
                                           surive_15_data$hispanic == 0), ]

surive_15_data_4 = surive_15_data_2[-which(surive_15_data_2$white == 0 &
                                             surive_15_data_2$black == 0 &
                                             surive_15_data_2$asian == 0 &
                                             surive_15_data_2$native == 0 &
                                             surive_15_data_2$multi == 0 &
                                             surive_15_data_2$hispanic == 0), ]


classifier = glm(formula = cancel_boolean  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   upgrade_boolean + white + black + asian + native + hispanic +
                   multi,
                 family = binomial(link = "probit"),
                 data = surive_15_data_3)

summary(classifier)


classifier = glm(formula = cancel_boolean  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean +
                   white + black + asian + native + hispanic + multi,
                 family = binomial(link = "probit"),
                 data = surive_15_data_4)

summary(classifier)


classifier = glm(formula = cancel_boolean  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 +
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean + 
                   white + black + asian + native + hispanic + multi,
                 family = binomial(link = "probit"),
                 data = surive_15_data_3)

summary(classifier)

#Survive 3 - income and education

surive_15_data_5 = surive_15_data_3[which(surive_15_data_3$income > 0 &
                                            surive_15_data_3$high_school > 0 &
                                             surive_15_data_3$bachelors > 0), ]

surive_15_data_6 = surive_15_data_4[which(surive_15_data_4$income > 0 &
                                             surive_15_data_4$high_school > 0 &
                                             surive_15_data_4$bachelors > 0), ]

surive_15_data_5$income = as.integer(surive_15_data_5$income)
surive_15_data_6$income = as.integer(surive_15_data_6$income)

classifier = glm(formula = cancel_boolean  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   upgrade_boolean + white + black + asian + native + hispanic +
                   multi + income + high_school + bachelors,
                 family = binomial(link = "probit"),
                 data = surive_15_data_5)

summary(classifier)


classifier = glm(formula = cancel_boolean  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean +
                   white + black + asian + native + hispanic + multi +
                   income + high_school + bachelors,
                 family = binomial(link = "probit"),
                 data = surive_15_data_6)

summary(classifier)


classifier = glm(formula = cancel_boolean  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean + 
                   white + black + asian + native + hispanic + multi +
                   income + high_school + bachelors,
                 family = binomial(link = "probit"),
                 data = surive_15_data_5)

summary(classifier)

#---------------------
#Day Pass 1 Comparison

classifier = glm(formula = day_pass_comparison ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   upgrade_boolean,
                 family = binomial(link = "probit"),
                 data = surive_15_data)

summary(classifier)


classifier = glm(formula = day_pass_comparison  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean,
                 family = binomial(link = "probit"),
                 data = surive_15_data_2)

summary(classifier)


classifier = glm(formula = day_pass_comparison  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean,
                 family = binomial(link = "probit"),
                 data = surive_15_data)

summary(classifier)

#Day Pass 2 + Race

classifier = glm(formula = day_pass_comparison ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   upgrade_boolean + white + black + asian + native + hispanic +
                   multi,
                 family = binomial(link = "probit"),
                 data = surive_15_data_3)

summary(classifier)


classifier = glm(formula = day_pass_comparison  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean +
                   white + black + asian + native + hispanic + multi,
                 family = binomial(link = "probit"),
                 data = surive_15_data_4)

summary(classifier)


classifier = glm(formula = day_pass_comparison  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 +
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean + 
                   white + black + asian + native + hispanic + multi,
                 family = binomial(link = "probit"),
                 data = surive_15_data_3)

summary(classifier)

#Day Pass 3 - income and education

classifier = glm(formula = day_pass_comparison ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   upgrade_boolean + white + black + asian + native + hispanic +
                   multi + income + high_school + bachelors,
                 family = binomial(link = "probit"),
                 data = surive_15_data_5)

summary(classifier)


classifier = glm(formula = day_pass_comparison  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean +
                   white + black + asian + native + hispanic + multi +
                   income + high_school + bachelors,
                 family = binomial(link = "probit"),
                 data = surive_15_data_6)

summary(classifier)


classifier = glm(formula = day_pass_comparison  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean + 
                   white + black + asian + native + hispanic + multi +
                   income + high_school + bachelors,
                 family = binomial(link = "probit"),
                 data = surive_15_data_5)

summary(classifier)



#Cancellation lags 1

customer_survival = read_csv("Customer_survival.csv")

customer_survival = surive_15_data[which(surive_15_data$cancel_boolean == 1), ]

customer_survival_2 = surive_15_data_2[which(surive_15_data_2$cancel_boolean == 1), ]

customer_survival_3 = surive_15_data_3[which(surive_15_data_3$cancel_boolean == 1), ]

customer_survival_4 = surive_15_data_4[which(surive_15_data_4$cancel_boolean == 1), ]

customer_survival_5 = surive_15_data_5[which(surive_15_data_5$cancel_boolean == 1), ]

customer_survival_6 = surive_15_data_6[which(surive_15_data_6$cancel_boolean == 1), ]

reg = lm(formula = cancel_lag ~ female + age + age_square + 
           Sign_up_feb + Sign_up_mar + Sign_up_apr +
           Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
           Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
           upgrade_boolean,
         data = customer_survival)

summary(reg)


reg = lm(formula = cancel_lag  ~ female + age + age_square + 
           Sign_up_feb + Sign_up_mar + Sign_up_apr +
           Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
           Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
           Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
           Sign_up_2014 + Sign_up_2015 + upgrade_boolean,
         data = customer_survival_2)

summary(reg)


reg= lm(formula = day_pass_comparison  ~ female + age + age_square + 
          Sign_up_feb + Sign_up_mar + Sign_up_apr +
          Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
          Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
          Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
          Sign_up_2014 + Sign_up_2015 + upgrade_boolean,
        data = customer_survival)

summary(reg)

#Cancellation Lags 2 + Race

reg = lm(formula = cancel_lag ~ female + age + age_square + 
           Sign_up_feb + Sign_up_mar + Sign_up_apr +
           Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
           Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
           upgrade_boolean + white + black + asian + native + hispanic +
           multi,
         data = customer_survival_3)

summary(reg)


reg = lm(formula = cancel_lag  ~ female + age + age_square + 
           Sign_up_feb + Sign_up_mar + Sign_up_apr +
           Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
           Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
           Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
           Sign_up_2014 + Sign_up_2015 + upgrade_boolean +
           white + black + asian + native + hispanic + multi,
         data = customer_survival_4)

summary(reg)


reg= lm(formula = day_pass_comparison  ~ female + age + age_square + 
          Sign_up_feb + Sign_up_mar + Sign_up_apr +
          Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
          Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
          Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
          Sign_up_2014 + Sign_up_2015 + upgrade_boolean + 
          white + black + asian + native + hispanic + multi,
        data = customer_survival_3)

summary(reg)

#Cancellation Lags 3 + income and education
reg = lm(formula = cancel_lag ~ female + age + age_square + 
           Sign_up_feb + Sign_up_mar + Sign_up_apr +
           Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
           Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
           upgrade_boolean + white + black + asian + native + hispanic +
           multi + income + high_school + bachelors,
         data = customer_survival_5)

summary(reg)


reg = lm(formula = cancel_lag  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean +
                   white + black + asian + native + hispanic + multi +
                   income + high_school + bachelors,
                 data = customer_survival_6)

summary(reg)


reg= lm(formula = day_pass_comparison  ~ female + age + age_square + 
                   Sign_up_feb + Sign_up_mar + Sign_up_apr +
                   Sign_up_may + Sign_up_jun + Sign_up_jul + Sign_up_aug +
                   Sign_up_sep + Sign_up_oct + Sign_up_nov + Sign_up_dec +
                   Sign_up_2010 + Sign_up_2011 + Sign_up_2012 + Sign_up_2013 + 
                   Sign_up_2014 + Sign_up_2015 + upgrade_boolean + 
                   white + black + asian + native + hispanic + multi +
                   income + high_school + bachelors,
                 data = customer_survival_5)

summary(reg)

#-----------------------------------------------------
#Write Files

write.csv(zip_codes, "zip_code_data.csv")

write.csv(Customer_agreement, "diss_customer_agreement.csv")









#Finding number of unique gyms attendend

participant_ids = Customer_agreement$participant_id
club_nums = participant_checkin_2$club_num
different_gyms_list = c()

for(i in 1:length(participant_ids)){
  different_gyms = length(unique(club_nums[which(participant_checkin_2$participant_id == 
                                                   participant_ids[i])]))
  different_gyms_list = c(different_gyms_list, different_gyms)
}

max(different_gyms_list)
mean(different_gyms_list)
median(different_gyms_list)


#Finding zip codes of gyms attendended

zip_codes_2 = unique(participant_ids)
zip_codes = as.data.frame(unique(participant_ids))
participant_ids = unique(participant_ids)

zip_codes$zip_code1 = NA
zip_codes$zip_code2 = NA
zip_codes$zip_code3 = NA
zip_codes$zip_code4 = NA
zip_codes$zip_code5 = NA
zip_codes$zip_code6 = NA

zip_codes$zip_code_attendance_1 = NA
zip_codes$zip_code_attendance_2 = NA
zip_codes$zip_code_attendance_3 = NA
zip_codes$zip_code_attendance_4 = NA
zip_codes$zip_code_attendance_5 = NA
zip_codes$zip_code_attendance_6 = NA

zip_codes$club_num1 = NA
zip_codes$club_num2 = NA
zip_codes$club_num3 = NA
zip_codes$club_num4 = NA
zip_codes$club_num5 = NA
zip_codes$club_num6 = NA

zip_codes$total_attendance = 0

zip_codes = zip_codes[which(!is.na(zip_codes$zip_code1)),]

participant_ids = zip_codes$`unique(participant_ids)`
for(i in 1:nrow(zip_codes)){
  checkins = participant_checkin_2$club_num[which(participant_checkin_2$participant_id == participant_ids[i])]
  if(length(checkins) > 0){
    table = as.data.frame(table(checkins))
    ordered_table = table[order(table$Freq, decreasing = TRUE),]
    zip_code_values = ordered_table$checkins
    zip_code_values = droplevels(zip_code_values)
    zip_code_freq = ordered_table$Freq
    zip_code_total = sum(zip_code_freq)
    for(j in 1:min(length(zip_code_values), 6)){
      zip_codes[i, j+13] = as.character(zip_code_values[j])
      zip_codes[i, j+1] = zip_code_freq[j]
    }
    zip_codes$total_attendance[i] = length(checkins)
  }
}


for(i in 1:nrow(zip_codes)){
  for(j in 1:6){
    gym = zip_codes[i, j+13]
    if(length(which(Club_site_expanded$club_src_num == gym) > 0)){
      zip_codes[i, j+7] = Club_site_expanded$club_postal[which(Club_site_expanded$club_src_num == gym)]
    }
  }
}

zip_codes$percentage_2_gyms = 1

for(i in 1:nrow(zip_codes)){
  if(!is.na(zip_codes$zip_code2[i])){
    gym_attendance = zip_codes$zip_code1[i] + zip_codes$zip_code2[i]
    total_attendance = zip_codes$total_attendance[i]
    zip_codes$percentage_2_gyms[i] = gym_attendance/total_attendance
  }
}

for(i in 1:6){
  zip_codes_2 = zip_codes_2[-which(zip_codes[, i+13] %in% "Unknown"),]
}

unique(zip_codes$zip_code_attendance_1)



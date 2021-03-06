library(readr)
Customer_agreement = read_csv("Customer_Agreement_subset_4.csv")

Gym_customer = read_csv("Gym_Customer.csv")

Customer_agreement$gender = NA

for(i in 1:length((Customer_agreement$participant_id))){
  participant_gender = Gym_customer$gender[which(Gym_customer$participant_id == Customer_agreement$participant_id[i])]
  participant_gender_predict = Gym_customer$gender_predict[which(Gym_customer$participant_id == Customer_agreement$participant_id[i])]
  
  if(!is.na(participant_gender) | participant_gender != "I"){
    if(participant_gender == "M"){
      Customer_agreement$gender[i] = 0
    }
    else if(participant_gender == "F"){
      Customer_agreement$gender[i] = 1
    }
  else{
    if(!is.na(participant_gender_predict)){
      if(participant_gender_predict == "M"){
          Customer_agreement$gender[i] = 0
      }
      else if(participant_gender_predict == "F"){
          Customer_agreement$gender[i] = 1
      }
    }
  }
  }
}

Customer_agreement$cancel_boolean = 0

Customer_agreement$cancel_boolean[-which(Customer_agreement$end_date == "2017-01-01")] = 1

#---------------------------------------------------------------------------------------------------------------
install.packages("survival")
library(survival)
cancellation_lag_subset = Customer_agreement[which(!is.na(Customer_agreement$cancel_lag)),]

kmsurvival = survfit(Surv(Customer_agreement$date_diff_month, Customer_agreement$cancel_boolean) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab = "Time", ylab = "Survival Probability")


require(survival)
library(ggplot2)
install.packages("survminer")
library(survminer)

qts = Surv(Customer_agreement$date_diff_month, Customer_agreement$cancel_boolean)
est1 = survfit(qts ~ 1)
str(est1) # Summary returned as a list
plot(est1, main = "Kaplan-Meier estimate with 95% confidence bounds", xlab="Time (months)", ylab = "Survival function")
### Plot treatment
stype = survfit(qts ~ Customer_agreement$upgrade_boolean, data = Customer_agreement)
names(stype$strata) = c("Upgrade","No Upgrade")
summary(stype)
plt = ggsurvplot(stype)
plt +   theme(legend.title=element_blank(),
           legend.position = c(0.8,0.8), text = element_text(size=18),
           axis.title.y=element_text(margin=margin(0,20,0,0)),
           legend.key.size = unit(1.5, 'lines'),
           plot.margin = unit(c(1,1,1,1), "cm")) +
 scale_x_continuous(limits=c(0, 12), expand = c(0, 0), breaks=seq(0, 12, 4)) +
 scale_y_continuous(limits=c(0.5,1), expand = c(0, 0), breaks=seq(0,1,0.2)) # Change lims

plt
#
#Frailty Package - 


#-------------------------------------------------------------------------------------------------------------------------
library(readr)
Customer_agreement = read_csv("Customer_Agreement_subset_5.csv")
Customer_agreement_2 = read_csv("Customer_Agreement_subset_4.csv")
Customer_agreement = Customer_agreement[,-c(1,2)]


Customer_survival = Customer_agreement[which(!is.na(Customer_agreement$cancel_lag)),]

Customer_survival$survival_15 = 0

Customer_survival$survival_15[which(Customer_survival$date_diff_month > 15)] = 1

Customer_survival$survival_12 = 0

Customer_survival$survival_12[which(Customer_survival$date_diff_month > 12)] = 1

library(lubridate)

Customer_survival$Sign_up_month = ""

Months = c("January", "Februray", "March", "April", "May", "June", "July",
           "August", "September", "October", "November", "December")

for(i in 1:nrow(Customer_survival)){
  Customer_survival$Sign_up_month[i] = Months[
    month(as.POSIXlt(Customer_survival$cleaned_agreement_sign_dt[i], format="%d/%m/%Y"))]
}

Customer_survival$Sign_up_month_Jan = 0
Customer_survival$Sign_up_month_Feb = 0
Customer_survival$Sign_up_month_Mar = 0
Customer_survival$Sign_up_month_Apr = 0
Customer_survival$Sign_up_month_May = 0
Customer_survival$Sign_up_month_Jun = 0
Customer_survival$Sign_up_month_Jul = 0
Customer_survival$Sign_up_month_Aug = 0
Customer_survival$Sign_up_month_Sep = 0
Customer_survival$Sign_up_month_Oct = 0
Customer_survival$Sign_up_month_Nov = 0
Customer_survival$Sign_up_month_Dec = 0

for(i in 1:length(Months)){
  for(j in 1:nrow(Customer_survival)){
    if(Customer_survival$Sign_up_month[j] == Months[i]){
      Customer_survival[j, 55 + i] = 1
    }
  }
}


Registration$date = format(strptime(Registration$signup_datetime_localtime,
                                    "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")

Customer_survival$age = 0

for(i in 1:nrow(Customer_survival)){
  age = Registration$age[which(Registration$participant_id == Customer_survival$participant_id[i])]
  Customer_survival$age[i] = age
}

Customer_survival$diff_age = 0
for(i in 1:nrow(Customer_survival)){
  index = which(Registration$participant_id == Customer_survival$participant_id[i])
  diff_age = difftime(Registration$date[index], Customer_survival$cleaned_agreement_sign_dt[i])
  Customer_survival$diff_age[i] = diff_age
}
Customer_survival$diff_age = Customer_survival$diff_age/365.25

Customer_survival$age_signup = 0
Customer_survival$age_signup = Customer_survival$age - Customer_survival$diff_age
Customer_survival$age_signup = round(Customer_survival$age_signup)

#----------------------------------------------------------------------------------
#Probit

Customer_survival$age_signup_square = (Customer_survival$age_signup)**2

classifier = glm(formula = survival_15 ~ gender + age_signup + age_signup_square + upgrade_boolean + day_pass_comparison + 
                   Sign_up_month_Jan + Sign_up_month_Feb + Sign_up_month_Mar + Sign_up_month_Apr + Sign_up_month_May +
                   Sign_up_month_Jun + Sign_up_month_Jul + Sign_up_month_Aug + Sign_up_month_Sep + Sign_up_month_Oct +
                   Sign_up_month_Nov + Sign_up_month_Dec,
                 family = binomial(link = "probit"),
                 data = Customer_survival)

summary(classifier)

classifier = glm(formula = day_pass_comparison ~ gender + age_signup + age_signup_square + upgrade_boolean + 
                   Sign_up_month_Jan + Sign_up_month_Feb + Sign_up_month_Mar + Sign_up_month_Apr + Sign_up_month_May +
                   Sign_up_month_Jun + Sign_up_month_Jul + Sign_up_month_Aug + Sign_up_month_Sep + Sign_up_month_Oct +
                   Sign_up_month_Nov + Sign_up_month_Dec,
                 family = binomial(link = "probit"),
                 data = Customer_survival)

summary(classifier)

classifier = glm(formula = survival_12 ~ gender + age_signup + age_signup_square + upgrade_boolean + day_pass_comparison + 
                   Sign_up_month_Jan + Sign_up_month_Feb + Sign_up_month_Mar + Sign_up_month_Apr + Sign_up_month_May +
                   Sign_up_month_Jun + Sign_up_month_Jul + Sign_up_month_Aug + Sign_up_month_Sep + Sign_up_month_Oct +
                   Sign_up_month_Nov + Sign_up_month_Dec,
                 family = binomial(link = "probit"),
                 data = Customer_survival)

summary(classifier)


write.csv(Customer_survival, "Customer_survival.csv")
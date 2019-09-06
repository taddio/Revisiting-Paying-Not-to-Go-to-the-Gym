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

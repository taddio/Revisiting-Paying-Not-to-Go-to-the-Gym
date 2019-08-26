Survey_Results = read_csv("Survey_Results.csv") #Read Club Site data

Caltech_expected_attendance = Survey_Results$Coded_Q4[which(Survey_Results$Q1 == 1)] #Sample of 36
Caltech_expected_attendance_all = Survey_Results$Coded_Q4[which(Survey_Results$Q1 %in% c(1, 2))] #Sample of 41

hist(Caltech_expected_attendance, breaks = c(0,4,8,12,16,20,24,28, 32), freq=FALSE, col="gray", 
     xlab="Expected Average Monthly Gym Attendance", 
     main="Expected Average Monthly Gym Attendance Distribution (Caltech)")
curve(dnorm(x, mean=mean(Caltech_expected_attendance),
            sd(Caltech_expected_attendance)), add=TRUE, col="red") #line

hist(Caltech_expected_attendance_all, breaks = c(0,4,8,12,16,20,24,28, 32), freq=FALSE, col="gray", 
     xlab="Expected Average Monthly Gym Attendance", 
     main="Expected Average Monthly Gym Attendance Distribution (Caltech and other)")
curve(dnorm(x, mean=mean(Caltech_expected_attendance_all),
            sd(Caltech_expected_attendance_all)), add=TRUE, col="red") #line

mean(Caltech_expected_attendance_all)
median(Caltech_expected_attendance_all)
var(Caltech_expected_attendance_all)
sqrt(var(Caltech_expected_attendance_all))
max(Caltech_expected_attendance_all)
min(Caltech_expected_attendance_all)
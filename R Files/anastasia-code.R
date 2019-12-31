library(data.table)
#Importing large dataset
data2= fread("FinishedPopulatedTruncatedMasterfile2018ALLv8.csv")

participant_ids = unique(data2$participant_id)
indices = c()
for(i in 1:length(participant_ids)){
  participant_indices = which(data2$participant_id == participant_ids[i])
  subset_streaks = data2$streak_length[participant_indices]
  if(length(which(subset_streaks == 3)) > 0){
    indices = c(indices, participant_indices)
  }
}



data2.AtLeastOne3DayStreak = data2[indices, ]

participant_ids = unique(data2.AtLeastOne3DayStreak$participant_id)


data2$total_attendance = 0

total_attendance = data2$total_attendance

attendance_table = table(data2$participant_id[which(data2$attended == 1)])
attendance_table = data.table(attendance_table)

attendance_frequencies = attendance_table$N
attendance_pid = attendance_table$V1
participants = data2$participant_id
for(i in 1:length(attendance_pid)){
  indices = which(participants == attendance_pid[i])
  total_attendance[indices] = attendance_frequencies[i]
}

data2$total_attendance = total_attendance

write.csv(data2, "FinishedPopulatedTruncatedMasterfile2018ALLv8.csv")

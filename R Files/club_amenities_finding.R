#Importing smaller dataset
library(readr)
Club_site = read_csv("Club_site_expanded.csv")
Club_site = Club_site[, -1]
Club_amenities = read_csv("Club_amenity.csv")

#-----------------------------------------------------------------
#Number of unique amenities
library(dplyr)
amenities = Club_amenities$club_amenity_description
unique_amenities = unique(amenities)
print(unique_amenities)

club_src_numbers = Club_site$club_src_num
club_numbers_amenities = Club_amenities$club_number
no_amenities = c()

for(i in 1:length(club_src_numbers)){
  index = which(club_numbers_amenities %in% club_src_numbers[i])
  if(length(index) == 0){
    no_amenities = c(no_amenities, club_src_numbers[i])
  }
}

club_amenities_check = c()
for(i in 1:length(club_src_numbers)){
  index = which(club_numbers_amenities %in% club_src_numbers[i])
  if(length(index) != 0){
    club_amenities_check = c(club_amenities_check, club_src_numbers[i])
  }
}

Club_site$closed = 0
closed = Club_site$closed
closed[191] = 1

print(no_amenities)

#---------------------------------
#Adding Amenities to Club Site Data
Club_site$group_exercise = 0
Club_site$steam_room = 0
Club_site$indoor_lap_pool = 0
Club_site$training_club_24_ignite = 0
Club_site$trx_suspension_training = 0
Club_site$cardio_equipment = 0
Club_site$group_cycling = 0
Club_site$pro_shop = 0
Club_site$free_weights = 0
Club_site$whirlpool = 0
Club_site$personal_training_area = 0
Club_site$personal_training = 0
Club_site$training_club_24_torch = 0
Club_site$sauna = 0
Club_site$strength_machines = 0
Club_site$kids_club = 0
Club_site$circuit_training = 0
Club_site$free_wifi = 0
Club_site$circuit_training = 0
Club_site$coin_collect_lockers = 0
Club_site$outdoor_lap_pool = 0
Club_site$full_sized_basketball_court = 0
Club_site$towel_service = 0
Club_site$volleyball = 0
Club_site$sun_deck = 0
Club_site$lounge = 0
Club_site$personal_viewing_screens = 0
Club_site$racquetball_court = 0
Club_site$juice_bar = 0
Club_site$practice_basketball_court = 0
Club_site$rock_climbing = 0
Club_site$physical_therapy_sports_med = 0
Club_site$massage = 0
Club_site$tennis = 0
Club_site$executive_lockers = 0
Club_site$pilates_reformer_studio = 0
Club_site$squash_courts = 0
Club_site$restaurant = 0
Club_site$non_reg_basketball_court = 0
Club_site$family_pool = 0
Club_site$turf_zone = 0
Club_site$tenth_degree = 0
Club_site$training_club_24_snow = 0
Club_site$olympic_training_rig = 0
Club_site$safesplash = 0
Club_site$se_habla_espanol = 0
Club_site$water_aerobics_pool = 0
Club_site$exec_lockers_w_laundry = 0
Club_site$parisi_speed_school = 0
Club_site$tanning = 0
Club_site$spa = 0

unique_amenities_col_list = c("group_exercise",
                              "steam_room",
                              "training_club_24_ignite",
                              "indoor_lap_pool",
                              "cardio_equipment",
                              "trx_suspension_training",
                              "group_cycling",
                              "pro_shop",
                              "free_weights",
                              "whirlpool",
                              "personal_training_area",
                              "personal_training",
                              "training_club_24_torch",
                              "sauna",
                              "strength_machines",
                              "kids_club",
                              "free_wifi",
                              "circuit_training",
                              "coin_collect_lockers",
                              "outdoor_lap_pool",
                              "full_sized_basketball_court",
                              "towel_service",
                              "volleyball",
                              "sun_deck",
                              "lounge",
                              "personal_viewing_screens",
                              "racquetball_court",
                              "juice_bar",
                              "practice_basketball_court",
                              "rock_climbing",
                              "physical_therapy_sports_med",
                              "massage",
                              "tennis",
                              "executive_lockers",
                              "pilates_reformer_studio",
                              "squash_courts",
                              "restaurant",
                              "non_reg_basketball_court",
                              "family_pool",
                              "olympic_training_rig",
                              "turf_zone",
                              "tenth_degree",
                              "training_club_24_snow",
                              "tanning",
                              "spa",
                              "safesplash",
                              "se_habla_espanol",
                              "water_aerobics_pool",
                              "exec_lockers_w_laundry",
                              "parisi_speed_school")

print(Club_site[, unique_amenities_col_list[1]])

for(i in 1:length(unique_amenities_col_list)){
  club_list = c()
  indices = which(amenities %in% unique_amenities[i])
  for(j in 1:length(indices)){
    club_list = c(club_list, club_numbers_amenities[indices[j]])
  }
  club_list_indices = which(club_src_numbers %in% club_list)
  Club_site[club_list_indices, unique_amenities_col_list[i]] = 1
}

#------------------------------------------
#Club Type IDs
Club_site = Club_site[c(1:457),]
ids = c()
club_type = Club_site$club_type
for(i in 1:length(club_src_numbers)){
  if(club_type[i] == "Active"){
    ids = c(ids, 1)
  }
  else if(club_type[i] == "Sport"){
    ids = c(ids, 2)
  }
  else if(club_type[i] == "SuperSport"){
    ids = c(ids, 3)
  }
  else{
    ids = c(ids, 4)
  }
}
Club_site$club_type_id = ids

#-------------------------------------
#Identifying Manual Changes in Amenity Data

manual_entry_list = c(229, 386, 388, 398, 410, 427, 441, 602, 618, 633,
                      634, 635, 636, 637, 640, 644, 731, 732, 780, 801,
                      844, 846, 847, 874, 876, 898, 936, 937, 940, 970)

Club_amenities$manual_check = 0
for(i in manual_entry_list){
  indices = which(Club_amenities$club_number %in% manual_entry_list)
  Club_amenities[indices, "manual_check"] = 1
}

#------------------
#Save Data
write.csv(Club_site, "Club_site_expanded.csv")
write.csv(Club_amenities, "Club_amenities_expanded.csv")
library(readxl)
library(tidyverse)
#read the scene list from Payne and Kensinger list
scene_list<- read_excel("C:/Stanford/PSYCH251 experimental methods/replication project/my tasks/Payne_Kensinger_stimuli_list.xlsx")

# exclude unnecessary scenes 
my_list <- scene_list %>% 
  filter(counterbalancing != "new")

# according to the list1 in the counterbalancing plan, create the list of scenes in the coding task
my_list = my_list %>% 
  mutate(filename1 = ifelse(counterbalancing == 'A', str_c(background, "1_", neutral_obj, "1.png", sep = ""), #counterbalancing label A, neutral1 + background1
                           ifelse(counterbalancing == 'B', str_c(background, "1_", neutral_obj, "2.png", sep = ""), #counterbalancing label B
                                  ifelse(counterbalancing == 'C', str_c(background, "1_", negative_obj, "1.png", sep = ""), #counterbalancing label C
                                    ifelse(counterbalancing == 'D', str_c(background, "1_", negative_obj, "2.png", sep = ""), #counterbalancing label D
                                           ifelse(counterbalancing == 'E', str_c(background, "2_", neutral_obj, "1.png", sep = ""), #counterbalancing label E 
                                                  ifelse(counterbalancing == 'F', str_c(background, "2_", neutral_obj, "2.png", sep = ""), #counterbalancing label F
                                                         ifelse(counterbalancing == 'G', str_c(background, "2_", negative_obj, "1.png", sep = ""), #counterbalancing label G
                                                                ifelse(counterbalancing == 'H', str_c(background, "2_", negative_obj, "2.png", sep = ""), 'not known yet')))))))))  #counterbalancing label H

my_list = my_list %>% 
  mutate(filename1_raw = ifelse(counterbalancing == 'A', str_c(background, "1_", neutral_obj, "1.pict", sep = ""), #counterbalancing label A, neutral1 + background1
                            ifelse(counterbalancing == 'B', str_c(background, "1_", neutral_obj, "2.pict", sep = ""), #counterbalancing label B
                                   ifelse(counterbalancing == 'C', str_c(background, "1_", negative_obj, "1.pict", sep = ""), #counterbalancing label C
                                          ifelse(counterbalancing == 'D', str_c(background, "1_", negative_obj, "2.pict", sep = ""), #counterbalancing label D
                                                 ifelse(counterbalancing == 'E', str_c(background, "2_", neutral_obj, "1.pict", sep = ""), #counterbalancing label E 
                                                        ifelse(counterbalancing == 'F', str_c(background, "2_", neutral_obj, "2.pict", sep = ""), #counterbalancing label F
                                                               ifelse(counterbalancing == 'G', str_c(background, "2_", negative_obj, "1.pict", sep = ""), #counterbalancing label G
                                                                      ifelse(counterbalancing == 'H', str_c(background, "2_", negative_obj, "2.pict", sep = ""), 'not known yet')))))))))  #counterbalancing label H


#create the list of background in the recognition task (same and similar)
my_list = my_list %>% 
  mutate(recog_background = str_c(background, "1.png", sep = ""))
#create the list of objects in the recognition task (same and similar)
my_list = my_list %>% 
  mutate(recog_object = ifelse(counterbalancing == 'A'| counterbalancing == 'B'|counterbalancing == 'E'|counterbalancing == 'F', str_c(neutral_obj, "1.png", sep = ""), str_c(negative_obj, "1.png", sep = "")))
my_list = my_list %>% 
  mutate(recog_object_raw = str_c(substr(recog_object, 1, str_length(recog_object)-5), "1.pict", sep = ""))
#create the list of new objects
my_list = my_list %>% 
  mutate(new_object = ifelse(counterbalancing == 'A'| counterbalancing == 'B'|counterbalancing == 'E'|counterbalancing == 'F', str_c(negative_obj, "1.pict", sep = ""), str_c(neutral_obj, "1.pict", sep = "")))

#output my list
write_csv(my_list, file.path("C:/Stanford/PSYCH251 experimental methods/replication project/my tasks", "my_stimuli_list.csv"))



#read the processed list spreedsheet
final_list<- read_excel("C:/Stanford/PSYCH251 experimental methods/replication project/my tasks/list_all_files_in_use.xlsx")
#combine the conventional filename and the adhoc filenames
final_list<- final_list %>% 
  mutate(scene_final_raw = ifelse(is.na(adhoc_filenames), filename1_raw, adhoc_filenames))
#make the list of .png files
final_list <- final_list %>% 
  mutate(scene_final = str_c(substr(scene_final_raw, 1, str_length(scene_final_raw)-5), ".png", sep = ""),
         recog_background = str_c(substr(background_file, 1, str_length(background_file)-5), ".png", sep = ""),
         recog_object = str_c(substr(recog_object_raw, 1, str_length(recog_object_raw)-5), ".png", sep = ""))

# a list of new objects in .png
new_object = c()
for (name in final_list$new_object) {
  if(name != "na"){
    new_object = append(new_object, str_c(substr(name, 1, str_length(name)-5), ".png", sep = ""))
  }
}
# a list of new backgrounds
new_background = c()
for (name in final_list$new_background) {
  if(name != "na"){
    new_background = append(new_background, str_c(substr(name, 1, str_length(name)-5), ".png", sep = ""))
  }
}
# combine new objects and backgrounds
final_list$new_item = append(new_object,new_background[1:32])

link_pre = '<img src="'
link_post = ' style="width: 450px; height: 600px;">'
link_website = "http://web.stanford.edu/~jzhang18/psych251/my_tasks/pic/"

final_list <- final_list %>% 
  mutate(scene_final_link = str_c(link_pre, link_website, scene_final, link_post))

#output the final list
write_csv(final_list, file.path("C:/Stanford/PSYCH251 experimental methods/replication project/my tasks", "final_list.csv"))



save.image(file.path("C:/Stanford/PSYCH251 experimental methods/replication project/my tasks", "make_stimuli_list.Rdata"))

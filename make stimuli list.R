library(readxl)
library(tidyverse)
# link to the original stimuli list on Kensinger's labpage: https://www2.bc.edu/elizabeth-kensinger/NegNeutScenes.html
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
  mutate(recog_object = ifelse(counterbalancing == 'A'| counterbalancing == 'B'|counterbalancing == 'E'|counterbalancing == 'F', 
                               str_c(neutral_obj, "1.png", sep = ""), str_c(negative_obj, "1.png", sep = "")))
my_list = my_list %>% 
  mutate(recog_object_raw = str_c(substr(recog_object, 1, str_length(recog_object)-5), 
                                  "1.pict", sep = ""))
#create the list of new objects
my_list = my_list %>% 
  mutate(new_object = ifelse(counterbalancing == 'A'| counterbalancing == 'B'|counterbalancing == 'E'|counterbalancing == 'F', 
                             str_c(negative_obj, "1.pict", sep = ""), str_c(neutral_obj, "1.pict", sep = "")))

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
link_post = '" style="width: 450px; height: 600px;">'
link_website = "http://web.stanford.edu/~jzhang18/psych251/my_tasks/pic/processed/"

final_list <- final_list %>% 
  mutate(scene_final_link = str_c(link_pre, link_website, scene_final, link_post),
         recog_object_link = str_c(link_pre, link_website, recog_object, link_post),
         recog_background_link = str_c(link_pre, link_website, recog_background, link_post),
         new_item_link = str_c(link_pre, link_website, new_item, link_post))

#output the final list
write_csv(final_list, file.path("C:/Stanford/PSYCH251 experimental methods/replication project/my tasks", "final_list.csv"))

#create a list of all items in Session 2
#item: 64 recog_object, 64 recog_background, 64 new items
item <- append(final_list$recog_object, final_list$recog_background)
item <- append(item, final_list$new_item)

#create a list of all item links in Session 2
item_link <- append(final_list$recog_object_link, final_list$recog_background_link)
item_link <- append(item_link, final_list$new_item_link)

#import the questions from the excel spreedsheet
questions <- read_excel("C:/Stanford/PSYCH251 experimental methods/replication project/my tasks/Payne_Kensinger_stimuli_list.xlsx", 3, range = cell_cols("A:B"))
names(questions)[2] = "item"

#create a dataframe of items, item links, and their corresponding questions
questions_list <- data.frame("item" = item,"item_link" = item_link)
#find the corresponding question
for (i in 1:nrow(questions_list)){
  for (j in 1:nrow(questions)){
    tmp_item = questions_list$item[i]
    tmp_name = questions$item[j]
    if (str_sub(tmp_item, 1, str_length(tmp_item)-5) == str_sub(tmp_name, 1, str_length(tmp_name)-6)){
      questions_list$question[i] = questions$question[j]
      #print(sprintf("%d %d", i, j)) #show the i-j pair
      break
    }
  }
}

#output the question list
write_csv(questions_list, file.path("C:/Stanford/PSYCH251 experimental methods/replication project/my tasks", "questions_list.csv"))


#practice files
practice <- data.frame("scene" = c("zoo.png","show.png"), "item" = c("polar_bear.png","rhinoceros.png", "desert.png", "tennis court.png"))
practice <- practice %>% 
  mutate(scene_link = str_c(link_pre, link_website, scene, link_post),
         item_link = str_c(link_pre, link_website, item, link_post),
         question = str_c("Did you see a ", str_sub(item, 1, str_length(item)-4),"?"))
#export practice list
write_csv(practice, file.path("C:/Stanford/PSYCH251 experimental methods/replication project/my tasks", "practice_list.csv"))


save.image(file.path("C:/Stanford/PSYCH251 experimental methods/replication project/my tasks", "make_stimuli_list.Rdata"))

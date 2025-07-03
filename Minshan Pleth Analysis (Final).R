#Reorganized Minshan's Pleth Script


#Set variables here------------------------------------------------------------------------------------------------------------------------

#directory containing all pleth-related folders 
#(there should be a folder named "Raw" in here)
root_folder = "F:/01 Data/06 Pleth/Pleth_manual_selected_data/ChatPitx2_2M_5co2_PDF"


#this is the file containing your sample data
#it should be placed in the root folder
sample_data_file = "F:/01 Data/06 Pleth/Pleth_manual_selected_data/ChatPitx2CRE_mouse_details.xlsx"


#this is the file containing manually selected resting periods
#it should be placed in the root folder
#its columns should include "File.name", "Start.time", and "End.time"
manual_rest_file = "F:/01 Data/06 Pleth/Pleth_manual_selected_data/P5_Pleth_trials_selected.xlsx"


#what format should the graphs be exported as
export_as = "pdf"


#list out whichever parameters you would like plotted
#the options are:
    #"PIF","PEF",
    #"Tidal.Volume","Expired.Volume"
    #"Frequency",
    #"Inspiratory.Time","Expiratory.Time",
    #"Minute.Ventilation"
plot_values = c("PIF","PEF",
                "Tidal.Volume",
                "Frequency",
                "Minute.Ventilation")


#set this to however many bins you want on your distribution plots
number_of_bins = 25



#the number of breaths that will be grouped to determine resting periods
#more breaths means a broader view, but standard deviation is also related to sample number
breaths_per_chunk = 10

#Define cutoffs for exclusion
TV_EV_cutoff = 40     #the permissible % difference between TV and EV on a given breath
Rejected_cutoff = 25  #the permissible number of peaks rejected by iox within the chunk

#c1 = criteria 1: TV_EV_cutoff = 30, Rejected_cutoff = 5
#c2 = criteria 2: TV_EV_cutoff = 40, Rejected_cutoff = 25

time_from_trial_start = 0 #seconds # 600 without manual selection
time_from_CO2_switch = 0 # 300 without manual selection
time_in_CO2 = 900








#Import libraries & save functions---------------------------------------------------------------------------------------------------------
library(stringr)
library(chron)
library(ggplot2)
library(hms)
library(gridExtra)
library(dplyr)
library(readxl)
library(ggpubr)

#generates box plot which includes statistic test based on distribution
custom_boxplot = function(dataframe, 
                          x, 
                          y, 
                          grouping = NA, 
                          #shape = NA, 
                          outfile = NA, 
                          y_label = NA, 
                          stats = T, 
                          y_max = NA, 
                          colors = NA,
                          num_ticks = 5,
                          dotsize = 1.2,
                          output = ".pdf"){
  
  if(is.na(grouping)){grouping = x} #in case I forget to name it
  if(is.na(outfile)){outfile = y} #in case I forget to name it
  if(is.na(y_label)){y_label = y} #in case I forget to name it
  
  #set export name
  out_file = gsub("[.]", " ", outfile)
  out_file = paste0(out_file, ".", output)
  
  
  if(is.na(y_max)){y_max = max(dataframe[[y]]) * 1.15}
  file_width = 1 + length(unique(dataframe[[x]])) * length(unique(dataframe[[grouping]]))
  if(is.na(colors)){colors = c("blue", "red")}
  
  
  #check for normal distribution using the Shapiro test
  normally_distributed = T
  if(stats){
    for(single_x in unique(dataframe[[x]])){ #split data by condition
      x_dataframe = dataframe[dataframe[[x]] == single_x,] 
      if(nrow(x_dataframe) < 3){next} #cannot perform shapiro test with less than 3 samples
      for(group in unique(x_dataframe[[grouping]])){
        group_dataframe = x_dataframe[x_dataframe[[grouping]] == group,] #split by condition
        if(var(group_dataframe[[y]]) == 0 | 
           nrow(group_dataframe) < 3){next} #cannot perform shapiro test with zero varience
        normality = shapiro.test(group_dataframe[[y]])$p.value
        is_normal = normality > 0.05
        normally_distributed = normally_distributed && is_normal
      }
    }
  }
  
  
  #plotting
  ggplot(dataframe, aes_string(col=grouping, fill=grouping, y=y, x=x)) + 
    geom_boxplot(outlier.shape = NA, lwd = 0.5, alpha = 0.2) + 
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize = dotsize,
                 binwidth = max(dataframe[[y]])/75, #shape = 18,
                 #stackgroups = T, binpositions = "all",
                 aes_string(fill = grouping), position = position_dodge(0.75)) +
    theme_classic() + 
    theme(text = element_text(family = "sans"), axis.text = element_text(colour = "black")) +
    
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    
    {if(normally_distributed & stats){
      stat_compare_means(method = "t.test", label.x.npc = "center", label.y.npc = "bottom")
    }else if(!normally_distributed & stats){
      stat_compare_means(method = "wilcox.test", label.x.npc = "center", label.y.npc = "bottom")
    }} +
    {if(normally_distributed & stats){
      ggtitle(paste(outfile,"T.Test"))
    }else if(!normally_distributed & stats){
      ggtitle(paste(outfile,"Wilcox.Test"))
    }else{
      ggtitle(outfile)
    }} +
    
    ylab(y_label) +
    scale_y_continuous(limits = c(0, y_max),
                       breaks = pretty(c(0,y_max), n = num_ticks),
                       expand = c(0, 0))
  
  ggsave(out_file, height = 4.5, width = file_width, device = output) 
}


#Read in sample data--------------------------------------------------------------------------------------

setwd(root_folder)

#generates a dataframe based on file type
if(grepl(".csv",sample_data_file)){
  sample_data = read.csv(sample_data_file)
}else if(grepl(".xlsx",sample_data_file)){
  sample_data = as.data.frame(read_excel(sample_data_file, sheet = 1))
}


sample_data = sample_data[,c("Animal_ID","Genotype","Group","Sex","DOB")]
sample_data = sample_data[which(sample_data$Animal_ID != ""),]

sample_data$Animal_ID = toupper(sample_data$Animal_ID)
sample_data$Group = factor(sample_data$Group, levels = c("wt","mut"))

animal_identifier = unique(gsub("[^a-zA-Z]", "", sample_data$Animal_ID))



if(length(animal_identifier) > 1){
  
  print("You cannot use different letters in the Animal_ID column")
  
}
stopifnot(length(animal_identifier) == 1)

rm(sample_data_file)



#Read in manual selections--------------------------------------------------------------------------------

setwd(root_folder)

#checks whether a manual selection file exists
if(manual_rest_file == ""){ #NA will skip manual selection
  manual_selections = NA
}else{ #parses manual selections based on file type
  
  if(grepl(".csv",manual_rest_file)){
    manual_selections = read.csv(manual_rest_file)
  }else if(grepl(".xlsx",manual_rest_file)){
    manual_selections = as.data.frame(read_excel(manual_rest_file, sheet = 1))
  }
  
  manual_selections = manual_selections[,grepl("File",colnames(manual_selections)) | 
                                         grepl("Start",colnames(manual_selections)) |
                                         grepl("End",colnames(manual_selections))]
  colnames(manual_selections) = c("Filename","Start","End")
  
  manual_selections$Start = gsub(".*\\ ","",manual_selections$Start)
  manual_selections$End = gsub(".*\\ ","",manual_selections$End)
  
}

rm(manual_rest_file)





#Read in raw files-------------------------------------------------------------------------------------------------------------------------

#check that the raw folder is there
if(!("Raw" %in% list.files())){
  print("Cannot recognize Raw folder. Either rename it, or adjust your root_folder")
  stopifnot("Raw" %in% list.files())
}

#get the names of all raw files
all_raw_files = list.files("Raw")
all_raw_files = all_raw_files[grepl(".txt",all_raw_files)]

#make a Trials folder if there isn't one
#if there, check which files have already been analyzed
dir.create("Trials", showWarnings = FALSE)

files_to_analyze = all_raw_files
rm(all_raw_files)


#print a notice for how many files will be analyzed
#print(paste(length(files_to_analyze), "files to analyze"))
stopifnot(length(files_to_analyze) > 0)

#begin a counter to show progress
raw_file_counter = 1

#make a list in which to store original files
original_file_list = list()

#make a dataframe that will be used to assign pairs
pair_data = data.frame()

for(raw_path in files_to_analyze){
  
  #print a message for while file
  print(paste0("Reading in file ",raw_file_counter," of ",length(files_to_analyze),
               " (",raw_path,")"))
  raw_file_counter = raw_file_counter + 1
  
  #read in the raw file
  original_file = read.delim(paste0("Raw/",raw_path), skip = 60, stringsAsFactors = F)
  original_file = original_file[,c(3,4,7,
                                   10,11,12,
                                   13,14,
                                   15,16,
                                   17,18,
                                   20,21)]
  colnames(original_file) = c("Time","File.Time","Comment",
                              "Sto-id","First Beat","Last Beat",
                              "Inspiratory.Time","Expiratory.Time",
                              "PIF","PEF",
                              "Tidal.Volume","Expired.Volume",
                              "Minute.Ventilation","Frequency")
  
  #pull the date out of the file name
  date = substr(raw_path,1,10)
  
  #adjust the time's format
  split_times = str_split_fixed(original_file$Time, "[.]", 2)
  split_times[,2] = paste(".",split_times[,2]," ", sep="")
  
  reorganized_times = c()
  for (n in 1:nrow(split_times)){
    reorganized_times[n] = gsub(" ", split_times[n,2], split_times[n,1])
  }
  options(digits.secs = 3)
  
  #re-parse the global time columns
  finished_times = as.POSIXct(paste(date, reorganized_times), format="%Y_%m_%d %I:%M:%OS %p")
  original_file$Time = finished_times
  
  #parses the file time column based on whether the file contains hours, minutes, or seconds
  if(str_count(original_file$File.Time[1], ":") == 1){
    
    print(paste("Double check File Time for",raw_path))
    
    first_file_time = original_file$File.Time[1]
    offset_seconds = seconds(as.POSIXct(first_file_time, format="%M:%S"))
    offset_fraction = as.numeric(unlist(strsplit(first_file_time, "\\."))[2])
    offset = as.numeric(paste0(offset_seconds,".",offset_fraction))
    
    start_time = original_file$Time[1] - offset
    
    original_file$File.Time = hms(as.numeric(original_file$Time - start_time))
    original_file$File.Time = as.POSIXct(original_file$File.Time, format="%H:%M:%S")
    
    rm(first_file_time, offset_seconds, offset_fraction, offset, start_time)
    
  }else{
    original_file$File.Time = as.POSIXct(original_file$File.Time, format="%H:%M:%S")
  }

  #check for manual selections before further filtering
  if(all(is.na(manual_selections))){ #no manual selection file
    
    curated_data = original_file
    
  }else{
    
    relevant_manual = manual_selections[which(manual_selections$Filename == raw_path),]
    
    if(nrow(relevant_manual) == 0){ #there are no manual selections for this file
      
      print("--No manual selection detected")
      
      curated_data = original_file
      
      rm(relevant_manual)
      
    }else{ #there are manual selections for this file
      
      print("Manual selection detected")
      
      comments = original_file[which(original_file$Comment != ""),]
      comments = comments[which(!is.na(comments$File.Time)),]
      
      #go row by row based on selection structure and isolate selection regions
      selected_data = data.frame()
      for(row in 1:nrow(relevant_manual)){
        
        current_start = relevant_manual[row,"Start"]
        current_end   = relevant_manual[row,"End"]
        
        if(grepl(":",current_start) & grepl(":",current_end)){
          
          current_start = as_hms(current_start)
          current_end   = as_hms(current_end)
          
        }else if(!grepl(":",current_start) & !grepl(":",current_end)){
          
          current_start = as.integer(current_start)
          current_end   = as.integer(current_end)
          
        }else{
          
          print("ERROR IN MANUAL SELECTIONS - YOU'VE MIXED STO-ID AND TIMES")
          
        }
        
        current_selection = original_file[which(as_hms(original_file$File.Time) >= current_start & 
                                                  as_hms(original_file$File.Time) <= current_end &
                                                  original_file$Comment == ""),]
        
        selected_data = rbind(selected_data,current_selection)
        
        rm(current_start, current_end, current_selection, row)
      }
      
      curated_data = rbind(selected_data, comments)
      curated_data = curated_data[order(curated_data$File.Time),]
      
      rm(comments, selected_data, relevant_manual)
      
    }

  }

  
  #count how many breaths are being rejected
  curated_data$Rejected = as.numeric(curated_data$`Last Beat`) - as.numeric(curated_data$`First Beat`)
  
  #calculate important ratios
  curated_data$TV_EV = curated_data$Tidal.Volume / curated_data$Expired.Volume * 100
  curated_data$PEF_PIF = curated_data$PEF / curated_data$PIF * 100
  curated_data$Te_Ti = curated_data$Expiratory.Time / curated_data$Inspiratory.Time * 100
  
  #remove the suffix from the filename
  if(grepl(".iox.txt", raw_path)){
    raw_name = substr(raw_path, 1, nchar(raw_path)-8)
  }else{
    raw_name = substr(raw_path, 1, nchar(raw_path)-4)
  }
  
  #save the original file in a list to be accessed later
  original_file_list[[raw_name]] = curated_data
  
  
  #save the file's start time for pairing
  start_row = read.delim(paste0("Raw/",raw_path), header = F, skip = 1, nrows = 1, stringsAsFactors = F)
  start_time = paste(start_row[,"V9"],start_row[,"V10"])
  pair_data[raw_name,"Start.Time"] = start_time
  
  #save the file's animal_id for pairing
  id = unlist(strsplit(raw_name, "_"))[grepl(animal_identifier, toupper(unlist(strsplit(raw_name, "_"))))]
  id = unlist(strsplit(id, " "))[grepl(animal_identifier, toupper(unlist(strsplit(id, " "))))]
  pair_data[raw_name,"Animal_ID"] = toupper(id)
  
  pair_data[raw_name,"Date"] = gsub("_","-",date)
  
  rm(raw_path, original_file, date, split_times, reorganized_times, n, 
     finished_times, raw_name, start_time, start_row, id,
     curated_data)
  
}
rm(raw_file_counter)



#Break files by trial----------------------------------------------------------------------------------------------------------------------

#move into the trials folder
setwd(paste0(root_folder,"/Trials"))

#make a list in which to store original files
trial_df_list = list()

misread_files = c()

#generate a list for the animal_id associated with each file
mouse_ids = sapply(names(original_file_list), function(x) unlist(strsplit(x, "_"))[grepl(animal_identifier, toupper(unlist(strsplit(x, "_"))))])
mouse_ids = sapply(mouse_ids, function(x) unlist(strsplit(x, " "))[grepl(animal_identifier, toupper(unlist(strsplit(x, " "))))])
rm(animal_identifier)

for(original_file in names(original_file_list)){
  
  #pull out the file we're going to work on
  original_df = original_file_list[[original_file]]
  
  current_mouse_id = mouse_ids[original_file]
  
  #move into the proper folders
  dir.create(current_mouse_id, showWarnings = FALSE)
  dir.create(paste0(current_mouse_id,"/csvs"), showWarnings = FALSE)
  
  
  #identify switches
  CO_switches = original_df[grepl("CO2",original_df$Comment),c("Time","File.Time","Comment")]
  NA_switches = original_df[grepl("Normal Air",original_df$Comment),c("Time","File.Time","Comment")]
  NA_switches = NA_switches[-nrow(NA_switches),]
  
  if(nrow(CO_switches) != nrow(NA_switches)+1){
    
    misread_files = c(misread_files,original_file)
    next
  }
  
  #remove all rows that don't have breath data (like comments, signal record etc.)
  no_comment_df = original_df[!is.na(original_df$`Sto-id`),] 
  
  #identify the first and last comments (important to know when you're adjacent to the file's ends)
  first_start = no_comment_df[1,"Time"]
  final_end   = no_comment_df[nrow(no_comment_df),"Time"]
  
  
  #make counters
  num_trials = nrow(CO_switches)
  
  #loop through each trial
  for(current_trial in 1:num_trials){
    
    #print counter to track progress
    print(paste0("Delineating ", original_file, " Trial ", current_trial, " of ", num_trials))
    
    #if this is the first chunk, use the start of the file, otherwise, use the end of the last chunk
    if(current_trial == 1){
      current_start = first_start
    }else{
      current_start = current_end
    }
    
    #if this is the last chunk use the end of the file, otherwise, use the next gas switch
    if(current_trial == nrow(CO_switches)){
      current_end = final_end
    }else{
      current_end = NA_switches[current_trial,"Time"]
    }
    
    #isolate a chunk based on time
    single_trial_data = no_comment_df[no_comment_df$Time >= current_start &
                                        no_comment_df$Time <= current_end,]
    
    if(nrow(single_trial_data) == 0){next}
    
    #assign the gas condition of each breath
    current_CO2 = CO_switches[current_trial,"Time"]
    single_trial_data$Air = ifelse(single_trial_data$Time >= current_CO2,
                                   "CO2",
                                   "Normal")
    
    #calculate alternative ways to measure a breath's time
    single_trial_data$Trial.Time = as_hms(single_trial_data$Time - current_start)
    single_trial_data$CO2.Time = as_hms(single_trial_data$Time - current_CO2)
    

    if(!any(grepl(original_file, manual_selections$Filename))){
      
      #remove any bad starts
      previous_sto = c(single_trial_data$`Sto-id`[1],
                       single_trial_data$`Sto-id`[-nrow(single_trial_data)])
      single_trial_data$Sto.diff = single_trial_data$`Sto-id` - previous_sto
      
      if(any(single_trial_data$Sto.diff > 2)){
        
        jumps = single_trial_data[which(single_trial_data$Sto.diff > 1),]
        new_start = jumps$`Sto-id`[nrow(jumps)]
        
        if(nrow(single_trial_data[which(single_trial_data$`Sto-id` >= new_start),]) < 10){
          single_trial_data = single_trial_data[which(single_trial_data$`Sto-id` < new_start),]
          print(paste("False start detected in",original_file,"- Deleting after", new_start))
        }else{
          single_trial_data = single_trial_data[which(single_trial_data$`Sto-id` >= new_start),]
          print(paste("False start detected in",original_file,"- Keeping after", new_start))
        }
        
        rm(jumps, new_start)
      }
      
    }
    
    #make some useful columns & reorganize
    single_trial_data$Trial = current_trial
    single_trial_data = single_trial_data[,c("Time","File.Time",
                                             "Trial","Trial.Time",
                                             "CO2.Time","Air",
                                             "Sto-id","Rejected",
                                             "Inspiratory.Time","Expiratory.Time","Te_Ti",
                                             "Frequency",
                                             "PIF","PEF","PEF_PIF",
                                             "Tidal.Volume","Expired.Volume","TV_EV",
                                             "Minute.Ventilation")]

    
    #make new trial identifier
    trial_ID = paste(original_file, "Trial", current_trial)
    
    #save the single trial into a new dataframe
    trial_df_list[[trial_ID]] = single_trial_data
    
    #save the single trial as a .csv
    write.csv(single_trial_data,
              file = paste0(current_mouse_id,"/csvs/",trial_ID,".csv")) 
    
    rm(single_trial_data, current_CO2, trial_ID)
  }
  
  rm(original_file, original_df, CO_switches, NA_switches, no_comment_df, 
     first_start, final_end, num_trials, current_trial, current_start, current_end)
}

rm(animal_identifier)



#Calculate variance for each measure-------------------------------------------------------------------------------------------------------

qc_df_list = list()

qc_measures = c("TV_EV",
                "Frequency",
                "Rejected")

for(trial_file in names(trial_df_list)){
  
  
  print(paste("Calculating QC variables for",trial_file))
  
  #grab the relevant dataframe
  trial_df = trial_df_list[[trial_file]]
  
  #to prep for aggregation, we're making a column to bin by
  num_chunks = ceiling(nrow(trial_df) / breaths_per_chunk)
  trial_df$Chunk = rep(1:num_chunks, 
                       each = breaths_per_chunk)[1:nrow(trial_df)]
  trial_df$Chunk = (trial_df$Chunk * breaths_per_chunk) + min(trial_df$`Sto-id`) -  breaths_per_chunk
  
  #aggregate by chunk to get variance across the new column
  trial_qc_df = aggregate(trial_df[,c(qc_measures[qc_measures != "Rejected"])],
                                by = list(trial_df$Chunk),
                                FUN = function(x) sd(x) / mean(x) * 100)
  colnames(trial_qc_df) = paste0("CoV_", colnames(trial_qc_df))
  
  trial_qc_df[,"Rejected_Count"] = aggregate(trial_df[,c("Rejected")],
                                             by = list(trial_df$Chunk),
                                             FUN = sum)[,2]
  colnames(trial_qc_df)[1] = c("Chunk")
  
  trial_qc_df[,c("Min.Sto")] = aggregate(trial_df[,c("Sto-id")],
                                         by = list(trial_df$Chunk),
                                         FUN = min)[,2]
  trial_qc_df[,c("Max.Sto")] = aggregate(trial_df[,c("Sto-id")],
                                         by = list(trial_df$Chunk),
                                         FUN = max)[,2]
  
  times = c("File.Time","CO2.Time","Trial.Time")
  trial_qc_df[,times] = aggregate(trial_df[,times],
                                  by = list(trial_df$Chunk),
                                  FUN = mean)[,c(2:4)]
  
  trial_qc_df$Air = ifelse(trial_qc_df$CO2.Time <= 0, "Normal", "CO2")
  
  #save the variance data for later
  qc_df_list[[trial_file]] = trial_qc_df
  
  rm(trial_file, trial_df, num_chunks, trial_qc_df, times)
}





#Clean data-------------------------------------------------------------------------------------------------

#good_chunk_list =list()
trimmed_df_list = list()

n = 1
num_trials = length(trial_df_list)

for(current_mouse_id in unique(mouse_ids)){
  
  #pull out only the relevant dataframes
  relevant_measure_dfs = trial_df_list[which(grepl(current_mouse_id, names(trial_df_list)))]
  
  for(trial_name in names(relevant_measure_dfs)){
    
    #print counter to track progress
    print(paste("Cleaning trial", n, "of", num_trials, "-", trial_name))
    n = n+1
    
    #get the single trial
    trial_data = relevant_measure_dfs[[trial_name]]
    
    #get the variance data
    qc_data = qc_df_list[[trial_name]]
    
    
    good_chunks = qc_data[which(qc_data$Rejected_Count <= Rejected_cutoff),]
    good_chunks = good_chunks[which(good_chunks$CoV_TV_EV <= TV_EV_cutoff),]
    
    
    good_chunks = good_chunks[which(good_chunks$Trial.Time >= time_from_trial_start),]
    
    #take data only within the expected CO2 challenge time
    good_chunks = good_chunks[which(good_chunks$CO2.Time < time_in_CO2),]
    
    #take data 
    good_chunks = good_chunks[which(good_chunks$CO2.Time < 0 | 
                                      good_chunks$CO2.Time >= time_from_CO2_switch),]
    
    
    good_breaths = c()
    for(x in 1:nrow(good_chunks)){
      
      chunk_breaths = seq(from = good_chunks[x,c("Min.Sto")],
                          to = good_chunks[x,c("Max.Sto")],
                          by = 1)
      
      good_breaths = c(good_breaths, chunk_breaths)
      
      rm(chunk_breaths, x)
    }
    
    
    trimmed_trial_data = trial_data[which(trial_data$`Sto-id` %in% good_breaths),]
    
    #take data only a certain amount of time into the trial
    trimmed_trial_data = trimmed_trial_data[which(trimmed_trial_data$Trial.Time >= time_from_trial_start),]
    
    #take data only within the expected CO2 challenge time
    trimmed_trial_data = trimmed_trial_data[which(trimmed_trial_data$CO2.Time < time_in_CO2),]
    
    #take data 
    trimmed_trial_data = trimmed_trial_data[which(trimmed_trial_data$CO2.Time < 0 | 
                                                    trimmed_trial_data$CO2.Time >= time_from_CO2_switch),]
    
    
    trimmed_df_list[[trial_name]] = trimmed_trial_data
    
    
    #save the single trial as a .csv
    write.csv(trimmed_trial_data,
              file = paste0(current_mouse_id,"/csvs/",trial_name," Trimmed",".csv")) 
    
    
    rm(trial_name, trial_data, qc_data, good_chunks, good_breaths, trimmed_trial_data)
  }
  
  rm(current_mouse_id, relevant_measure_dfs)
}
rm(time_from_CO2_switch, time_from_trial_start, time_in_CO2, TV_EV_cutoff, Rejected_cutoff, n, num_trials, breaths_per_chunk)






#Generate and save measure/variance/range plots------------------------------------------------------------

#make a counter
number_of_plots = length(trial_df_list) * length(qc_measures)
n = 1

for(current_mouse_id in unique(mouse_ids)){
  
  #make a folder to save the plots in
  dir.create(paste0(current_mouse_id,"/value plots"), showWarnings = FALSE)
  
  #pull out only the relevant dataframes
  relevant_measure_dfs = trial_df_list[which(grepl(current_mouse_id, names(trial_df_list)))]
  
  for(trial_name in names(relevant_measure_dfs)){
    
    #get the single trial
    trial_data = relevant_measure_dfs[[trial_name]]
    
    #get the variance data
    qc_data = qc_df_list[[trial_name]]
    
    #get the trimmed data
    trimmed_data = trimmed_df_list[[trial_name]]
    
    
    x_range = range(trial_data$File.Time)
    
    
    for(value in qc_measures){
      
      #print for progress
      print(paste("Plotting trial QC", n, "of", number_of_plots, " - ", trial_name, value))
      
      n = n+1
      
      #make a name for the plot
      plot_title = paste(trial_name, value)
      
      measure_y_max = max(trial_data[[value]][!is.infinite(trial_data[[value]])])
      qc_y_max = ifelse(value == "Rejected", max(qc_data[["Rejected_Count"]]),
                              max(qc_data[[paste0("CoV_",value)]]))
      
      
      measure_plot = ggplot(trial_data, aes_string(x = "File.Time", y = value, col = "Air")) +
        geom_point(size = 0.1) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              axis.title.x = element_blank()) +
        scale_y_continuous(limits = c(0,measure_y_max)) +
        scale_x_datetime(breaks = scales::date_breaks("5 mins"),
                         date_labels = "%H:%M",
                         limits = x_range) +
        ggtitle(plot_title)
      
      
      qc_title = ifelse(value == "Rejected", paste(plot_title, "Sum"),
                                             paste(plot_title, "Variance"))
      
      qc_y = ifelse(value == "Rejected", paste0(value,"_Count"),
                                         paste0("CoV_",value))
      
      qc_plot = ggplot(qc_data, aes_string(x = "File.Time", y = qc_y, col = "Air")) +
        geom_point(size = .25) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              axis.title.x = element_blank()) +
        scale_y_continuous(limits = c(0,qc_y_max)) +
        scale_x_datetime(breaks = scales::date_breaks("5 mins"),
                         date_labels = "%H:%M",
                         limits = x_range)

      
      trimmed_plot = ggplot(trimmed_data, aes_string(x = "File.Time", y = value, col = "Air")) +
        geom_point(size = 0.1) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        scale_y_continuous(limits = c(0,measure_y_max)) +
        scale_x_datetime(breaks = scales::date_breaks("5 mins"),
                         date_labels = "%H:%M",
                         limits = x_range)

      
      ggsave(paste0(current_mouse_id, "/value plots/", plot_title, ".tiff"), 
             arrangeGrob(measure_plot, qc_plot, trimmed_plot),
             height = 6, width = 8, device = "tiff")
      
      rm(value, plot_title, measure_plot, qc_plot, trimmed_plot, qc_title, qc_y_max, measure_y_max, qc_y)
    }
    
    rm(trial_name, trial_data, qc_data, trimmed_data, x_range)
  }
  
  rm(current_mouse_id, relevant_measure_dfs)
}
rm(n, number_of_plots, qc_measures, qc_df_list, trial_df_list)





#Merge trial dfs into single sample dfs-------------------------------------------------------------------

sample_df_list = list()

for(mouse in unique(mouse_ids)){
  
  #pull out only the relevant dataframes
  relevant_trimmed_dfs = trimmed_df_list[which(grepl(mouse, names(trimmed_df_list)))]
  
  #make distribution plot for each one, saving them in the Trimmed/Mouse directory
  single_sample_df = bind_rows(relevant_trimmed_dfs)
  single_sample_df$Date = substr(single_sample_df$Time, 1, 10)
  
  single_sample_df$Animal_ID = toupper(mouse)
  
  
  sample_df_list[[toupper(mouse)]] = single_sample_df
  
  rm(mouse, relevant_trimmed_dfs, single_sample_df)
}

rm(mouse_ids, trimmed_df_list)






#Violin plots to compare trials within sample----------------------------------------------------------------------

validation_variables = c("Frequency", "Tidal.Volume", "Minute.Ventilation", 
                         "PIF", "PEF")

for(mouse in names(sample_df_list)){
  
  print(paste("Plotting Trial Distribution for", mouse))
  
  current_df = sample_df_list[[mouse]]
  
  all_days = current_df
  all_days$Date = "All"
  all_days$Trial = "All"
  
  master_df = rbind(current_df, all_days)
  
  
  for(value in validation_variables){
    
    
    plot_list = list()
    for(condition in unique(master_df$Air)){
      
      air_df = master_df[which(master_df$Air == condition),]
      
      plot_title = paste("Trial Comparison", mouse, condition, value)
      
      plot_list[[condition]] = ggplot(air_df, aes_string(x = "Date", y = value, 
                                                         color = "Trial", fill = "Trial")) +
        geom_violin(alpha = 0.3) +
        theme_classic() +
        ggtitle(plot_title)
      
      rm(air_df, plot_title, condition)
    }
    
    ggsave(paste0(mouse, "/value plots/", mouse, " Trial Comparison ", value, ".tiff"),
           arrangeGrob(plot_list[[1]], plot_list[[2]]),
           height = 20, width = 20, device = "tiff")
    
    
    rm(value, plot_list)
  }
  
  rm(mouse, all_days, master_df, current_df)
}

rm(validation_variables)



#Merge sample data with recorded data---------------------------------------------------------------------

setwd(root_folder)


all_trial_data = bind_rows(sample_df_list)

mega_dataframe = merge(sample_data, all_trial_data, by = "Animal_ID")

mega_dataframe = mega_dataframe[,c("Animal_ID","Genotype","Group","Sex","DOB",
                                   "Date","Time","File.Time", "Air", "Sto-id", "Rejected",
                                   "Inspiratory.Time", "Expiratory.Time", "Frequency",
                                   "PIF", "PEF", "Tidal.Volume", "Expired.Volume", 
                                   "Minute.Ventilation")]

rm(all_trial_data)




#Make final violin plots----------------------------------------------------------------------------------

dir.create("Violin Plots", showWarnings = F)

#make a different plot for each condition, with sexes split on top and bottom
for(condition in unique(mega_dataframe$Air)){
  
  condition_df = mega_dataframe[which(mega_dataframe$Air == condition),]
  
  for(value in plot_values){
    
    print(paste("Making Violin Plot -", value, condition))
    
    plot_list = list()
    for(sex in unique(condition_df$Sex)){
      
      sex_df = condition_df[which(condition_df$Sex == sex),]
      
      plot_title = paste(sex, value, condition)
      
      plot_list[[sex]] = ggplot(sex_df, aes_string(x = "Group", y = value, 
                                                   color = "Animal_ID", fill = "Animal_ID")) +
        geom_violin(alpha = 0.3) +
        theme_classic() +
        ggtitle(plot_title)

      
      rm(sex_df, plot_title, sex)
    }
    
    ggsave(paste("Violin Plots", paste0(value, " ", condition,".",export_as), sep = "/"), 
           arrangeGrob(plot_list[[1]], plot_list[[2]]),
           height = 20, width = 20, device = export_as)
    
    rm(value, plot_list)
    
  }
  
  
  rm(condition, condition_df)
}




#Prepare data for box plot--------------------------------------------------------------------------------

#aggregate by sample, date/trial, and air condition, giving the average per trial
data_by_date = aggregate(mega_dataframe[,c("Inspiratory.Time", "Expiratory.Time", "Frequency",
                                           "PIF", "PEF", "Tidal.Volume", "Expired.Volume", 
                                           "Minute.Ventilation")], 
                         by = list(mega_dataframe$Animal_ID, 
                                   mega_dataframe$Date,
                                   mega_dataframe$Air),
                         FUN = mean)
colnames(data_by_date) = c("Animal_ID", "Date", "Air",
                           "Inspiratory.Time", "Expiratory.Time", "Frequency",
                           "PIF", "PEF", "Tidal.Volume", "Expired.Volume", 
                           "Minute.Ventilation")


#append paired control data
pair_references = merge(sample_data[,c("Animal_ID","Group","Sex")], pair_data, by = "Animal_ID")

pair_references$Start.Time = str_sub(pair_references$Start.Time, start = -11)
pair_references$Start.Time = paste(pair_references$Date, pair_references$Start.Time)

#loop through each row and normalize values to paired control
for(n in c(1:nrow(data_by_date))){
  

  current_row = data_by_date[n,]
  
  current_start = pair_references[which(pair_references$Animal_ID == current_row$Animal_ID &
                                        pair_references$Date == current_row$Date),"Start.Time"]

  
  paired_id = pair_references[which(pair_references$Start.Time %in% agrep(current_start, 
                                                                          pair_references$Start.Time, 
                                                                          max.distance = 1, value = TRUE) & 
                                      pair_references$Group == "wt"),"Animal_ID"]
  
  
  if(length(paired_id) == 0){
    print("No pair detected for:")
    print(data_by_date[n,c("Animal_ID","Date","Air")])  
  }
  
  stopifnot(length(paired_id) >= 1)
  
  if(length(paired_id) > 1){
    
    print("Multiple controls detected - ignoring pair:")
    print(data_by_date[n,c("Animal_ID","Date","Air")])
    
    next
  }
  
  data_by_date[n,"Paired_Control"] = paired_id
  
  
  for(variable in plot_values){
    
    current_mouse_variable = current_row[,variable]
    paired_control_variable = data_by_date[which(data_by_date$Animal_ID == paired_id &
                                                 data_by_date$Date == current_row$Date &
                                                 data_by_date$Air == current_row$Air),variable]
    
    data_by_date[n,paste0(variable,"_Paired")] = current_mouse_variable / paired_control_variable
    
    
    rm(variable, current_mouse_variable, paired_control_variable)
  }
  
  rm(n, current_row, current_start, paired_id)
  
}

#re-merge sample data with the new paired data and save
date_final = merge(sample_data, data_by_date, by = "Animal_ID")
date_final = date_final[,c(colnames(sample_data), 
                       "Date","Paired_Control","Air",
                       plot_values,
                       paste0(plot_values,"_Paired"))]

write.csv(date_final, file = "Averages by Day.csv")



#aggregate by sample and condition, getting the average per sample
final_agg = aggregate(data_by_date[,c(plot_values, paste0(plot_values,"_Paired"))], 
                      by = list(data_by_date$Animal_ID, data_by_date$Air),
                      FUN = mean)

colnames(final_agg) = c("Animal_ID", "Air",
                       plot_values, 
                       paste0(plot_values,"_Paired"))


summarized_data = merge(sample_data, final_agg, by = "Animal_ID")
summarized_data$Air = factor(summarized_data$Air, levels = c("Normal", "CO2"))

rm(final_agg, pair_references, pair_data, data_by_date)

write.csv(summarized_data, file = "Averages by Sample.csv")


#Generate box plots---------------------------------------------------------------------------------------
dir.create("Box Plots", showWarnings = F)
setwd("Box Plots")

#iterate through variables
for(variable in c(plot_values, paste0(plot_values,"_Paired"))){
  
  print(paste("Making Box Plot -", variable))
  
  data_to_plot = summarized_data[which(!is.na(summarized_data[,variable])),]
  
  custom_boxplot(dataframe = data_to_plot,
                 x = "Air",
                 grouping = "Group",
                 y = variable,
                 output = export_as,
                 dotsize = 1.4)
  
  rm(variable, data_to_plot)
}

setwd(root_folder)



#Calculate the hypercapnic response-----------------------------------------------------------------------

#reshape dataframe to make CO2 vs Normal Air values into columns for easier manipulation
wide_df = reshape(date_final[,c("Animal_ID","Date","Air",plot_values)],
                  idvar = c("Animal_ID","Date"),
                  timevar = "Air",
                  direction = "wide")

#loop through variables of interest to calculate ratio of CO2 to NA
for(variable in plot_values){

  wide_df[,paste0(variable,".Ratio")] = wide_df[,paste0(variable,".CO2")] / wide_df[,paste0(variable,".Normal")]
  
}

#merge new values with sample data
wide_merge = merge(sample_data, wide_df, by = "Animal_ID")
wide_merge = wide_merge[,c(colnames(sample_data),
                           unlist(lapply(plot_values, 
                                         function(x) paste0(x,c(".Normal",".CO2",".Ratio")))))]

write.csv(wide_merge, file = "Ratios by Day.csv")


#aggregate by sample, giving average per saple
wide_agg = aggregate(wide_merge[,c((ncol(sample_data)+1):(ncol(wide_merge)))], 
                     by = list(wide_merge$Animal_ID),
                     FUN = mean)
colnames(wide_agg)[1] = "Animal_ID"
wide_agg_merge = merge(sample_data, wide_agg, by = "Animal_ID")

write.csv(wide_agg_merge, file = "Ratios by Sample.csv")

rm(wide_df, wide_merge, wide_agg)


#Make scatter plots---------------------------------------------------------------------------------------

dir.create("Scatter Plots", showWarnings = F)


wide_agg_merge$x = "X"

for(variable in plot_values){
  
  print(paste("Making Scatter Plot -", variable))
  
  x_axis = paste0(variable,".Normal")
  y_axis = paste0(variable,".CO2")
  
  axis_max = max(wide_agg_merge[[x_axis]], wide_agg_merge[[y_axis]]) * 1.15
  
  ggplot(wide_agg_merge, aes_string(x = x_axis, y = y_axis,shape = "Sex", color = "Group")) +
    geom_point(size = 2) +
    geom_abline() +
    theme_classic() +
    theme(text = element_text(family = "sans"), axis.text = element_text(colour = "black")) +
    scale_color_manual(values = c("blue","red")) +
    scale_x_continuous(limits = c(0, axis_max), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, axis_max), expand = c(0,0))
  
  setwd(paste0(root_folder,"/Scatter Plots"))
  
  ggsave(paste0(variable,".",export_as), 
         height = 8, width = 9, device = export_as)
    
  
  
  print(paste("Making Box Plot -", variable, "Ratio"))

  setwd(paste0(root_folder,"/Box Plots"))

  custom_boxplot(dataframe = wide_agg_merge,
                 x = "x",
                 grouping = "Group",
                 y = paste0(variable,".Ratio"),
                 output = export_as,
                 dotsize = 1.4)

  rm(variable, single_variable_data, wide_df, scatter_df, x_axis, y_axis, axis_max)
}

setwd(root_folder)



#Generating Bins and Plotting Distribution----------------------------------------------------------------

#calculate bin sizes
variable_bin_size = list()

for(variable in plot_values){
  
  print(paste("Calculating Bins for",variable))
  
  all_sample_maxs = lapply(sample_df_list, FUN = function(x) quantile(x[[variable]], probs = .997))
  
  single_max = max(unlist(all_sample_maxs))
  variable_bin_size[[variable]] = signif(single_max/(number_of_bins-1), digits = 2)

  
  rm(all_sample_maxs, single_max, variable)
}



#Calculate distributions
sample_condition_summaries = list()

for(trimmed_file in sample_df_list){
  
  animal_id = unique(trimmed_file$Animal_ID)
  group = sample_data$Group[which(sample_data$Animal_ID == unique(trimmed_file$Animal_ID))]
  
  print(paste("Calculating Distributions for", animal_id))
  
  for(condition in unique(trimmed_file[["Air"]])){
    
    single_trimmed_condition_df = trimmed_file[which(trimmed_file$Air == condition),]
    
    air = unique(single_trimmed_condition_df$Air)
    
    #generate a dataframe to be filled in teh following loop
    single_binned_df = data.frame(Animal_ID = rep(animal_id, times = number_of_bins),
                                  Group = rep(group, times = number_of_bins),
                                  Air = rep(air, times = number_of_bins),
                                  Bin_Num = seq(from = 1, to = number_of_bins, by = 1))

    for(variable in plot_values){
      
      current_bin_size = variable_bin_size[[variable]]

      single_binned_df[[paste0(variable,"_Dist")]] = sapply(single_binned_df$Bin_Num, FUN = function(x) 
        nrow(single_trimmed_condition_df[which(single_trimmed_condition_df[[variable]] >= (x-1)*current_bin_size & 
                                                 single_trimmed_condition_df[[variable]] < x*current_bin_size),])
        / nrow(single_trimmed_condition_df) * 100)
      #explanation for this function:
      #it iterates through each bin number, subsetting the main dataframe based on breaths within that bin
      #it counts how many breaths there are with nrow, then normalizes that to the number of breaths in that trial
      

      rm(variable, current_bin_size)
    }
    
    sample_condition_summaries[[paste(animal_id, condition)]] = single_binned_df
    
    rm(single_binned_df, air, single_trimmed_condition_df, condition)
  }
  rm(animal_id, group, trimmed_file)
}


dir.create("Distribution Plots",showWarnings = F)

#Plot Distributions
for(condition in unique(mega_dataframe$Air)){
  
  relevant_summaries = sample_condition_summaries[which(grepl(condition, names(sample_condition_summaries)))]
  plottable_data = bind_rows(relevant_summaries)

  
  for(variable in plot_values){
    
    print(paste("Generating", condition, variable, "Distribution Plot"))
    
    
    current_bin_size = variable_bin_size[[variable]]
    plottable_data$Bin_Value = plottable_data$Bin_Num * current_bin_size
    
    y_axis = paste0(variable,"_Dist")
    
    variable_means = aggregate(plottable_data[[y_axis]], 
                                    by = list(plottable_data$Bin_Value,plottable_data$Group),
                                    FUN = mean)
    
    variable_se = aggregate(plottable_data[[y_axis]], 
                               by = list(plottable_data$Bin_Value,plottable_data$Group),
                               FUN = function(x) sd(x) / sqrt(length(x)))

    to_plot = variable_means
    colnames(to_plot) = c("Bin_Value","Group","Mean")
    to_plot$SE = variable_se$x

    
    ggplot(NULL, aes_string(x = "Bin_Value", color = "Group", fill = "Group")) +
      geom_line(data = to_plot, aes(y = Mean)) +
      geom_ribbon(data = to_plot, aes(y = Mean, ymin = Mean-SE, ymax = Mean+SE), 
                  alpha = 0.3, linetype = 0) +
      theme_classic() +
      theme(text = element_text(family = "sans"), axis.text = element_text(colour = "black")) +
      scale_color_manual(values = c("blue","darkorange2")) +
      scale_fill_manual(values = c("blue","darkorange2")) +
      ggtitle(paste("Distribution of",variable,"in",condition))
    
    
    ggsave(paste0("Distribution Plots/",variable," ",condition," Ribbon Points.",export_as), 
           height = 2, width = 3, device = export_as)
    
    
    rm(variable, current_bin_size, y_axis, variable_means, variable_se, to_plot)
  }
  rm(relevant_summaries, plottable_data, condition)
}



for(file in misread_files){
  
  print(paste("Check", file, "- The first and last comment should both be CO2"))
  
}


# load the dataset(s)
getwd()
setwd("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/descriptive_survey_data_nnpp/PrivateSpeech_PerformanceCalculation")
library(tidyverse)
library(readr)
library(stringr)

# remove the table 1 cells
# not necessary, but might need to change the names - remove "table1" in the .csv names

# instead of checking, I'll just remove the cells that contains "Table 1",  string? grepl? gsub?

performance_calculation = function(x){
  DF = read.csv(x, header = FALSE)[1] %>%
    filter(V1 != "Table 1") # load CSV
test_processing = DF %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) %>% # remove the space for words with two words 
  mutate(tap_order = 1:n()) %>% # create a tap order/sequence for later reference 1) the odd and the even are pairs
  mutate(pair_order = (tap_order +1)%/%2) %>% # track back to which are pairs 1) assigning the order of turning.
  mutate(tap_sequence = accumulate(V1, c)) %>% # 
  mutate(times_appeared = as.numeric(ave(V1, V1, FUN = seq_along)) - 1) %>% # accumulating the times the an element has appeared before
  group_by(pair_order) %>% # grouping by pair_order to figure out if it is a match or not
  mutate(match_or_not = case_when( # seeing if each pair is a match or not
    length(unique(V1)) == 1 ~ "match",
    TRUE ~ "mismatch")) %>%
  mutate(categorized_pair = case_when( 
    match_or_not == "match" & max(times_appeared) ==0 ~ "lucky_match",
    match_or_not == "mismatch" & max(times_appeared) ==0 ~ "unlucky_no_info", 
    TRUE ~ "other")) %>%
  ungroup()
  
table_clean = test_processing

participant_performance = as.data.frame(table(table_clean$categorized_pair)) %>%
  rename(categorized_pair = Var1, 
         count_category = Freq)

participant_performance$categorized_pair = factor(participant_performance$categorized_pair, 
                                                  levels = c("unlucky_no_info", "lucky_match", "other"))

performance = nrow(table_clean)/2 + sum(participant_performance$count_category[participant_performance$categorized_pair != "other"]) - 24
return(performance)
}

L = list.files("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/descriptive_survey_data_nnpp/PrivateSpeech_PerformanceCalculation", ".csv")

performance_calcuated = lapply(L, performance_calculation)

merged_performance_calcuated =  do.call(rbind, performance_calcuated)
merged_df = cbind(as.data.frame(L) ,merged_performance_calcuated)

write.csv(merged_df,"/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/descriptive_survey_data_nnpp/PrivateSpeech_PerformanceCalculation/merged_performance_calcuated.csv", row.names = FALSE)


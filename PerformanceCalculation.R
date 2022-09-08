# load the dataset(s)
setwd("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/descriptive_survey_data_nnpp/PrivateSpeech_PerformanceCalculation")
library(tidyverse)
library(readr)
library(stringr)


test_raw = read_csv("Isabella Ramos 1.csv", col_names = FALSE)
glimpse(test_raw)

performance_calculation = function(x){
  DF = read_csv(x, col_names = FALSE)
test_processing = DF %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) %>%
  mutate(tap_order = 1:n()) %>%
  mutate(pair_order = (tap_order +1)%/%2) %>% # track back to which are pairs 
  mutate(tap_sequence = accumulate(X1, c)) %>%
  mutate(times_appeared = as.numeric(ave(X1, X1, FUN = seq_along)) - 1) %>%
  group_by(pair_order) %>%
  mutate(match_or_not = case_when( # seeing if each pair is a match or not
    length(unique(X1)) == 1 ~ "match",
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

performance = nrow(table_clean) + sum(participant_performance$count_category[participant_performance$categorized_pair != "other"]) - 24
return(performance)
}

L = list.files("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/descriptive_survey_data_nnpp/PrivateSpeech_PerformanceCalculation", ".csv")

performance_calcuated = lapply(L, performance_calculation)

merged_performance_calcuated =  do.call(rbind, performance_calcuated)
merged_df = cbind(as.data.frame(L) ,merged_performance_calcuated)

write.csv(merged_df,"/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/descriptive_survey_data_nnpp/PrivateSpeech_PerformanceCalculation/merged_performance_calcuated.csv", row.names = FALSE)

#Handling missing values in master concordance 
library(dplyr)
library(tidyr)

master_concordance_imputed <- master_concordance %>%
  mutate(
    NAICS_3 = substr(NAICS_4, 1, 3),
    NAICS_2 = substr(NAICS_4, 1, 2)
  )

#build imputation tables (averages for parents) from data we have
aiie_3_digit_avg <- master_concordance_imputed %>%
  filter(!is.na(AIIE)) %>% #use only non-missing values 
  group_by(NAICS_3) %>%
  summarise(AIIE_avg_3 = mean(AIIE, na.rm = TRUE)) %>%
  distinct() 

aiie_2_digit_avg <- master_concordance_imputed %>%
  filter(!is.na(AIIE)) %>%
  group_by(NAICS_2) %>%
  summarise(AIIE_avg_2 = mean(AIIE, na.rm = TRUE)) %>%
  distinct() 

#perform the hierarchical imputation 
master_concordance_imputed <- master_concordance_imputed %>% 
  #create the flag *before* filling NAs
  mutate(Imputed_AIIE_Flag = ifelse(is.na(AIIE), 1, 0)) %>%
  
  #join the parent averages 
  left_join(aiie_3_digit_avg, by = "NAICS_3") %>%
  left_join(aiie_2_digit_avg, by = "NAICS_2") %>%
  
  #use coalesce () to fill NAs in order of priority 
  # 1. keep original AIIE if it exists 
  # 2. else, use the 3-digit average 
  # 3. else, use the 2-digit average 
  mutate(AIIE_filled = coalesce(AIIE, AIIE_avg_3, AIIE_avg_2)) %>%
  
  #clean up 
  select(-AIIE) %>% 
  rename(AIIE = AIIE_filled) %>% 
  select(-c(NAICS_3, NAICS_2, AIIE_avg_3, AIIE_avg_2)) # Drop helper columns

# ---
# 1. IDENTIFY THE "STILL MISSING" ROWS
# ---
# (This assumes 'master_concordance_imputed' is in your environment)

out_of_scope_rows <- master_concordance_imputed %>%
  filter(is.na(AIIE))

print(paste("Total rows still missing after imputation:", nrow(out_of_scope_rows)))

# ---
# 2. CREATE THE SUMMARY TABLE YOU ASKED FOR
# ---
# This new table shows *which* industries are the problem.

out_of_scope_sectors <- out_of_scope_rows %>%
  # We need the NAICS_4 and NAICS4_DESC from the *original* file
  # to see their names. Let's join back to your 'aiie_clean' table.
  # (Assuming 'aiie_clean' is still in your environment)
  
  # A simpler way: Just use the columns we have
  select(NAICS_4, CPC, ISIC4) %>% 
  distinct(NAICS_4) %>%
  arrange(NAICS_4)

print("--- Unique NAICS 4-Digit Codes for 'Out-of-Scope' Rows ---")
print(out_of_scope_sectors)

#filter out the "still missing" rows 
master_concordance_final <- master_concordance_inspect %>%
  filter(Imputation_Status != "Still Missing") %>%
  
  # Create the 0/1 flag for our robustness checks
  mutate(Imputed_AIIE_Flag = ifelse(Imputation_Status == "Imputed", 1, 0)) %>%
  
  # Clean up the columns
  select(-AIIE) %>% # Remove the old column with NAs
  rename(AIIE = AIIE_filled) %>% # Rename the new filled column
  
  # Drop all the other helper columns
  select(-c(NAICS_3, NAICS_2, AIIE_avg_3, AIIE_avg_2, Imputation_Status))

#remove public administration sector 
master_concordance_final <- master_concordance_imputed %>%
  filter(!is.na(AIIE))

# 4. CHECK YOUR WORK & SAVE
# ---
original_rows <- nrow(master_concordance)
final_rows <- nrow(master_concordance_final)
imputed_rows <- sum(master_concordance_final$Imputed_AIIE_Flag)

print(paste("Original rows in master_concordance:", original_rows))
print(paste("Final rows in master_concordance_final:", final_rows))
print(paste("Total rows dropped (NAICS 92):", original_rows - final_rows))
print(paste("Total rows that were imputed:", imputed_rows))

write.csv(master_concordance_final, "master_concordance_final.csv", row.names = FALSE)

print("--- 'master_concordance_final.csv' is saved. ---")

cpc_aiie_final <- master_concordance_final[,c("CPC", "AIIE")]









#useful 
#to glimpse type 
glimpse(cpc_isic_naics_1)
glimpse(c)
#to download data frame 
write.csv(master_concordance, "master_concordance.csv", row.names = FALSE)
glimpse(lic_cpc_tradeflow)
glimpse(cpc_aiie_lookup)
#to check if NA 
sum(is.na(master_concordance_final$AIIE)) 



write.csv(cpc_descriptions_new, "cpc_desc.csv", row.names = FALSE)






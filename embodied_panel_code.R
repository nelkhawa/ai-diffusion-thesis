#filtering cpc data for just BEC - capital goods and intermediate goods 
becfile <- "~/Downloads/CPC2_1_BEC5_06_Nov_2018 (1) (1).xlsx"
bec <- read_excel(becfile)

hs_cpc1 <- "https://unstats.un.org/unsd/classifications/Econ/tables/CPC/CPCv21_HS12/cpc21-hs2012.txt"
hs_cpc <- read_csv(hs_cpc1, show_col_types = FALSE)

hs_bec <- HS2012_17_BEC5_08_Nov_2018
hs_bec_filtered <- hs_bec %>%
  select(
    HS4, HS6, BEC5Code1, BEC5EndUse
  )

cpc_hs_clean <- hs_cpc %>%
  mutate(
    HS6_Clean = str_remove_all(HS12code, "\\."),
    CPC4_Link = str_sub(str_pad(CPC21code, 5, "left", "0"), 1, 4)
  )

# ==============================================================================
# CHECKPOINT 2: CLASSIFY ALL GOODS (KEEP EVERYTHING)
# ==============================================================================

# --- BLOCK 2: CLASSIFY ---
# We keep ALL matches, but we create a label for them.
classified_goods_list <- raw_matches %>%
  mutate(
    # Create a descriptive label based on BEC
    Economic_Category = case_when(
      BEC5EndUse == "CAP" ~ "Capital_Good",
      BEC5EndUse == "INT" ~ "Intermediate_Good",
      BEC5EndUse == "CONS" ~ "Consumption_Good",
      TRUE ~ "Other" # Handles NAs or unclassified codes
    )
  )

# --- DIAGNOSTIC CHECK 2 ---
cat("Original Row Count:", nrow(raw_matches), "\n")
cat("Classified Row Count:", nrow(classified_goods_list), "\n")
cat("(These should be identical)\n")

cat("\nBreakdown of All Economic Categories:\n")
table(classified_goods_list$Economic_Category)

# ==============================================================================
# CHECKPOINT 3: AGGREGATE TO 4-DIGIT CPC
# ==============================================================================

# --- BLOCK 3: ROLL UP TO 4-DIGITS ---
cpc_tech_classification <- classified_goods_list %>%
  group_by(CPC4_Link) %>%
  summarize(
    # Check for presence of each type in this 4-digit group
    Has_Capital = any(Economic_Category == "Capital_Good"),
    Has_Intermediate = any(Economic_Category == "Intermediate_Good"),
    Has_Consumption = any(Economic_Category == "Consumption_Good")
  ) %>%
  mutate(
    # Priority Logic: Capital > Intermediate > Consumption > Other
    # This ensures "Computers" (which has machines and parts) becomes Capital,
    # but "Rice" (which is purely consumption) becomes Consumption.
    Tech_Type = case_when(
      Has_Capital ~ "Capital_Good",           # Highest Tech Signal
      Has_Intermediate ~ "Intermediate_Good", # Medium Tech Signal
      Has_Consumption ~ "Consumption_Good",   # Low/No Tech Signal
      TRUE ~ "Other"
    )
  )

# --- DIAGNOSTIC CHECK 3 ---
cat("Unique 4-Digit CPC Codes Classified:", nrow(cpc_tech_classification), "\n")
cat("\nFinal Classification Breakdown:\n")
table(cpc_tech_classification$Tech_Type)

# ==============================================================================
# CHECKPOINT 4: MERGE INTO MASTER TRADE PANEL (KEEP ALL)
# ==============================================================================

# --- BLOCK 4: MERGE ---

master_panel_nov29 <- final_master_panel %>%
  left_join(cpc_tech_classification, by = c("cpc" = "CPC4_Link")) %>%
  mutate(
    Tech_Type = replace_na(Tech_Type, "Unclassified"),
    Embodied_Tech_Dummy = ifelse(Tech_Type %in% c("Capital_Good", "Intermediate_Good"), 1, 0),
    #dummy 2 "placebo" (cons. only)
    Consumption_Dummy = ifelse(Tech_Type == "Consumption_Good", 1, 0)
  ) 
# --- DIAGNOSTIC CHECK ---
cat("Total Rows:", nrow(master_panel_nov29), "\n")
cat("Tech Rows (Dummy=1):", sum(master_panel_nov29$Embodied_Tech_Dummy), "\n")
cat("Consumption Rows:", sum(master_panel_nov29$Consumption_Dummy), "\n")

#CONSTRUCTING AI INTENSITY MEASURE 
country_panel_nov29 <- master_panel_nov29 %>%
  group_by(code, country, year, ExpImp) %>%
  summarise(
    total_value = sum(trade_value, na.rm = TRUE),
    total_tech_value = sum(trade_value*Embodied_Tech_Dummy, na.rm = TRUE),
    #aiie intensity formula - how smart is the tech they buy?
    ai_intensity = sum(trade_value * aiie * Embodied_Tech_Dummy, na.rm = TRUE) / sum(trade_value * Embodied_Tech_Dummy, na.rm=TRUE),
    #ai exposure - how much of total basket is AI tech?
    ai_exposure = sum(trade_value * aiie * Embodied_Tech_Dummy, na.rm = TRUE) / 
      sum(trade_value, na.rm = TRUE),
    #placebo (consumption)
    ai_placebo = sum(trade_value * aiie * Consumption_Dummy, na.rm = TRUE) / 
      sum(trade_value * Consumption_Dummy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #pivot to wide 
  tidyr::pivot_wider(
    names_from = ExpImp,
    values_from = c(ai_intensity, ai_exposure, ai_placebo, total_tech_value, total_value),
    names_sep = "_") %>%
  rename(
    total_imports = total_value_Import,
    ai_import_intensity = ai_intensity_Import,
    total_tech_imports = total_tech_value_Import, 
    ai_import_exposure = ai_exposure_Import
  ) %>%
  select(code, country, year, ai_import_intensity, ai_import_exposure, ai_placebo_Import, total_tech_imports, total_imports)

#contructing AI intensity measure 
country_panel_nov30 <- master_panel_nov29 %>%
  group_by(code, country, year, ExpImp) %>%
  filter(Embodied_Tech_Dummy == 1) 

 

country_level_panel <- final_master_panel %>%
  # Group by Country and Year (and Flow)
  group_by(code, country, year, ExpImp) %>%
  summarise(
    Total_Value = sum(trade_value, na.rm = TRUE),
    
    # THE FORMULA: Weighted Average
    # (Sum of Trade * Score) / (Total Trade)
    AI_Intensity = sum(trade_value * aiie, na.rm = TRUE) / sum(trade_value, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # Pivot to put Imports and Exports side-by-side
  tidyr::pivot_wider(
    names_from = ExpImp,
    values_from = c(Total_Value, AI_Intensity),
    names_sep = "_"
  ) %>%
  # Rename to final variable names
  rename(
    Total_Imports = Total_Value_Import,
    Total_Exports = Total_Value_Export,
    AI_Import_Intensity = AI_Intensity_Import, # This is your X variable
    AI_Export_Intensity = AI_Intensity_Export  # This is your Structural Y
  )



#creating grand master panel (this time with the AI intensity measure created with the dummy)
grand_master_embodied <- grand_master2 %>%
  select(-any_of(c("Total_Exports", "Total_Imports", 
                         "AI_Export_Intensity", "AI_Import_Intensity"))) %>%
  inner_join(country_panel_nov29, by = c("code", "year")) %>%
  select(-country.y, country = country.x)
  
#diagnostics

# Ensure Income_Group is a factor for proper ordering in plots/tables if needed
diag_data <- grand_master_embodied %>%
  filter(!is.na(Income_Group)) %>%
  mutate(Income_Group = factor(Income_Group, levels = c("HIC", "LMIC", "LIC"))) 


######################################################
#new ai intensity measure 
#######################################################
dafu <- grand_master2 %>%
  select(code, country, year, Total_Imports, GDP_PC_Real, Investment_Rate, Human_Capital, Pop_Growth, Trade_Openness, Govt_Size, WGI_Rule_of_Law, Human_Capital_Index, Capital_Stock, TFP, Emp, Income_Group)
 

write.csv(dafu, "dafu.csv", row.names = FALSE)
write.csv(master_panel_nov29, "master_panel_nov29.csv", row.names = FALSE)

#caselli-coleman ai stock measure - sum(trade value * embodied tech dummy * aiie)/sum(emp) 
min_score <- min(master_panel_nov29$aiie, na.rm = TRUE)
max_score <- max(master_panel_nov29$aiie, na.rm = TRUE)

master_panel_nov30 <- master_panel_nov29 %>%
  mutate(
    aiie_scaled = (aiie - min_score) / (max_score - min_score) 
  )
summary(master_panel_nov30$aiie_scaled)

ai_trade_data <- master_panel_nov30 %>%
  filter(Embodied_Tech_Dummy == 1,
         ExpImp == "Import") %>%
  select(-Has_Capital, -Has_Intermediate, -Has_Consumption, -Consumption_Dummy)

ai_flows_aggregated <- ai_trade_data %>%
  mutate(
    ai_content_value = trade_value * aiie_scaled 
  ) %>%
  group_by(code, year) %>%
  summarize(
    #numerator for ai_stock & ai_trade_intensity (total ai mass)
    total_ai_flow_value = sum(ai_content_value, na.rm = TRUE),
    #denominator for coe-helpman (total imports of "embodied tech" from US and china)
    total_embodied_import_value = sum(trade_value, na.rm = TRUE),
    .groups = "drop"
  )
print(head(ai_flows_aggregated))

final_panel <- dafu %>%
  left_join(ai_flows_aggregated, by = c("code", "year")) %>%
  mutate(
    ai_weighted_imports_per_worker = total_ai_flow_value / Emp, #caselli-coleman
    ai_intensity_ratio = total_ai_flow_value / total_embodied_import_value, #coe-helpman
    capital_imports_per_worker = total_embodied_import_value / Emp
  ) %>%
  select(-Total_Imports)
summary(final_panel)

write.csv(final_panel, "final_panel.csv", row.names = FALSE)

#variable transformations + new variables for regression 
regression_data <- final_panel %>%
  arrange(code, year) %>%
  mutate(
    #log transformations (levels)
    log_gdp_pc = log(GDP_PC_Real),
    log_tfp = log(TFP),
    log_AI_imports_pw = log(ai_weighted_imports_per_worker),
    log_embodied_imports_pw = log(capital_imports_per_worker),
    log_investment_rate = log(Investment_Rate),
    log_trade_openness = log(Trade_Openness),
    is_developing = ifelse(Income_Group %in% c("LIC", "LMIC"), 1, 0),
    #lags
    ai_imports_pw_lag = lag(ai_weighted_imports_per_worker),
    embodied_imports_pw_lag = lag(capital_imports_per_worker),
    ai_imports_pw_lag_log = lag(log_AI_imports_pw),
    embodied_imports_pw_lag_log = lag(log_embodied_imports_pw),
    initial_income = lag(log_gdp_pc, 1),
    initial_tfp = lag(log_tfp, 1),
  ) %>%
    #first differences (growth rate)
  group_by(code) %>%
  mutate(
    gdp_growth = log_gdp_pc - lag(log_gdp_pc),
    tfp_growth = log_tfp - lag(log_tfp),
    AI_import_pw_growth = log_AI_imports_pw - lag(log_AI_imports_pw),
    embodied_imports_pw_growth = log_embodied_imports_pw - lag(log_embodied_imports_pw)
  ) %>%
  ungroup()
  
#model 1: baseline TFP 
model_1 <- feols(log_tfp ~ log_AI_imports_pw + log_embodied_imports_pw + Human_Capital_Index + Investment_Rate + Trade_Openness |
                   code + year,
                 data = regression_data,
                 cluser = ~code)
etable(model_1)

model_gdp <- feols(gdp_growth ~ AI_import_pw_growth + embodied_imports_pw_growth + Human_Capital + Investment_Rate + initial_income + Pop_Growth + Trade_Openness + Govt_Size |
                     code + year,
                   data = regression_data,
                   cluser = ~code)
etable(model_gdp)

model_split <- feols(gdp_growth ~ AI_import_pw_growth + i(Income_Group, AI_import_pw_growth, ref = "HIC") + 
                       embodied_imports_pw_growth + 
                       Human_Capital + 
                       Investment_Rate + 
                       initial_income + 
                       Pop_Growth + 
                       Trade_Openness + 
                       Govt_Size |
                       code + year,
                     data = regression_data,
                     cluster = ~code)

etable(model_split)


# Load necessary libraries
library(fixest)
library(dplyr) 
library(stats) # For log()

# --- STEP 1: Calculate Necessary Log and Interaction Variables ---

regression_data <- regression_data %>%
  # 1a. Calculate Log(Human_Capital) as it wasn't explicitly logged in the final dataframe preview
  mutate(
    log_Human_Capital = log(pmax(Human_Capital, 1)) 
  ) %>%
  
  # 1b. Create Model 1 Interaction Term: AI Diffusion * Log(Human Capital)
  mutate(
    AI_x_HC = log_AI_imports_pw * log_Human_Capital
  ) %>%
  
  # 1c. Create Model 2 Interaction Term: AI Diffusion * Developing Dummy
  mutate(
    AI_x_Dev = log_AI_imports_pw * is_developing
  ) %>%
  
  # Drop NAs based on the variables used in the regression (safety step)
  drop_na(tfp_growth, log_AI_imports_pw, log_embodied_imports_pw, log_Human_Capital, is_developing, AI_x_HC, AI_x_Dev)


# --- STEP 2: Run Fixed Effects Regressions using feols() ---

# Model 1: Absorptive Capacity Test (AI x Log(HC))
# Tests if the return to AI is conditional on Human Capital.
model_tfp_hc <- feols(
  tfp_growth ~ log_AI_imports_pw + log_embodied_imports_pw + log_Human_Capital + AI_x_HC |
    code + year, 
  data = regression_data,
  cluster = ~code # Cluster standard errors by Country Code
)


# Model 2: Direct Penalty Test (AI x Dev. Dummy)
# Tests if the return is reduced in developing countries (is_developing = 1).
# NOTE: The stand-alone dummy 'is_developing' is included as a control.
model_tfp_penalty <- feols(
  tfp_growth ~ log_AI_imports_pw + log_embodied_imports_pw + is_developing + AI_x_Dev |
    code + year,
  data = regression_data,
  cluster = ~code
)

# --- STEP 3: Print and Compare Results ---
# Use the 'etable' function for a clean presentation-ready output
etable(model_tfp_hc, model_tfp_penalty, 
       dict = c("log_AI_imports_pw" = "Log AI Diffusion",
                "log_embodied_imports_pw" = "Log Embodied Tech",
                "log_Human_Capital" = "Log Human Capital",
                "AI_x_HC" = "AI x Log(HC) (Interaction)",
                "AI_x_Dev" = "AI x Dev. Dummy (Interaction)",
                "is_developing" = "Dev. Dummy (Level)"),
       style.tex = TRUE)

# Load the necessary library
library(dplyr)

summary_ai_diffusion <- final_panel %>%
  # 1. Group the data by the income classification
  group_by(Income_Group) %>%
  # 2. Calculate essential descriptive statistics for the variable
  summarise(
    N = n(),
    Mean_AI_Imports = mean(ai_weighted_imports_per_worker, na.rm = TRUE),
    SD_AI_Imports = sd(ai_weighted_imports_per_worker, na.rm = TRUE),
    Median_AI_Imports = median(ai_weighted_imports_per_worker, na.rm = TRUE),
    Min_AI_Imports = min(ai_weighted_imports_per_worker, na.rm = TRUE),
    Max_AI_Imports = max(ai_weighted_imports_per_worker, na.rm = TRUE),
    .groups = 'drop'
  )

# Print the resulting table. Use kable for clean output in R Markdown.
print(summary_ai_diffusion)

# Optional: You can add a column for the Coefficient of Variation (CV = SD / Mean) 
# to quantify heterogeneity.
summary_ai_diffusion <- summary_ai_diffusion %>%
  mutate(
    CV_AI_Imports = SD_AI_Imports / Mean_AI_Imports
  )

print(summary_ai_diffusion)




# Load necessary libraries
# library(lfe)
# library(dplyr) 

# 1. Create necessary interaction and log terms (if not already done outside the main data frame)
# NOTE: We use I() for interactions within the formula for safety, but pre-calculating is also fine.

# Model 1: Absorptive Capacity (AI x Log(HC))
# Tests if the return to AI is conditional on Human Capital.
model_tfp_hc <- felm(
  tfp_growth ~ log_AI_imports_pw + log_embodied_imports_pw + log(Human_Capital) + 
    I(log_AI_imports_pw * log(Human_Capital)) | # AI * Log(HC) Interaction
    code + year | 
    0 | 
    code, # Cluster by country code
  data = regression_data
)

# Model 2: Direct Penalty Test (AI x Dev. Dummy)
# Tests if the return to AI is lower in developing countries (is_developing = 1).
model_tfp_penalty <- felm(
  tfp_growth ~ log_AI_imports_pw + log_embodied_imports_pw + is_developing + # is_developing as control
    I(log_AI_imports_pw * is_developing) | # AI * Dev Dummy Interaction
    code + year |
    0 |
    code,
  data = regression_data
)

# Print summaries for comparison
summary(model_tfp_hc)
summary(model_tfp_penalty)




#model gdp conditional convergence (baseline) 
model_gdp <- feols(gdp_growth ~ initial_income + AI_import_pw_growth + embodied_imports_pw_growth + log_investment_rate + log_trade_openness | year,
                   data = regression_data, cluser = ~code) 

write.csv(regression_data, "regression_data.csv", row.names = FALSE)
#model 3: interaction effect 


#include later 
    ai_weighted_imports_per_worker_lag = lag(ai_weighted_imports_per_worker),
    initial_income = lag(log_gdp_pc, 1),
    log_inv = log(Investment_Rate),
    log_trade_openness = log(Trade_Openness)
  )
  

# --- Model 1: GDP Conditional Convergence (Baseline) ---
# Testing if AI boosts growth after controlling for catch-up (Initial Income)
model_a <- feols(gdp_growth ~ initial_income + ai_imports_pw_lag_log + embodied_imports_pw_growth + 
                   log_investment_rate + log_trade_openness | year, 
                 data = regression_data, cluster = ~code)

# --- Model 2: TFP Conditional Convergence (Baseline) ---
# Same as Model 1 but for Productivity Growth
model_b <- feols(tfp_growth ~ initial_tfp + ai_imports_pw_lag_log + embodied_imports_pw_growth + 
                   log_investment_rate + log_trade_openness | year, 
                 data = regression_data, cluster = ~code)
  
# --- Model 3: Income Group Heterogeneity (GDP) ---
# Interaction: Does AI work better for LICs vs HICs?
model_c <- feols(gdp_growth ~ ai_imports_pw_lag_log * Income_Group + 
                   initial_income + embodied_imports_pw_growth | year, 
                 data = regression_data, cluster = ~code)

# --- Model 4: Nelson-Phelps / Absorptive Capacity (GDP) ---
# Interaction: Does Human Capital unlock AI growth?
model_d <- feols(gdp_growth ~ ai_imports_pw_lag_log * Human_Capital_Index + 
                   initial_income + embodied_imports_pw_growth | year, 
                 data = regression_data, cluster = ~code)

# --- Model 5: Developing Country Penalty (TFP) ---
# Interaction: AI * Is_Developing Dummy on Productivity
model_e <- feols(tfp_growth ~ ai_imports_pw_lag_log * is_developing + 
                   initial_tfp + embodied_imports_pw_growth | year, 
                 data = regression_data, cluster = ~code)

# Create a professional table comparing the GDP models
print("---------------- GDP GROWTH MODELS ----------------")
etable(model_a, model_c, model_d, 
       headers = c("1. Baseline", "3. Income Inter.", "4. Nelson-Phelps"),
       fitstat = c("n", "r2"),
       digits = 3)

# Create a professional table comparing the TFP models
print("---------------- TFP GROWTH MODELS ----------------")
etable(model_b, model_e, 
       headers = c("2. Baseline", "5. Dev. Penalty"),
       fitstat = c("n", "r2"),
       digits = 3)


  #CREATING THE AI INTENSITY MEASURE FOR IMPORTS AND EXPORTS SEPARATELY
  
  # 3. CALCULATE INTENSITY (The Aggregation)
  country_level_panel <- final_master_panel %>%
  # Group by Country and Year (and Flow)
  group_by(code, country, year, ExpImp) %>%
  summarise(
    Total_Value = sum(trade_value, na.rm = TRUE),
    
    # THE FORMULA: Weighted Average
    # (Sum of Trade * Score) / (Total Trade)
    AI_Intensity = sum(trade_value * aiie, na.rm = TRUE) / sum(trade_value, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # Pivot to put Imports and Exports side-by-side
  tidyr::pivot_wider(
    names_from = ExpImp,
    values_from = c(Total_Value, AI_Intensity),
    names_sep = "_"
  ) %>%
  # Rename to final variable names
  rename(
    Total_Imports = Total_Value_Import,
    Total_Exports = Total_Value_Export,
    AI_Import_Intensity = AI_Intensity_Import, # This is your X variable
    AI_Export_Intensity = AI_Intensity_Export  # This is your Structural Y
  )

# Check the result
head(country_level_panel)
write.csv(country_level_panel, "country_level_panel.csv", row.names = FALSE)




# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)

# --- Define Target Variables ---
# NOTE: Using 'final_panel' as the input dataframe, as per your previous code.
target_vars <- c("ai_weighted_imports_per_worker", "gdp_growth", "tfp_growth")

# --- Calculate Summary Statistics ---
summary_table_raw <- final_panel %>%
  summarise(
    N = n(),
    # Calculate Mean, Median, SD, Min, and Max for all target variables
    across(all_of(target_vars),
           list(
             Mean = ~ mean(., na.rm = TRUE),
             Median = ~ median(., na.rm = TRUE),
             SD = ~ sd(., na.rm = TRUE),
             Min = ~ min(., na.rm = TRUE),
             Max = ~ max(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}" # Creates names like 'gdp_growth_Mean'
    ),
    .groups = 'drop'
  )

# --- Format for Slide Presentation (Transpose) ---

# 1. Prepare data for long-to-wide transformation
summary_long <- summary_table_raw %>%
  pivot_longer(
    cols = -N, # Exclude N from pivoting
    names_to = "Statistic",
    values_to = "Value"
  ) %>%
  # Separate variable name from statistic (e.g., 'gdp_growth' from 'Mean')
  separate(Statistic, into = c("Variable", "Stat_Name"), sep = "_([A-Za-z]+)$", extra = "merge") %>%
  # Extract the single N count row and duplicate the value for all stats
  mutate(N = first(N))

# 2. Transpose the table
summary_final <- summary_long %>%
  pivot_wider(
    names_from = Variable,
    values_from = Value
  ) %>%
  # Select and arrange the final columns/rows
  select(Stat_Name, N, ai_weighted_imports_per_worker, gdp_growth, tfp_growth) %>%
  # Clean up variable names
  rename(
    `N (Count)` = N,
    `AI Diffusion` = ai_weighted_imports_per_worker,
    `GDP Growth` = gdp_growth,
    `TFP Growth` = tfp_growth,
    `Statistic` = Stat_Name
  )

# --- Print Final Table ---
print(summary_final)

# Assuming the necessary growth variables have been calculated and the dataframe is loaded as 'df'

# 1. Prepare: Calculate the interaction term
# NOTE: Centering Human_Capital is often better for interpretation of the main coefficient (beta1), 
# but for simplicity, we use the raw variable here.
df <- df %>%
  mutate(
    # Interaction Term: AI Diffusion * Human Capital
    AI_x_HC = ai_weighted_imports_per_worker * Human_Capital,
    # Log-linear controls (needed for stability in FE models)
    log_ai_imports_pw = log(ai_weighted_imports_per_worker),
    log_hc = log(Human_Capital)
  )

# 2. Run the Fixed Effects Regression
# Formula: TFP Growth ~ AI Diffusion + Controls + (AI * HC) | Fixed Effects | Clustering
model_tfp_hc <- felm(
  tfp_growth ~ log_ai_imports_pw + log(capital_imports_per_worker) + log_hc + AI_x_HC | 
    code + year | 
    0 | 
    code, # Cluster standard errors by Country Code
  data = df
)

summary(model_tfp_hc)

  #CONSTRUCTING AI INTENSITY MEASURE 
  country_panel_nov29 <- master_panel_nov29 %>%
  group_by(code, country, year, ExpImp) %>%
  summarise(
    total_value = sum(trade_value, na.rm = TRUE),
    total_tech_value = sum(trade_value*Embodied_Tech_Dummy, na.rm = TRUE),
    #aiie intensity formula - how smart is the tech they buy?
    ai_intensity = sum(trade_value * aiie * Embodied_Tech_Dummy, na.rm = TRUE) / sum(trade_value * Embodied_Tech_Dummy, na.rm=TRUE),
    #ai exposure - how much of total basket is AI tech?
    ai_exposure = sum(trade_value * aiie * Embodied_Tech_Dummy, na.rm = TRUE) / 
      sum(trade_value, na.rm = TRUE),
    #placebo (consumption)
    ai_placebo = sum(trade_value * aiie * Consumption_Dummy, na.rm = TRUE) / 
      sum(trade_value * Consumption_Dummy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #pivot to wide 
  tidyr::pivot_wider(
    names_from = ExpImp,
    values_from = c(ai_intensity, ai_exposure, ai_placebo, total_tech_value, total_value),
    names_sep = "_") %>%
  rename(
    total_imports = total_value_Import,
    ai_import_intensity = ai_intensity_Import,
    total_tech_imports = total_tech_value_Import, 
    ai_import_exposure = ai_exposure_Import
  ) %>%
  select(code, country, year, ai_import_intensity, ai_import_exposure, ai_placebo_Import, total_tech_imports, total_imports)




dafu <- grand_master2 %>%
  select(code, country, year, Total_Imports, GDP_PC_Real, Investment_Rate, Human_Capital, Pop_Growth, Trade_Openness, Govt_Size, WGI_Rule_of_Law, Human_Capital_Index, Capital_Stock, TFP, Emp, Income_Group) %>%

master_dafu_panel <- final_master_panel %>%
  left_join(cpc_tech_classification, by = c("cpc" = "CPC4_Link")) %>%
  mutate(
    Tech_Type = replace_na(Tech_Type, "Unclassified"),
    Embodied_Tech_Dummy = ifelse(Tech_Type %in% c("Capital_Good", "Intermediate_Good"), 1, 0),
    #dummy 2 "placebo" (cons. only)
    Consumption_Dummy = ifelse(Tech_Type == "Consumption_Good", 1, 0)
  ) 

cc_panel_dafu <- master_dafu

#CONSTRUCTING AI INTENSITY MEASURE 
country_panel_nov29 <- master_panel_nov29 %>%
  group_by(code, country, year, ExpImp) %>%
  summarise(
    total_value = sum(trade_value, na.rm = TRUE),
    total_tech_value = sum(trade_value*Embodied_Tech_Dummy, na.rm = TRUE),
    #aiie intensity formula - how smart is the tech they buy?
    ai_intensity = sum(trade_value * aiie * Embodied_Tech_Dummy, na.rm = TRUE) / sum(trade_value * Embodied_Tech_Dummy, na.rm=TRUE),
    #ai exposure - how much of total basket is AI tech?
    ai_exposure = sum(trade_value * aiie * Embodied_Tech_Dummy, na.rm = TRUE) / 
      sum(trade_value, na.rm = TRUE),
    #placebo (consumption)
    ai_placebo = sum(trade_value * aiie * Consumption_Dummy, na.rm = TRUE) / 
      sum(trade_value * Consumption_Dummy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #pivot to wide 
  tidyr::pivot_wider(
    names_from = ExpImp,
    values_from = c(ai_intensity, ai_exposure, ai_placebo, total_tech_value, total_value),
    names_sep = "_") %>%
  rename(
    total_imports = total_value_Import,
    ai_import_intensity = ai_intensity_Import,
    total_tech_imports = total_tech_value_Import, 
    ai_import_exposure = ai_exposure_Import
  ) %>%
  select(code, country, year, ai_import_intensity, ai_import_exposure, ai_placebo_Import, total_tech_imports, total_imports)







# ------------------------------------------------------------------------------
# TEST 1: THE "LIC BIAS" CHECK (Summary Statistics by Income Group)
# ------------------------------------------------------------------------------
# Hypothesis: 'Exposure' should be very low/negative for LICs (Rice Effect).
# Hypothesis: 'Intensity' should be higher/positive (Tech Quality Effect).

cat("\n--- MEAN SCORES BY INCOME GROUP ---\n")
bias_check <- grand_master_embodied %>%
  filter(!is.na(Income_Group)) %>%
  group_by(Income_Group) %>%
  summarise(
    Mean_Intensity = mean(ai_import_intensity, na.rm = TRUE), # Quality
    Mean_Exposure = mean(ai_import_exposure, na.rm = TRUE),   # Penetration
    Mean_Placebo = mean(ai_placebo_Import, na.rm = TRUE),     # Consumption
    Count = n()
  )
print(bias_check)

# ------------------------------------------------------------------------------
# TEST 2: CORRELATION MATRIX (Redundancy Check)
# ------------------------------------------------------------------------------
# If Intensity and Exposure have correlation > 0.9, you don't need both.
# If Intensity and Placebo are correlated, your "Tech" signal might just be "Rich Country" signal.

cat("\n--- CORRELATION MATRIX ---\n")
# Select the variables for correlation
cor_vars <- diag_data %>%
  select(ai_import_intensity, ai_import_exposure, ai_placebo_Import, total_tech_imports) 

# Calculate correlation matrix
cor_matrix <- cor(cor_vars, use = "complete.obs")

print(round(cor_matrix, 3))

# ------------------------------------------------------------------------------
# TEST 3: THE "HORSE RACE" (Predictive Power)
# ------------------------------------------------------------------------------
# Let's see which variable actually explains variation in economic development (GDP).
# We use log(GDP_PC_Real) as a quick proxy for "Development Level".

cat("\n--- HORSE RACE REGRESSION (Outcome: Log GDP per Capita) ---\n")
# We use a simple cross-section or pooled OLS for this quick check
horse_race_data <- diag_data %>%
  mutate(log_gdp = log(GDP_PC_Real))

summary(lm(log_gdp ~ ai_import_intensity + ai_import_exposure + ai_placebo_Import, 
           data = horse_race_data))

# ------------------------------------------------------------------------------
# TEST 4: VISUALIZING THE "QUALITY VS QUANTITY" SPLIT
# ------------------------------------------------------------------------------
# This plot shows if LICs (Red) are separated from HICs (Blue) by Quality or Quantity.

p_diag <- ggplot(diag_data, aes(x = log(total_tech_imports), y = ai_import_intensity, color = Income_Group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Quality vs. Quantity of AI Imports",
    subtitle = "Does buying MORE tech mean buying BETTER tech?",
    x = "Log Volume of Tech Imports (Quantity)",
    y = "AI Import Intensity (Quality)"
  ) +
  theme_minimal()

print(p_diag)


write.csv(country_panel_nov29, "country_panel_emb.csv", row.names = FALSE)
  
  
write.csv(grand_master_embodied, "grand_master_embodied.csv", row.names = FALSE)

#descriptive & summary stats 
datasummary_skim(grand_master_embodied, type = "numeric")

grand_master_embodied %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2) 


#running the regressions 
# 1 - baseline solow + ai 
# gdp growth = lag_log_gdp + log_inv + pop_growth + log_trade + log_

library(tidyverse)
library(fixest)
library(modelsummary)

# ==============================================================================
# 1. DATA PREPARATION (Creating 'reg_data_29')
# ==============================================================================
# Create the regression-ready panel
reg_data_29 <- grand_master_embodied %>%
  arrange(code, year) %>%
  group_by(code) %>%
  mutate(
    # --- Dependent Variables ---
    log_gdp = log(GDP_PC_Real),
    gdp_growth = (log_gdp - lag(log_gdp)) * 100,
    
    log_tfp = log(TFP),
    tfp_growth = (log_tfp - lag(log_tfp)) * 100,
    
    # --- Convergence Controls ---
    lag_log_gdp = lag(log_gdp),
    lag_log_tfp = lag(log_tfp),
    
    # --- Independent Variables ---
    # 1. AI Quality (Intensity)
    AI_Quality = ai_import_intensity,
    
    # 2. Tech Volume (Quantity) - Logged
    log_Tech_Vol = log(total_tech_imports),
    
    # 3. Placebo (Consumption)
    AI_Placebo = ai_placebo_Import,
    
    # --- Controls (Logs) ---
    log_Inv = log(Investment_Rate),
    log_Trade = log(Trade_Openness),
    log_Govt = log(Govt_Size),
    
    # --- Controls (Levels) ---
    HC_Index = Human_Capital_Index,
    Inst_RuleLaw = WGI_Rule_of_Law,
    Inst_Corrupt = WGI_Control_Corruption,
    
    # --- Distance to Frontier ---
    Dist_Frontier = GDP_PC_Real / max(GDP_PC_Real, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  # Filter for analysis
  filter(is.finite(log_Tech_Vol), !is.na(gdp_growth))

# Check the new dataframe
print(head(reg_data_29))

# ==============================================================================
# 2. RUN THE 10 MODELS (Using 'reg_data_29')
# ==============================================================================

# --- BLOCK A: BASELINE (Main Effect) ---

# Model 1: Augmented Solow Baseline
m1_solow <- feols(gdp_growth ~ lag_log_gdp + log_Inv + Pop_Growth + log_Trade + 
                    log_Tech_Vol + AI_Quality | code + year, 
                  data = reg_data_29, cluster = ~code)

# Model 2: Adding Human Capital
m2_hc <- feols(gdp_growth ~ lag_log_gdp + log_Inv + Pop_Growth + log_Trade + 
                 log_Tech_Vol + AI_Quality + HC_Index | code + year, 
               data = reg_data_29, cluster = ~code)

# Model 3: Adding Institutions (Preferred Baseline)
m3_inst <- feols(gdp_growth ~ lag_log_gdp + log_Inv + Pop_Growth + log_Trade + 
                   log_Tech_Vol + AI_Quality + HC_Index + Inst_RuleLaw | code + year, 
                 data = reg_data_29, cluster = ~code)

# Model 4: Full Controls (+ Govt Size)
m4_full <- feols(gdp_growth ~ lag_log_gdp + log_Inv + Pop_Growth + log_Trade + 
                   log_Tech_Vol + AI_Quality + HC_Index + Inst_RuleLaw + log_Govt | code + year, 
                 data = reg_data_29, cluster = ~code)

# --- BLOCK B: FALSIFICATION ---

# Model 5: Placebo Test (Consumption)
# Should be insignificant
m5_placebo <- feols(gdp_growth ~ lag_log_gdp + log_Inv + Pop_Growth + log_Trade + 
                      log_Tech_Vol + AI_Placebo + HC_Index + Inst_RuleLaw | code + year, 
                    data = reg_data_29, cluster = ~code)

# --- BLOCK C: MECHANISMS (Interactions) ---

# Model 6: Absorptive Capacity (AI * HC)
# Tests Nelson & Phelps (1966)
m6_absorb <- feols(gdp_growth ~ lag_log_gdp + log_Inv + Pop_Growth + log_Trade + 
                     log_Tech_Vol + AI_Quality * HC_Index + Inst_RuleLaw | code + year, 
                   data = reg_data_29, cluster = ~code)

# Model 7: Structural Divergence (AI * Income Group)
# Tests Acemoglu & Restrepo (2018)
m7_diverge <- feols(gdp_growth ~ lag_log_gdp + log_Inv + Pop_Growth + log_Trade + 
                      log_Tech_Vol + AI_Quality * i(Income_Group, ref="HIC") + 
                      HC_Index + Inst_RuleLaw | code + year, 
                    data = reg_data_29, cluster = ~code)

# Model 8: Appropriate Tech (AI * Distance to Frontier)
# Tests Basu & Weil (1998)
m8_dist <- feols(gdp_growth ~ lag_log_gdp + log_Inv + Pop_Growth + log_Trade + 
                   log_Tech_Vol + AI_Quality * Dist_Frontier + 
                   HC_Index + Inst_RuleLaw | code + year, 
                 data = reg_data_29, cluster = ~code)

# --- BLOCK D: CHANNELS ---

# Model 9: TFP Growth (Efficiency)
# Tests Keller (2004)
m9_tfp <- feols(tfp_growth ~ lag_log_tfp + log_Inv + Pop_Growth + log_Trade + 
                  log_Tech_Vol + AI_Quality + HC_Index + Inst_RuleLaw | code + year, 
                data = reg_data_29, cluster = ~code)

# Model 10: Labor Share (Displacement)
# Tests Acemoglu & Restrepo (2018)
m10_lab <- feols(Labor_Share ~ lag_log_gdp + log_Inv + Pop_Growth + log_Trade + 
                   log_Tech_Vol + AI_Quality + HC_Index + Inst_RuleLaw | code + year, 
                 data = reg_data_29, cluster = ~code)

# ==============================================================================
# 3. DISPLAY RESULTS (FIXED TO PRINT IN CONSOLE)
# ==============================================================================

# Table 1: Main Results
modelsummary(list("Solow"=m1_solow, "+HC"=m2_hc, "+Inst"=m3_inst, "+Govt"=m4_full, "Placebo"=m5_placebo),
             stars = TRUE, 
             gof_map = c("nobs", "r.squared", "aic"),
             title = "Table 1: Baseline Results",
             output = "markdown")  # <--- THIS IS THE FIX

# Table 2: Mechanisms
modelsummary(list("Absorb"=m6_absorb, "Diverge"=m7_diverge, "Frontier"=m8_dist, "TFP"=m9_tfp, "Labor"=m10_lab),
             stars = TRUE, 
             gof_map = c("nobs", "r.squared", "aic"),
             title = "Table 2: Mechanisms",
             output = "markdown")  # <--- THIS IS THE FIX




#################
#ME!!!!
###############
new_df <- grand_master_embodied %>%
  arrange(code, year) %>%
  group_by(code) %>%
  #log of real gdp pc 
  mutate(
    log_gdp_pc = log(GDP_PC_Real),
    #gdp growth 
    gdp_growth = log_gdp_pc - lag(log_gdp_pc, 1),
    #tfp growth 
    tfp_growth = TFP - lag(TFP, 1),
    #initial income 
    initial_income = lag(log_gdp_pc, 1),
    #lag of AI import intensity (1 year lag)
    ai_import_intensity_lag = lag(ai_import_intensity, 1)
  ) %>%
  select(-WGI_Control_Corruption, -Capital_Stock, -Emp, -Labor_Share, -pop, -GNI_2010) %>%
  ungroup()

#regression 1: baseline growth model 
library(fixest)
model_base <- feols(
  log_gdp_pc ~ initial_income + Investment_Rate + Human_Capital + Pop_Growth + Trade_Openness | 
    code + year,
  data = new_df,
  cluster = "code"
)

summary(model_base) 

library(fixest)
model_base2 <- feols(
  log_gdp_pc ~ initial_income + Investment_Rate + Human_Capital + Pop_Growth + Trade_Openness + Govt_Size | 
    code + year,
  data = new_df,
  cluster = "code"
)

summary(model_base2)

#regression 2 - adding AI import intensity to baseline 
model_ai <- feols(
  log_gdp_pc ~ initial_income + Investment_Rate + Human_Capital + Pop_Growth + Trade_Openness + Govt_Size + ai_import_intensity_lag + log(total_tech_imports) | 
    code + year,
  data = new_df,
  cluster = "code"
)

summary(model_ai)


#regression 3 - does ai import intensity get a null effect bc of income group? ai import intensity interactions
model_ai_inc <- feols(
  log_gdp_pc ~ initial_income + Investment_Rate + Human_Capital + Pop_Growth + Trade_Openness + Govt_Size + ai_import_intensity_lag*Income_Group + log(total_tech_imports) | 
    code + year,
  data = new_df,
  cluster = "code"
)

summary(model_ai_inc)


#regression 3 - dependent variable is gdp growth 
model_gdp_growth <- feols(
  gdp_growth ~ initial_income + log(Investment_Rate) + Human_Capital + Pop_Growth + log(Trade_Openness) + Govt_Size + ai_import_intensity + log(total_tech_imports) | 
    code + year,
  data = new_df,
  cluster = "code"
)

summary(model_gdp_growth)

model_gdp_growth1 <- feols(
  gdp_growth ~ initial_income + Investment_Rate + Human_Capital + Pop_Growth + Trade_Openness + Govt_Size + ai_import_intensity + log(total_tech_imports) | 
    code + year,
  data = new_df,
  cluster = "code"
)

summary(model_gdp_growth1)



mod_ai_hc <- feols(
  tfp_growth ~ lag_log_tfp + ai_import_intensity * Human_Capital + Trade_Openness |
    code + year,
  data = new_df,
  cluster = "code"
)
summary(mod_ai_hc_)

































#code from previous regs 
sum(!is.na(grand_master2$TFP)) #429 
sum(!is.na(grand_master2$Investment_Rate)) #478 - instead use capital stock 
sum(!is.na(grand_master2$Human_Capital)) #412 - instead use human cap index 
sum(!is.na(grand_master2$Trade_Openness)) #489 
sum(!is.na(grand_master2$Govt_Size)) #485 
sum(!is.na(grand_master2$Labor_Share)) #485 

#panel with only variables with n = 517 (all data available for all countries in panel)
regression_panel_1 <- grand_master2 %>%
  select(
    -pop, -TFP, -Investment_Rate, -Human_Capital, -Labor_Share, -Trade_Openness, -Govt_Size
  )

write.csv(regression_panel_1, "panel1.csv")


install.packages(c("tidyverse", "fixest", "modelsummary"))
library(tidyverse)
library(fixest)
library(modelsummary) 

#variable transformations 
reg_data <- regression_panel_1 %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    #dependent variable: gdp growth (%) 
    log_gdp = log(GDP_PC_Real),
    gdp_growth = (log_gdp - lag(log_gdp)) * 100,
    #convergence term: lagged gdp level 
    lag_log_gdp = lag(log_gdp),
    #solow controls 
    log_cap = log(Capital_Stock), #capital_stock 
    cap_growth = (log_cap - lag(log_cap)) * 100, 
    log_emp = log(Emp),
    emp_growth = (log_emp - lag(log_emp)) * 100
  ) %>% 
  ungroup() %>%
  filter(!is.na(gdp_growth))

# Fix: Convert Rule of Law from "Factor/Character" to "Numeric"
reg_data$WGI_Rule_of_Law <- as.numeric(as.character(reg_data$WGI_Rule_of_Law))

#regression 1
#specification: Growth ~ convergence + AI + capital growth + labor growth + human cap + institutions 
model_1 <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity + cap_growth + emp_growth + Human_Capital_Index + WGI_Rule_of_Law | 
                   country + year,
                 data = reg_data,
                 cluster = ~country)

# --- 5. Display Results ---
etable(model_1, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
       fitstat = c("n", "r2", "aic", "bic"),
       title = "Regression 1: Maximal Sample Baseline")


#regression 2 
#specification: gdp growth 

num_cols <- c("GDP_PC_Real", "Capital_Stock", "Emp", "AI_Import_Intensity", 
              "Investment_Rate", "Human_Capital_Index", "Trade_Openness", 
              "WGI_Rule_of_Law", "Govt_Size")

for(col in num_cols) {
  grand_master2[[col]] <- as.numeric(as.character(grand_master2[[col]]))
}

reg_data2 <- grand_master2 %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    log_gdp = log(GDP_PC_Real),
    gdp_growth = (log_gdp - lag(log_gdp)) * 100,
    # Convergence Control
    lag_log_gdp = lag(log_gdp),
    # Controls (Log Levels)
    log_inv = log(Investment_Rate),
    log_trade = log(Trade_Openness),
    log_govt = log(Govt_Size),
    # Labor Growth (Solow 'n') - Using Emp as requested
    log_emp = log(Emp),
    emp_growth = (log_emp - lag(log_emp)) * 100
  ) %>%
  ungroup() %>%
  # Clean Sample (Rows with missing controls dropped)
  filter(!is.na(gdp_growth), !is.na(log_inv), !is.na(Human_Capital_Index), 
         !is.na(log_trade), !is.na(WGI_Rule_of_Law), !is.na(log_govt))


# --- REGRESSION 2: Augmented Solow ---
# Growth = AI + Inv + HC + Pop growth + Trade + Law
model_2 <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity + 
                   log_inv + Human_Capital_Index + Pop_Growth + 
                   log_trade + WGI_Rule_of_Law | 
                   country + year, 
                 data = reg_data2, cluster = ~country)

# --- 5. Display Results ---
etable(model_2, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
       fitstat = c("n", "r2", "aic", "bic"),
       title = "Regression 2: Solow+Inst")

#same but using human capital instead of human capital index 
model_2b <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity + 
                    log_inv + Human_Capital + Pop_Growth + 
                    log_trade | 
                    country + year, 
                  data = reg_data2, cluster = ~country)

etable(model_2b, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
       fitstat = c("n", "r2", "aic", "bic"),
       title = "Regression 2: Solow+Inst")


#model 3 - divergence interaction model - do effect of ai imports differ for LMIC compared to HIC? 
#spec: growth ~ conv + AI + log_inv + pop growth + log trade + human cap index + wgi rule of law
# Regression 3: Divergence Test (Interaction on Large Sample)
model_3 <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity * i(Income_Group, ref="HIC") + 
                   log_inv + Pop_Growth + Trade_Openness + 
                   Human_Capital_Index + WGI_Rule_of_Law | 
                   country + year, 
                 data = reg_data2,
                 cluster = ~country)

etable(model_3, title = "Regression 3: Divergence Test (Interaction)")


# Regression 4: Absorptive Capacity Test
model_4 <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity * Human_Capital + 
                   log_inv + Pop_Growth + log_trade + WGI_Rule_of_Law | 
                   country + year, 
                 data = reg_data2,
                 cluster = ~country)

etable(model_4, title = "Regression 4: Absorptive Capacity")


# Create TFP Variables (Add to your reg_data2 pipeline)
reg_data_tfp <- grand_master2 %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    log_tfp = log(TFP),
    tfp_growth = (log_tfp - lag(log_tfp)) * 100,
    lag_log_tfp = lag(log_tfp),
    # Keep other controls...
    log_inv = log(Investment_Rate),
    log_trade = log(Trade_Openness)
  ) %>%
  ungroup() %>%
  filter(!is.na(tfp_growth), !is.na(log_inv))

# Regression 5: TFP Impact
model_5 <- feols(tfp_growth ~ lag_log_tfp + AI_Import_Intensity * i(Income_Group, ref="HIC") + 
                   log_trade + Human_Capital_Index | 
                   country + year, 
                 data = reg_data_tfp,
                 cluster = ~country)

etable(model_5, title = "Regression 5: TFP Efficiency Channel")



#chat gpt log transformations + models
gem_reg <- grand_master2 %>%
  mutate(across(c(GDP_PC_Real, TFP, Capital_Stock, Investment_Rate, 
                  Trade_Openness, Human_Capital_Index, AI_Import_Intensity), 
                ~as.numeric(as.character(.)))) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    # 1. GDP Variables
    log_gdp = log(GDP_PC_Real),
    gdp_growth = (log_gdp - lag(log_gdp)) * 100,
    lag_log_gdp = lag(log_gdp),
    
    # 2. TFP Variables (Efficiency)
    log_tfp = log(TFP),
    tfp_growth = (log_tfp - lag(log_tfp)) * 100,
    lag_log_tfp = lag(log_tfp),
    
    # 3. Capital Stock Variables (Diffusion Channel)
    log_cap = log(Capital_Stock),
    cap_growth = (log_cap - lag(log_cap)) * 100,
    
    # 4. Controls (Logs)
    log_inv = log(Investment_Rate),
    log_trade = log(Trade_Openness),
    
    # 5. AI Lags
    AI_Lag1 = lag(AI_Import_Intensity)
  ) %>%
  ungroup()

# --- 3. RUNNING THE MODELS ---

# --- BLOCK A: TFP GROWTH (The Efficiency Story) ---

# Model 1: Divergence Mechanism (AI * Income Group on TFP)
# Hypothesis: AI hurts TFP in LMICs.
mod_1 <- feols(tfp_growth ~ lag_log_tfp + AI_Import_Intensity * i(Income_Group, ref="HIC") + 
                 log_trade + Human_Capital_Index | country + year, 
               data = gem_reg, cluster = ~country)

# Model 2: Robustness Check (LMIC Subsample Only)
# Hypothesis: Effect holds when looking ONLY at LMICs.
mod_2 <- feols(tfp_growth ~ lag_log_tfp + AI_Import_Intensity + 
                 log_trade + Human_Capital_Index | country + year, 
               data = subset(gem_reg, Income_Group == "LMIC"), cluster = ~country)


#mod 1 w diff controls 
mod_1b <- feols(tfp_growth ~ lag_log_tfp + AI_Import_Intensity * i(Income_Group, ref="HIC") + 
                  Trade_Openness + Human_Capital | country + year, 
                data = gem_reg, cluster = ~country)

mod_2b <- feols(tfp_growth ~ lag_log_tfp + AI_Import_Intensity + 
                  log_trade + Human_Capital | country + year, 
                data = subset(gem_reg, Income_Group == "LMIC"), cluster = ~country)

etable(mod_1b, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
       fitstat = c("n", "r2", "aic", "bic"),
       title = "Regression 2: Solow+Inst")

etable(mod_2b, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
       fitstat = c("n", "r2", "aic", "bic"),
       title = "Regression 2: Solow+Inst")


# Model 4: Absorptive Capacity (AI * Human Capital)
# Hypothesis: Education reverses the negative TFP shock.
mod_4 <- feols(tfp_growth ~ lag_log_tfp + AI_Import_Intensity * Human_Capital_Index + 
                 log_trade | country + year, 
               data = gem_reg, cluster = ~country)

mod_4b <- feols(tfp_growth ~ lag_log_tfp + AI_Import_Intensity * Human_Capital + 
                  Trade_Openness| country + year, 
                data = gem_reg, cluster = ~country)
etable(mod_5, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
       fitstat = c("n", "r2", "aic", "bic"),
       title = "Regression 2: Solow+Inst")

# Model 5: The "Base Cost" (Main Effect in HC model)
# Hypothesis: Shows the negative intercept of AI before Education kicks in.
# (This is effectively covered by Mod 4 output, but can be run sans interaction to see average)
mod_5 <- feols(tfp_growth ~ lag_log_tfp + AI_Import_Intensity + Human_Capital_Index + 
                 log_trade | country + year, 
               data = gem_reg, cluster = ~country)


# --- BLOCK B: CAPITAL ACCUMULATION (The Diffusion Story) ---

# Model 6: Diffusion Channel (Capital Stock Growth)
# Hypothesis: AI imports successfully build physical capital (K).
mod_6 <- feols(cap_growth ~ lag_log_gdp + AI_Import_Intensity + 
                 log_trade | country + year, 
               data = gem_reg, cluster = ~country)


# --- BLOCK C: GDP GROWTH (The Aggregate Story) ---

# Model 3: Openness Paradox (AI * Trade Openness)
# Hypothesis: Open economies suffer more displacement.
mod_3 <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity * log_trade + 
                 log_inv | country + year, 
               data = gem_reg, cluster = ~country)

# Model 7: Hidden Benefit (Main Effect after controls)
# Hypothesis: AI is positive once you control for the Trade interaction.
# (This coefficient appears in Mod 3 output)

# Model 8: Crowding Out (AI * Investment)
# Hypothesis: Diminishing returns if Inv is already high.
mod_8 <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity * log_inv + 
                 log_trade | country + year, 
               data = gem_reg, cluster = ~country)

# Model 9 & 10: Time-to-Build (Lagged AI * Income Group)
# Hypothesis: Does the shock persist or reverse after 1 year?
mod_9_10 <- feols(gdp_growth ~ lag_log_gdp + AI_Lag1 * i(Income_Group, ref="HIC") + 
                    log_inv + log_trade | country + year, 
                  data = gem_reg, cluster = ~country)


# --- 4. DISPLAY RESULTS (CORRECTED) ---

# Table 1: The Productivity Paradox
# We name the models directly: "Name" = model_object
etable("Diffusion (K)" = mod_6, 
       "Divergence (TFP)" = mod_1, 
       "Solution (Educ)" = mod_4,
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
       fitstat = c("n", "r2", "aic"))

# Table 2: GDP Dynamics
etable("Openness Risk" = mod_3, 
       "Crowding Out" = mod_8, 
       "Lagged Effect" = mod_9_10,
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
       fitstat = c("n", "r2", "aic"))



#grand master panel with variable transformations 
grandmaster_wt <- grand_master2 %>%
  
  
  
  #niebel 
  
  # --- TEST 1: Niebel's Split Sample Approach ---
  # Run the exact same model separately for each group
  
  # High Income (HIC)
  model_HIC <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity + 
                       log_inv + Pop_Growth + log_trade + Human_Capital_Index | 
                       country + year, 
                     data = reg_data2[reg_data2$Income_Group == "HIC",], cluster = ~country)

# Lower-Middle Income (LMIC) - The "Trap" Group
model_LMIC <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity + 
                      log_inv + Pop_Growth + log_trade + Human_Capital_Index | 
                      country + year, 
                    data = reg_data2[reg_data2$Income_Group == "LMIC",], cluster = ~country)

# Low Income (LIC)
model_LIC <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity + 
                     log_inv + Pop_Growth + log_trade + Human_Capital_Index | 
                     country + year, 
                   data = reg_data2[reg_data2$Income_Group == "LIC",], cluster = ~country)

# Display Side-by-Side
etable(model_HIC, model_LMIC, model_LIC, 
       headers = c("HIC", "LMIC", "LIC"),
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10))


# Regression 6: Testing the Displacement Mechanism (Labor Share)
# Theory: Acemoglu & Restrepo / Ricardian Trade
# Hypothesis: AI reduces labor share in LMICs (Displacement)

# 1. Prepare Data (Labor Share)
reg_data_lab <- grand_master2 %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    # Labor Share is usually already a ratio (0-1), check scale
    # If it's 0-1, keep as is. If 0-100, keep as is.
    # We focus on the Level of Labor Share as the outcome
    labor_share = as.numeric(as.character(Labor_Share)),
    
    # Controls from Model 2B
    lag_log_gdp = lag(log(GDP_PC_Real)),
    log_inv = log(Investment_Rate),
    log_trade = log(Trade_Openness),
    Human_Capital_Index = as.numeric(as.character(Human_Capital_Index))
  ) %>%
  ungroup() %>%
  filter(!is.na(labor_share), !is.na(log_inv))

# 2. Run the Regression
model_6 <- feols(labor_share ~ lag_log_gdp + AI_Import_Intensity * i(Income_Group, ref="HIC") + 
                   log_inv + log_trade + Human_Capital | 
                   country + year, 
                 data = reg_data_lab,
                 cluster = ~country)

# 3. View Results
etable(model_6, title = "Regression 6: Labor Displacement Channel")


#country level panel, wdi_final, wgi_final, pwt, gni_final 

#INSTIT



# Create Growth & Solow Variables
df <- grand_master2 %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    log_gdp = log(GDP_PC_Real),
    gdp_growth = (log_gdp - lag(log_gdp)) * 100,
    lag_log_gdp = lag(log_gdp),
    log_inv = log(Investment_Rate),
    log_trade = log(Trade_Openness)
  ) %>%
  ungroup()


# --- CONSTRUCT "DISTANCE TO FRONTIER" (DTF) ---
# 1. Find US GDP per capita for each year
us_data <- WDI(country = "US", indicator = "NY.GDP.PCAP.KD", start = 2012, end = 2022) %>%
  select(year, US_GDP = NY.GDP.PCAP.KD)

# 2. Merge and Calculate Ratio (0 to 1 scale)
# DTF = 1 means you are the frontier. DTF = 0.01 means you are far behind.
df <- df %>%
  left_join(us_data, by = "year") %>%
  mutate(Distance_to_Frontier = GDP_PC_Real / US_GDP)

# Clean Sample (Model 2B baseline)
reg_data <- df %>%
  filter(!is.na(gdp_growth), !is.na(log_inv), !is.na(Pop_Growth), 
         !is.na(log_trade), !is.na(Human_Capital_Index), 
         !is.na(Distance_to_Frontier))

# --- REGRESSION 7: "Appropriate Technology" (Distance Interaction) ---
# Does AI work better when you are closer to the US?
model_dtf <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity * Distance_to_Frontier + 
                     log_inv + Pop_Growth + log_trade + Human_Capital_Index + WGI_Rule_of_Law | 
                     country + year, 
                   data = reg_data, cluster = ~country)

# --- REGRESSION 8: "Institutional Complementarity" (Rule of Law Interaction) ---
# Does AI work better when you have good laws?
model_inst <- feols(gdp_growth ~ lag_log_gdp + AI_Import_Intensity * WGI_Rule_of_Law + 
                      log_inv + Pop_Growth + log_trade + Human_Capital_Index | 
                      country + year, 
                    data = reg_data, cluster = ~country)

# --- VIEW RESULTS ---
etable(model_dtf, model_inst, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
       headers = c("Reg 7: Dist. to Frontier", "Reg 8: Institutions"),
       fitstat = c("n", "r2", "aic"))

#summary stats
summary(grand_master_embodied)



#add GNI and income level to master trade panel 
master_trade_panel_inc <- master_trade_panel %>%
  left_join(GNI_2010, by = c("Reporter_Code" = "Country Code"))

missing_codes <- master_trade_panel_inc %>%
  filter(is.na(Income_Group)) %>%
  distinct(Reporter_Code, Reporter_Name)

print(missing_codes) #need to add zimbabwe 

master_trade_panel_inc$GNI_2010 <- NULL 

write.csv(new_df, "new_df.csv", row.names = FALSE)

# Best Model found by Python search
model_lagged_best <- feols(gdp_growth ~ lag_log_gdp + ai_import_intensity_lag + 
                             log(total_tech_imports) + log(Investment_Rate) + 
                             Pop_Growth + log(Trade_Openness) | 
                             code + year, 
                           data = reg_data_29, cluster = ~code)

modelsummary(model_lagged_best, stars = TRUE, title = "Preferred Specification: Lagged AI Effect")








master_panel_cleaned <- master_panel_nov30 %>%
  filter(ExpImp == "Import") %>%
  select(country, code, cpc, year, trade_value, aiie, Embodied_Tech_Dummy, aiie_scaled) 
  


























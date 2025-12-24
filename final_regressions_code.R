#final paper regressions 


model_gdp_growth <- feols(gdp_growth ~ lag_log_gdp + 
                            log_AI_imports_pw +      # The AI Capital Channel
                            log_investment_rate +    # The General Capital Channel
                            Pop_Growth + 
                            Human_Capital_Index + 
                            log_trade_openness + 
                            WGI_Rule_of_Law | 
                            code + year, 
                          data = regression_data, 
                          cluster = ~code)

summary(model_gdp_growth)



model_gdp_growth <- feols(gdp_growth ~ lag_log_gdp + 
                            log_AI_imports_pw +          # AI Capital Channel
                            log_embodied_imports_pw +    # CRITICAL CONTROL: General Capital Channel
                            log_investment_rate +        # General Investment
                            Pop_Growth + 
                            Human_Capital_Index + 
                            log_trade_openness + 
                            WGI_Rule_of_Law | 
                            code + year, 
                          data = regression_data, 
                          cluster = ~code)

summary(model_gdp_growth)

# -------------------------------------------------------
# 1. Define the Groups (Developed vs. Developing)
# -------------------------------------------------------
# Assuming you have a variable 'Income_Group' (e.g., "HIC", "LIC", "LMIC")
# We group LIC and LMIC together to maintain sample size.

regression_data <- regression_data %>%
  mutate(
    Development_Level = case_when(
      Income_Group == "HIC" ~ "Developed",
      Income_Group %in% c("LIC", "LMIC", "UMIC") ~ "Developing", 
      TRUE ~ NA_character_
    )
  )

# -------------------------------------------------------
# 2. Run the Split Regressions
# -------------------------------------------------------
# The 'split' argument in fixest runs the model separately for each group

model_gdp_split <- feols(gdp_growth ~ lag_log_gdp + 
                           log_AI_imports_pw +          # Testing if this differs by group
                           log_embodied_imports_pw +    # Control
                           log_investment_rate + 
                           Pop_Growth + 
                           Human_Capital_Index + 
                           log_trade_openness + 
                           WGI_Rule_of_Law | 
                           code + year, 
                         data = regression_data, 
                         cluster = ~code,
                         split = ~Development_Level)  # This does the split

# -------------------------------------------------------
# 3. Compare Results
# -------------------------------------------------------
etable(model_gdp_split, 
       headers = c("Developed", "Developing"),
       fitstat = c("n", "r2"),
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))


library(fixest)
library(modelsummary)
library(tidyverse)

#TFP GROWTH MODELS !!!!!
# --- Model 1: Absorptive Capacity (The Core Mechanism) ---
# Dependent Variable: TFP Growth
# Interaction: AI Imports * Human Capital Index
# Controls: Capital Imports (to isolate AI 'quality' from just more machines), Trade, Lagged TFP


model_tfp_growth <- feols(tfp_growth ~ lag_log_tfp + log_AI_imports_pw + AI_x_HC + log_embodied_imports_pw + 
    Human_Capital_Index + log_trade_openness | 
    code + year, 
    data = regression_data, 
    cluster = ~code
    )

summary(model_tfp_growth)

model <- feols(tfp_growth ~ lag_log_tfp + log_AI_imports_pw * is_developing + 
  log_embodied_imports_pw + Human_Capital_Index + 
  log_trade_openness | code + year,
  data = regression_data,
  cluster = ~code
)
summary(model)

# Load necessary libraries
library(fixest)
library(tidyverse)
library(modelsummary) # For nice tables

# 1. Prepare the Data
# Create a dummy variable: 1 if Developing (LIC/LMIC), 0 if Developed (HIC)
regression_data <- regression_data %>%
  mutate(
    is_developing = ifelse(Income_Group %in% c("LIC", "LMIC"), 1, 0),
    # Ensure TFP growth and lags are clean
    tfp_growth = tfp_growth, 
    lag_log_tfp = lag_log_tfp
  )

# 2. Run the Regression with Interaction
# Formula: TFP Growth ~ Initial TFP + (AI Imports * Developing_Dummy) + Controls
model_tfp_interaction <- feols(tfp_growth ~ lag_log_tfp + 
                                 
                                 # INTERACTION OF INTEREST:
                                 log_AI_imports_pw * is_developing + 
                                 
                                 # Standard Controls
                                 log_embodied_imports_pw + 
                                 Human_Capital_Index + 
                                 log_trade_openness | 
                                 
                                 # Fixed Effects
                                 code + year, 
                               
                               data = regression_data, 
                               cluster = ~code)

# 3. View the Results
summary(model_tfp_interaction)

# -------------------------------------------------------
# R Code: Split-Sample TFP Growth Regression
# -------------------------------------------------------

# 1. Load Libraries
library(fixest)
library(tidyverse)

# 2. Prepare the Data
# Ensure we have the Development_Level variable and clean TFP data
regression_data <- regression_data %>%
  mutate(
    # Create the split variable
    Development_Level = case_when(
      Income_Group == "HIC" ~ "Developed",
      Income_Group %in% c("LIC", "LMIC", "UMIC") ~ "Developing", 
      TRUE ~ NA_character_
    )
  ) %>%
  # Filter out rows where TFP growth is missing (essential for clean N)
  filter(!is.na(tfp_growth))

# 3. Run the Split Regression
# We use the 'split' argument to automatically run the model for each group
model_tfp_split <- feols(tfp_growth ~ lag_log_tfp + 
                           log_AI_imports_pw +          # Variable of Interest
                           Human_Capital_Index +        # Absorptive Capacity Control
                           log_embodied_imports_pw +    # General Capital Control
                           log_trade_openness | 
                           
                           # Fixed Effects
                           code + year, 
                         
                         data = regression_data, 
                         cluster = ~code,
                         split = ~Development_Level)

# 4. View the Results
print(model_tfp_split)

# 5. Generate a Publication-Ready Table
etable(model_tfp_split, 
       headers = c("Developed", "Developing"),
       fitstat = c("n", "r2"),
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.10),
       digits = 3)

# 4. Optional: Print a nice table to copy into Overleaf
etable(model_tfp_interaction, 
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.10),
       title = "TFP Growth: Interaction with Development Status")


#model 2 - tfp growth and developing dummies 
tfp_growth_byinc <- feols(
  tfp_growth ~ lag_log_tfp + log_AI_imports_pw + AI_x_Dev + is_developing + log_embodied_imports_pw + log_trade_openness |
    code + year,
  data = regression_data,
  cluster = ~code
)

summary(tfp_growth_byinc)


model1 <- feols(
  tfp_growth ~ lag_log_tfp + ai_imports_pw_lag_log +
    log_embodied_imports_pw + Human_Capital_Index + log_trade_openness |
    code + year,
  data = regression_data,
  cluster = ~code
)
summary(model1)

model2 <- feols(
  tfp_growth ~ lag_log_tfp + ai_imports_pw_lag_log + AI_x_HC +
    log_embodied_imports_pw + Human_Capital_Index + log_trade_openness |
    code + year,
  data = regression_data,
  cluster = ~code
)
summary(model2)


model3 <- feols(
  tfp_growth ~ lag_log_tfp + ai_imports_pw_lag_log + AI_x_Dev +
    log_embodied_imports_pw + Human_Capital_Index + log_trade_openness |
    code + year,
  data = regression_data,
  cluster = ~code
)
summary(model3)

# Create high-HC dummy (e.g., HCI above 0.5 or median)
regression_data$high_HC <- ifelse(regression_data$Human_Capital_Index > 0.5, 1, 0)

# Interaction model
model_thresh <- feols(
  tfp_growth ~ lag_log_tfp + ai_imports_pw_lag_log * high_HC +
    log_embodied_imports_pw + log_trade_openness |
    code + year,
  data = regression_data,
  cluster = ~code
)


#model 3 (robustness checks) - split sample TFP growth models 
# ==============================================================================
# MODEL 3: AYERST ET AL. (2023) - KNOWLEDGE VS. PRODUCTION INPUTS
# ==============================================================================
# Source: Ayerst et al. (2023), "Trade and Diffusion of Embodied Technology"
# Concept: Distinguish "Knowledge Spillovers" (AI) from "Input Usage" (General Machines).

model_ayerst <- feols(
  tfp_growth ~ 
    lag_log_tfp + 
    
    # 1. Knowledge-Weighted Imports (The "Idea" Channel)
    # Ayerst uses patent-weighted trade. You use AI-weighted trade.
    # This captures the diffusion of the *frontier*.
    log_AI_imports_pw + 
    
    # 2. Production-Weighted Imports (The "Input" Channel)
    # This captures the standard gain from just having more machines.
    # Crucial control to prove AI is special.
    log_embodied_imports_pw + 
    
    # 3. Ayerst Controls
    # They control for domestic scale and openness.
    log_trade_openness +
    log(Capital_Stock/Emp) | 
    
    # High-Dimensional Fixed Effects (Ayerst uses Country-Sector & Year)
    code + year,
  
  data = regression_data, 
  cluster = ~code
)

# OUTPUT 1: The Baseline Ayerst Result
etable(model_ayerst)

# ==============================================================================
# MODEL 3b: AYERST WITH DEVELOPMENT HETEROGENEITY
# ==============================================================================
# We interact the "Knowledge" variable with Development Status.
# Hypothesis: The "Knowledge Channel" is blocked for LICs/LMICs.

model_ayerst_dev <- feols(
  tfp_growth ~ 
    lag_log_tfp + 
    
    # Base Effect (Knowledge Channel)
    log_AI_imports_pw + 
    
    # The Blockage (Interaction)
    log_AI_imports_pw : is_developing +
    
    # Production Input Channel (Should remain positive for everyone)
    log_embodied_imports_pw + 
    
    log_trade_openness | 
    code + year,
  
  data = regression_data, 
  cluster = ~code
)

# OUTPUT 2: The Heterogeneity Result
etable(model_ayerst_dev)



#GDP GROWTH MODELS !!!!!

# 1. PREPARE THE DATA --------------------------------------------------------
library(fixest)
library(tidyverse)

# Load necessary library
library(fixest)

# -------------------------------------------------------
# MODEL 1: GDP Growth Regression (The Welfare Effect)
# -------------------------------------------------------
# Dependent Variable: Annual Growth of Real GDP per Capita
# Independent Variables:
#   - Convergence Term: Lagged Log GDP (t-1)
#   - AI Diffusion: log_AI_imports_pw
#   - Interaction: AI_x_HC
#   - Solow Controls: Investment Rate, Pop Growth
#   - Structural Controls: Trade Openness, Govt Size, Rule of Law
# Fixed Effects: Country (code) + Year (year)
# Clustering: Country level

model_gdp_growth <- feols(gdp_growth ~ 
                            lag_log_gdp +          # Convergence term
                            log_AI_imports_pw +       # Main Effect (Direct)
                            AI_x_HC +                 # Interaction (Absorptive Capacity)
                            log_investment_rate +     # Solow Control (Capital Acc)
                            Pop_Growth +              # Solow Control (Capital Dilution)
                            Human_Capital_Index +     # Absorptive Capacity Level
                            log_trade_openness +      # General Integration
                            log_embodied_imports_pw + # Control for GENERAL capital imports
                           WGI_Rule_of_Law               # Institutional Quality
                          | 
                            code + year,              # Two-way Fixed Effects
                          data = regression_data, 
                          cluster = ~code             # Cluster SEs by Country
)

# -------------------------------------------------------
# View Results
# -------------------------------------------------------
print(summary(model_gdp_growth))

# To generate a publication-ready LaTeX/HTML table:
# library(modelsummary)
# modelsummary(model_gdp_growth, stars = TRUE, gof_map = c("nobs", "r.squared.within"))

# Reload to ensure clean slate
data <- read.csv("regression_data.csv")

# Create necessary growth and lag variables if they don't exist
data <- data %>%
  arrange(code, year) %>%
  group_by(code) %>%
  mutate(
    # Log Levels
    log_gdp_pc = log(GDP_PC_Real),
    
    # Lagged GDP (Convergence Term: y_{t-1})
    lag_log_gdp_pc = lag(log_gdp_pc, 1),
    
    # Growth Rates (Difference in logs approx growth)
    # If 'gdp_growth' is already in %, convert to decimal or use consistently
    
    # AI Variables
    log_AI_imports_pw = log(ai_weighted_imports_per_worker),
    AI_growth = log_AI_imports_pw - lag(log_AI_imports_pw, 1),
    
    # Non-AI Capital Control
    log_embodied_pw = log(capital_imports_per_worker),
    Capital_Import_Growth = log_embodied_pw - lag(log_embodied_pw, 1),
    
    # Solow Controls
    log_inv = log(Investment_Rate),
    log_open = log(Trade_Openness),
    
    # Dummy
    is_developing = ifelse(Income_Group %in% c("LIC", "LMIC"), 1, 0)
  ) %>%
  ungroup()

# 2. RUN THE GDP GROWTH MODELS (Niebel 2018 Framework) -----------------------

# Model A: Baseline Solow + AI
# Does AI boost growth on average?
mod_growth_base <- feols(
  gdp_growth ~ lag_log_gdp_pc +           # Convergence
    AI_growth +                # AI Diffusion
    Capital_Import_Growth +    # General Capital Control
    log_inv + Pop_Growth +     # Solow Controls
    Human_Capital_Index + 
    log_open | 
    year,                      # Time FE (Country FE might absorb too much in short panels, but try both)
  data = regression_data,
  cluster = ~code
)

# Model B: The Heterogeneity Test (The Research Question)
# Do Developing countries gain MORE (Leapfrogging) or LESS (Absorptive Capacity)?
mod_growth_interact <- feols(
  gdp_growth ~ lag_log_gdp_pc + 
    AI_growth * is_developing + # THE KEY INTERACTION
    Capital_Import_Growth + 
    log_inv + Pop_Growth + 
    Human_Capital_Index + 
    log_open | 
    year, 
  data = data,
  cluster = ~code
)

# Model C: Absorptive Capacity Channel (Human Capital Interaction)
# Does AI require skills to generate growth?
mod_growth_hc <- feols(
  gdp_growth ~ lag_log_gdp_pc + 
    AI_growth * Human_Capital_Index + 
    Capital_Import_Growth + 
    log_inv + Pop_Growth + 
    log_open | 
    year, 
  data = data,
  cluster = ~code
)

# 3. VIEW RESULTS ------------------------------------------------------------
etable(mod_growth_base, mod_growth_interact, mod_growth_hc, 
       headers = c("Baseline", "Dev Interaction", "HC Interaction"))


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

library(fixest)
library(dplyr)
library(modelsummary)

# -------------------------------------------------------
# 1. Define the Groups (Niebel Classification)
# -------------------------------------------------------
# We define "Developed" as High Income, and "Developing" as LIC + LMIC.
# Ensure 'Income_Group' matches your CSV strings ("HIC", "LIC", "LMIC")

regression_data <- regression_data %>%
  mutate(
    Development_Level = case_when(
      Income_Group == "HIC" ~ "Developed",
      Income_Group %in% c("LIC", "LMIC", "UMIC") ~ "Developing", 
      TRUE ~ NA_character_
    )
  )

# -------------------------------------------------------
# 2. Run the Regressions Side-by-Side
# -------------------------------------------------------
# We use the 'split' argument in feols to run the exact same model on subsets.
regression_data <- regression_data %>%
  mutate(
    Development_Level = case_when(
      Income_Group == "HIC" ~ "Developed",
      Income_Group %in% c("LIC", "LMIC", "UMIC") ~ "Developing", 
      TRUE ~ NA_character_
    )
  )
# Define the Formula (Your GDP Growth Model)
gdp_formula <- gdp_growth ~ lag_log_gdp + 
  log_AI_imports_pw + 
  AI_x_HC + 
  log_investment_rate + 
  Pop_Growth + 
  Human_Capital_Index + 
  log_trade_openness + 
  WGI_Rule_of_Law | code + year

# Run Split Regression
models_split <- feols(gdp_formula, 
                      data = regression_data, 
                      cluster = ~code,
                      split = ~Development_Level) # This automatically splits sample

# -------------------------------------------------------
# 3. View Results (Side-by-Side Table)
# -------------------------------------------------------
etable(models_split, 
       headers = c("Developed", "Developing"),
       fitstat = c("n", "r2", "ar2"),
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.10))

# Create a Dummy: 1 if Developed, 0 if Developing
regression_data$Is_Developed <- ifelse(regression_data$Development_Level == "Developed", 1, 0)

# Run Interaction Model
model_interaction <- feols(gdp_growth ~ lag_log_gdp + 
                             # Interact AI with Development Status
                             log_AI_imports_pw * Is_Developed + 
                             # Controls
                             log_investment_rate + Pop_Growth + Human_Capital_Index + 
                             log_trade_openness + WGI_Rule_of_Law | code + year,
                           data = regression_data, cluster = ~code)

summary(model_interaction)

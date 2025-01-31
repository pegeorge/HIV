

###################### INTRODUCTION ---------------------------------------------------

# This R code creates OU data, from OU_IM dataset, to be used to compare peds vs adult vs adolescent (if available) data 

library(tidyverse)
library(ggplot2)
library(tidyr)

rm(list = ls())

#OU_data = read.delim(file = 'C:/Users/pgeorge/Desktop/databases/input/DATIM_Genie/OU_IM_database/Genie-OU_IM-Africa-frozen-2024-12-18.txt', sep = '\t')
OU_data = read.delim(file = 'C:/Users/georg/Desktop/databases/input/DATIM_Genie/OU_IM_database/Genie-OU_IM-MultipleOUs-frozen-2024-12-25.txt', sep = '\t')



OU_data = OU_data %>% filter(funding_agency == 'USAID')

# Function to create quarterly target variables
create_quarterly_targets <- function(data) {
  data %>%
    mutate(
      tgt1 = if_else(!is.na(targets), targets / 4, NA_real_),
      tgt2 = if_else(!is.na(targets), targets / 4, NA_real_),
      tgt3 = if_else(!is.na(targets), targets / 4, NA_real_),
      tgt4 = if_else(!is.na(targets), targets / 4, NA_real_)
    )
}


OU_data = create_quarterly_targets(OU_data)

table(OU_data$indicator)

PrEPdf = OU_data %>% filter(indicator %in% c("PrEP_CT" , "PrEP_CURR", "PrEP_NEW"))

table(PrEPdf$ageasentered)

OU_data_short <- OU_data %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_PVLS", "TX_ML", "TX_NEW", "TX_NET_NEW_SHIFT", "PrEP_CT" , "PrEP_CURR", "PrEP_NEW") |
           str_starts(indicator, "TX_CURR"))


# Create the new variable 'indicator_ND'
OU_data_short <- OU_data_short %>%
  mutate(indicator_ND = if_else(!is.na(numeratordenom), 
                                paste(indicator, numeratordenom, sep = "_"), 
                                indicator))

# Check the result
table(OU_data_short$indicator_ND)



# Reshape the data to long format
DATIM_long <- OU_data_short %>%
  pivot_longer(
    cols = starts_with("qtr"),
    names_to = "quarter",
    names_prefix = "qtr",
    values_to = "value",
    values_drop_na = FALSE
  ) %>%
  mutate(
    quarter_num = as.numeric(quarter)
  )


DATIM_long_targets <- OU_data_short %>%
  filter(is.na(targets) == FALSE) %>% 
  pivot_longer(
    cols = starts_with("tgt"),
    names_to = "quarter",
    names_prefix = "tgt",
    values_to = "target_value",
    values_drop_na = FALSE
  ) %>%
  mutate(
    quarter_num = as.numeric(quarter)
  )



# Define the age groups and corresponding dataframe names

age_groups_1 <- list(
  peds = c('<01', '01-04', '05-09', '10-14'),
  adolescent = c('10-14', '15-19'), 
  adult = c("20-24", "25-29", "30-34", "35-39", "40-44", 
            "45-49", "50-54", "55-59", "60-64", "65+")
)

table(DATIM_long$indicator_ND, DATIM_long$ageasentered)

# TX_CURR_ARVDispense does NOT include fine age bands (e.g., 15-19, 10-14), so can't do adolescent specific analysis there,
# and therefore must use the ages below instead .  
age_groups_2 = list(
  peds = '<15', 
  adult = "15+"
)


# Apply filtering logic with the custom HTS age group for adults
DATIM_long <- DATIM_long %>%
  filter(
    # HTS_TST_N and HTS_TST_POS_N use their specific adult group
    (indicator_ND %in% c("HTS_TST_N", "HTS_TST_POS_N") & 
       ageasentered %in% c('<01', '01-04', '05-09', '10-14', '10-14', '15-19', 
                           "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+")) |
      
      # Indicators that use age_groups_1
      (indicator_ND %in% c("TX_NEW_N", "TX_PVLS_N", "TX_PVLS_D", 
                           "TX_NET_NEW_SHIFT_N", "TX_ML_N", "TX_CURR_Lag2_N", "TX_CURR_N", "PrEP_CT_N" , "PrEP_CURR_N", "PrEP_NEW_N") & 
         ageasentered %in% c('<01', '01-04', '05-09', '10-14', '10-14', '15-19', 
                             "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                             "50-54", "55-59", "60-64", "65+")) |
      
      # Indicators that use age_groups_2
      (indicator_ND %in% c("TX_CURR_ARVDisp_less_three_mo_N", 
                           "TX_CURR_ARVDisp_six_more_mo_N", 
                           "TX_CURR_ARVDisp_three_five_mo_N") & 
         ageasentered %in% c('<15', "15+"))
  )

# Check the resulting dataframe
glimpse(DATIM_long)

table(DATIM_long$indicator)


# Create peds, adolescent, and adult variables
DATIM_long <- DATIM_long %>%
  mutate(
    peds = if_else(ageasentered %in% c('<01', '01-04', '05-09', '10-14', '<15'), 1, 0),
    adolescent = if_else(ageasentered %in% c('10-14', '15-19'), 1, 0),
    adult = if_else(ageasentered %in% c("15+", '15-19', "20-24", "25-29", "30-34", "35-39", 
                                        "40-44", "45-49", "50-54", "55-59",
                                        "60-64", "65+",  "50+"), 1, 0)
  )

#### peds age group df -----------------

# Aggregate the values by country, fiscal_year, and quarter
DATIM_aggregated_peds <- DATIM_long %>% 
  filter(peds == 1) %>% 
  group_by(country, fiscal_year, quarter, indicator_ND) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),  # Aggregate the 'value' column
    .groups = "drop"  # Ungroup after summarizing
  )

# Aggregate the values by country, fiscal_year, and quarter
DATIM_aggregated_peds_gender <- DATIM_long %>% 
  filter(peds == 1) %>% 
  group_by(country, fiscal_year, quarter, indicator_ND, sex) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),  # Aggregate the 'value' column
    .groups = "drop"  # Ungroup after summarizing
  )


# Transform the dataframe to make it wider
DATIM_wider_peds <- DATIM_aggregated_peds %>%
  pivot_wider(
    names_from = indicator_ND,  # Use 'indicator_ND' values as column names
    values_from = total_value,  # Fill these columns with 'total_value'
    values_fill = list(total_value = 0)  # Fill missing values with 0
  )

# Transform the dataframe to make it wider
DATIM_wider_peds_gender <- DATIM_aggregated_peds_gender %>%
  pivot_wider(
    names_from = indicator_ND,  # Use 'indicator_ND' values as column names
    values_from = total_value,  # Fill these columns with 'total_value'
    values_fill = list(total_value = 0)  # Fill missing values with 0
  )


# Calculate proxy variables
DATIM_wider_peds <- DATIM_wider_peds %>%
  mutate(
    # Proxy Linkage
    Proxy_Linkage = TX_NEW_N / HTS_TST_POS_N,
    
    # Proxy Continuity of Treatment
    Proxy_Continuity = TX_CURR_N / (TX_CURR_N - TX_NET_NEW_SHIFT_N + TX_NEW_N),
    
    # Proxy VL Coverage
    Proxy_VL_Coverage = TX_PVLS_D / TX_CURR_Lag2_N, 
    
    # Viral Suppression
    Viral_Suppression = TX_PVLS_N / TX_PVLS_D,
    
    # Proxy Positivity
    Proxy_Positivity = HTS_TST_POS_N / HTS_TST_N,
    
    # Numbers Needed to Test
    Numbers_Needed_to_Test = HTS_TST_N / HTS_TST_POS_N, 
    
    age_group = 'peds'
  )

# Calculate proxy variables
DATIM_wider_peds_gender <- DATIM_wider_peds_gender %>%
  mutate(
    # Proxy Linkage
    Proxy_Linkage = TX_NEW_N / HTS_TST_POS_N,
    
    # Proxy Continuity of Treatment
    Proxy_Continuity = TX_CURR_N / (TX_CURR_N - TX_NET_NEW_SHIFT_N + TX_NEW_N),
    
    # Proxy VL Coverage
    Proxy_VL_Coverage = TX_PVLS_D / TX_CURR_Lag2_N, 
    
    # Viral Suppression
    Viral_Suppression = TX_PVLS_N / TX_PVLS_D,
    
    # Proxy Positivity
    Proxy_Positivity = HTS_TST_POS_N / HTS_TST_N,
    
    # Numbers Needed to Test
    Numbers_Needed_to_Test = HTS_TST_N / HTS_TST_POS_N, 
    
    age_group = 'peds'
  )


#### adult age group df -----------------

# Aggregate the values by country, fiscal_year, and quarter
DATIM_aggregated_adult <- DATIM_long %>% 
  filter(adult == 1) %>% 
  group_by(country, fiscal_year, quarter, indicator_ND) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),  # Aggregate the 'value' column
    .groups = "drop"  # Ungroup after summarizing
  )


# Transform the dataframe to make it wider
DATIM_wider_adult <- DATIM_aggregated_adult %>%
  pivot_wider(
    names_from = indicator_ND,  # Use 'indicator_ND' values as column names
    values_from = total_value,  # Fill these columns with 'total_value'
    values_fill = list(total_value = 0)  # Fill missing values with 0
  )


# Calculate proxy variables
DATIM_wider_adult <- DATIM_wider_adult %>%
  mutate(
    # Proxy Linkage
    Proxy_Linkage = TX_NEW_N / HTS_TST_POS_N,
    
    # Proxy Continuity of Treatment
    Proxy_Continuity = TX_CURR_N / (TX_CURR_N - TX_NET_NEW_SHIFT_N + TX_NEW_N),
    
    # Proxy VL Coverage
    Proxy_VL_Coverage = TX_PVLS_D / TX_CURR_Lag2_N, 
    
    # Viral Suppression
    Viral_Suppression = TX_PVLS_N / TX_PVLS_D,
    
    # Proxy Positivity
    Proxy_Positivity = HTS_TST_POS_N / HTS_TST_N,
    
    # Numbers Needed to Test
    Numbers_Needed_to_Test = HTS_TST_N / HTS_TST_POS_N, 
    
    age_group = 'adult'
  )



# Aggregate the values by country, fiscal_year, and quarter
DATIM_aggregated_adult_gender <- DATIM_long %>% 
  filter(adult == 1) %>% 
  group_by(country, fiscal_year, quarter, indicator_ND, sex) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),  # Aggregate the 'value' column
    .groups = "drop"  # Ungroup after summarizing
  )


# Transform the dataframe to make it wider
DATIM_wider_adult_gender <- DATIM_aggregated_adult_gender %>%
  pivot_wider(
    names_from = indicator_ND,  # Use 'indicator_ND' values as column names
    values_from = total_value,  # Fill these columns with 'total_value'
    values_fill = list(total_value = 0)  # Fill missing values with 0
  )


# Calculate proxy variables
DATIM_wider_adult_gender <- DATIM_wider_adult_gender %>%
  mutate(
    # Proxy Linkage
    Proxy_Linkage = TX_NEW_N / HTS_TST_POS_N,
    
    # Proxy Continuity of Treatment
    Proxy_Continuity = TX_CURR_N / (TX_CURR_N - TX_NET_NEW_SHIFT_N + TX_NEW_N),
    
    # Proxy VL Coverage
    Proxy_VL_Coverage = TX_PVLS_D / TX_CURR_Lag2_N, 
    
    # Viral Suppression
    Viral_Suppression = TX_PVLS_N / TX_PVLS_D,
    
    # Proxy Positivity
    Proxy_Positivity = HTS_TST_POS_N / HTS_TST_N,
    
    # Numbers Needed to Test
    Numbers_Needed_to_Test = HTS_TST_N / HTS_TST_POS_N, 
    
    age_group = 'adult'
  )




#### adolescent age group df -----------------

# Aggregate the values by country, fiscal_year, and quarter
DATIM_aggregated_adolescent <- DATIM_long %>% 
  filter(adolescent == 1) %>% 
  group_by(country, fiscal_year, quarter, indicator_ND) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),  # Aggregate the 'value' column
    .groups = "drop"  # Ungroup after summarizing
  )


# Transform the dataframe to make it wider
DATIM_wider_adolescent <- DATIM_aggregated_adolescent %>%
  pivot_wider(
    names_from = indicator_ND,  # Use 'indicator_ND' values as column names
    values_from = total_value,  # Fill these columns with 'total_value'
    values_fill = list(total_value = 0)  # Fill missing values with 0
  )


# Calculate proxy variables
DATIM_wider_adolescent <- DATIM_wider_adolescent %>%
  mutate(
    # Proxy Linkage
    Proxy_Linkage = TX_NEW_N / HTS_TST_POS_N,
    
    # Proxy Continuity of Treatment
    Proxy_Continuity = TX_CURR_N / (TX_CURR_N - TX_NET_NEW_SHIFT_N + TX_NEW_N),
    
    # Proxy VL Coverage
    Proxy_VL_Coverage = TX_PVLS_D / TX_CURR_Lag2_N, 
    
    # Viral Suppression
    Viral_Suppression = TX_PVLS_N / TX_PVLS_D,
    
    # Proxy Positivity
    Proxy_Positivity = HTS_TST_POS_N / HTS_TST_N,
    
    # Numbers Needed to Test
    Numbers_Needed_to_Test = HTS_TST_N / HTS_TST_POS_N, 
    
    age_group = 'adolescent', 
    
    # need to add blank rows for MMD data (since not available for adolescents)
    TX_CURR_ARVDisp_three_five_mo_N = NA, 
    TX_CURR_ARVDisp_six_more_mo_N = NA, 
    TX_CURR_ARVDisp_less_three_mo_N = NA
    
  )


# Aggregate the values by country, fiscal_year, and quarter
DATIM_aggregated_adolescent_gender <- DATIM_long %>% 
  filter(adolescent == 1) %>% 
  group_by(country, fiscal_year, quarter, indicator_ND, sex) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),  # Aggregate the 'value' column
    .groups = "drop"  # Ungroup after summarizing
  )


# Transform the dataframe to make it wider
DATIM_wider_adolescent_gender <- DATIM_aggregated_adolescent_gender %>%
  pivot_wider(
    names_from = indicator_ND,  # Use 'indicator_ND' values as column names
    values_from = total_value,  # Fill these columns with 'total_value'
    values_fill = list(total_value = 0)  # Fill missing values with 0
  )


# Calculate proxy variables
DATIM_wider_adolescent_gender <- DATIM_wider_adolescent_gender %>%
  mutate(
    # Proxy Linkage
    Proxy_Linkage = TX_NEW_N / HTS_TST_POS_N,
    
    # Proxy Continuity of Treatment
    Proxy_Continuity = TX_CURR_N / (TX_CURR_N - TX_NET_NEW_SHIFT_N + TX_NEW_N),
    
    # Proxy VL Coverage
    Proxy_VL_Coverage = TX_PVLS_D / TX_CURR_Lag2_N, 
    
    # Viral Suppression
    Viral_Suppression = TX_PVLS_N / TX_PVLS_D,
    
    # Proxy Positivity
    Proxy_Positivity = HTS_TST_POS_N / HTS_TST_N,
    
    # Numbers Needed to Test
    Numbers_Needed_to_Test = HTS_TST_N / HTS_TST_POS_N, 
    
    age_group = 'adolescent', 
    
    # need to add blank rows for MMD data (since not available for adolescents)
    TX_CURR_ARVDisp_three_five_mo_N = NA, 
    TX_CURR_ARVDisp_six_more_mo_N = NA, 
    TX_CURR_ARVDisp_less_three_mo_N = NA
    
  )





# Combine the three dataframes
OU_adult_peds_adol_df <- bind_rows(
  DATIM_wider_adolescent,
  DATIM_wider_peds,
  DATIM_wider_adult
)

# Combine the three dataframes
OU_adult_peds_adol_df_gender <- bind_rows(
  DATIM_wider_adolescent_gender,
  DATIM_wider_peds_gender,
  DATIM_wider_adult_gender
)

OU_adult_peds_adol_df = OU_adult_peds_adol_df %>% 
  mutate(funding_agency = 'USAID')       #at the beginning, I had filtered by USAID funding mechanism

OU_adult_peds_adol_df_gender = OU_adult_peds_adol_df_gender %>% 
  mutate(funding_agency = 'USAID')       #at the beginning, I had filtered by USAID funding mechanism


#create ALL rows


# Summarize data for 'ALL' country  (i HOPE I didn't mess up this part....)
all_rows <- OU_adult_peds_adol_df %>%
  group_by(fiscal_year, quarter, age_group) %>%
  summarize(
    HTS_TST_N = sum(HTS_TST_N, na.rm = TRUE),
    HTS_TST_POS_N = sum(HTS_TST_POS_N, na.rm = TRUE),
    TX_CURR_Lag2_N = sum(TX_CURR_Lag2_N, na.rm = TRUE),
    TX_CURR_N = sum(TX_CURR_N, na.rm = TRUE),
    TX_ML_N = sum(TX_ML_N, na.rm = TRUE),
    TX_NET_NEW_SHIFT_N = sum(TX_NET_NEW_SHIFT_N, na.rm = TRUE),
    TX_NEW_N = sum(TX_NEW_N, na.rm = TRUE),
    TX_PVLS_D = sum(TX_PVLS_D, na.rm = TRUE),
    TX_PVLS_N = sum(TX_PVLS_N, na.rm = TRUE),
    PrEP_NEW_N = sum(PrEP_NEW_N, na.rm = TRUE),
    PrEP_CT_N = sum(PrEP_CT_N, na.rm = TRUE),
    PrEP_CURR_N = sum(PrEP_CURR_N, na.rm = TRUE),
    Proxy_Linkage = sum(TX_NEW_N, na.rm = TRUE) / sum(HTS_TST_POS_N, na.rm = TRUE),
    Proxy_Continuity = sum(TX_CURR_N, na.rm = TRUE) / (sum(TX_CURR_N, na.rm = TRUE) - sum(TX_NET_NEW_SHIFT_N, na.rm = TRUE) + sum(TX_NEW_N, na.rm = TRUE)),
    Proxy_VL_Coverage = sum(TX_PVLS_D, na.rm = TRUE) / sum(TX_CURR_Lag2_N, na.rm = TRUE),
    Viral_Suppression = sum(TX_PVLS_N, na.rm = TRUE) / sum(TX_PVLS_D, na.rm = TRUE),
    Proxy_Positivity = sum(HTS_TST_POS_N, na.rm = TRUE) / sum(HTS_TST_N, na.rm = TRUE),
    Numbers_Needed_to_Test = sum(HTS_TST_N, na.rm = TRUE) / sum(HTS_TST_POS_N, na.rm = TRUE),
    TX_CURR_ARVDisp_less_three_mo_N = sum(TX_CURR_ARVDisp_less_three_mo_N, na.rm = TRUE), 
    TX_CURR_ARVDisp_six_more_mo_N = sum(TX_CURR_ARVDisp_six_more_mo_N, na.rm = TRUE), 
    TX_CURR_ARVDisp_three_five_mo_N = sum(TX_CURR_ARVDisp_three_five_mo_N, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(HTS_TST_N, HTS_TST_POS_N, TX_CURR_Lag2_N, TX_CURR_N, TX_ML_N, PrEP_CT_N, PrEP_NEW_N, PrEP_CURR_N, 
        TX_NET_NEW_SHIFT_N, TX_NEW_N, TX_PVLS_D, TX_PVLS_N, 
        TX_CURR_ARVDisp_less_three_mo_N, TX_CURR_ARVDisp_six_more_mo_N, 
        TX_CURR_ARVDisp_three_five_mo_N),
      ~ ifelse(. == 0, NA, .)
    )
  ) %>%
  mutate(country = "ALL", 
         funding_agency = 'USAID') %>%
  relocate(country, fiscal_year, quarter) 


# Summarize data for 'ALL' country
all_rows_gender <- OU_adult_peds_adol_df_gender %>%
  group_by(fiscal_year, quarter, age_group, sex) %>%
  summarize(
    HTS_TST_N = sum(HTS_TST_N, na.rm = TRUE),
    HTS_TST_POS_N = sum(HTS_TST_POS_N, na.rm = TRUE),
    TX_CURR_Lag2_N = sum(TX_CURR_Lag2_N, na.rm = TRUE),
    TX_CURR_N = sum(TX_CURR_N, na.rm = TRUE),
    TX_ML_N = sum(TX_ML_N, na.rm = TRUE),
    TX_NET_NEW_SHIFT_N = sum(TX_NET_NEW_SHIFT_N, na.rm = TRUE),
    TX_NEW_N = sum(TX_NEW_N, na.rm = TRUE),
    TX_PVLS_D = sum(TX_PVLS_D, na.rm = TRUE),
    TX_PVLS_N = sum(TX_PVLS_N, na.rm = TRUE),
    PrEP_NEW_N = sum(PrEP_NEW_N, na.rm = TRUE),
    PrEP_CT_N = sum(PrEP_CT_N, na.rm = TRUE),
    PrEP_CURR_N = sum(PrEP_CURR_N, na.rm = TRUE),
    Proxy_Linkage = sum(TX_NEW_N, na.rm = TRUE) / sum(HTS_TST_POS_N, na.rm = TRUE),
    Proxy_Continuity = sum(TX_CURR_N, na.rm = TRUE) / (sum(TX_CURR_N, na.rm = TRUE) - sum(TX_NET_NEW_SHIFT_N, na.rm = TRUE) + sum(TX_NEW_N, na.rm = TRUE)),
    Proxy_VL_Coverage = sum(TX_PVLS_D, na.rm = TRUE) / sum(TX_CURR_Lag2_N, na.rm = TRUE),
    Viral_Suppression = sum(TX_PVLS_N, na.rm = TRUE) / sum(TX_PVLS_D, na.rm = TRUE),
    Proxy_Positivity = sum(HTS_TST_POS_N, na.rm = TRUE) / sum(HTS_TST_N, na.rm = TRUE),
    Numbers_Needed_to_Test = sum(HTS_TST_N, na.rm = TRUE) / sum(HTS_TST_POS_N, na.rm = TRUE),
    TX_CURR_ARVDisp_less_three_mo_N = sum(TX_CURR_ARVDisp_less_three_mo_N, na.rm = TRUE), 
    TX_CURR_ARVDisp_six_more_mo_N = sum(TX_CURR_ARVDisp_six_more_mo_N, na.rm = TRUE), 
    TX_CURR_ARVDisp_three_five_mo_N = sum(TX_CURR_ARVDisp_three_five_mo_N, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(HTS_TST_N, HTS_TST_POS_N, TX_CURR_Lag2_N, TX_CURR_N, TX_ML_N, PrEP_CT_N, PrEP_NEW_N, PrEP_CURR_N,
        TX_NET_NEW_SHIFT_N, TX_NEW_N, TX_PVLS_D, TX_PVLS_N, 
        TX_CURR_ARVDisp_less_three_mo_N, TX_CURR_ARVDisp_six_more_mo_N, 
        TX_CURR_ARVDisp_three_five_mo_N),
      ~ ifelse(. == 0, NA, .)
    )
  ) %>%
  mutate(country = "ALL", 
         funding_agency = 'USAID') %>%
  relocate(country, fiscal_year, quarter)


# Add summarized data back to the original dataset
OU_adult_peds_adol_all_df <- bind_rows(OU_adult_peds_adol_df, all_rows) %>% 
  arrange(country, fiscal_year, quarter, age_group)

# Add summarized data back to the original dataset
OU_adult_peds_adol_all_df_gender <- bind_rows(OU_adult_peds_adol_df_gender, all_rows_gender) %>% 
  arrange(country, fiscal_year, quarter, age_group)



# Add MMD calculations 

OU_adult_peds_adol_all_df <- OU_adult_peds_adol_all_df %>%
  mutate(
    # Calculate the total ARV dispensed
    total_ARV_dispensed = TX_CURR_ARVDisp_six_more_mo_N +
      TX_CURR_ARVDisp_three_five_mo_N +
      TX_CURR_ARVDisp_less_three_mo_N,
    
    # Calculate MMD_3_month
    MMD_3_month = (TX_CURR_ARVDisp_six_more_mo_N + TX_CURR_ARVDisp_three_five_mo_N) /
      total_ARV_dispensed * 100,
    
    # Calculate MMD_6_month
    MMD_6_month = TX_CURR_ARVDisp_six_more_mo_N / total_ARV_dispensed * 100
  )


OU_adult_peds_adol_all_df_gender <- OU_adult_peds_adol_all_df_gender %>%
  mutate(
    # Calculate the total ARV dispensed
    total_ARV_dispensed = TX_CURR_ARVDisp_six_more_mo_N +
      TX_CURR_ARVDisp_three_five_mo_N +
      TX_CURR_ARVDisp_less_three_mo_N,
    
    # Calculate MMD_3_month
    MMD_3_month = (TX_CURR_ARVDisp_six_more_mo_N + TX_CURR_ARVDisp_three_five_mo_N) /
      total_ARV_dispensed * 100,
    
    # Calculate MMD_6_month
    MMD_6_month = TX_CURR_ARVDisp_six_more_mo_N / total_ARV_dispensed * 100
  )





save(OU_adult_peds_adol_df, file = 'C:/Users/georg/Desktop/databases/output/adult_adol_peds_OU/OU_adult_peds_adol_df.rda')
save(OU_adult_peds_adol_all_df, file = 'C:/Users/georg/Desktop/databases/output/adult_adol_peds_OU/OU_adult_peds_adol_all_df.rda')
save(OU_adult_peds_adol_all_df_gender, file = 'C:/Users/georg/Desktop/databases/output/adult_adol_peds_OU/OU_adult_peds_adol_all_df_gender.rda')







###################### INTRODUCTION ---------------------------------------------------

# This R code creates OU data, from OU_IM dataset, to be used to compare peds vs adult vs adolescent (if available) data 

library(tidyverse)
library(ggplot2)
library(tidyr)

rm(list = ls())

#OU_data = read.delim(file = 'C:/Users/pgeorge/Desktop/databases/input/DATIM_Genie/OU_IM_database/Genie-OU_IM-MultipleOUs-frozen-2024-12-25.txt', sep = '\t')
OU_data = read.delim(file = 'C:/Users/georg/Desktop/databases/input/DATIM_Genie/OU_IM_database/Genie-OU_IM-MultipleOUs-frozen-2024-12-25.txt', sep = '\t')


OU_data = OU_data %>% filter(funding_agency == 'USAID')

table(OU_data$country)

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

# Define the search strings
search_strings <- c("PBFW", "pregnan", "breastfeeding")

# Build a single regex pattern with ORs and ignore case
search_pattern <- regex(
  paste(search_strings, collapse = "|"), 
  ignore_case = TRUE
)

# Find variable names containing any of these strings (case-insensitive)
matching_vars <- OU_data %>%
  select(where(~ any(
    !is.na(.) & str_detect(as.character(.), search_pattern)
  ))) %>%
  names()

# Print the variable names
print(matching_vars)



### creating DATIM_long -----------------------------------------------------------------------------------------------------
# might be able to make this list shorter, especially when doing this with SITExIM files

OU_data_short <- OU_data %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_PVLS", "TX_ML", "TX_NEW", "TX_NET_NEW_SHIFT", "PrEP_CT" , "PrEP_CURR", "PrEP_NEW") |
           str_starts(indicator, "TX_CURR") | str_starts(indicator, "PMTCT"))



# Create the new variable 'indicator_ND'
OU_data_short <- OU_data_short %>%
  mutate(indicator_ND = if_else(!is.na(numeratordenom), 
                                paste(indicator, numeratordenom, sep = "_"), 
                                indicator))




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







### creating individual dataframes  -----------------------------------------------------------------------------------------------------

# I think the thing to do is create a dataframe, variable at a time, as opposed to the previous method, as this might be a bit more tricky for PBFW

table(DATIM_long$indicator_ND)

## PMTCT_STAT ----------------------

PMTCT_STAT_df = DATIM_long %>% 
  filter(str_starts(indicator, "PMTCT_STAT")) %>% 
  filter(ageasentered != "")

table(PMTCT_STAT_df$indicator_ND)


PMTCT_STAT_summary_df <- PMTCT_STAT_df %>%
  group_by(country, fiscal_year, quarter) %>%
  summarise(
    PMTCT_STAT_D = sum(value[indicator_ND == "PMTCT_STAT_D"], na.rm = TRUE),
    PMTCT_STAT_N = sum(value[indicator_ND == "PMTCT_STAT_N"], na.rm = TRUE),
    PMTCT_STAT_POS_N = sum(value[indicator_ND == "PMTCT_STAT_POS_N"], na.rm = TRUE),
    PMTCT_STAT_POS_N_HIV_known_at_entry = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Known at Entry"], na.rm = TRUE),
    PMTCT_STAT_POS_N_HIV_newly_diagnosed = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Newly Identified"], na.rm = TRUE)
  ) %>%
  ungroup()


PMTCT_STAT_summary_age_df <- PMTCT_STAT_df %>%
  group_by(country, fiscal_year, quarter, ageasentered) %>%
  summarise(
    PMTCT_STAT_D = sum(value[indicator_ND == "PMTCT_STAT_D"], na.rm = TRUE),
    PMTCT_STAT_N = sum(value[indicator_ND == "PMTCT_STAT_N"], na.rm = TRUE),
    PMTCT_STAT_POS_N = sum(value[indicator_ND == "PMTCT_STAT_POS_N"], na.rm = TRUE),
    PMTCT_STAT_POS_N_HIV_known_at_entry = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Known at Entry"], na.rm = TRUE),
    PMTCT_STAT_POS_N_HIV_newly_diagnosed = sum(value[indicator_ND == "PMTCT_STAT_POS_N" & otherdisaggregate == "Newly Identified"], na.rm = TRUE)
  ) %>%
  ungroup()



## PMTCT_ART ----------------------

PMTCT_ART_df = DATIM_long %>% 
  filter(str_starts(indicator, "PMTCT_ART")) %>% 
  filter(ageasentered != "")                                                      # if something goes wrong, check here 

table(PMTCT_ART_df$indicator_ND, PMTCT_ART_df$otherdisaggregate)


PMTCT_ART_summary_df <- PMTCT_ART_df %>%
  group_by(country, fiscal_year, quarter) %>%
  summarise(
    PMTCT_ART_D = sum(value[indicator_ND == "PMTCT_ART_D"], na.rm = TRUE),
    PMTCT_ART_N = sum(value[indicator_ND == "PMTCT_ART_N"], na.rm = TRUE),
    PMTCT_ART_N_ART_NEW = sum(value[indicator_ND == "PMTCT_ART_N" & otherdisaggregate == "Life-long ART, New"], na.rm = TRUE),
    PMTCT_ART_N_ART_Already = sum(value[indicator_ND == "PMTCT_ART_N" & otherdisaggregate == "Life-long ART, Already"], na.rm = TRUE)
  ) %>%
  ungroup()




## PMTCT_EID ----------------------

# note, for PMTCT_EID, in theory, PMTCT_EID_N should equal PMTCT_EID_Zero_to_Twelve_Mo, 
#       however, in some cases, it seems not to.  this could be a data quality issue. 
#       also, please note the difference between PMTCT_EID_N_first and PMTCT_EID_N_all 


PMTCT_EID_df <- DATIM_long %>%
  filter(str_starts(indicator_ND, "PMTCT_EID")) %>%  # Keep rows where indicator_ND starts with "PMTCT_EID"
  filter(!(indicator_ND == "PMTCT_EID_N" & ageasentered == ""))  # Exclude rows with specific conditions


table(PMTCT_EID_df$indicator_ND, PMTCT_EID_df$otherdisaggregate, PMTCT_EID_df$fiscal_year)
table(PMTCT_EID_df$indicator_ND, PMTCT_EID_df$ageasentered, PMTCT_EID_df$fiscal_year)
table(PMTCT_EID_df$indicator_ND, PMTCT_EID_df$standardizeddisaggregate)
table(PMTCT_EID_df$indicator_ND, PMTCT_EID_df$modality)


PMTCT_EID_summary_df <- PMTCT_EID_df %>%
  group_by(country, fiscal_year, quarter) %>%
  summarise(
    PMTCT_EID_D = sum(value[indicator_ND == "PMTCT_EID_D"], na.rm = TRUE),
    PMTCT_EID_N_first = sum(
      value[
        (indicator_ND == "PMTCT_EID_N") &
          ((fiscal_year == 2024 & otherdisaggregate == "EID First Test") | fiscal_year != 2024)
      ],
      na.rm = TRUE
    ),
    PMTCT_EID_N_all = sum(
      value[
        (indicator_ND == "PMTCT_EID_N") &
          ((fiscal_year == 2024 & otherdisaggregate %in% c("EID First Test", "EID Second Test or more")) | fiscal_year != 2024)
      ],
      na.rm = TRUE
    ),
    PMTCT_EID_Less_Equal_Two_Months_N = sum(value[indicator_ND == "PMTCT_EID_Less_Equal_Two_Months_N"], na.rm = TRUE),
    PMTCT_EID_Two_Twelve_Months_N = sum(value[indicator_ND == "PMTCT_EID_Two_Twelve_Months_N"], na.rm = TRUE)
  ) %>%
  mutate(
    PMTCT_EID_Zero_to_Twelve_Mo = PMTCT_EID_Less_Equal_Two_Months_N + PMTCT_EID_Two_Twelve_Months_N
  ) %>%
  ungroup()





## PMTCT_HEI ----------------------

### note - this one gives me some trouble, there have been a lot of switches, so proceed with caution!  


PMTCT_HEI_df <- DATIM_long %>%
  filter(str_starts(indicator_ND, "PMTCT_HEI")) %>%  # Keep rows where indicator_ND starts with "PMTCT_HEI"
  filter(!(indicator_ND %in% c("PMTCT_HEI_N", "PMTCT_HEI_POS_ART_N", "PMTCT_HEI_POS_N") & ageasentered == ""))


table(PMTCT_HEI_df$indicator_ND, PMTCT_HEI_df$standardizeddisaggregate)
table(PMTCT_HEI_df$indicator_ND, PMTCT_HEI_df$ageasentered)
table(PMTCT_HEI_df$indicator_ND, PMTCT_HEI_df$modality)
table(PMTCT_HEI_df$indicator_ND, PMTCT_HEI_df$otherdisaggregate)
table(PMTCT_HEI_df$indicator_ND, PMTCT_HEI_df$otherdisaggregate_sub)
table(PMTCT_HEI_df$indicator_ND, PMTCT_HEI_df$categoryoptioncomboname)
table(PMTCT_HEI_df$indicator_ND, PMTCT_HEI_df$fiscal_year)


check3df = PMTCT_HEI_df %>% 
  filter(indicator_ND == "PMTCT_HEI_NEG_N")
table(check3df$standardizeddisaggregate)


PMTCT_HEI_summary_df <- PMTCT_HEI_df %>%
  group_by(country, fiscal_year, quarter) %>%
  summarise(
    PMTCT_HEI_N = sum(value[indicator_ND == "PMTCT_HEI_N"], na.rm = TRUE),
    PMTCT_HEI_N_2MO_N = sum(value[indicator_ND == "PMTCT_HEI_N" & ageasentered == "<=02 Months"], na.rm = TRUE), 
    PMTCT_HEI_NEG_N = sum(value[indicator_ND == "PMTCT_HEI_NEG_N"], na.rm = TRUE),
    PMTCT_HEI_POS_N = sum(value[indicator_ND == "PMTCT_HEI_POS_N"], na.rm = TRUE),
    PMTCT_HEI_POS_2MO_N = sum(value[indicator_ND == "PMTCT_HEI_POS_2MO_N"], na.rm = TRUE),
    PMTCT_HEI_POS_ART_N = sum(value[indicator_ND == "PMTCT_HEI_POS_ART_N"], na.rm = TRUE),
    PMTCT_HEI_POS_Two_Twelve_Months_N = sum(value[indicator_ND == "PMTCT_HEI_POS_N" & ageasentered == "02 - 12 Months"], na.rm = TRUE)
  ) %>%
  ungroup()



## HTS_TST ----------------------
#    note, looks like in some years/countries (perhaps later year/countries), you have to add up HTS_TST_N_PMTCT_breastfeeding + HTS_TST_N_POSTANC1_PregLD to get the correct numbers?

HTS_TST_df <- DATIM_long %>%
  filter(str_starts(indicator_ND, "HTS_TST")) %>% 
  filter(modality %in% c("PMTCT ANC", "PMTCT Post ANC1 Breastfeeding", 
                         "PMTCT Post ANC1 Pregnant/L&D", "Post ANC1"))




table(HTS_TST_df$indicator_ND, HTS_TST_df$standardizeddisaggregate)
table(HTS_TST_df$indicator_ND, HTS_TST_df$ageasentered)
table(HTS_TST_df$indicator_ND, HTS_TST_df$modality, HTS_TST_df$fiscal_year)
table(HTS_TST_df$indicator_ND, HTS_TST_df$otherdisaggregate)
table(HTS_TST_df$indicator_ND, HTS_TST_df$otherdisaggregate_sub)
table(HTS_TST_df$indicator_ND, HTS_TST_df$categoryoptioncomboname)



HTS_TST_summary_df <- HTS_TST_df %>%
  group_by(country, fiscal_year, quarter) %>%
  summarise(
    HTS_TST_N = sum(value[indicator_ND == "HTS_TST_N"], na.rm = TRUE),
    HTS_TST_POS_N = sum(value[indicator_ND == "HTS_TST_POS_N"], na.rm = TRUE),
    HTS_TST_N_PMTCT_ANC = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT ANC" ], na.rm = TRUE),
    HTS_TST_POS_N_PMTCT_ANC = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT ANC" ], na.rm = TRUE),
    HTS_TST_N_PMTCT_POSTANC1_breastfeeding = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT Post ANC1 Breastfeeding" ], na.rm = TRUE),
    HTS_TST_POS_N_PMTCT_POSTANC1_breastfeeding = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT Post ANC1 Breastfeeding" ], na.rm = TRUE),
    HTS_TST_N_PMTCT_POSTANC1_PregLD = sum(value[indicator_ND == "HTS_TST_N" & modality == "PMTCT Post ANC1 Pregnant/L&D" ], na.rm = TRUE),
    HTS_TST_POS_N_PMTCT_POSTANC1_PregLD = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "PMTCT Post ANC1 Pregnant/L&D" ], na.rm = TRUE),
    HTS_TST_N_POST_ANC1 = sum(value[indicator_ND == "HTS_TST_N" & modality == "Post ANC1" ], na.rm = TRUE),
    HTS_TST_POS_N_POST_ANC1 = sum(value[indicator_ND == "HTS_TST_POS_N" & modality == "Post ANC1" ], na.rm = TRUE),
  ) %>%
  ungroup()



## TX_PVLS ----------------------


TX_PVLS_df <- DATIM_long %>%
  filter(str_starts(indicator_ND, "TX_PVLS")) %>% 
  filter(standardizeddisaggregate == "PregnantOrBreastfeeding/Indication/HIVStatus" | standardizeddisaggregate == "Sex/PregnantOrBreastfeeding/HIVStatus")




table(TX_PVLS_df$indicator_ND, TX_PVLS_df$standardizeddisaggregate)
table(TX_PVLS_df$indicator_ND, TX_PVLS_df$ageasentered)
table(TX_PVLS_df$indicator_ND, TX_PVLS_df$modality)
table(TX_PVLS_df$indicator_ND, TX_PVLS_df$otherdisaggregate)
table(TX_PVLS_df$indicator_ND, TX_PVLS_df$otherdisaggregate_sub)
table(TX_PVLS_df$indicator_ND, TX_PVLS_df$categoryoptioncomboname)


checkdf = TX_PVLS_df %>% 
  filter(standardizeddisaggregate == "Age/Sex/Indication/HIVStatus")

table(checkdf$categoryoptioncomboname)


TX_PVLS_summary_df <- TX_PVLS_df %>%
  group_by(country, fiscal_year, quarter) %>%
  summarise(
    TX_PVLS_D = sum(value[indicator_ND == "TX_PVLS_D"], na.rm = TRUE),
    TX_PVLS_N = sum(value[indicator_ND == "TX_PVLS_N"], na.rm = TRUE),
    TX_PVLS_D_pregnant = sum(value[indicator_ND == "TX_PVLS_D" & otherdisaggregate == "Pregnant" ], na.rm = TRUE),
    TX_PVLS_N_pregnant = sum(value[indicator_ND == "TX_PVLS_N" & otherdisaggregate == "Pregnant" ], na.rm = TRUE),
    TX_PVLS_D_breastfeeding = sum(value[indicator_ND == "TX_PVLS_D" & otherdisaggregate == "Breastfeeding" ], na.rm = TRUE),
    TX_PVLS_N_breastfeeding = sum(value[indicator_ND == "TX_PVLS_N" & otherdisaggregate == "Breastfeeding" ], na.rm = TRUE),
  ) %>%
  ungroup()


TX_check = TX_PVLS_summary_df %>% 
  filter(country == 'Mozambique', 
         fiscal_year == 2024, 
         quarter == 4)

TX_check


## PrEP ----------------------
#    note, the pregnant/breastfeeding disaggregate is optional 

PrEP_df <- DATIM_long %>%
  filter(str_starts(indicator_ND, "PrEP")) %>% 
  filter(standardizeddisaggregate == "Sex/PregnantBreastfeeding") 


table(PrEP_df$indicator_ND, PrEP_df$standardizeddisaggregate)
table(PrEP_df$indicator_ND, PrEP_df$ageasentered)
table(PrEP_df$indicator_ND, PrEP_df$modality)
table(PrEP_df$indicator_ND, PrEP_df$otherdisaggregate)
table(PrEP_df$indicator_ND, PrEP_df$otherdisaggregate_sub)
table(PrEP_df$indicator_ND, PrEP_df$categoryoptioncomboname)



PrEP_summary_df <- PrEP_df %>%
  group_by(country, fiscal_year, quarter) %>%
  summarise(
    PrEP_CT_N = sum(value[indicator_ND == "PrEP_CT_N"], na.rm = TRUE),
    PrEP_NEW_N = sum(value[indicator_ND == "PrEP_NEW_N"], na.rm = TRUE),
    PrEP_CT_N_pregnant = sum(value[indicator_ND == "PrEP_CT_N" & categoryoptioncomboname == "Pregnant, Female" ], na.rm = TRUE),
    PrEP_NEW_N_pregnant = sum(value[indicator_ND == "PrEP_NEW_N" & categoryoptioncomboname == "Pregnant, Female" ], na.rm = TRUE),
    PrEP_CT_N_breastfeeding = sum(value[indicator_ND == "PrEP_CT_N" & categoryoptioncomboname == "Breastfeeding, Female" ], na.rm = TRUE),
    PrEP_NEW_N_breastfeeding = sum(value[indicator_ND == "PrEP_NEW_N" & categoryoptioncomboname == "Breastfeeding, Female" ], na.rm = TRUE),
  ) %>%
  ungroup()






### bind_cols by $country, fiscal_year, quarter to create the final PVT OU dataframe -----------------------------------------------------------------------------------------------------

PVT_OU_df <- full_join(
  PMTCT_STAT_summary_df, 
  PMTCT_ART_summary_df, 
  by = c("country", "fiscal_year", "quarter")
) %>% 
  full_join(
    PMTCT_EID_summary_df, 
    by = c("country", "fiscal_year", "quarter")
  ) %>% 
  full_join(
    PMTCT_HEI_summary_df, 
    by = c("country", "fiscal_year", "quarter")
  ) %>% 
  full_join(
    HTS_TST_summary_df, 
    by = c("country", "fiscal_year", "quarter")
  ) %>% 
  full_join(
    TX_PVLS_summary_df, 
    by = c("country", "fiscal_year", "quarter")
  ) %>% 
  full_join(
    PrEP_summary_df, 
    by = c("country", "fiscal_year", "quarter")
  )



## add totals rows -----------------------------------------------------------------------------------
# Summarize the data to create totals for each fiscal year and quarter
totals_df <- PVT_OU_df %>%
  group_by(fiscal_year, quarter) %>%
  summarize(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(country = "ALL")



# Bind the summarized data back to the original dataframe
PVT_OU_df <- bind_rows(PVT_OU_df, totals_df)

# Arrange the data to ensure it is in a logical order
PVT_OU_df <- PVT_OU_df %>%
  arrange(country, fiscal_year, quarter)



str(PVT_OU_df)

# lets make some new new variables

PVT_OU_df <- PVT_OU_df %>%
  mutate(
    PMTCT_STAT_testing_coverage = round((PMTCT_STAT_N / PMTCT_STAT_D) * 100, 1),
    positivity_overall_ANC = round((PMTCT_STAT_POS_N / PMTCT_STAT_N) * 100, 1),
    ANC1_missed_number = PMTCT_STAT_D - PMTCT_STAT_N, 
    positivity_new = round((PMTCT_STAT_POS_N_HIV_newly_diagnosed / PMTCT_STAT_N) * 100, 1),
    positivity_postANC1_breastfeeding = round((HTS_TST_POS_N_PMTCT_POSTANC1_breastfeeding / HTS_TST_N_PMTCT_POSTANC1_breastfeeding) * 100, 1),
    positivity_postANC1_preg = round((HTS_TST_POS_N_PMTCT_POSTANC1_PregLD / HTS_TST_N_PMTCT_POSTANC1_PregLD) * 100, 1),
    HIV_not_on_ART = PMTCT_STAT_POS_N - PMTCT_ART_N, 
    HEI_pos_linkage_ART_proxy = round((PMTCT_HEI_POS_ART_N / PMTCT_HEI_POS_N) * 100, 1),
    PMTCT_ART_linkage_new_proxy = round((PMTCT_ART_N_ART_NEW / PMTCT_STAT_POS_N_HIV_newly_diagnosed) * 100, 1),
    PMTCT_ART_coverage_total = round(PMTCT_ART_N/PMTCT_ART_D * 100, 1), 
    EID_coverage_2mo = round((PMTCT_EID_Less_Equal_Two_Months_N / PMTCT_EID_D) * 100, 1),
    EID_coverage = round((PMTCT_EID_N_first / PMTCT_EID_D) * 100, 1),
    HEI_pos_2mo = round((PMTCT_HEI_POS_2MO_N / (PMTCT_HEI_N_2MO_N)) * 100, 1),
    HEI_pos_12mo = round((PMTCT_HEI_POS_N / PMTCT_HEI_N) * 100, 1),
    Missed_reporting_proxy = round((1 - (PMTCT_HEI_N / PMTCT_EID_N_all)) * 100, 1),
    viral_suppression = round((TX_PVLS_N / TX_PVLS_D) * 100, 1),
    viral_suppression_pregnant = round((TX_PVLS_N_pregnant / TX_PVLS_D_pregnant) * 100, 1),
    viral_suppression_breastfeeding = round((TX_PVLS_N_breastfeeding / TX_PVLS_D_breastfeeding) * 100, 1), 
    HEI_POS_on_ART = round((PMTCT_HEI_POS_ART_N / PMTCT_HEI_POS_N) * 100, 1)
  )

summary(PVT_OU_df$ANC1_missed_number)

PVT_OU_df = PVT_OU_df %>% 
  mutate(country = ifelse(country == 'Democratic Republic of the Congo', 'DRC', country))

save(PVT_OU_df, file = 'C:/Users/georg/Desktop/databases/output/PVT/PVT_OU_df.rda')


load('C:/Users/georg/Desktop/databases/output/PVT/PVT_OU_df.rda')

checkdf = PVT_OU_df %>% 
  filter(fiscal_year  == 2024, 
         quarter == 3, 
         country == "ALL") %>% 
  select(PMTCT_STAT_testing_coverage, positivity_overall_ANC, positivity_new, PMTCT_ART_coverage_total, EID_coverage_2mo, EID_coverage, HEI_pos_2mo, HEI_pos_12mo, HEI_pos_linkage_ART_proxy)


checkdf

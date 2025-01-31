

############## Initialization and loading/shaping data ----------------------------------------------


library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(digest)

#Uganda_data = read.delim(file = 'C:/Users/pgeorge/Desktop/databases/input/DATIM_Genie/Genie-SITE_IM-Uganda-frozen-2024-12-13.txt', 
#                         sep = '\t' )

Uganda_data = load('C:/Users/georg/Desktop/databases/input/DATIM_Genie/2024_Q4/Uganda_SITE_IM_2022_2024Q4.rda')
Uganda_data = Uganda_SITE_IM_2022_2024Q4
rm(Uganda_SITE_IM_2022_2024Q4)

table(Uganda_data$otherdisaggregate_sub)

Uganda_data_skinny = Uganda_data %>% 
  select(country, snu1, snu2, psnu, prime_partner_name, mech_name, community, facility, facilityuid, funding_agency, fiscal_year, ageasentered, target_age_2024, qtr1, qtr2, qtr3, qtr4, sex, cumulative, targets, 
         target_age_2024, standardizeddisaggregate, otherdisaggregate, otherdisaggregate_sub, numeratordenom, indicator, indicatortype, categoryoptioncomboname, 
         modality, safe_for_net_new, safe_for_vlc)


# Reshape the data to long format
DATIM_long <- Uganda_data_skinny %>%
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



############## function to look at trends across time by indicators and subgroups ----------------------------------------------


# Define the age groups and corresponding age categories
age_groups <- list(
  under1 = '<01', 
  under5 = c('<01', '01-04'),
  peds = c('<01', '01-04', '01-09', '05-09', '10-14'),
  adolescent = c('10-14', '15-19')
)


trend_summary <- function(data, indicator_name, grouping_var, age_group = "peds") {
  # Check if the specified age_group exists in age_groups
  if(!age_group %in% names(age_groups)) {
    stop("Invalid age_group. Choose from: under1, under5, peds, adolescent.")
  }
  
  # Extract the appropriate age categories
  selected_ages <- age_groups[[age_group]]
  
  # Filter the data
  filtered_data <- data %>%
    dplyr::filter(
      .data$indicator == indicator_name,
      ageasentered %in% selected_ages
    )
  
  # Group and summarize
  summarized <- filtered_data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(grouping_var)), 
      fiscal_year, 
      quarter_num
    ) %>%
    dplyr::summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop")
  
  # Create a time variable for chronological plotting
  summarized <- summarized %>%
    dplyr::mutate(time_order = fiscal_year + (quarter_num - 1)/4)
  
  # Create a ggplot line chart with thicker lines and rotated x-axis text
  p <- ggplot2::ggplot(summarized, 
                       ggplot2::aes(
                         x = time_order, 
                         y = total_value, 
                         group = .data[[grouping_var]], 
                         color = .data[[grouping_var]]
                       )) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(
      title = paste("Trend of", indicator_name, "by", grouping_var, 'for age', age_group),
      x = "Fiscal Year and Quarter",
      y = "Total Value"
    ) +
    ggplot2::scale_x_continuous(
      breaks = unique(summarized$time_order),
      labels = function(x) {
        fy <- floor(x)
        q <- (x - fy)*4 + 1
        paste0("FY", fy, "-Q", as.integer(q))
      }
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  p_interactive = ggplotly(p)
  return(list(data = summarized, plot = p_interactive))
}


results = trend_summary(data = DATIM_long, indicator_name = 'HTS_TST_POS', grouping_var = 'snu1', age_group = 'under1')

summarized_df = results$data
results$plot

############## looking at MMD data ----------------------------------------------

table(DATIM_long$indicator)



MMD_df = DATIM_long %>% 
  filter(ageasentered == '<15', 
         indicator == 'TX_CURR')

table(MMD_df$otherdisaggregate, MMD_df$ageasentered, useNA = 'always')


MMD_df_skinny <- MMD_df %>%
  # Create the combined variable
  mutate(unique_implementing_site = paste(country, snu1, snu2, psnu, prime_partner_name, community, facilityuid, funding_agency, sep = " | ")) %>%
  # Create a unique 6-character alphanumeric variable
  rowwise() %>%
  mutate(unique_implementing_site_factor = substr(digest(unique_implementing_site, algo = "sha1"), 1, 6)) %>%
  ungroup() %>%
  mutate(unique_implementing_site_factor = as.factor(unique_implementing_site_factor)) %>% 
  select(unique_implementing_site, unique_implementing_site_factor, snu1, indicator, numeratordenom, ageasentered, target_age_2024, standardizeddisaggregate, otherdisaggregate, sex, fiscal_year, quarter_num, value)

n_distinct(MMD_df_skinny$unique_implementing_site)

MMD_df_skinny_2 <- MMD_df_skinny %>%
  # Group by fiscal year, quarter, and site
  group_by(fiscal_year, quarter_num, unique_implementing_site) %>%
  # Summarize values for each disaggregate
  summarise(
    value_3_months = sum(value[otherdisaggregate == "ARV Dispensing Quantity - 3 to 5 months"], na.rm = TRUE),
    value_6_months = sum(value[otherdisaggregate == "ARV Dispensing Quantity - 6 or more months"], na.rm = TRUE),
    value_less_3_months = sum(value[otherdisaggregate == "ARV Dispensing Quantity - Less than 3 months"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate the total denominator
  mutate(
    total_dispensing = value_3_months + value_6_months + value_less_3_months,
    percent_MMD_3months = ifelse(total_dispensing > 0, value_3_months / total_dispensing * 100, NA),
    percent_MMD_6month = ifelse(total_dispensing > 0, (value_3_months + value_6_months) / total_dispensing * 100, NA)
  )

site1 = MMD_df_skinny$unique_implementing_site[1]


check1 = MMD_df_skinny %>% filter(unique_implementing_site == site1) 









MMDcheck <- DATIM_long %>%
  filter(ageasentered %in% c('<01', '01-04', '01-09', '05-09', '10-14', '<15')) %>%
  filter(indicator %in% c('TX_CURR', 'TX_CURR_Lag2', 'TX_PVLS', 'TX_NET_NEW_SHIFT', 
                          'TX_NEW', 'TX_PVLS', 'TX_RTT') | 
           str_starts(indicator, "TX_ML"))





str(MMDcheck)

table(MMDcheck$standardizeddisaggregate, MMDcheck$ageasentered)
table(MMDcheck$otherdisaggregate, MMDcheck$ageasentered)
table(MMDcheck$numeratordenom, useNA = 'always')

MMDcheck <- MMDcheck %>%
  mutate(indicator_ND = case_when(
    numeratordenom == "N" ~ paste0(indicator, "_N"),
    numeratordenom == "D" ~ paste0(indicator, "_D"),
    TRUE ~ indicator # Retain the original indicator if numeratordenom is neither "N" nor "D"
  )) %>% 
  select(c(-indicator, -numeratordenom))





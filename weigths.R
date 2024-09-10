correntes_pib = sidrar::get_sidra(api = "/t/1846/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v585%200")

# Load necessary libraries
library(dplyr)
library(tidyr)

# Separate 'Trimestre' into 'Trimestre' (quarter) and 'Ano' (year)
correntes_pib <- correntes_pib %>%
  separate(Trimestre, into = c("Trimestre", "Ano"), sep = " trimestre ") %>%
  mutate(Ano = as.numeric(Ano),
         Trimestre = case_when(
           Trimestre == "1º" ~ 1,
           Trimestre == "2º" ~ 2,
           Trimestre == "3º" ~ 3,
           Trimestre == "4º" ~ 4
         ))

# Convert the 'Valor' column to numeric, in case it's not already
correntes_pib$Valor <- as.numeric(correntes_pib$Valor)

# Create time series for each sector

# For PIB a preços de mercado
pib_market_ts <- correntes_pib %>%
  filter(`Setores e subsetores` == "PIB a preços de mercado") %>%
  arrange(Ano, Trimestre) %>%
  select(Ano, Trimestre, Valor) %>%
  ts(start = c(min(.$Ano), min(.$Trimestre)), frequency = 4)

# For Agropecuária - total
agropecuaria_ts <- correntes_pib %>%
  filter(`Setores e subsetores` == "Agropecuária - total") %>%
  arrange(Ano, Trimestre) %>%
  select(Ano, Trimestre, Valor) %>%
  ts(start = c(min(.$Ano), min(.$Trimestre)), frequency = 4)

# For Indústria - total
industria_ts <- correntes_pib %>%
  filter(`Setores e subsetores` == "Indústria - total") %>%
  arrange(Ano, Trimestre) %>%
  select(Ano, Trimestre, Valor) %>%
  ts(start = c(min(.$Ano), min(.$Trimestre)), frequency = 4)

# For Serviços - total
servicos_ts <- correntes_pib %>%
  filter(`Setores e subsetores` == "Serviços - total") %>%
  arrange(Ano, Trimestre) %>%
  select(Ano, Trimestre, Valor) %>%
  ts(start = c(min(.$Ano), min(.$Trimestre)), frequency = 4)

# Load necessary libraries
library(dplyr)

# Function to calculate weights, year-over-year growth, and contributions for any sector
calculate_weights_and_contributions_yoy <- function(sector_name) {
  # Filter the data for the given sector and for the total GDP
  sector_data <- correntes_pib %>%
    filter(`Setores e subsetores` == sector_name) %>%
    arrange(Ano, Trimestre) %>%
    select(Ano, Trimestre, Valor)
  
  total_gdp_data <- correntes_pib %>%
    filter(`Setores e subsetores` == "PIB a preços de mercado") %>%
    arrange(Ano, Trimestre) %>%
    select(Ano, Trimestre, Valor) %>%
    rename(Total_GDP = Valor)
  
  # Merge sector data with total GDP data to compute the weight and year-over-year growth
  merged_data <- sector_data %>%
    left_join(total_gdp_data, by = c("Ano", "Trimestre")) %>%
    mutate(
      # Calculate the weight of the sector in total GDP
      Weight = Valor / Total_GDP,
      
      # Calculate the year-over-year growth rate of the sector and total GDP
      Sector_Growth_YoY = (Valor - lag(Valor, 4)) / lag(Valor, 4),
      GDP_Growth_YoY = (Total_GDP - lag(Total_GDP, 4)) / lag(Total_GDP, 4),
      
      # Calculate the contribution of the sector to GDP growth (YoY)
      Contribution_YoY = Weight * Sector_Growth_YoY
    )
  
  # Return the data for analysis
  return(merged_data)
}

# Supply-side sectors: Agropecuária, Indústria, and Serviços
agropecuaria_contrib <- calculate_weights_and_contributions_yoy("Agropecuária - total")
industria_contrib <- calculate_weights_and_contributions_yoy("Indústria - total")
servicos_contrib <- calculate_weights_and_contributions_yoy("Serviços - total")

# Demand-side sectors: Household consumption, Government consumption, Gross fixed capital formation, Exports, Imports
household_consumption_contrib <- calculate_weights_and_contributions_yoy("Despesa de consumo das famílias")
government_consumption_contrib <- calculate_weights_and_contributions_yoy("Despesa de consumo da administração pública")
gross_fixed_capital_contrib <- calculate_weights_and_contributions_yoy("Formação bruta de capital fixo")
exports_contrib <- calculate_weights_and_contributions_yoy("Exportação de bens e serviços")
imports_contrib <- calculate_weights_and_contributions_yoy("Importação de bens e serviços (-)")



# Combine all sectors into a single DataFrame with only weights and contributions
combined_contrib_yoy <- agropecuaria_contrib %>%
  rename(Agropecuaria_Weight = Weight, Agropecuaria_Contribution_YoY = Contribution_YoY) %>%
  left_join(industria_contrib %>%
              select(Ano, Trimestre, Weight, Contribution_YoY) %>%
              rename(Industria_Weight = Weight, Industria_Contribution_YoY = Contribution_YoY),
            by = c("Ano", "Trimestre")) %>%
  left_join(servicos_contrib %>%
              select(Ano, Trimestre, Weight, Contribution_YoY) %>%
              rename(Servicos_Weight = Weight, Servicos_Contribution_YoY = Contribution_YoY),
            by = c("Ano", "Trimestre")) %>%
  left_join(household_consumption_contrib %>%
              select(Ano, Trimestre, Weight, Contribution_YoY) %>%
              rename(Household_Weight = Weight, Household_Contribution_YoY = Contribution_YoY),
            by = c("Ano", "Trimestre")) %>%
  left_join(government_consumption_contrib %>%
              select(Ano, Trimestre, Weight, Contribution_YoY) %>%
              rename(Government_Weight = Weight, Government_Contribution_YoY = Contribution_YoY),
            by = c("Ano", "Trimestre")) %>%
  left_join(gross_fixed_capital_contrib %>%
              select(Ano, Trimestre, Weight, Contribution_YoY) %>%
              rename(GFCF_Weight = Weight, GFCF_Contribution_YoY = Contribution_YoY),
            by = c("Ano", "Trimestre")) %>%
  left_join(exports_contrib %>%
              select(Ano, Trimestre, Weight, Contribution_YoY) %>%
              rename(Exports_Weight = Weight, Exports_Contribution_YoY = Contribution_YoY),
            by = c("Ano", "Trimestre")) %>%
  left_join(imports_contrib %>%
              select(Ano, Trimestre, Weight, Contribution_YoY) %>%
              rename(Imports_Weight = Weight, Imports_Contribution_YoY = Contribution_YoY),
            by = c("Ano", "Trimestre")) %>%
  # Select only the columns for weights and contributions
  select(Ano, Trimestre,
         Agropecuaria_Weight, Agropecuaria_Contribution_YoY,
         Industria_Weight, Industria_Contribution_YoY,
         Servicos_Weight, Servicos_Contribution_YoY,
         Household_Weight, Household_Contribution_YoY,
         Government_Weight, Government_Contribution_YoY,
         GFCF_Weight, GFCF_Contribution_YoY,
         Exports_Weight, Exports_Contribution_YoY,
         Imports_Weight, Imports_Contribution_YoY)

# Sort the DataFrame by year and quarter from oldest to present
combined_contrib_yoy <- combined_contrib_yoy %>%
  arrange(Ano, Trimestre)

# Display the first few rows of the combined DataFrame in time-series format
View(combined_contrib_yoy)

# mean
# Compute the mean of the last 4 quarters for each column
last_4_quarters_mean <- combined_contrib_yoy %>%
  tail(4) %>%  # Select the last 4 rows
  summarise(across(starts_with("Agropecuaria_Weight"):starts_with("Imports_Contribution_YoY"), 
                   \(x) mean(x, na.rm = TRUE)))

# Add the year and quarter for the mean row (convert to character)
last_4_quarters_mean <- last_4_quarters_mean %>%
  mutate(Ano = as.character("Mean of last 4 Quarters"), 
         Trimestre = as.character("N/A"))

# Convert the 'Ano' and 'Trimestre' columns in the original data to character before binding rows
combined_contrib_yoy <- combined_contrib_yoy %>%
  mutate(Ano = as.character(Ano), Trimestre = as.character(Trimestre))

# Append the mean row to the original DataFrame
combined_contrib_yoy_with_mean <- bind_rows(combined_contrib_yoy, last_4_quarters_mean)

# Display the final DataFrame with the added mean row
final_weights = combined_contrib_yoy_with_mean[nrow(combined_contrib_yoy_with_mean),3:ncol(combined_contrib_yoy_with_mean)]
supply_weights = final_weights[1:6]
demand_weights = final_weights[7:ncol(final_weights)]


















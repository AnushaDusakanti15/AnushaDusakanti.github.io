library(tidyverse)
library(dplyr)
library(plotly)
library(tidyr)

# Importing the CSV File containing Raw Data of Unemployment Rate alone
lfs_sexage_raw = read.csv("/Users/anushadusakanti/Documents/GitHub/AnushaDusakanti15.github.io/LFS_SEXAGE_I_R_05032024021852784.csv")

# Understanding the structure of the data frame
str(lfs_sexage_raw) 

# Sub-setting the raw data to select Unemployment Rate data of Japanese Youth
unemployment_rate_japan = subset(lfs_sexage_raw, Country == 'Japan' & Age == '15 to 24' & Series == 'Unemployment rate')

# Keeping only the required columns, rounding the Unemployment Rate to 2 digits, renaming columns
# to appropriate names and resetting the row numbers
unemployment_rate_japan = select(unemployment_rate_japan, Sex, Time, Value)
unemployment_rate_japan$Value = round(unemployment_rate_japan$Value, digits = 2)
unemployment_rate_japan <- unemployment_rate_japan %>% 
  rename(Year = Time)

row.names(unemployment_rate_japan) = NULL

# Selecting only the Unemployment Ice Age Years and resetting the row numbers
data1994_2004 = subset(unemployment_rate_japan, Year >= 1994 & Year  <= 2004)
row.names(data1994_2004) = NULL

#Transposing the Unemployment Ice Age data by pivoting the table from long to wide.
transposed_1994_2004 = pivot_wider(data1994_2004, names_from = "Sex", values_from = "Value")

#Grouping the transposed data by Year and renaming the column containing spaces in between
grouped_1994_2004 = transposed_1994_2004 %>%
  group_by(Year)
grouped_1994_2004 = grouped_1994_2004 %>% 
  rename(All.persons = 'All persons')

# Creating an interactive bar plot with the manipulated data frame t portray the population during Unemployment Ice Age
bar_plt_1994_2004 = plot_ly(grouped_1994_2004, x = ~Year, y = ~All.persons, type = 'bar', name = 'All persons',
             marker = list(color = 'green')) %>% 
    add_trace(y = ~Men, name = 'Men', 
              marker = list(color = 'blue')) %>%
    add_trace(y = ~Women, name = 'Women', 
              marker = list(color = 'red')) %>%
    layout(xaxis = list(title = "Years", tickmode='linear', tickangle = -90),
           yaxis = list(title = "Unemployment Rate"),
           title = 'Unemployment Rate during Ice Age \n Japan (1994-2004)',
            barmode = 'group')
bar_plt_1994_2004





# Importing the CSV file containing Japan's Anual GDP Growth Rate
# obtained from https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.MKTP.KD.ZG&country=JPN#advancedDownloadOptions

gdp_annual_1960 = read.csv("/Users/anushadusakanti/Documents/GitHub/AnushaDusakanti15.github.io/Japan_GDP_Annual_Growth.csv")

# Understanding the structure of the dataset
str(gdp_annual_1960) 

#Selecting only the columns required
gdp_annual_1960 = select(gdp_annual_1960, Time, Value)

#Keeping only the meaningful rows
gdp_annual = slice(gdp_annual_1960, 9:63)

#Data Manipulation - converting character columns to numeric, rounding off percentage to 2 digits, and renaming columns appropriately
gdp_annual$Time = as.numeric(gdp_annual$Time)
gdp_annual$Value = as.numeric(gdp_annual$Value)
gdp_annual$Value = round(gdp_annual$Value, digits = 2)
gdp_annual = gdp_annual %>% 
  rename(GDP_Annual_Growth = 'Value')
gdp_annual = gdp_annual %>% 
  rename(Year = 'Time')

#Subsetting the population to 'All persons' to get an overall unemployment outlook from the previously created 
# Unemployment Rate data of Japanese Youth data frame
unemployment_all_persons = subset(unemployment_rate_japan, Sex == 'All persons')

#Data Manipulation - converting character columns to numeric, rounding off percetnage to 2 digits, and renaming columns appropriately
unemployment_time_value = select(unemployment_all_persons, Year, Value)
unemployment_time_value = unemployment_time_value %>% 
  rename(Unemployment_Rate = 'Value')


# Doing an inner join on the GDP data frame and Unemployment Years data frame with the common column 'Year'
gdp_unemployment = merge(x = gdp_annual, y = unemployment_time_value, by = 'Year')
gdp_unemployment

# Creating an interactive line chart to depict the relationship between GDP Growth Rate and Unemployment Rate
line_gdp_unemp = plot_ly(gdp_unemployment, x = ~Year) %>% 
    add_trace(y = ~GDP_Annual_Growth, name = 'GDP Annual Growth (in Percentage)', type = 'scatter', mode = 'lines') %>% 
    add_trace(y = ~Unemployment_Rate, name = 'Unemployment Rate (in Percentage)', type = 'scatter', mode = 'lines') %>%
    layout(xaxis = list(title = "Years", tickmode='linear', tickangle = -90),
           yaxis = list(title = "Percentage"),
           title = 'GDP Growth Rate vs Unemployment Rate \n Japan (1968-2022)')
line_gdp_unemp









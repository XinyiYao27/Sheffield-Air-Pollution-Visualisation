##Download and transform the files into the format that is suitable to analyse 

library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)

Temp <- read_csv("open-meteo-53.39N1.37W75m.csv") 
# Add variables year, month, day, hour
Temp$year <- as.character(format(Temp$time, "%Y"))
Temp$month <- as.integer(format(Temp$time, "%m"))
Temp$day   <- as.integer(format(Temp$time, "%d"))
Temp$hour  <- as.integer(format(Temp$time, "%H"))



barn_23 <- read_csv("SHBR_2023.csv") %>%
  mutate(time = as.POSIXct(paste(Date, time),
                           format = "%d-%m-%Y %H:%M:%S",
                           tz = "UTC")) %>%
  select(time, no = `Nitric oxide`,  # Rename the variables
         nox = `Nitrogen oxides as nitrogen dioxide`,
         no2 = `Nitrogen dioxide`,
         pm25 = `PM<sub>2.5</sub> particulate matter (Hourly measured)`)
barn_24 <- read_csv("SHBR_2024.csv") %>%
  mutate(time = as.POSIXct(paste(Date, time),
                           format = "%d-%m-%Y %H:%M:%S",
                           tz = "UTC")) %>%
  select(time, no = `Nitric oxide`,
         nox = `Nitrogen oxides as nitrogen dioxide`,
         no2 = `Nitrogen dioxide`,
         pm25 = `PM<sub>2.5</sub> particulate matter (Hourly measured)`)
barn_25 <- read_csv("SHBR_2025.csv") %>%
  mutate(time = as.POSIXct(paste(Date, time),
                           format = "%d-%m-%Y %H:%M:%S",
                           tz = "UTC")) %>%
  select(time, no = `Nitric oxide`,
         nox = `Nitrogen oxides as nitrogen dioxide`,
         no2 = `Nitrogen dioxide`,
         pm25 = `PM<sub>2.5</sub> particulate matter (Hourly measured)`)
barn <- rbind(barn_23,barn_24,barn_25)

devon_23 <- read_csv("SHDG_2023.csv") %>%
  mutate(time = as.POSIXct(paste(Date, time),
                           format = "%d-%m-%Y %H:%M:%S",
                           tz = "UTC")) %>%
  select(time, o3 = Ozone, no = `Nitric oxide`,
         nox = `Nitrogen oxides as nitrogen dioxide`,
         no2 = `Nitrogen dioxide`,
         pm10 = `PM<sub>10</sub> particulate matter (Hourly measured)`,
         pm25 = `PM<sub>2.5</sub> particulate matter (Hourly measured)`)
devon_24 <- read_csv("SHDG_2024.csv") %>%
  mutate(time = as.POSIXct(paste(Date, time),
                           format = "%d-%m-%Y %H:%M:%S",
                           tz = "UTC")) %>%
  select(time, o3 = Ozone, no = `Nitric oxide`,
         nox = `Nitrogen oxides as nitrogen dioxide`,
         no2 = `Nitrogen dioxide`,
         pm10 = `PM<sub>10</sub> particulate matter (Hourly measured)`,
         pm25 = `PM<sub>2.5</sub> particulate matter (Hourly measured)`)
devon_25 <- read_csv("SHDG_2025.csv") %>%
  mutate(time = as.POSIXct(paste(Date, time),
                           format = "%d-%m-%Y %H:%M:%S",
                           tz = "UTC")) %>%
  select(time, o3 = Ozone, no = `Nitric oxide`,
         nox = `Nitrogen oxides as nitrogen dioxide`,
         no2 = `Nitrogen dioxide`,
         pm10 = `PM<sub>10</sub> particulate matter (Hourly measured)`,
         pm25 = `PM<sub>2.5</sub> particulate matter (Hourly measured)`)
devon <- rbind(devon_23,devon_24,devon_25)

tin_23 <- read_csv("SHE_2023.csv") %>%
  mutate(time = as.POSIXct(paste(Date, time),
                           format = "%d-%m-%Y %H:%M:%S",
                           tz = "UTC")) %>%
  select(time, o3 = Ozone, no = `Nitric oxide`,
         nox = `Nitrogen oxides as nitrogen dioxide`,
         no2 = `Nitrogen dioxide`,
         pm10 = `PM<sub>10</sub> particulate matter (Hourly measured)`,
         pm25 = `PM<sub>2.5</sub> particulate matter (Hourly measured)`)
tin_24 <- read_csv("SHE_2024.csv") %>%
  mutate(time = as.POSIXct(paste(Date, time),
                           format = "%d-%m-%Y %H:%M:%S",
                           tz = "UTC")) %>%
  select(time, o3 = Ozone, no = `Nitric oxide`,
         nox = `Nitrogen oxides as nitrogen dioxide`,
         no2 = `Nitrogen dioxide`,
         pm10 = `PM<sub>10</sub> particulate matter (Hourly measured)`,
         pm25 = `PM<sub>2.5</sub> particulate matter (Hourly measured)`)
tin_25 <- read_csv("SHE_2025.csv") %>%
  mutate(time = as.POSIXct(paste(Date, time),
                           format = "%d-%m-%Y %H:%M:%S",
                           tz = "UTC")) %>%
  select(time, o3 = Ozone, no = `Nitric oxide`,
         nox = `Nitrogen oxides as nitrogen dioxide`,
         no2 = `Nitrogen dioxide`,
         pm10 = `PM<sub>10</sub> particulate matter (Hourly measured)`,
         pm25 = `PM<sub>2.5</sub> particulate matter (Hourly measured)`)
tin <- rbind(tin_23,tin_24,tin_25)


#Compile files and create site variables, merging weather files
wide <- bind_rows(Devon = devon,
                  Tin = tin,
                  Barn = barn,
                  .id = "site")

wide <- left_join(wide, Temp, by = 'time') %>%
  rename(date = time,
         temp = `temperature_2m (°C)`,
         ws = `wind_speed_10m (km/h)`,
         humidity = `relative_humidity_2m (%)`,
         rain = `rain (mm)`,
         preci = `precipitation (mm)`,
         wd = `wind_direction_10m (°)`)


#Add new variables: month_label, season, and day of the week
wide <- wide %>%
  mutate(
    season = case_when(month %in% c(12, 1, 2) ~ "Winter",
                       month %in% c(3, 4, 5) ~ "Spring",
                       month %in% c(6, 7, 8) ~ "Summer",
                       month %in% c(9, 10, 11) ~ "Autumn"),
    day_of_week = wday(date, label = TRUE, abbr = TRUE, 
                       locale = "English"),
    is_weekend = if_else(day_of_week %in% c("Sat", "Sun"), 
                         "Weekend", "Weekday"),
    month_label = factor(month, levels = 1:12,
                         labels = c( "Jan", "Feb", "Mar", "Apr", 
                                     "May", "Jun", "Jul", "Aug",
                                     "Sept", "Oct", "Nov", "Dec"),
                         ordered = TRUE),
    season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
    is_weekend = as.factor(is_weekend),
    site = as.factor(site)
    )






##Examine the correlation between the various coefficients

library(corrplot)

cor_data<- 
  wide %>%
  select(`O3` = o3,
         `NO` = no,
         `NOx` = nox,
         `NO2` = no2,
         `PM10` = pm10,
         `PM2.5` = pm25,
         `Temp` = temp,
         `WindS` = ws,
         `Humidity` = humidity,
         `Rain` = rain,
         `Precipitation` = preci,
         `WindD` = wd) %>%
  drop_na()

#Calculate the matix
M <- cor(cor_data)

corrplot(M, 
         method = "circle", 
         type = "full", 
         tl.cex = 0.8,
         tl.col = "black", 
         tl.srt = 90,  
         addgrid.col = "grey90", 
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         title = "Correlation Matrix",
         mar = c(0,0,2,0))

#Delete irrelevant variables
wide <- wide %>% select(-rain, -preci)






##Dealing with missing data


# 1. Calculate the rate of missing data by year, month, and site
missing_data <- wide %>% 
  group_by(site, year, month_label) %>%
  summarise(miss_pct_no2 = sum(is.na(no2)) / n() * 100,
            miss_pct_pm25 = sum(is.na(pm25)) / n() * 100,
            .groups = "drop") %>%
  pivot_longer(cols = c(miss_pct_no2, miss_pct_pm25),
               names_to = "Pollutant", 
               values_to = "Missing_Pct") %>%
  mutate(Pollutant = ifelse(Pollutant == "miss_pct_no2", "NO2", "PM2.5"))

p_missing_bar <- 
  ggplot(missing_data, 
         aes(x = month_label, 
             y = Missing_Pct, 
             fill = Pollutant)) +
  geom_col(position = "dodge", width = 0.7) + 
  facet_grid(site ~ year) + 
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", size = 0.5) +
  scale_fill_manual(values = c("NO2" = "#1f78b4", "PM2.5" = "#ff7f00")) + 
  labs(title = "Monthly Missing Data Percentage by Site and Year",
       subtitle = "Red dashed line indicates 10% missing threshold",
       x = "Month",
       y = "Missing Percentage (%)",
       fill = "Pollutant") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
        legend.position = "top")

p_missing_bar


# 2. Imputing missing values by random forest regression

#Extract NO₂ data from different sites, merging with other variables
no2_wide <- 
  wide %>%
  select(date, site, no2) %>%
  pivot_wider(names_from = site, values_from = no2, names_prefix = "NO2_")

no2_wide %>% select(-date) %>% drop_na() %>% cor()

dev_aux <- wide %>%
  filter(site == "Devon") %>% 
  select(date, month, hour, o3, pm10, pm25, temp, ws, wd, humidity)

no2_impute_data <- left_join(no2_wide, dev_aux, by = "date")

#Train different models
no2_train_data <- no2_impute_data %>% 
  select(-date) %>% 
  drop_na()

#GLM
no2_glm_model <- glm(NO2_Devon ~ ., 
               data = no2_train_data)
print(summary(no2_glm_model))

#Random Forest
library(randomForest)
no2_rf_model <- randomForest(NO2_Devon ~ .,
                         data = no2_train_data,    
                         do.trace = 10)   # to know it's still computing
print(no2_rf_model)


#Imputing missing data
rows_to_predict <- is.na(no2_impute_data$NO2_Devon) &
  !is.na(no2_impute_data$NO2_Barn) & 
  !is.na(no2_impute_data$NO2_Tin) & 
  !is.na(no2_impute_data$o3) &
  !is.na(no2_impute_data$pm10) &
  !is.na(no2_impute_data$pm25) &
  !is.na(no2_impute_data$temp) &
  !is.na(no2_impute_data$ws) &
  !is.na(no2_impute_data$wd) &
  !is.na(no2_impute_data$humidity)
  
no2_impute_data$NO2_Devon_Imputed <- no2_impute_data$NO2_Devon
no2_impute_data$NO2_Devon_Imputed[rows_to_predict] <- 
  predict(no2_rf_model, newdata = no2_impute_data[rows_to_predict,])
  
no2_train_data$predicted <- predict(no2_rf_model, no2_train_data)

#Visualise the result
ggplot(no2_train_data, aes(x = NO2_Devon, y = predicted)) +
  geom_point(alpha = 0.3, color = "#1f78b4") +
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "dashed", size = 1) +
  labs(title = "Model Validation: Random Forest (R² = 90.8%)",
       x = "Observed NO2 (Devonshire)",
       y = "Predicted NO2 (Random Forest)") +
  theme_bw() +
  coord_fixed()

#Imput the data back to original file and label it
dev_filled_vector <- no2_impute_data %>%
  select(date, no2_fixed = NO2_Devon_Imputed) 

df_final <- wide %>%
  left_join(dev_filled_vector, by = "date") %>%
  mutate(
    flag_no2 = case_when(
      site == "Devon" & is.na(no2) & !is.na(no2_fixed) ~ "Imputed",
      TRUE ~ "Observed"),
    no2 = ifelse(site == "Devon", no2_fixed, no2)
    ) %>%
  select(-no2_fixed)

print(sum(is.na(df_final$no2[df_final$site == "Devon"]))) 


#Visualise the result
filter(df_final, site=='Devon') %>%
  ggplot(aes(x = date, y = no2)) +
  geom_line(color = "grey", alpha = 0.8) +
  geom_point(data = subset(df_final, flag_no2 == "Imputed" & site == 'Devon'), 
             aes(color = "Imputed"), size = 0.5, alpha = 0.3) +
  geom_point(data = subset(df_final, flag_no2 == "Observed" & site == 'Devon'), 
             aes(color = "Observed"), size = 0.5, alpha = 0.3) +
  scale_color_manual(values = c("Imputed" = "red", "Observed" = "steelblue")) +
  labs(title = "Imputation of Missing NO2 Data at Devonshire Green",
       color = "Data Type",
       x = "Year",
       y = "NO2 Concentration") +
  theme_minimal() +
  theme(legend.position = "top")



#Imput missing PM2.5
pm25_wide <- wide %>%
  select(date, month, hour, site, pm25) %>%
  pivot_wider(
    names_from = site,
    values_from = pm25,
    names_prefix = "PM25_"
  )
pm25_wide %>% select(-date) %>% drop_na() %>% cor()

barn_aux <- wide %>%
  filter(site == "Barn") %>%
  select(date, temp, ws, wd, humidity)

pm25_impute_data <- left_join(pm25_wide, barn_aux, by = "date")

pm25_train_data <- pm25_impute_data %>%
  select(-date) %>%
  na.omit() 

set.seed(123)
pm25_rf_model <- randomForest(PM25_Barn ~ ., 
                        data = pm25_train_data,
                        importance = TRUE,
                        do.trace = 10)

print(pm25_rf_model)
varImpPlot(pm25_rf_model)


rows_to_impute <- is.na(pm25_impute_data$PM25_Barn) & 
  !is.na(pm25_impute_data$PM25_Devon) & 
  !is.na(pm25_impute_data$PM25_Tin) 
  
pm25_impute_data$PM25_Barn_Imputed <- pm25_impute_data$PM25_Barn  
pm25_impute_data$PM25_Barn_Imputed[rows_to_impute] <- 
  predict(pm25_rf_model, newdata = pm25_impute_data[rows_to_impute,])

barn_fixed <- pm25_impute_data %>%
  select(date, pm25_fixed = PM25_Barn_Imputed)

df_final <- df_final %>%
  left_join(barn_fixed, by = "date") %>%
  mutate(
    flag_pm25 = case_when(
      site == "Barn" & is.na(pm25) & !is.na(pm25_fixed) ~ "Imputed",
      TRUE ~ "Observed"
      ),
    pm25 = ifelse(site == "Barn", pm25_fixed, pm25)
    ) %>%
  select(-pm25_fixed)

print(summary(df_final %>% filter(site == "Barn") %>% select(pm25)))




##The final data frame is ready to use
df <- df_final %>%
  mutate(weekend = ifelse(is_weekend == "Weekend", 1, 0)) 




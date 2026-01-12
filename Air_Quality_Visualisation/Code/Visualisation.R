library(ggplot2)
library(tidyverse)
library(viridis) #Color-blind friendly

##Figure 1 Line Chart

#Use the monthly average data
#Smooth out the daily random fluctuations, retaining only the major trend
avg_monthly <- 
  df %>% 
  filter(year != 2026) %>%
  mutate(month_date = floor_date(date, "month")) %>% #Align all dates to the 1st of each month
  group_by(site, month_date) %>%
  summarise(no2_mean = mean(no2, na.rm = T),
            pm25_mean = mean(pm25, na.rm = T),
            .groups = "drop") %>%
  pivot_longer(cols = c(no2_mean, pm25_mean),  #Convert to a long column to classify
               names_to = "Pollutant", 
               values_to = "Concentration") %>% 
  mutate(Pollutant = ifelse(Pollutant == "no2_mean", "NO2", "PM2.5"))%>%
  mutate(Site = case_when(site == "Barn" ~ "Barnsley Road",
                          site == "Tin" ~ "Tinsley", 
                          site == "Devon" ~ "Devonshire Green"))

A <- 
  ggplot(avg_monthly, aes(x = month_date, y = Concentration, color = Site, shape = Site)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_line(alpha = 0.3, size = 0.5) +
  geom_smooth(method = "lm", se = F, size = 0.8) + #Draw a smooth overall trend line
  facet_wrap(~Pollutant, scales = "free_y") + #The vertical axis scale is not fixed
  labs(title = "Three-Year Pollution Trends (Monthly Average) in Sheffield",
       subtitle = "Solid lines indicate the Linear Trend",
       x = "Date",
       y = expression(paste("Concentration (", mu, "g/", m^3, ")"))) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))+ #Bold heading
  scale_color_brewer(palette = "Set1")
A




##Figure 2 Bubble Chart

#Three-years' daily average
bubble_data <- 
  df %>% 
  group_by(month, day) %>%
  summarise(mean_temp = mean(temp, na.rm = T),
            mean_ws = mean(ws, na.rm = T),
            mean_humidity = mean(humidity, na.rm = T),
            mean_no2 = mean(no2, na.rm = T),
            mean_pm25 = mean(pm25, na.rm = T))

#Classified by quantity
st <- sort(bubble_data$mean_humidity) 
st[122]
st[122*2]
#Add humidity type
bubble_data$hm_group <-   
  cut(bubble_data$mean_humidity, 
      breaks = c(0, 76, 83, 100), 
      labels = c("Dry (<76%)", "Normal", "Wet (>83%)"))
B <- 
  ggplot(bubble_data, aes(x = mean_temp, y = mean_ws)) +
  geom_point(aes(size = mean_no2, color = mean_no2), alpha = 0.7) +
  scale_size(range = c(1, 8), name = expression(NO[2] ~ (mu * g / m^3))) +
  facet_wrap(~ hm_group) +
  scale_color_viridis_c(option = "magma", direction = -1, name = NULL) +
  scale_x_continuous(name = "Daily Mean Temperature (°C)") +
  scale_y_continuous(name = "Daily Mean Wind Speed (10m/s)") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(), #Remove the fine mesh
        plot.title = element_text(face = "bold")) +
  labs(title = "Meteorological Drivers of Pollution",
       subtitle = expression(paste("Relationship between Temperature, Wind Speed, Humidity and ",NO[2]," Levels")), 
       caption = "Larger and darker dots indicate higher pollution levels")
B





##Figure 3 Heatmap

#Group by day of the week and 24 hours
heatmap_data <- 
  df %>%
  group_by(day_of_week, hour) %>%
  summarise(mean_no2 = mean(no2, na.rm=T),
            mean_pm25 = mean(pm25, na.rm=T))

C <- 
  ggplot(heatmap_data, aes(x = hour, y = day_of_week, fill = mean_no2)) +
  geom_tile(color = NULL) +  #Ensure color blocks are continuous
  scale_fill_distiller(palette = "Spectral", direction = -1) +  #suitable for indicating pollutant concentrations 
  labs(title = "The Temporal Fingerprint: Pollution Heatmap",
       subtitle = "Heatmap reveals distinct commuter patterns (Mon-Fri) vs Leisure (Sat-Sun)",
       x = "Hour of Day", 
       y = "Day of Week", 
       fill = expression(NO[2]~(mu * g / m^3))) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

C




##Figure 4 Radar Chart

library(tidyverse)
library(cluster)    
library(gridExtra)

daily_before <- 
  df %>% 
  group_by(site, year, month, day) %>%
  summarise(no2 = mean(no2, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            ws = mean(ws, na.rm = T),
            wd = mean(wd, na.rm = T),
            humidity = mean(humidity, na.rm = T),
            .groups = "drop")

cluster_data <- 
  daily_before %>%
  select(no2, pm25, temp, ws, humidity) %>% #Columns containing values
  na.omit() #Ensure no missing values

#Scale the data
data_scaled <- scale(cluster_data)

set.seed(123)
p_elbow <- fviz_nbclust(data_scaled, kmeans, method = "wss")
print(p_elbow) #Turns out 3 clusters will be appropriate

set.seed(123)
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# Add cluster label
cluster_data$Cluster <- as.factor(km_res$cluster)



library(ggradar)
library(scales)
D <- 
  cluster_data %>%
  drop_na() %>%
  mutate_if(is.numeric, rescale) %>%
  group_by(Cluster) %>%
  summarise_if(is.numeric, mean) %>%
  mutate(Cluster = case_when(Cluster == 1 ~ "Windy & Clean",   #Change the name of each cluster
                             Cluster == 2 ~ "Warm & Moderate",
                             Cluster == 3 ~ "Cold & Polluted")) %>%
  rename("NO2" = no2,
         "PM2.5" = pm25,
         "Wind Speed" = ws,
         "Temperature" = temp,
         "Humidity" = humidity) %>%  #Change the name of each vector
  ggradar(axis.label.size = 4,
          grid.label.size = 4,
          group.colours = c("#2E9FDF", "#E7B800", "#FC4E07"),
          group.line.width = 1,
          group.point.size = 3,
          background.circle.colour = "white",
          gridline.mid.colour = "grey90",
          values.radar = c("0%", "40%", "80%"), #Coordinate axis range
          grid.min = 0, 
          grid.mid = 0.4, 
          grid.max = 0.8,
          legend.position = "bottom",
          legend.text.size = 10) +
  labs(title = "K-means Clustering: Pollution Type",
       subtitle = "Radar chart showing 3 clusters") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 10))
D




##Save these visualisations

ggsave("A.png", A, width = 9, height = 6, dpi = 600)
ggsave("B.png", B, width = 8, height = 5, dpi = 600)
ggsave("C.png", C, width = 8, height = 4, dpi = 600)
ggsave("D.png", D, width = 7, height = 6, dpi = 600)




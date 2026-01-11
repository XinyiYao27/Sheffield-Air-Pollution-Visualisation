library(ggplot2)
library(tidyverse)
library(viridis)

# 1. 数据准备：计算“月度平均值”
# 这一步可以平滑掉每天的随机波动，只保留大趋势
avg_monthly <- df %>% filter(year!=2026) %>%
  mutate(month_date = floor_date(date, "month")) %>% # 把日期对齐到每月1号
  group_by(site, month_date) %>%
  summarise(no2_mean = mean(no2, na.rm = TRUE),
            pm25_mean = mean(pm25, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = c(no2_mean, pm25_mean), 
               names_to = "Pollutant", 
               values_to = "Concentration") %>%
  mutate(Pollutant = ifelse(Pollutant == "no2_mean", "NO2", "PM2.5"))%>%
  mutate(Site = case_when(site == "Barn" ~ "Barnsley Road",
                          site == "Tin" ~ "Tinsley", 
                          site == "Devon" ~ "Devonshire Green"))



ggplot(avg_monthly, aes(x = month_date, y = Concentration, color = Site, shape = Site)) +
  # A. 画出月度波动的线 (较细，半透明)
  geom_point(alpha = 0.5, size = 1) +
  geom_line(alpha = 0.3, size = 0.5) +
  # B. 画出平滑的总体趋势线 (较粗)
  geom_smooth(method = "lm", se = F, size = 0.8) +
  facet_wrap(~Pollutant, scales = "free_y") +
  labs(
    title = "Three-Year Pollution Trends (Monthly Average)",
    subtitle = "Solid lines indicate the Linear Trend",
    x = "Date",
    y = expression(paste("Concentration (", mu, "g/", m^3, ")"))
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")















# 1. 数据预处理：计算日均值
# 原始的每小时数据点太多（8760个点），画散点图会重叠成一团黑
# 所以我们通常按"天"聚合，或者按"天+时段"聚合
bubble_data <- df %>% 
  group_by(month,day) %>%
  summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    mean_ws = mean(ws, na.rm = T),
    mean_humidity = mean(humidity, na.rm = TRUE),
    mean_no2 = mean(no2, na.rm = TRUE),
    mean_pm25 = mean(pm25, na.rm = TRUE)
  ) %>%
  drop_na()

bubble_data$hm_group <- 
  cut(bubble_data$mean_humidity, breaks = c(0, 76, 83, 100), 
      labels = c("Dry (<76%)", "Normal (<83%)", "Wet"))
# 2. 绘图
ggplot(bubble_data, aes(x = mean_temp, y = mean_ws)) +
  geom_point(aes(size = mean_no2, color = mean_no2), alpha = 0.7) +
  # B. 设置大小范围
  # range = c(1, 8): 控制最小点和最大点的尺寸差距
  scale_size(range = c(1, 8), name = expression(NO[2] ~ (mu * g / m^3))) +
  facet_wrap(~ hm_group)+
  # C. 设置颜色 (Viridis - Magma 色板，这种火红色调很适合表示"危险/热度")
  scale_color_viridis_c(option = "magma", direction = -1, name = NULL) +
  
  # D. 添加平滑趋势线 (可选)
  # 这条线能展示：随着风速增加，整体趋势是否下降？
  #geom_smooth(method = "loess", color = "black", se = FALSE, size = 0.5, linetype = "dashed") +
  
  # E. 设置坐标轴
  scale_x_continuous(name = "Daily Mean Temperature (°C)") +
  scale_y_continuous(name = "Daily Mean Wind Speed (10m/s)") +
  
  # F. 主题美化
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(), # 去掉细网格，让画面干净
    plot.title = element_text(face = "bold")
  ) +
  theme(legend.position = "top") +
  # G. 标题
  labs(
    title = "Meteorological Drivers of Pollution",
    subtitle = expression(paste("Relationship between Temperature, Wind Speed, Humidity and ",NO[2]," Levels")), 
    caption = "Larger and darker dots indicate higher pollution levels"
  )




















polar_data <- df %>% #filter(site!='Barn')%>%
  # 过滤掉缺失值
  filter(!is.na(wd) & !is.na(ws) & !is.na(no2) & !is.na(pm25)) %>%
  # 将风向圆整到最近的 10度 或 30度 (bin_width)
  # 将风速圆整到整数 (或者 0.5)
  mutate(
    wd_bin = round(wd / 10) * 10,  # 角度分箱
    ws_bin = round(ws, 0)          # 风速分箱
  ) %>%
  # 处理 360度 = 0度 的问题
  mutate(wd_bin = ifelse(wd_bin == 0, 360, wd_bin)) %>%
  # 按 分箱后的风向 和 风速 分组
  group_by(wd_bin, ws_bin) %>%
  # 计算每个格子里的平均浓度
  summarise(mean_no2 = mean(no2),
            mean_pm25 = mean(pm25),
            .groups = "drop")
polar_data$no2_group <- 
  cut(polar_data$mean_no2, breaks = c(0, 14.083, 20.204, 26.264, 78.7), 
      labels = c("Low", "Moderate", "High", "Extreme"))
polar_data$pm25_group <- 
  cut(polar_data$mean_pm25, breaks = c(0, 4.52583, 6.92771, 10.82016, 41), 
      labels = c("Low", "Moderate", "High", "Extreme"))


# 2. 绘图
ggplot(polar_data, aes(x = wd_bin, y = ws_bin, fill = no2_group, colour = no2_group)) +
  # 使用 geom_tile 画色块 (在直角坐标系下是矩形，极坐标下变成扇形块)
  geom_tile() + 
  
  # 关键步骤：转换为极坐标
  # start = 0 这里的设置取决于你的数据，通常0度在正北
  coord_polar(start = 0, theta = "x") +
  
  # 设置 X 轴 (风向)
  scale_x_continuous(
    breaks = c(5, 95, 185, 275), 
    labels = c("N", "E", "S", "W")
    #limits = c(0, 360) # 确保圆是完整的
  ) +
  
  # 设置 Y 轴 (风速)
  scale_y_continuous(
    name = "Wind Speed (10m/s)",
    # 1. 强制从 0 开始，NA 表示上限自动计算(或者你可以写具体数字比如 10)
    limits = c(-0.5, NA)
    
    # 2. 关键！这是去掉圆心白洞的核心
    # c(0, 0) 表示在轴的起点(圆心)和终点(最外圈)都不留白
    #expand = c(0, 0)
  ) +
  
  # 设置颜色 (使用色盲友好的 Viridis)
  scale_fill_manual(
    values = c("Low" = "#00E400", "Moderate" = "#FFFF00", "High" = "orange", "Extreme" = "red"),
    name = expression(NO[2]~Concentration)
    ) +
  scale_color_manual(
    values = c("Low" = "#00E400", "Moderate" = "#FFFF00", "High" = "orange", "Extreme" = "red"),
    name = expression(NO[2]~Concentration)
    ) +
  
  
  #option = "C" # "C" 是 plasma 色调，暖色代表高污染，视觉冲击力强) +
  
  # 美化主题
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), # 极坐标下不需要显示 X轴标题
    panel.grid.major = element_line(color = "grey90"), # 淡化网格线
    legend.position = "right",
    text = element_text(size = 12)
  ) +
  
  # 添加标题
  labs(
    title = expression(paste("Polar Heatmap: Sources of ", NO[2], " in Sheffield")),
    subtitle = expression(paste("Mean ", NO[2], " concentration by wind direction and speed")),
    caption = "Category depends on the quartile"
  )





























heatmap_data <- df %>%
  group_by(day_of_week, hour) %>%
  summarise(mean_no2 = mean(no2, na.rm=TRUE),
            mean_pm25 = mean(pm25, na.rm=TRUE))

ggplot(heatmap_data, aes(x=hour, y=day_of_week, fill=mean_no2)) +
  geom_tile(color=NULL) + 
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  labs(title="The Temporal Fingerprint: Pollution Heatmap",
       subtitle = "Heatmap reveals distinct commuter patterns (Mon-Fri) vs Leisure (Sat-Sun)",
       x="Hour of Day", y="Day of Week", 
       fill=expression(NO[2]~(mu * g / m^3))) +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,23,2))+
  theme(legend.position = "top")



















































library(tidyverse)
library(cluster)    # 聚类算法
library(factoextra) # 聚类可视化神器
library(gridExtra)

daily_before <- df %>% 
  group_by(site, year, month, day) %>%
  summarise(no2 = mean(no2, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            temp = mean(temp, na.rm = T),
            ws = mean(ws, na.rm = T),
            wd = mean(wd, na.rm = T),
            humidity = mean(humidity, na.rm = T),
            .groups = "drop")

# 1. 选取用于聚类的特征 (数值型)
# 我们以 Devonshire Green 站点为例，探索它的污染模式
cluster_data <- daily_before %>%
  select(no2, pm25, temp, ws, humidity) %>% # 选你有数值的列
  na.omit() # 再次确保没有空值

# 2. 标准化 (Scaling) - 核心步骤！
# scale() 会把每个变量变成 均值=0, 标准差=1
data_scaled <- scale(cluster_data)



# 使用 fviz_nbclust 自动画图
# 这一步会生成一张很专业的图，放在报告的 Methodology 里
set.seed(123)
p_elbow <- fviz_nbclust(data_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal K")

print(p_elbow)



# 1. 运行 K-Means (假设 K=3)
set.seed(123)
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# 2. 可视化聚类结果 (Cluster Plot)
# 这张图是报告里的颜值担当
p_cluster <- fviz_cluster(km_res, data = data_scaled,
                          palette = c("#2E9FDF", "#E7B800", "#FC4E07", "darkgreen","black"), # 配色
                          ggtheme = theme_minimal(),
                          main = "Cluster Analysis of Air Quality Modes"
)

print(p_cluster)




# 1. 将聚类标签 (Cluster Label) 加回原始数据
cluster_data$Cluster <- as.factor(km_res$cluster)
# 2. 计算每一类的均值，看看它们长什么样
profile <- cluster_data %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    NO2_Mean = mean(no2),
    PM25_Mean = mean(pm25),
    WS_Mean = mean(ws),
    Temp_Mean = mean(temp),
    Humidity_Mean = mean(humidity)
  )

print(profile)


library(ggradar)
library(scales)
cluster_data %>%
  drop_na()%>%
  mutate_if(is.numeric, rescale) %>%
  group_by(Cluster) %>%
  summarise_if(is.numeric, mean) %>%
  # 1. 改类别名 (不同线条的名字)
  # 逻辑：修改第一列的内容。这里假设第一列叫 site
  mutate(Cluster = case_when(
    Cluster == 1 ~ "Windy & Clean", # 把 site_1 改成 City Centre
    Cluster == 2 ~ "Warm & Moderate",
    Cluster == 3 ~ "Cold & Polluted"
    #TRUE ~ Cluster # 其他保持不
  )) %>%
  
  # 2. 改特征向量名 (雷达图顶点的名字)
  # 逻辑：修改列名 (rename(新名字 = 旧名字))
  rename(
    "NO2" = no2,
    "PM2.5" = pm25,
    "Wind Speed" = ws,
    "Temperature" = temp,
    "Humidity" = humidity
  ) %>%
  ggradar( 
    axis.label.size = 4,
    grid.label.size = 4,
    # --- A. 设置数据颜色 (关键) ---
    group.colours = c("#2E9FDF", "#E7B800", "#FC4E07"),
    # --- B. 设置填充和透明度 ---
    fill = F,       # 开启填充
    fill.alpha = 0.2,  # 设置半透明，这样重叠部分能看清
    
    # --- C. 设置线条粗细 ---
    group.line.width = 1,
    group.point.size = 3,
    
    # --- D. 设置背景网格颜色 (让图更干净) ---
    background.circle.colour = "white",
    gridline.mid.colour = "grey90",
    
    # --- E. 坐标轴范围 (重要) ---
    values.radar = c("0%", "40%", "80%"), # 圈圈上的文字
    grid.min = 0, 
    grid.mid = 0.4, 
    grid.max = 0.8,
    legend.position = "bottom",
    legend.text.size = 10
    ) +
  # --- 3. 添加标题和说明 ---
  labs(
    title = "K-means Clustering pollution type",
    subtitle = "Radar chart showing 3 clusters"
    ) +
  
  # --- 4. 调整标题位置 (可选，因为默认标题可能靠左) ---
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15), # 居中 + 加粗
    plot.subtitle = element_text(hjust = 0.5, color = "grey50", size = 10)       # 居中 + 灰色
    )

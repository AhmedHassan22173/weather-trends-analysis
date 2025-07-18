#First 6 rows 
head(weather)

# information about columns 
glimpse(weather)

# وsummary
summary(weather)

# adding column
weather <- weather %>%
  mutate(Year = year(Date),
         Month = month(Date, label = TRUE))

# Calculating average
monthly_temp <- weather %>%
  group_by(Year, Month) %>%
  summarise(Avg_Temp = mean(Temperature_C), .groups = "drop")

#first rows
head(monthly_temp)

# Plot monthly temperature trend
ggplot(monthly_temp, aes(x = interaction(Year, Month, sep = "-"), y = Avg_Temp, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Monthly Average Temperature",
       x = "Year-Month",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# average rainfall
monthly_rain <- weather %>%
  group_by(Year, Month) %>%
  summarise(Avg_Rainfall = mean(Rainfall_mm), .groups = "drop")

# Plot average monthly rainfall
ggplot(monthly_rain, aes(x = interaction(Year, Month, sep = "-"), y = Avg_Rainfall, group = 1)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Monthly Average Rainfall",
       x = "Year-Month",
       y = "Rainfall (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot histogram of daily rainfall
ggplot(weather, aes(x = Rainfall_mm)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Daily Rainfall",
       x = "Rainfall (mm)",
       y = "Frequency") +
  theme_minimal()

#average humidity
monthly_humidity <- weather %>%
  group_by(Year, Month) %>%
  summarise(Avg_Humidity = mean(Humidity_percent), .groups = "drop")

# Plot humidity trend
ggplot(monthly_humidity, aes(x = interaction(Year, Month, sep = "-"), y = Avg_Humidity, group = 1)) +
  geom_line(color = "purple", size = 1) +
  labs(title = "Monthly Average Humidity",
       x = "Year-Month",
       y = "Humidity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Histogram of humidity distribution
ggplot(weather, aes(x = Humidity_percent)) +
  geom_histogram(binwidth = 5, fill = "lightpink", color = "black") +
  labs(title = "Distribution of Daily Humidity",
       x = "Humidity (%)",
       y = "Frequency") +
  theme_minimal()


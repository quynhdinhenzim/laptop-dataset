### IMPORT PACKAGE
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

### IMPORT DATA
setwd("~/Downloads")
laptop_dataset <- read.csv("laptops.csv")
laptop_dataset
str(laptop_dataset)
head(laptop_dataset)

### DATA CLEANING AND MANIPULATION

# Are there any missing values in the dataset? How many and where are they located?
missing_values <- is.na(laptop_dataset)
total_missing <- sum(missing_values)
total_missing 

# Find the row and column indices of missing values
missing_row <- which(missing_values, arr.ind = TRUE)[, 1]
missing_col <- which(missing_values, arr.ind = TRUE)[, 2]
laptop_dataset <- na.omit(laptop_dataset)

# Check for duplicate rows in the dataset
duplicates <- duplicated(laptop_dataset)
sum(duplicates) 

### DATA EXPLORATION

# Which laptop brands are present in the dataset, and how many laptops belong to each brand? 
laptop_brands <- laptop_dataset %>%
  group_by(Brand) %>%
  count(sort = TRUE)

ggplot(laptop_brands, aes(x = Brand, y = n, fill = Brand)) +
  geom_col() +
  geom_text(data = subset(laptop_brands, Brand %in% c("Asus", "HP", "Lenovo", "MSI")), 
            aes(label = n), vjust = -0.5) +
  labs(title = "Distribution of Laptops by Brand", y = "Number of laptops") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# What is the average, minimum, and maximum values of the "RAM" and "Storage" variables?
laptop_dataset |>
  summarise(avg_RAM = mean(RAM),
            min_RAM = min(RAM),
            max_RAM = max(RAM),
            avg_Storage = mean (Storage),
            min_Storage = min(Storage),
            max_Storage = max(Storage))

laptop_dataset[which.min(laptop_dataset$Storage),] #which row has the smallest storage value

# What is the distribution of laptops based on the CPU?
laptop_CPU <- laptop_dataset %>%
  group_by(CPU) %>%
  count(sort = TRUE)

ggplot(laptop_CPU, aes(CPU, n, fill = CPU)) +
  geom_col() +
  geom_text(data = subset(laptop_CPU, 
                          CPU == c("Intel Core i7","Intel Core i5")), 
            aes(label = n), vjust = -0.5) +
  labs(title = "Distribution of Laptops by CPU", y = "Number of laptops") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# What is the overall distribution of laptops categorized as "New" vs. other statuses (e.g., refurbished, used, etc.)? Does the status impact the laptop price significantly?
distribution_by_status <- laptop_dataset |> count(Status)
ggplot(distribution_by_status, aes(Status, n)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Distribution of Laptops by Status", 
       y = "Number of laptops") +
  theme_bw()

status_price_summary <- laptop_dataset %>%
  group_by(Status) %>%
  summarize(avg_price = mean(Final.Price))

print(status_price_summary)

# How does the presence of a touchscreen (Touch = "Yes") affect the price of laptops? Are touch-enabled laptops generally more expensive?
touch_laptops <- laptop_dataset[laptop_dataset$Touch == "Yes", ]
avg_price_touch_laptops <- mean(touch_laptops$Final.Price)
avg_price_touch_laptops

non_touch_laptops <- laptop_dataset[laptop_dataset$Touch == "No", ]
avg_price_non_touch_laptops <- mean(non_touch_laptops$Final.Price)
avg_price_non_touch_laptops

cat("Average price of touch-enabled laptops:", avg_price_touch_laptops, "\n")
cat("Average price of non-touch laptops:", avg_price_non_touch_laptops, "\n")

# How does the average price vary across different laptop brands and models? Are there any notable trends or patterns?
avg_price_brand <- laptop_dataset %>%
  group_by(Brand) %>%
  summarise(avg_price = mean(Final.Price))

ggplot(avg_price_brand, aes(Brand, avg_price, fill = Brand)) +
  geom_col(color = "black") +
  labs(title = "Average Laptop Prices by brand", 
       x = "Brand", y = "Average price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Is there a correlation between the amount of RAM and the storage capacity in laptops? How does this relationship impact laptop pricing?
correlation <- cor(laptop_dataset$RAM, laptop_dataset$Storage)
correlation # the correlation is 0.7512965 --> a high positive correlation

ggplot(laptop_dataset, aes(RAM, Storage, color = Final.Price)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "lm") +
  labs(title = "RAM vs. Storage Capacity in Laptops",
       x = "RAM (GB)", y = "Storage Capacity (GB)") +
  theme_minimal()

# How does the screen size of laptops affect their pricing? Are larger screens associated with higher prices?
laptop_price_screen <- laptop_dataset %>%
  group_by(Screen) %>%
  summarize(avg_laptop_price_screen = mean(Final.Price))

ggplot(laptop_price_screen, aes(Screen, avg_laptop_price_screen)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Average laptop prices by screen size", 
       x = "Screen size", y = "Average laptop prices") +
  theme_minimal()

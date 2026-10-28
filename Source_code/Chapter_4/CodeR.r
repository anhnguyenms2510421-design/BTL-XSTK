clean_data <- read.csv("clean_data_full.csv")

describe_function <- function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE))
}
num_vars <- c("delivery_charges", "coupon_discount", "order_total", "distance_to_nearest_warehouse")
des_table <- apply(clean_data[, num_vars], 2, describe_function)
print(des_table)

freq_season <- table(clean_data$season)
freq_expedited <- table(clean_data$is_expedited_delivery)
freq_warehouse <- table(clean_data$nearest_warehouse)
freq_happy <- table(clean_data$is_happy_customer)
print(freq_season)
print(freq_expedited)
print(freq_warehouse)
print(freq_happy)

hist(clean_data$delivery_charges, main="Histogram of Delivery Charges", col="lightblue", xlab="Delivery Charges (USD)")

barplot(freq_warehouse, main="Orders by Warehouse", col="lightgreen")

boxplot(delivery_charges ~ season, data=clean_data, main="Delivery Charges by Season", col=c("pink","lightblue","lightgreen","yellow"), ylab="Delivery Charges")

boxplot(delivery_charges ~ is_expedited_delivery, data=clean_data, main="Delivery Charges by Expedited Status", col=c("pink","lightblue"), ylab="Delivery Charges")

library(corrplot)
cor_vars <- c("delivery_charges", "customer_lat", "customer_long", "distance_to_nearest_warehouse")
cor_matrix <- cor(clean_data[, cor_vars])
corrplot(cor_matrix, method="number")

plot(clean_data$distance_to_nearest_warehouse, clean_data$delivery_charges,
     main = "Scatter Plot: Distance vs Delivery Charges",
     xlab = "Distance to Nearest Warehouse ",
     ylab = "Delivery Charges ",
     col = "darkred", pch = 16)
plot(clean_data$customer_lat, clean_data$delivery_charges,
     main = "Scatter Plot: Customer lat vs Delivery Charges",
     xlab = "Customer lat ",
     ylab = "Delivery Charges ",
     col = "darkred", pch = 16)
plot(clean_data$customer_long, clean_data$delivery_charges,
     main = "Scatter Plot: customer long vs Delivery Charges",
     xlab = "Customer long ",
     ylab = "Delivery Charges ",
     col = "darkred", pch = 16)
qqnorm(clean_data$delivery_charges, 
       main = "Normal Q-Q Plot of Delivery Charges", 
       col = "black")
qqline(clean_data$delivery_charges, col = "red", lwd = 2)
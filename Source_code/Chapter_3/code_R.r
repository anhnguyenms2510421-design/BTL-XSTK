
library(readr)
dirty_data <- read_csv("dirty_data.csv", show_col_types = FALSE)
cat("Rows:", nrow(dirty_data), "Columns:", ncol(dirty_data), "\n")
print(head(dirty_data))
str(dirty_data)


selected_vars <- c("nearest_warehouse", "delivery_charges", "coupon_discount", "order_total", "season", "is_expedited_delivery", "distance_to_nearest_warehouse", "is_happy_customer", "customer_lat", "customer_long", "order_price")
new_data <- dirty_data[, selected_vars]
cat("Rows:", nrow(new_data), "Columns:", ncol(new_data), "\n")
print(names(new_data))


missing_before <- apply(is.na(new_data), 2, sum)
empty_before <- sapply(new_data, function(x) if (is.character(x)) sum(trimws(x) == "", na.rm = TRUE) else 0)
print(missing_before)
print(empty_before)
for (col in names(new_data)) {
  if (is.character(new_data[[col]])) {
    empty_index <- which(trimws(new_data[[col]]) == "")
    new_data[[col]][empty_index] <- NA
  }
}
rows_before_missing <- nrow(new_data)
new_data <- na.omit(new_data)
cat("Rows removed:", rows_before_missing - nrow(new_data), "\n")


print(unique(new_data$nearest_warehouse))
print(unique(new_data$season))
warehouse_map <- c(thompson = "Thompson", nickolson = "Nickolson", bakers = "Bakers")
warehouse_key <- tolower(trimws(new_data$nearest_warehouse))
new_data$nearest_warehouse <- unname(warehouse_map[warehouse_key])
season_map <- c(spring = "Spring", summer = "Summer", autumn = "Autumn", winter = "Winter")
season_key <- tolower(trimws(new_data$season))
new_data$season <- unname(season_map[season_key])
print(table(new_data$nearest_warehouse))
print(table(new_data$season))


new_data$calculated_total <- new_data$order_price * (1 - new_data$coupon_discount / 100) + new_data$delivery_charges
mismatch_before <- sum(abs(new_data$order_total - new_data$calculated_total) > 0.01)
cat("Rows outside tolerance before correction:", mismatch_before, "\n")
new_data$order_total <- new_data$calculated_total
mismatch_after <- sum(abs(new_data$order_total - new_data$calculated_total) > 0.01)
cat("Rows outside tolerance after correction:", mismatch_after, "\n")


new_data$calculated_total <- NULL
write_csv(new_data, "clean_data.csv")


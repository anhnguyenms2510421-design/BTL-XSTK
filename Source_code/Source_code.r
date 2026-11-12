library(readr)
library(car)
library(lmtest)
library(corrplot)

dirty_data <- read_csv("dirty_data.csv", show_col_types = FALSE)
cat("Rows:", nrow(dirty_data), "Columns:", ncol(dirty_data), "\n")
print(head(dirty_data))
str(dirty_data)

selected_vars <- c("nearest_warehouse", "delivery_charges", "coupon_discount", "order_total",
                   "season", "is_expedited_delivery", "distance_to_nearest_warehouse",
                   "is_happy_customer", "customer_lat", "customer_long", "order_price")
new_data <- dirty_data[, selected_vars]
cat("Rows:", nrow(new_data), "Columns:", ncol(new_data), "\n")
print(names(new_data))

missing_before <- apply(is.na(new_data), 2, sum)
empty_before <- sapply(new_data, function(x)
  if (is.character(x)) sum(trimws(x) == "", na.rm = TRUE) else 0)
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

new_data$calculated_total <- new_data$order_price * (1 - new_data$coupon_discount / 100) +
  new_data$delivery_charges
mismatch_before <- sum(abs(new_data$order_total - new_data$calculated_total) > 0.01)
cat("Rows outside tolerance before correction:", mismatch_before, "\n")
new_data$order_total <- new_data$calculated_total
mismatch_after <- sum(abs(new_data$order_total - new_data$calculated_total) > 0.01)
cat("Rows outside tolerance after correction:", mismatch_after, "\n")

new_data$calculated_total <- NULL
write_csv(new_data, "clean_data_full.csv")

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

hist(clean_data$delivery_charges, main="Histogram of Delivery Charges",
     col="lightblue", xlab="Delivery Charges (USD)")

barplot(freq_warehouse, main="Orders by Warehouse", col="lightgreen")

boxplot(delivery_charges ~ season, data=clean_data, main="Delivery Charges by Season",
        col=c("pink","lightblue","lightgreen","yellow"), ylab="Delivery Charges")

boxplot(delivery_charges ~ is_expedited_delivery, data=clean_data,
        main="Delivery Charges by Expedited Status",
        col=c("pink","lightblue"), ylab="Delivery Charges")

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

dat <- read_csv("clean_data_full.csv", show_col_types = FALSE)
dat$season                <- factor(dat$season)             
dat$nearest_warehouse     <- factor(dat$nearest_warehouse)  
dat$is_expedited_delivery <- factor(dat$is_expedited_delivery, levels = c(FALSE, TRUE))
dat$is_happy_customer     <- factor(dat$is_happy_customer,     levels = c(FALSE, TRUE))
n <- nrow(dat); alpha <- 0.05
cat("n =", n, "\n")
print(summary(dat$order_total))   

cat("\n=== 1.1 CI 95% cho trung binh phi giao hang ===\n")
x <- dat$delivery_charges
me_z <- qnorm(0.975) * sd(x) / sqrt(n)       
cat("mean =", mean(x), " sd =", sd(x), " sai so =", me_z, "\n")
cat("CI (z):", mean(x) - me_z, "-", mean(x) + me_z, "\n")

cat("\n=== 1.2 CI 95% cho ty le (cong thuc (10)) ===\n")
for (v in c("is_expedited_delivery", "is_happy_customer")) {
  k <- sum(dat[[v]] == TRUE); p <- k / n
  e <- qnorm(0.975) * sqrt(p * (1 - p) / n)
  cat(v, ": p =", round(p, 4), " CI =", round(p - e, 4), "-", round(p + e, 4),
      " (np =", k, ", n(1-p) =", n - k, ")\n")
}

cat("\n=== 2.1 Phi giao nhanh > giao thuong (z-test, cong thuc (11)) ===\n")
g1 <- dat$delivery_charges[dat$is_expedited_delivery == TRUE]
g0 <- dat$delivery_charges[dat$is_expedited_delivery == FALSE]
cat("mean:", mean(g1), mean(g0), " diff =", mean(g1) - mean(g0),
    " sd:", sd(g1), sd(g0), "\n")
z0 <- (mean(g1) - mean(g0)) / sqrt(var(g1)/length(g1) + var(g0)/length(g0))
cat("z0 =", z0, " p =", pnorm(z0, lower.tail = FALSE), "\n")

cat("\n=== 2.2 So sanh hai ty le theo ma giam gia (cong thuc (12)) ===\n")
dat$has_coupon <- dat$coupon_discount > 0
for (v in c("is_happy_customer", "is_expedited_delivery")) {
  tab <- table(dat$has_coupon, dat[[v]] == TRUE)
  cat("\n", v, "\n"); print(tab)
  print(prop.test(c(tab["TRUE", "TRUE"], tab["FALSE", "TRUE"]),
                  c(sum(tab["TRUE", ]), sum(tab["FALSE", ])), correct = FALSE))
}

cat("\n=== 2.3 Tuong quan Pearson (cong thuc (7)-(8)) ===\n")
for (v in c("distance_to_nearest_warehouse", "order_total", "coupon_discount")) {
  ct <- cor.test(dat[[v]], dat$delivery_charges)
  cat(v, ": Pearson r =", round(ct$estimate, 4), " p =", round(ct$p.value, 4), "\n")
}
cat("So don co khoang cach > 10 km:", sum(dat$distance_to_nearest_warehouse > 10), "\n")

cat("\n=== 3. ANOVA season x nearest_warehouse ===\n")
m_aov <- aov(delivery_charges ~ season * nearest_warehouse, data = dat)
print(table(dat$season, dat$nearest_warehouse))        
res <- resid(m_aov)
print(shapiro.test(res))                               
png("qqplot_anova.png"); qqnorm(res, main = "Q-Q Plot of ANOVA Residuals")
qqline(res, col = "red"); dev.off()
print(leveneTest(delivery_charges ~ season * nearest_warehouse, data = dat))
cat("Gia dinh doc lap: moi dong la mot don hang rieng, khong lap lai tren cung doi tuong.\n")
print(summary(m_aov))

m_add <- aov(delivery_charges ~ season + nearest_warehouse, data = dat)
print(summary(m_add))
print(TukeyHSD(m_add, "season"))                       

png("interaction_plot.png")
interaction.plot(dat$season, dat$nearest_warehouse, dat$delivery_charges, fun = mean,
                 col = c("blue", "red", "green"), lty = 1, lwd = 2,
                 xlab = "Season", ylab = "Mean Delivery Charges", trace.label = "Warehouse")
dev.off()

cat("\n=== 4. Hoi quy: mo hinh day du (7 bien) ===\n")
lm_full <- lm(delivery_charges ~ distance_to_nearest_warehouse + order_total + coupon_discount +
                season + nearest_warehouse + is_expedited_delivery + is_happy_customer, data = dat)
print(summary(lm_full))
lm_opt <- step(lm_full, direction = "both", trace = 0)   
print(summary(lm_opt))
cat("Cong thuc:", deparse(formula(lm_opt)), "\n")
cat("AIC day du =", AIC(lm_full), " AIC toi uu =", AIC(lm_opt), "\n")

cat("\n--- Gia dinh tren mo hinh cuoi ---\n")
print(vif(lm_opt))                                   
print(shapiro.test(resid(lm_opt)))
print(bptest(lm_opt))                                
cd <- cooks.distance(lm_opt)
cat("Cook's D > 4/n:", sum(cd > 4 / n), " | max =", max(cd), " | dong:", which.max(cd), "\n")
png("lm_diagnostics.png", width = 800, height = 600)
par(mfrow = c(2, 2)); plot(lm_opt); par(mfrow = c(1, 1)); dev.off()

lm_nohappy <- step(update(lm_full, . ~ . - is_happy_customer), direction = "both", trace = 0)
print(summary(lm_nohappy))

cat("\n=== 5. Train/Test ===\n")
evaluate <- function(fml, seed = 42) {
  set.seed(seed)
  idx   <- sample(seq_len(n), size = floor(0.8 * n))
  train <- dat[idx, ]; test <- dat[-idx, ]
  fit   <- lm(fml, data = train)
  y <- test$delivery_charges; yhat <- predict(fit, test)
  c(MSE = mean((y - yhat)^2), RMSE = sqrt(mean((y - yhat)^2)), MAE = mean(abs(y - yhat)),
    R2_test = 1 - sum((y - yhat)^2) / sum((y - mean(y))^2))
}
res_tab <- rbind(`Mo hinh cuoi`   = evaluate(formula(lm_opt)),
                 `Khong co happy` = evaluate(formula(lm_nohappy)))
print(round(res_tab, 3))
cat("sd(delivery_charges) =", sd(dat$delivery_charges), "\n")
for (nm in c("Mo hinh cuoi", "Khong co happy")) {
  f <- switch(nm, `Mo hinh cuoi` = formula(lm_opt), `Khong co happy` = formula(lm_nohappy))
  rr <- sapply(1:200, function(s) evaluate(f, seed = s))
  cat("\n", nm, "- trung binh / do lech chuan cua 200 lan chia\n")
  print(round(cbind(mean = rowMeans(rr), sd = apply(rr, 1, sd)), 3))
}
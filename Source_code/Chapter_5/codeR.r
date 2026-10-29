# =====================================================================
# BÀI TẬP LỚN XÁC SUẤT THỐNG KÊ - CHƯƠNG 1: THỐNG KÊ SUY DIỄN (bản sửa)
# Chạy: đặt file cùng thư mục với clean_data_full.csv (500 đơn, chưa loại ngoại lai order_total)
# Package: readr, car, lmtest
# =====================================================================
library(readr)
library(car)     # leveneTest, vif
library(lmtest)  # bptest (Breusch-Pagan) - lý thuyết mục 2.5.5 yêu cầu

dat <- read_csv("clean_data_full.csv", show_col_types = FALSE)
dat$season                <- factor(dat$season)             # mức cơ sở: Autumn
dat$nearest_warehouse     <- factor(dat$nearest_warehouse)  # mức cơ sở: Bakers
dat$is_expedited_delivery <- factor(dat$is_expedited_delivery, levels = c(FALSE, TRUE))
dat$is_happy_customer     <- factor(dat$is_happy_customer,     levels = c(FALSE, TRUE))
n <- nrow(dat); alpha <- 0.05
cat("n =", n, "\n")
print(summary(dat$order_total))   # lệch phải rất mạnh (max ~853 000, trung vị ~11 300)

# =====================================================================
# 1. ƯỚC LƯỢNG KHOẢNG TIN CẬY
# =====================================================================
cat("\n=== 1.1 CI 95% cho trung bình phí giao hàng ===\n")
x <- dat$delivery_charges
me_z <- qnorm(0.975) * sd(x) / sqrt(n)       # công thức (9) trong lý thuyết
cat("mean =", mean(x), " sd =", sd(x), " sai so =", me_z, "\n")
cat("CI (z):", mean(x) - me_z, "-", mean(x) + me_z, "\n")

cat("\n=== 1.2 CI 95% cho tỷ lệ (công thức (10)) ===\n")
for (v in c("is_expedited_delivery", "is_happy_customer")) {
  k <- sum(dat[[v]] == TRUE); p <- k / n
  e <- qnorm(0.975) * sqrt(p * (1 - p) / n)
  cat(v, ": p =", round(p, 4), " CI =", round(p - e, 4), "-", round(p + e, 4),
      " (np =", k, ", n(1-p) =", n - k, ")\n")
}

# =====================================================================
# 2. KIỂM ĐỊNH HAI MẪU
# =====================================================================
cat("\n=== 2.1 Phí giao nhanh > giao thường (z-test, công thức (11)) ===\n")
g1 <- dat$delivery_charges[dat$is_expedited_delivery == TRUE]
g0 <- dat$delivery_charges[dat$is_expedited_delivery == FALSE]
cat("mean:", mean(g1), mean(g0), " diff =", mean(g1) - mean(g0),
    " sd:", sd(g1), sd(g0), "\n")
z0 <- (mean(g1) - mean(g0)) / sqrt(var(g1)/length(g1) + var(g0)/length(g0))
cat("z0 =", z0, " p =", pnorm(z0, lower.tail = FALSE), "\n")

cat("\n=== 2.2 So sánh hai tỷ lệ theo mã giảm giá (công thức (12)) ===\n")
dat$has_coupon <- dat$coupon_discount > 0
for (v in c("is_happy_customer", "is_expedited_delivery")) {
  tab <- table(dat$has_coupon, dat[[v]] == TRUE)
  cat("\n", v, "\n"); print(tab)
  print(prop.test(c(tab["TRUE", "TRUE"], tab["FALSE", "TRUE"]),
                  c(sum(tab["TRUE", ]), sum(tab["FALSE", ])), correct = FALSE))
}

cat("\n=== 2.3 Tương quan Pearson (công thức (7)-(8)) ===\n")
for (v in c("distance_to_nearest_warehouse", "order_total", "coupon_discount")) {
  ct <- cor.test(dat[[v]], dat$delivery_charges)
  cat(v, ": Pearson r =", round(ct$estimate, 4), " p =", round(ct$p.value, 4), "\n")
}
cat("So don co khoang cach > 10 km:", sum(dat$distance_to_nearest_warehouse > 10), "\n")

# =====================================================================
# 3. ANOVA 2 NHÂN TỐ: season x nearest_warehouse
# =====================================================================
cat("\n=== 3. ANOVA season x nearest_warehouse ===\n")
m_aov <- aov(delivery_charges ~ season * nearest_warehouse, data = dat)
print(table(dat$season, dat$nearest_warehouse))        # thiết kế KHÔNG cân bằng
res <- resid(m_aov)
print(shapiro.test(res))                               # n < 5000 nên dùng toàn bộ
png("qqplot_anova.png"); qqnorm(res, main = "Q-Q Plot of ANOVA Residuals"); qqline(res, col = "red"); dev.off()
print(leveneTest(delivery_charges ~ season * nearest_warehouse, data = dat))
cat("Giả định độc lập: mỗi dòng là một đơn hàng riêng, không lặp lại trên cùng đối tượng.\n")
print(summary(m_aov))

# Tương tác không ý nghĩa -> mô hình cộng tính (đúng hướng dẫn mục 2.4.3)
m_add <- aov(delivery_charges ~ season + nearest_warehouse, data = dat)
print(summary(m_add))
print(TukeyHSD(m_add, "season"))                       # chỉ so sánh nhân tố có ý nghĩa

png("interaction_plot.png")
interaction.plot(dat$season, dat$nearest_warehouse, dat$delivery_charges, fun = mean,
                 col = c("blue", "red", "green"), lty = 1, lwd = 2,
                 xlab = "Season", ylab = "Mean Delivery Charges", trace.label = "Warehouse")
dev.off()

# =====================================================================
# 4. HỒI QUY TUYẾN TÍNH BỘI
# =====================================================================
cat("\n=== 4. Hồi quy: mô hình đầy đủ (7 biến) ===\n")
lm_full <- lm(delivery_charges ~ distance_to_nearest_warehouse + order_total + coupon_discount +
                season + nearest_warehouse + is_expedited_delivery + is_happy_customer, data = dat)
print(summary(lm_full))
lm_opt <- step(lm_full, direction = "both", trace = 0)   # MÔ HÌNH CUỐI
print(summary(lm_opt))
cat("Công thức:", deparse(formula(lm_opt)), "\n")
cat("AIC đầy đủ =", AIC(lm_full), " AIC tối ưu =", AIC(lm_opt), "\n")

# --- TẤT CẢ kiểm tra giả định phải chạy trên lm_opt (mô hình cuối) ---
cat("\n--- Giả định trên mô hình cuối ---\n")
print(vif(lm_opt))                                   # GVIF^(1/(2Df)) cho biến phân loại
print(shapiro.test(resid(lm_opt)))
print(bptest(lm_opt))                                # Breusch-Pagan (mục 2.5.5)
cd <- cooks.distance(lm_opt)
cat("Cook's D > 4/n:", sum(cd > 4 / n), " | max =", max(cd), " | dòng:", which.max(cd), "\n")
png("lm_diagnostics.png", width = 800, height = 600)
par(mfrow = c(2, 2)); plot(lm_opt); par(mfrow = c(1, 1)); dev.off()

# --- Mô hình KHÔNG dùng is_happy_customer (biến chỉ biết sau khi giao hàng) ---
lm_nohappy <- step(update(lm_full, . ~ . - is_happy_customer), direction = "both", trace = 0)
print(summary(lm_nohappy))

# =====================================================================
# 5. TRAIN / TEST 80/20  (RMSE, MAE, MSE và R^2 test - mục 2.5.6)
# =====================================================================
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
# Kiểm tra ổn định: lặp 200 lần chia ngẫu nhiên khác nhau
for (nm in c("Mo hinh cuoi", "Khong co happy")) {
  f <- switch(nm, `Mo hinh cuoi` = formula(lm_opt), `Khong co happy` = formula(lm_nohappy))
  rr <- sapply(1:200, function(s) evaluate(f, seed = s))
  cat("\n", nm, "- trung binh / do lech chuan cua 200 lan chia\n")
  print(round(cbind(mean = rowMeans(rr), sd = apply(rr, 1, sd)), 3))
}
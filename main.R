# 安裝並載入必要的套件
library(tseries)
library(forecast)
library(MASS)
library(dplyr)
library(car)
library(lmtest)
library(sandwich)

# 讀取資料
data <- read.csv('C:/Users/Yvonne/Downloads/NBA_Data.csv')

#####################################
# 使用ABF法對應變數salary做單根檢定 #
#####################################

# 獲取所有球員名稱
unique_players <- unique(data$player)
adf_test_results <- list()

for (player_name in unique_players) {
  # 提取每個球員的資料
  player_data <- subset(data, player == player_name)
  
  # 提取球員的薪資數據
  player_salaries <- player_data$salary
  
  # 檢查資料點數量
  if (length(player_salaries) > 1) {
    # 使用 tryCatch 捕獲可能的錯誤
    adf_test_result <- tryCatch({
      adf.test(player_salaries, alternative = "stationary")
    }, error = function(e) {
      return(NA)  # 如果發生錯誤，返回 NA
    })
    
    adf_test_results[[player_name]] <- adf_test_result
  } else {
    adf_test_results[[player_name]] <- "Not enough data points for ADF test"
  }
}

# 顯示所有球員的ADF檢定結果
cat("\nADF檢定結果:\n")
for (player_name in names(adf_test_results)) {
  cat("\nPlayer:", player_name, "\n")
  print(adf_test_results[[player_name]])
}

#######################################################
# 建立含有5期落後項的模型，並透過逐步回歸選擇最佳模型 #
#######################################################

# 創建滯後期數
data <- data %>%
  arrange(player, year) %>%
  group_by(player) %>%
  mutate(
    GP_lag1 = lag(GP, 1),
    GP_lag2 = lag(GP, 2),
    GP_lag3 = lag(GP, 3),
    GP_lag4 = lag(GP, 4),
    GP_lag5 = lag(GP, 5),
    MIN_lag1 = lag(MIN, 1),
    MIN_lag2 = lag(MIN, 2),
    MIN_lag3 = lag(MIN, 3),
    MIN_lag4 = lag(MIN, 4),
    MIN_lag5 = lag(MIN, 5),
    PTS_lag1 = lag(PTS, 1),
    PTS_lag2 = lag(PTS, 2),
    PTS_lag3 = lag(PTS, 3),
    PTS_lag4 = lag(PTS, 4),
    PTS_lag5 = lag(PTS, 5),
    FGA_lag1 = lag(FGA, 1),
    FGA_lag2 = lag(FGA, 2),
    FGA_lag3 = lag(FGA, 3),
    FGA_lag4 = lag(FGA, 4),
    FGA_lag5 = lag(FGA, 5),
    FG_lag1 = lag(FG., 1),
    FG_lag2 = lag(FG., 2),
    FG_lag3 = lag(FG., 3),
    FG_lag4 = lag(FG., 4),
    FG_lag5 = lag(FG., 5),
    X3PA_lag1 = lag(X3PA, 1),
    X3PA_lag2 = lag(X3PA, 2),
    X3PA_lag3 = lag(X3PA, 3),
    X3PA_lag4 = lag(X3PA, 4),
    X3PA_lag5 = lag(X3PA, 5),
    X3P_lag1 = lag(X3P., 1),
    X3P_lag2 = lag(X3P., 2),
    X3P_lag3 = lag(X3P., 3),
    X3P_lag4 = lag(X3P., 4),
    X3P_lag5 = lag(X3P., 5),
    FTA_lag1 = lag(FTA, 1),
    FTA_lag2 = lag(FTA, 2),
    FTA_lag3 = lag(FTA, 3),
    FTA_lag4 = lag(FTA, 4),
    FTA_lag5 = lag(FTA, 5),
    FT_lag1 = lag(FT., 1),
    FT_lag2 = lag(FT., 2),
    FT_lag3 = lag(FT., 3),
    FT_lag4 = lag(FT., 4),
    FT_lag5 = lag(FT., 5),
    REB_lag1 = lag(REB, 1),
    REB_lag2 = lag(REB, 2),
    REB_lag3 = lag(REB, 3),
    REB_lag4 = lag(REB, 4),
    REB_lag5 = lag(REB, 5),
    AST_lag1 = lag(AST, 1),
    AST_lag2 = lag(AST, 2),
    AST_lag3 = lag(AST, 3),
    AST_lag4 = lag(AST, 4),
    AST_lag5 = lag(AST, 5),
    STL_lag1 = lag(STL, 1),
    STL_lag2 = lag(STL, 2),
    STL_lag3 = lag(STL, 3),
    STL_lag4 = lag(STL, 4),
    STL_lag5 = lag(STL, 5),
    BLK_lag1 = lag(BLK, 1),
    BLK_lag2 = lag(BLK, 2),
    BLK_lag3 = lag(BLK, 3),
    BLK_lag4 = lag(BLK, 4),
    BLK_lag5 = lag(BLK, 5),
    TO_lag1 = lag(TO, 1),
    TO_lag2 = lag(TO, 2),
    TO_lag3 = lag(TO, 3),
    TO_lag4 = lag(TO, 4),
    TO_lag5 = lag(TO, 5),
    log_salary = log(salary)
  ) %>%
  ungroup()

# 去除包含NA值的行
data <- na.omit(data)

# 建立初始模型，包含所有變數和滯後項
initial_model <- lm(log_salary ~ GP + MIN + PTS + FGA + FG. + X3PA + X3P. + FTA + FT. + REB + AST + STL + BLK + TO +
                      GP_lag1 + GP_lag2 + GP_lag3 + GP_lag4 + GP_lag5 + 
                      MIN_lag1 + MIN_lag2 + MIN_lag3 + MIN_lag4 + MIN_lag5 + 
                      PTS_lag1 + PTS_lag2 + PTS_lag3 + PTS_lag4 + PTS_lag5 + 
                      FGA_lag1 + FGA_lag2 + FGA_lag3 + FGA_lag4 + FGA_lag5 + 
                      FG_lag1 + FG_lag2 + FG_lag3 + FG_lag4 + FG_lag5 + 
                      X3PA_lag1 + X3PA_lag2 + X3PA_lag3 + X3PA_lag4 + X3PA_lag5 + 
                      X3P_lag1 + X3P_lag2 + X3P_lag3 + X3P_lag4 + X3P_lag5 + 
                      FTA_lag1 + FTA_lag2 + FTA_lag3 + FTA_lag4 + FTA_lag5 + 
                      FT_lag1 + FT_lag2 + FT_lag3 + FT_lag4 + FT_lag5 + 
                      REB_lag1 + REB_lag2 + REB_lag3 + REB_lag4 + REB_lag5 + 
                      AST_lag1 + AST_lag2 + AST_lag3 + AST_lag4 + AST_lag5 + 
                      STL_lag1 + STL_lag2 + STL_lag3 + STL_lag4 + STL_lag5 + 
                      BLK_lag1 + BLK_lag2 + BLK_lag3 + BLK_lag4 + BLK_lag5 + 
                      TO_lag1 + TO_lag2 + TO_lag3 + TO_lag4 + TO_lag5, data = data)

# 使用逐步回歸選擇最佳模型
stepwise_model <- stepAIC(initial_model, direction = "both")

# 檢視逐步回歸選擇的模型摘要
summary(stepwise_model)

###########################################################
# 將VIF值大於10的自變數刪除，以解決嚴重的自變數多元共線性 #
###########################################################

# 計算VIF來檢查多重共線性
vif(stepwise_model)

# 移除VIF值超過10的變數
variables_to_remove <- c("MIN", "FGA", "PTS_lag1", "PTS_lag3", "PTS_lag4", "FGA_lag1", "FGA_lag2", "FGA_lag3", "FGA_lag4")
formula <- as.formula(paste("log_salary ~ GP + FG. + X3P. + FTA + AST + GP_lag2 + GP_lag5 + MIN_lag1 + MIN_lag3 + FGA_lag1 + FG_lag1 + FG_lag2 + X3PA_lag1 + FTA_lag5 + REB_lag1 + AST_lag1 + STL_lag1 + STL_lag5 + BLK_lag4 + TO_lag1 + TO_lag5", collapse = " + "))

# 建立新模型
new_model <- lm(formula, data = data)

# 檢視新模型摘要
summary(new_model)

# 計算新模型的VIF值
new_vif <- vif(new_model)
print(new_vif)

##############################################################
# 透過異方差穩健標準誤檢查自變數在統計上的顯著性，並判斷是否 #
# 該刪除統計上不顯著的自變數                                 #
##############################################################

# 計算異方差穩健標準誤差
hc_se <- vcovHC(new_model, type = "HC1")

# 使用異方差穩健標準誤差進行t檢定
robust_test <- coeftest(new_model, vcov = hc_se)

# 顯示帶有異方差穩健標準誤差的回歸結果
print(robust_test)

# 建立原始模型
original_model <- lm(log_salary ~ GP + MIN + PTS + FGA + FG. + X3P. + FTA + AST + GP_lag2 + GP_lag5 + MIN_lag1 + MIN_lag3 + FG_lag1 + FG_lag2 + X3PA_lag1 + FTA_lag5 + REB_lag1 + STL_lag1 + STL_lag5 + BLK_lag4 + TO_lag1 + TO_lag5, data = data)
summary(original_model)

# 計算原始模型的AIC和BIC值
original_aic <- AIC(original_model)
original_bic <- BIC(original_model)

# 打印原始模型的AIC和BIC值
print(paste("Original Model AIC:", original_aic))
print(paste("Original Model BIC:", original_bic))

# 移除不顯著變數（p值大於0.05）
variables_to_remove <- c("FG.", "X3P.", "GP_lag5", "AST_lag1")

# 更新公式，移除不顯著變數
formula <- as.formula("log_salary ~ GP + FTA + AST + GP_lag2 + MIN_lag1 + MIN_lag3 + FGA_lag1 + FG_lag1 + FG_lag2 + X3PA_lag1 + FTA_lag5 + REB_lag1 + STL_lag1 + STL_lag5 + BLK_lag4 + TO_lag1 + TO_lag5")

# 建立簡化模型
simplified_model <- lm(formula, data = data)
summary(simplified_model)

# 計算簡化模型的AIC和BIC值
simplified_aic <- AIC(simplified_model)
simplified_bic <- BIC(simplified_model)

# 打印簡化模型的AIC和BIC值
print(paste("Simplified Model AIC:", simplified_aic))
print(paste("Simplified Model BIC:", simplified_bic))

# 使用ANOVA進行F檢定
anova_result <- anova(simplified_model, original_model)

# 打印ANOVA結果
print(anova_result)

##########################################################
# 最終模型log_salary ~ GP + MIN + PTS + FGA + FG. + X3P. #
# + FTA + AST + GP_lag2 + GP_lag5 + MIN_lag1 + MIN_lag3  #
# + FG_lag1 + FG_lag2 + X3PA_lag1 + FTA_lag5 + REB_lag1  #
# + STL_lag1 + STL_lag5 + BLK_lag4 + TO_lag1 + TO_lag5   #
##########################################################
final_model <- lm(log_salary ~ GP + MIN + PTS + FGA + FG. + X3P. + FTA + AST + GP_lag2 + GP_lag5 + MIN_lag1 + MIN_lag3 + FG_lag1 + FG_lag2 + X3PA_lag1 + FTA_lag5 + REB_lag1 + STL_lag1 + STL_lag5 + BLK_lag4 + TO_lag1 + TO_lag5, data = data)

summary(final_model)
# 模型診斷圖
par(mfrow = c(2, 2))
plot(original_model)

# 殘差正態性檢定
shapiro.test(residuals(original_model))

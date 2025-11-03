# @author: my@daisg.com
# @date: 2016-09-19
# @version: 1.0
# Copyright (c) 2016 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
## Super 3D lottery with Fractal Theory
## 福彩3D累计积分形分析算法
# library(GH.AN.LIST) # 最新数据获取
# # -----------------------------------------
# # A：数据准备与任务创建
# # -----------------------------------------
# # 方法1：自建包获取数据：数据 3D_Data。
# Super3D_Data <- GH_LIST(5,1000,6)
# str(Super3D_Data) # 数据集的基本信息
# head(Super3D_Data)  # 前几行数据
# sum(is.na(Super3D_Data)) # 检查缺失值

# 方法3：导入本地CSV格式的历史数据
# 方法3：模拟福彩3D历史数据（假设已有100期数据）
# 格式：期号(ISSUE), 百位(Hundreds), 十位(Tens), 个位(Units)
set.seed(123) # 设置随机种子确保示例可重现
n_periods <- 100

# 生成数据
Super3D_history <- data.frame(
  ISSUE = 240001:(240000 + n_periods),
  Hundreds = sample(0:9, n_periods, replace = TRUE),
  Tens = sample(0:9, n_periods, replace = TRUE),
  Units = sample(0:9, n_periods, replace = TRUE)
)

# # -----------------------------------------
# # B：累计积分形转换函数
# # -----------------------------------------
fractal_cumulative_transform <- function(ts_data, window_size = 10) {
  # 参数:
  # ts_data: 数值向量，一个位置的历史号码序列
  # window_size: 滑动窗口大小，用于平滑序列
  
  n <- length(ts_data)
  if (n < window_size) {
    stop("数据量不足，需要至少", window_size, "期数据进行分形分析")
  }
  
  # 基础累计积：对原始序列进行累加
  simple_cumsum <- cumsum(ts_data)
  
  # 滑动窗口平均累计积：平滑序列，减少随机波动
  smoothed_cumsum <- stats::filter(simple_cumsum, 
                                   rep(1/window_size, window_size), 
                                   sides = 1)
  
  # 标准化累计积分形值
  normalized_fractal <- (smoothed_cumsum - min(smoothed_cumsum, na.rm = TRUE)) / 
    (max(smoothed_cumsum, na.rm = TRUE) - min(smoothed_cumsum, na.rm = TRUE))
  
  return(list(
    simple_cumsum = simple_cumsum,
    smoothed_cumsum = smoothed_cumsum,
    normalized_fractal = normalized_fractal
  ))
}

# # -----------------------------------------
# # C：分形维数估算函数（通过Hurst指数）
# # -----------------------------------------
calculate_hurst <- function(ts_data) {
  # 使用重标极差分析法(R/S分析法)计算Hurst指数
  # Hurst指数与分形维数D的关系：D = 2 - H
  # H > 0.5: 序列具有持续性; H < 0.5: 序列具有反持续性; H = 0.5: 随机序列
  
  n <- length(ts_data)
  if (n < 10) return(NA)
  
  # 将序列分成多个子区间
  max_k <- floor(n/2)
  r_s_ratio <- numeric(max_k)
  
  for (k in 2:max_k) {
    # 将序列分成m个长度为k的子区间
    m <- floor(n/k)
    if (m < 2) next
    
    # 计算每个子区间的重标极差(R/S)
    r_s_values <- numeric(m)
    for (i in 1:m) {
      subset_data <- ts_data[((i-1)*k+1):(i*k)]
      mean_val <- mean(subset_data)
      deviation <- subset_data - mean_val
      z <- cumsum(deviation) # 累积离差
      r <- max(z) - min(z)   # 极差
      s <- sd(subset_data)    # 标准差
      if (s > 0) r_s_values[i] <- r/s
    }
    
    r_s_ratio[k] <- mean(r_s_values, na.rm = TRUE)
  }
  
  # 剔除无效值
  valid_indices <- which(r_s_ratio > 0)
  if (length(valid_indices) < 3) return(NA)
  
  # 对log(k)和log(R/S)进行线性回归，斜率即为Hurst指数
  k_values <- valid_indices
  log_k <- log(k_values)
  log_rs <- log(r_s_ratio[k_values])
  
  model <- lm(log_rs ~ log_k)
  hurst <- coef(model)[2]
  
  return(hurst)
}

# # -----------------------------------------
# # D：累计积分形预测函数
# # -----------------------------------------
fractal_predict <- function(ts_data, steps = 1, window_size = 10) {
  # 基于累计积分形序列的趋势进行预测
  n <- length(ts_data)
  
  # 计算累计积分形序列
  fractal_seq <- fractal_cumulative_transform(ts_data, window_size)
  cumsum_vals <- fractal_seq$simple_cumsum
  
  # 使用线性回归拟合近期趋势
  recent_data <- data.frame(
    time = 1:length(cumsum_vals),
    value = cumsum_vals
  )
  
  # 拟合最近window_size*2期的趋势
  recent_window <- max(1, n - window_size*2):n
  model <- lm(value ~ time, data = recent_data[recent_window, ])
  
  # 预测未来steps期的累计积分形值
  future_times <- data.frame(time = (n+1):(n+steps))
  future_cumsum <- predict(model, future_times)
  
  # 将累计积分形预测值转换回原始号码预测
  current_cumsum <- cumsum_vals[n]
  predicted_diff <- future_cumsum - current_cumsum
  
  # 预测的原始号码值（取整并限制在0-9范围内）
  predicted_values <- round(predicted_diff / steps)
  predicted_values <- pmax(0, pmin(9, predicted_values))
  
  return(predicted_values)
}

# # -----------------------------------------
# # E：主分析函数
# # -----------------------------------------
analyze_3d_fractal <- function(data, positions = c("Hundreds", "Tens", "Units"), 
                               window_size = 10) {
  # 对3D每个位置进行累计积分形分析
  
  results <- list()
  
  for (pos in positions) {
    cat("=== 分析位置:", pos, "===\n")
    
    # 提取该位置的历史数据
    ts_data <- data[[pos]]
    
    # 计算累计积分形序列
    fractal_result <- fractal_cumulative_transform(ts_data, window_size)
    results[[pos]]$fractal <- fractal_result
    
    # 计算Hurst指数
    hurst <- calculate_hurst(ts_data)
    results[[pos]]$hurst <- hurst
    fractal_dim <- ifelse(!is.na(hurst), 2 - hurst, NA)
    
    cat("Hurst指数:", round(hurst, 4), "\n")
    cat("分形维数:", round(fractal_dim, 4), "\n")
    
    # 判断序列特性
    if (!is.na(hurst)) {
      if (hurst > 0.6) {
        cat("序列特性: 强持续性（趋势增强）\n")
      } else if (hurst > 0.5) {
        cat("序列特性: 弱持续性\n")
      } else if (hurst > 0.4) {
        cat("序列特性: 弱反持续性\n")
      } else {
        cat("序列特性: 强反持续性（均值回归）\n")
      }
    }
    
    # 进行预测
    predicted <- fractal_predict(ts_data, steps = 1, window_size)
    results[[pos]]$predicted <- predicted
    cat("下期预测号码:", predicted, "\n\n")
  }
  
  return(results)
}

# # -----------------------------------------
# # F：执行分析
# # -----------------------------------------
if (exists("Super3D_history")) {
  # 运行累计积分形分析
  fractal_results <- analyze_3d_fractal(Super3D_history)
  
  # G： 可视化结果
  # 绘制累计积分形序列图
  par(mfrow = c(2, 2))
  
  positions <- c("Hundreds", "Tens", "Units")
  colors <- c("red", "blue", "darkgreen")
  
  for (i in 1:length(positions)) {
    pos <- positions[i]
    fractal_data <- fractal_results[[pos]]$fractal
    
    plot(Super3D_history[[pos]], type = "l", col = "gray",
         main = paste("位置:", pos, "- 原始序列与累计积分形"),
         xlab = "期数", ylab = "号码值")
    
    lines(fractal_data$normalized_fractal * 9, col = colors[i], lwd = 2)
    legend("topright", legend = c("原始号码", "累计积分形"), 
           col = c("gray", colors[i]), lty = 1)
  }
  
  # 绘制Hurst指数比较图
  hurst_values <- sapply(positions, function(pos) fractal_results[[pos]]$hurst)
  if (all(!is.na(hurst_values))) {
    barplot(hurst_values, col = colors, main = "各位置Hurst指数比较",
            ylab = "Hurst指数", ylim = c(0, 1))
    abline(h = 0.5, lty = 2, col = "red")
    text(x = 1:3, y = hurst_values + 0.05, 
         labels = round(hurst_values, 3))
  }
  
} else {
  cat("请提供福彩3D历史数据，数据框需包含'Hundreds','Tens','Units'三列\n")
}

# # -----------------------------------------
# # H：结果汇总
# # -----------------------------------------
cat("====== 福彩3D累计积分形分析报告 ======\n")
if (exists("fractal_results")) {
  for (pos in c("Hundreds", "Tens", "Units")) {
    cat("位置:", pos, "\n")
    cat("  预测号码:", fractal_results[[pos]]$predicted, "\n")
    cat("  Hurst指数:", round(fractal_results[[pos]]$hurst, 4), "\n")
    cat("  分形维数:", round(2 - fractal_results[[pos]]$hurst, 4), "\n")
    cat("  ", 
        ifelse(fractal_results[[pos]]$hurst > 0.5, 
               "提示: 序列具有持续性，近期趋势可能延续",
               "提示: 序列具有反持续性，近期趋势可能反转"), "\n\n")
  }
}
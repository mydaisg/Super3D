# @author: my@daisg.com
# @date: 2025-11-03
# @version: 0.1.0
# Copyright (c) 2025 daisg
# SPDX-License-Identifier: GPL-3
# @brief/ Description
## Super 3D lottery with Fractal Theory
## 福彩3D累计积分形分析算法

// Cargo.toml 依赖项：
// [dependencies]
// ndarray = "0.15"
// statrs = "0.16"

use ndarray::{Array1, Array2, Axis};
use std::collections::VecDeque;

// 福彩3D数据结构
#[derive(Debug, Clone)]
pub struct Lottery3D {
    pub issue: String,
    pub hundreds: u8,  // 百位 (0-9)
    pub tens: u8,      // 十位 (0-9)
    pub units: u8,     // 个位 (0-9)
}

#[derive(Debug)]
pub struct FractalAnalyzer {
    pub history_data: Vec<Lottery3D>,
    pub window_size: usize,
}

impl FractalAnalyzer {
    pub fn new(data: Vec<Lottery3D>) -> Self {
        FractalAnalyzer {
            history_data: data,
            window_size: 10,
        }
    }

    // 累计积分形转换
    pub fn cumulative_fractal_transform(&self, position_data: &[u8]) -> Array1<f64> {
        let n = position_data.len();
        let mut cumulative = Array1::<f64>::zeros(n);
        
        // 基础累计和
        let mut sum = 0.0;
        for (i, &value) in position_data.iter().enumerate() {
            sum += value as f64;
            cumulative[i] = sum;
        }
        
        // 滑动窗口平滑
        let smoothed = self.moving_average(&cumulative, self.window_size);
        
        // 标准化
        self.normalize(&smoothed)
    }

    // 滑动窗口平均
    fn moving_average(&self, data: &Array1<f64>, window: usize) -> Array1<f64> {
        let n = data.len();
        let mut result = Array1::<f64>::zeros(n);
        
        for i in 0..n {
            let start = if i >= window { i - window + 1 } else { 0 };
            let window_data = &data.slice(ndarray::s![start..=i]);
            result[i] = window_data.sum() / window_data.len() as f64;
        }
        
        result
    }

    // 数据标准化
    fn normalize(&self, data: &Array1<f64>) -> Array1<f64> {
        let min = data.fold(f64::INFINITY, |acc, &x| acc.min(x));
        let max = data.fold(f64::NEG_INFINITY, |acc, &x| acc.max(x));
        
        if (max - min).abs() < f64::EPSILON {
            return Array1::zeros(data.len());
        }
        
        (data - min) / (max - min)
    }

    // 计算Hurst指数（R/S分析法）
    pub fn calculate_hurst(&self, data: &[u8]) -> Option<f64> {
        let n = data.len();
        if n < 10 {
            return None;
        }

        let data_f64: Vec<f64> = data.iter().map(|&x| x as f64).collect();
        let mut r_s_ratios = Vec::new();
        let mut time_scales = Vec::new();

        // 对不同时间尺度计算R/S
        for k in 2..=(n / 2) {
            let m = n / k;
            if m < 2 {
                continue;
            }

            let mut r_s_values = Vec::new();
            
            for i in 0..m {
                let start = i * k;
                let end = (i + 1) * k;
                if end > n {
                    break;
                }
                
                let subset = &data_f64[start..end];
                let mean = subset.iter().sum::<f64>() / subset.len() as f64;
                
                // 计算累积离差
                let deviations: Vec<f64> = subset.iter()
                    .map(|&x| x - mean)
                    .collect();
                
                let cumulative_deviations: Vec<f64> = deviations
                    .iter()
                    .scan(0.0, |acc, &x| {
                        *acc += x;
                        Some(*acc)
                    })
                    .collect();
                
                let r = cumulative_deviations.iter()
                    .fold(f64::NEG_INFINITY, |a, &b| a.max(b)) - 
                    cumulative_deviations.iter()
                    .fold(f64::INFINITY, |a, &b| a.min(b));
                
                let s = self.standard_deviation(subset);
                
                if s > 0.0 {
                    r_s_values.push(r / s);
                }
            }
            
            if !r_s_values.is_empty() {
                let avg_r_s = r_s_values.iter().sum::<f64>() / r_s_values.len() as f64;
                r_s_ratios.push(avg_r_s.ln());
                time_scales.push((k as f64).ln());
            }
        }

        // 线性回归计算Hurst指数
        if r_s_ratios.len() < 3 {
            return None;
        }

        self.linear_regression(&time_scales, &r_s_ratios)
    }

    // 线性回归计算斜率
    fn linear_regression(&self, x: &[f64], y: &[f64]) -> Option<f64> {
        let n = x.len() as f64;
        
        let sum_x: f64 = x.iter().sum();
        let sum_y: f64 = y.iter().sum();
        let sum_xy: f64 = x.iter().zip(y.iter()).map(|(a, b)| a * b).sum();
        let sum_x2: f64 = x.iter().map(|a| a * a).sum();
        
        let denominator = n * sum_x2 - sum_x * sum_x;
        if denominator.abs() < f64::EPSILON {
            return None;
        }
        
        let slope = (n * sum_xy - sum_x * sum_y) / denominator;
        Some(slope)
    }

    // 标准差计算
    fn standard_deviation(&self, data: &[f64]) -> f64 {
        let n = data.len() as f64;
        let mean = data.iter().sum::<f64>() / n;
        
        let variance = data.iter()
            .map(|&x| (x - mean).powi(2))
            .sum::<f64>() / n;
        
        variance.sqrt()
    }

    // 分形预测函数
    pub fn fractal_predict(&self, position_data: &[u8], steps: usize) -> Option<u8> {
        if position_data.len() < self.window_size {
            return None;
        }

        // 计算累计积分形序列
        let fractal_seq = self.cumulative_fractal_transform(position_data);
        let n = fractal_seq.len();

        // 使用近期趋势进行线性外推
        let recent_window = std::cmp::min(2 * self.window_size, n);
        let x: Vec<f64> = (0..recent_window).map(|i| i as f64).collect();
        let y: Vec<f64> = fractal_seq.slice(ndarray::s![n-recent_window..]).to_vec();

        if let Some(slope) = self.linear_regression(&x, &y) {
            // 预测下一个累计和
            let last_value = fractal_seq[n - 1];
            let next_cumulative = last_value + slope;
            
            // 转换为原始号码预测
            let current_cumulative: f64 = position_data.iter().map(|&x| x as f64).sum();
            let predicted_diff = next_cumulative - current_cumulative;
            
            // 限制在0-9范围内
            let predicted = predicted_diff.round().max(0.0).min(9.0) as u8;
            Some(predicted)
        } else {
            None
        }
    }

    // 主分析函数
    pub fn analyze_positions(&self) -> FractalAnalysisResult {
        let mut results = FractalAnalysisResult::default();
        
        let positions = [
            ("hundreds", self.history_data.iter().map(|x| x.hundreds).collect::<Vec<u8>>()),
            ("tens", self.history_data.iter().map(|x| x.tens).collect()),
            ("units", self.history_data.iter().map(|x| x.units).collect()),
        ];

        for (pos_name, data) in positions {
            if let Some(hurst) = self.calculate_hurst(&data) {
                let fractal_dim = 2.0 - hurst;
                let prediction = self.fractal_predict(&data, 1);
                
                let pos_result = PositionResult {
                    hurst_exponent: hurst,
                    fractal_dimension: fractal_dim,
                    predicted_number: prediction,
                    trend: if hurst > 0.6 {
                        "强持续性".to_string()
                    } else if hurst > 0.5 {
                        "弱持续性".to_string()
                    } else if hurst > 0.4 {
                        "弱反持续性".to_string()
                    } else {
                        "强反持续性".to_string()
                    },
                };
                
                match pos_name {
                    "hundreds" => results.hundreds = Some(pos_result),
                    "tens" => results.tens = Some(pos_result),
                    "units" => results.units = Some(pos_result),
                    _ => {}
                }
            }
        }
        
        results
    }
}

// 结果结构体
#[derive(Debug, Default)]
pub struct FractalAnalysisResult {
    pub hundreds: Option<PositionResult>,
    pub tens: Option<PositionResult>,
    pub units: Option<PositionResult>,
}

#[derive(Debug, Clone)]
pub struct PositionResult {
    pub hurst_exponent: f64,
    pub fractal_dimension: f64,
    pub predicted_number: Option<u8>,
    pub trend: String,
}

// 示例使用
fn main() {
    // 模拟福彩3D历史数据
    let history_data = vec![
        Lottery3D { issue: "2025001".to_string(), hundreds: 3, tens: 5, units: 7 },
        Lottery3D { issue: "2025002".to_string(), hundreds: 1, tens: 8, units: 2 },
        Lottery3D { issue: "2025003".to_string(), hundreds: 4, tens: 2, units: 9 },
        // ... 更多数据
    ];

    let analyzer = FractalAnalyzer::new(history_data);
    let results = analyzer.analyze_positions();

    // 输出分析结果
    println!("福彩3D累计积分形分析结果:");
    
    if let Some(hundreds) = results.hundreds {
        println!("百位分析:");
        println!("  Hurst指数: {:.4}", hundreds.hurst_exponent);
        println!("  分形维数: {:.4}", hundreds.fractal_dimension);
        println!("  趋势特征: {}", hundreds.trend);
        if let Some(pred) = hundreds.predicted_number {
            println!("  预测号码: {}", pred);
        }
    }
    
    // 类似输出十位和个位结果...
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cumulative_fractal() {
        let data = vec![1, 2, 3, 4, 5];
        let analyzer = FractalAnalyzer::new(Vec::new());
        let result = analyzer.cumulative_fractal_transform(&data);
        
        assert_eq!(result.len(), 5);
        assert!(result[4] > result[0]); // 累计和应该递增
    }
}
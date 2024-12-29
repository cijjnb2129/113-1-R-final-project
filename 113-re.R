data <- read_csv("ENB2012_data.csv")

# 顯示檔案前幾行資料
head(data)

# 單開表格
View(data)

data <- data |> 
  rename(
    `相對緊湊度` = X1,
    `表面積` = X2,
    `牆壁面積` = X3,
    `屋頂面積` = X4,
    `總高度` = X5,
    `方向` = X6,
    `玻璃面積` = X7,
    `玻璃面積分佈` = X8,
    `熱負荷` = Y1,
    `冷負荷` = Y2
  )

# 檢查每個欄位的空值資料
sapply(data, function(x) sum(is.na(x)))
library(tidyr)

# 將資料轉換為長格式
data_long <- data |> 
  pivot_longer(cols = everything(), names_to = "變數", values_to = "值")

# 繪製箱型圖
ggplot(data_long, aes(x = 變數, y = 值)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 過濾數值型欄位並處理缺失值
data_numeric <- data |> 
  select(where(is.numeric)) |> 
  drop_na()

# Robust 標準化
data_normalized_robust <- data_numeric |> 
  mutate(across(everything(), ~ ( . - median(.) ) / IQR(.) ))

# 顯示結果
head(data_normalized_robust)

# 選擇數值型變數
data_numeric <- data |> 
  select(where(is.numeric))

# 計算皮爾森相關性係數
correlations <- cor(data_numeric, method = "pearson")

# 顯示與熱負荷相關性係數
correlations_with_target <- correlations["熱負荷", ]
correlations_with_target

# 選擇數值型變數
data_numeric <- data |> 
  select(where(is.numeric))

# 計算肯德爾相關性係數
correlations_kendall <- cor(data_numeric, method = "kendall")

# 顯示與熱負荷相關性係數
correlations_with_target_kendall <- correlations_kendall["熱負荷", ]
correlations_with_target_kendall

# 選擇數值型變數
data_numeric <- data |> 
  select(where(is.numeric))

# 計算斯皮爾曼相關性係數
correlations_spearman <- cor(data_numeric, method = "spearman")

# 顯示與熱負荷相關性係數
correlations_with_target_spearman <- correlations_spearman["熱負荷", ]
correlations_with_target_spearman



# 選擇數值型變數
data_numeric <- data |> 
  select(where(is.numeric))

# 計算相關性係數
correlations_pearson <- cor(data_numeric, method = "pearson")
correlations_kendall <- cor(data_numeric, method = "kendall")
correlations_spearman <- cor(data_numeric, method = "spearman")

# 提取與熱負荷的相關性係數
correlations_with_target <- data.frame(
  皮爾森 = correlations_pearson["熱負荷", ],
  肯德爾 = correlations_kendall["熱負荷", ],
  斯皮爾曼 = correlations_spearman["熱負荷", ]
)

# 顯示結果
correlations_with_target

library(tidyverse)


# 選擇數值型變數
data_numeric <- data |> 
  select(where(is.numeric))

# 計算相關性係數
correlations_pearson <- cor(data_numeric, method = "pearson")["熱負荷", ]
correlations_kendall <- cor(data_numeric, method = "kendall")["熱負荷", ]
correlations_spearman <- cor(data_numeric, method = "spearman")["熱負荷", ]

# 整理成長格式的數據框
correlations_df <- tibble(
  變數 = names(correlations_pearson),
  皮爾森 = correlations_pearson,
  肯德爾 = correlations_kendall,
  斯皮爾曼 = correlations_spearman
) |> 
  pivot_longer(
    cols = c("皮爾森", "肯德爾", "斯皮爾曼"),
    names_to = "相關係數類型",
    values_to = "相關性"
  )

# 繪製折線圖
ggplot(correlations_df, aes(x = 變數, y = 相關性, color = 相關係數類型, group = 相關係數類型)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "熱負荷相關性折線圖",
    x = "變數",
    y = "相關性",
    color = "相關係數類型"
  )


library(ggplot2)

# 選擇數值型變數
data_numeric <- data |> 
  select(where(is.numeric))

# 繪製每個變數的直方圖
data_numeric |> 
  pivot_longer(cols = everything(), names_to = "變數", values_to = "值") |> 
  ggplot(aes(x = 值)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ 變數, scales = "free_x") +  # 分面圖，每個變數一個子圖
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "各變數的直方圖", x = "值", y = "頻率")

library(ggplot2)

# 選擇特徵值與目標值
data_selected <- data |> 
  select(相對緊湊度, 表面積, 牆壁面積, 屋頂面積, 總高度, 方向, 玻璃面積, 玻璃面積分佈, 冷負荷, 熱負荷)

# 繪製每個特徵值與熱負荷的散點圖
data_selected |> 
  pivot_longer(cols = -熱負荷, names_to = "變數", values_to = "值") |> 
  ggplot(aes(x = 值, y = 熱負荷)) +
  geom_point(color = "blue", alpha = 0.6) +
  facet_wrap(~ 變數, scales = "free") +  # 分面圖，每個特徵值一個子圖
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "特徵值與熱負荷的散點圖", x = "特徵值", y = "熱負荷")


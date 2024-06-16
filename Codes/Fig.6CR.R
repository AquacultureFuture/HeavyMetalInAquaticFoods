library(readxl)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(dplyr)
library(patchwork)
library(scales)
library(viridis)
library(RColorBrewer)

# 读取数据
data_path <- "D:/R Graph Data/R Graph/Fig.6CR.xlsx" # Ensure the path is correct
df <- read_excel(data_path)

# 加载世界地图，移除南极洲
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")

# 初始化图形列表
plots <- list()

# 从列名中提取不同的指标类型（例如Total, Cadmium等）
indicators <- unique(gsub("CR_BAU_(.*)", "\\1", names(df)[grepl("BAU", names(df))]))

# 对每个指标类型执行操作
for (indicator in indicators) {
  # 为该指标类型找到所有相关的列
  indicator_cols <- grep(indicator, names(df), value = TRUE)
  
  # 确定BAU列并计算颜色映射的最大和最小值
  bau_cols <- grep("BAU", indicator_cols, value = TRUE)
  bau_col <- bau_cols[1]  # Use only the first BAU column if multiple are found
  max_val <- max(df[[bau_col]], na.rm = TRUE)
  min_val <- min(df[[bau_col]], na.rm = TRUE)
  
  # 定义颜色映射
  color_values <- scale_fill_gradientn(
    colours = brewer.pal(9, "YlGnBu"),
    limits = c(min_val, max_val),
    name = "",
    labels = scales::percent,
    oob = scales::squish  # Use this to ensure values beyond limits are squished into the limit range
  )
  
  # 生成地图
  for (col in indicator_cols) {
    merged_data <- world %>%
      left_join(df, by = c("name" = "Country")) %>%
      select(geometry, all_of(col))
    
    # Ensure values don't exceed the BAU maximum
    merged_data[[col]] <- pmin(merged_data[[col]], max_val)
    
    p <- ggplot(merged_data) +
      geom_sf(aes(fill = !!rlang::sym(col)), na.rm = TRUE) +
      color_values +
      labs(title = col) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 12),
        legend.key.size = unit(0.9, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
      )
    
    plots[[col]] <- p
  }
}

# 使用 patchwork 组合图形，每行显示3个
plot_grid <- wrap_plots(plots, ncol = 3)

# 显示组合后的图形
print(plot_grid)


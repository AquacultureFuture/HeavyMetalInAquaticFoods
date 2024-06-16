library(readxl)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(dplyr)
library(patchwork)
library(scales)
library(viridis)
library(RColorBrewer)
library(scales) # 确保加载了scales包以使用percent格式

# 假设数据已经读取和预处理
data_path <- "D:/R Graph Data/R Graph/Fig.4a.xlsx"
df <- read_excel(data_path)

# 加载自然地球数据并移除南极洲
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")

# 初始化一个空的图形列表
plots <- list()

# 遍历数据帧的每个数值列
for (col in names(df)[-1]) {
  merged_data <- world %>%
    left_join(df, by = c("name" = "Country")) %>%
    select(geometry, all_of(col))
  
  unique_non_na_values <- unique(df[[col]][!is.na(df[[col]])])
  
  # 获取"YlGnBu"色板中的最浅颜色
  lightest_YlGnBu <- brewer.pal(9, "YlGnBu")[1]
  
  if(length(unique_non_na_values) == 1) {
    merged_data$single_value <- as.factor(unique_non_na_values[1])
    
    p <- ggplot(merged_data) +
      geom_sf(aes(fill = single_value), na.rm = TRUE) +
      scale_fill_gradientn(colours = brewer.pal(9, "YlGnBu"), 
                           limits = c(0, 1), # 修改为你想要的最低和最高范围
                           name = "") +
      labs(title = paste("", col)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18),
        legend.key.size = unit(0.8, "cm"), 
        legend.text = element_text(size = 14)
      )
  } else {
    p <- ggplot(merged_data) +
      geom_sf(aes(fill = !!sym(col)), na.rm = TRUE) +
      scale_fill_gradientn(colours = brewer.pal(9, "YlGnBu"), name = "") +
      labs(title = paste("", col)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18),
        legend.key.size = unit(0.8, "cm"), 
        legend.text = element_text(size = 14)
      )
  }
  
  plots[[col]] <- p
}

# 使用 patchwork 组合图形，每行显示4个图形
plot_grid <- wrap_plots(plots, ncol = 3)

# 打印组合后的大图
print(plot_grid)


library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(readxl)
library(dplyr)
library(patchwork)

# 读取数据
data_path <- "D:/R Graph Data/R Graph/Fig.4b.xlsx" # 请修改为实际路径
data <- read_excel(data_path)

# 获取世界地图数据
world <- ne_countries(scale = "medium", returnclass = "sf")
# 移除南极洲
world <- world[world$admin != "Antarctica", ]

# 准备颜色方案，使用更柔和的颜色
soft_colors <- c(
  "Freshwater fish" = "#B0C4DE",  # 淡钢蓝
  "Pelagic marine fish" = "#FFE4E1",  # 米色
  "Cephalopods" = "#E6E6FA",  # 苍绿
  "Crustaceans" = "#FFB6C1",  # 浅粉红
  "Other marine fish" = "#EEE8AA",  # 苍黄
  "Molluscs" = "#DDA0DD",  # 李子色
  "Demersal marine fish" = "#AFEEEE"  # 苍蓝
)

# 初始化一个列表来存储ggplot对象
plots <- list()

# 为每个类别绘制地图
categories <- c("Consumption", "Mercury", "Cadmium", "Lead", "Arsenic")
for (category in categories) {
  # 合并数据
  merged_data <- world %>%
    left_join(data, by = c("admin" = "CountryNameInR")) %>%
    mutate(Category = .[[category]]) # 根据当前类别设置Category列
  
  # 创建ggplot对象
  plot <- ggplot(merged_data) +
    geom_sf(aes(fill = Category), color = "grey") + # 根据Category填充颜色
    scale_fill_manual(values = soft_colors) + # 应用颜色方案
    labs(title = paste("", category, ""),
         fill = "") +
    theme_void() +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) + # 排除南极洲
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(size = 15, face = "bold", hjust = 0.5),  # 调整标题大小和位置
          legend.text = element_text(size = 12))  # 调整图例文字大小
  
  # 将ggplot对象添加到列表中
  plots[[category]] <- plot
}

# 使用patchwork组合所有地图
combined_plot <- wrap_plots(plots, ncol = 3)
print(combined_plot)


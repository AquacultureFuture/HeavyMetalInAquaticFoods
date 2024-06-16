library(tidyverse)
library(readxl)
library(networkD3)

# 从Excel文件加载数据
data <- read_excel("D:/R Graph Data/R Graph/Fig.3EDI.xlsx")

# 设置列名称，确保第一列为分类列
colnames(data) <- c("Category", "Mercury", "Cadmium", "Lead", "Arsenic")

# 转换数据为长格式，以便进行网络可视化
data_long <- data %>%
  pivot_longer(cols = -Category, names_to = "Pollutant", values_to = "Value") %>%
  filter(Value > 0)  # 确保只保留值大于0的数据

# 创建节点数据框
nodes <- data_long %>%
  select(Category, Pollutant) %>%
  pivot_longer(everything(), names_to = "type", values_to = "name") %>%
  distinct(name) %>%
  mutate(ID = row_number() - 1)

# 更新链接数据框以包括节点ID
data_long <- data_long %>%
  left_join(nodes, by = c("Category" = "name")) %>%
  rename(IDsource = ID) %>%
  left_join(nodes, by = c("Pollutant" = "name")) %>%
  rename(IDtarget = ID)

# 定义颜色方案
ColourScal <- 'd3.scaleOrdinal().range(["#B0C4DE", "#9CAFB7", "#D1C4B0", "#C2BDB6", "#B0A8B9", "#FFE4E1", "#E6E6FA","#FFB6C1","#EEE8AA","#AFEEEE", "#DDA0DD"])'

# 建立桑基图
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Value", NodeID = "name",
              sinksRight = FALSE, colourScale = ColourScal,
              nodeWidth = 20, fontSize = 17, nodePadding = 10)
print(nodes)


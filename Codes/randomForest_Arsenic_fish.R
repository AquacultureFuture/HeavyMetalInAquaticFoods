library(rfUtilities)
library(randomForest)
library(rfPermute)
library(ggplot2)
library(psych)
library(reshape2)
library(patchwork)
library(svglite)

# 读取数据
df <- read.csv("D:/R Graph Data/R Graph/Arsenic_fish.csv")

# 设置随机种子，使结果能够重现
set.seed(123)

# 运行随机森林
rf_results <- randomForest(Arsenic ~ ., data = df, importance = TRUE, ntree = 500)

# 查看随机森林主要结果
print(rf_results)

# 提取预测因子的解释率
predictor_var <- data.frame(importance(rf_results, scale = TRUE), check.names = FALSE)

# 自定义顺序列表
custom_order <- c("MarineFreshwater", "ClimateZone", "FeedingPath", "BodyShape", "DemersalPelagic", "K", "DepthRangeDeep", "tm", "FishingVulnerabilityScore", "Lmax", "Resilience", "TrophicLevel")
# 创建行名列
predictor_var$rowname <- factor(rownames(predictor_var), levels = custom_order)

# 绘制柱状图，使用自定义顺序
p1 <- ggplot(data = predictor_var, aes(x = `%IncMSE`, y = rowname)) +
  geom_bar(stat = 'identity', width = 0.7, fill = "#17BECF") +
  theme_classic() + labs(x = 'Increase in MSE(%)', y = '') +
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.text.y = element_blank()) +
  theme(axis.text = element_text(color = "black", size = 11),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
print(p1)

# 读取环境变量和物种丰度矩阵
env <- df$Arsenic
spe <- df[,-1]

# 环境变量和物种丰度的相关性分析
pearson <- corr.test(env, spe, method = 'pearson', adjust = 'none')
r <- data.frame(pearson$r)  # pearson 相关系数矩阵
p <- data.frame(pearson$p)  # p 值矩阵
# 结果整理以便于作图
r$env <- rownames(r)
p$env <- rownames(p)
r <- melt(r, id = 'env')
p <- melt(p, id = 'env')
pearson <- cbind(r, p$value)
colnames(pearson) <- c('env', 'spe', 'pearson_correlation', 'p.value')
pearson$spe <- factor(pearson$spe, levels = colnames(spe))
head(pearson)  # 整理好的环境变量和物种丰度的 pearson 相关性统计表

# ggplot2 作图，绘制环境变量和物种丰度的 pearson 相关性热图
custom_order <- c("TrophicLevel", "Resilience", "Lmax", "FishingVulnerabilityScore", "tm", "DepthRangeDeep", "K", "DemersalPelagic", "BodyShape", "FeedingPath", "ClimateZone", "MarineFreshwater")

# 将 'env' 列转换为因子，并按照自定义顺序排序
pearson$env <- factor(pearson$env, levels = custom_order)

# 使用排序后的数据绘制热图，并调整字体大小
p2 <- ggplot() +
  geom_tile(data = pearson, aes(x = env, y = spe, fill = pearson_correlation)) +
  scale_fill_gradientn(colors = c('#00B554', 'white', '#B586CA'), limit = c(-1, 1)) +
  theme(panel.grid = element_line(), panel.background = element_rect(color = 'black'), 
        legend.key = element_blank(), legend.position = "bottom",
        axis.text.x = element_text(color = 'black', angle = 45, hjust = 1, vjust = 1, size = 14), 
        axis.text.y = element_text(color = 'black', size = 14), axis.ticks = element_line(color = 'black')) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_fixed(ratio = 1) +
  theme(axis.text.x = element_text(size = 0),
        axis.text.y = element_text(size = 16),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = '', x = '', fill = '')

print(p2)

# 如果想把 pearson 相关系数的显著性也标记在图中，参考如下操作
pearson[which(pearson$p.value < 0.001), 'sig'] <- '***'
pearson[which(pearson$p.value < 0.01 & pearson$p.value > 0.001), 'sig'] <- '**'
pearson[which(pearson$p.value < 0.05 & pearson$p.value > 0.01), 'sig'] <- '*'
head(pearson)  # 整理好的环境变量和物种丰度的 pearson 相关性统计表

p3 <- p2 +
  geom_text(data = pearson, aes(x = env, y = spe, label = sig), size = 8)

print(p3)

# 合并柱状图和热图
combined_plot <- p3 + p1
print(combined_plot)

# 保存图像为SVG格式
ggsave("Arsenic_fish_plots.svg", plot = combined_plot, width = 12, height = 8, device = "svg")

# 计算 MAE, RMSE 和 R²
predictions <- predict(rf_results, df)
mae <- mean(abs(predictions - df$Arsenic))
rmse <- sqrt(mean((predictions - df$Arsenic)^2))
r2 <- cor(predictions, df$Arsenic)^2

cat("MAE: ", mae, "\n")
cat("RMSE: ", rmse, "\n")
cat("R²: ", r2, "\n")


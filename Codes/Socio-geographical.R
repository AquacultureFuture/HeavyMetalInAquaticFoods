library(randomForest)
library(corrplot)
library(readxl)

# 读取Excel文件
data <- readxl::read_xlsx("D:/R Graph Data/R Graph/Socio-geographical.xlsx")

# 将'Continent'分类变量转换为哑变量
data_with_dummies <- cbind(data[, -which(names(data) == "Continent")], model.matrix(~Continent-1, data=data))

# 处理可能存在的NA值
data_with_dummies[is.na(data_with_dummies)] <- sapply(data_with_dummies, function(x) median(x, na.rm = TRUE))

# 分离特征和目标变量 (以健康风险指标HI作为例子)
features <- setdiff(names(data_with_dummies), "HI")
X <- data_with_dummies[, features]
y <- data_with_dummies$HI

# 划分训练集和测试集
set.seed(42)
trainIndex <- sample(1:nrow(X), size = 0.8 * nrow(X))
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

# 训练随机森林模型
rf_model <- randomForest(x = X_train, y = y_train, ntree=500)

# 预测测试集
predictions <- predict(rf_model, X_test)

# 计算均方根误差(RMSE)
rmse <- sqrt(mean((predictions - y_test)^2))

# 计算均方误差(MAE)
mae <- mean(abs(predictions - y_test))

# 计算决定系数(R^2)
sst <- sum((y_test - mean(y_test))^2)
ssr <- sum((predictions - y_test)^2)
r_squared <- 1 - ssr / sst

# 打印模型性能指标
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R^2:", r_squared))

# 选取相关性热图所需的变量
X_variables <- c('EDI_Mercury', 'EDI_Cadmium', 'EDI_Lead', 'EDI_Arsenic',
                 'THQ_Mercury', 'THQ_Cadmium', 'THQ_Lead', 'THQ_Arsenic',
                 'HI', 'CR_Cadmium', 'CR_Lead', 'CR_Arsenic')
Y_variables <- c('PerCapitaConsumption', 'Area_SquareKm', 'Coastline_Km', 'Population',
                 'CoastlineByPopulation', 'CoastlineByArea', 'GDP')

# 包含从Continent派生的哑变量
Continent_dummies <- names(data_with_dummies)[grep("Continent", names(data_with_dummies))]
Y_variables_full <- c(Y_variables, Continent_dummies)

# 计算相关性矩阵
cor_matrix_selected <- cor(data_with_dummies[c(X_variables, Y_variables_full)])
cor_matrix_final <- cor_matrix_selected[Y_variables_full, X_variables]

# 绘制相关性热图
corrplot(cor_matrix_final, method = "color", type = "full", tl.cex = 0.6, tl.col = "black",
         cl.cex = 0.6, number.cex = 0.6, addCoef.col = "black",
         col = colorRampPalette(c("#6D9EC1", "#FFFFFF", "#E46726"))(200))


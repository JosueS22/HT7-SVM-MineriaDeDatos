library(e1071)
library(caret)

# Conjunto de datos
porcentaje<-0.7

casas <- read.csv("./data/train.csv")
porcentaje <- 70/100

set.seed(314)

casas[is.na(casas)] <- 0
casas$Id <- NULL

# Conjunto de entrenamiento y prueba

casas$clasificacion <- ifelse(casas$SalePrice <= 251000, 1, ifelse(casas$SalePrice <= 538000, 2, ifelse(casas$SalePrice <= 755000, 3,0)))

casas <- casas[,c(4,12,17,34,38,46,62,67,81)]

casas_economicas <- casas[casas$clasificacion==1,]
casas_intermedias <- casas[casas$clasificacion==2,]
casas_caras <- casas[casas$clasificacion==3,]

train_nrow_economicas <- sample(nrow(casas_economicas), porcentaje*nrow(casas_economicas))
train_economicas <- casas_economicas[train_nrow_economicas,]

train_nrow_intermedias <- sample(nrow(casas_intermedias), porcentaje*nrow(casas_intermedias))
train_intermedias <- casas_intermedias[train_nrow_intermedias,]

train_nrow_caras <- sample(nrow(casas_caras), porcentaje*nrow(casas_caras))
train_caras <- casas_caras[train_nrow_caras,]

training <- rbind(train_economicas, train_intermedias, train_caras)
test <- casas[setdiff(rownames(casas), rownames(training)),]

# Modelos SVM

modelosvm<-svm(clasificacion~., data = training, scale = F)

plot(modelosvm,training,LotArea~GrLivArea)


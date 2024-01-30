# Installation of required packages and loading them
install.packages(c("MASS", "car","leaps","dplyr","DescTools","glmnet","UsingR"))
library(MASS)
library(car)
library(dplyr)
library(leaps)
library(DescTools)
library(glmnet)
library(UsingR)
library(leaps)
library(caret)
library(RANN)
library(class)



data_train=read.csv('train.csv')
data_test=read.csv('test.csv')
head(data_test)
head(data_train)


#Transform the variable we are going to study 
data_train$SalePrice<-log(data_train$SalePrice)

#la columna id no aporta nada, la quito
data_train<- data_train[,-1]  
data_test<- data_test[,-1]
#Quitamos la variable utilities porque todas las observaciones son iguales
data_train$Utilities
data_train=data_train[,-9]
data_test=data_test[,-9]

#Variables with NAs, that in reality are not NA's, we rename those NA's as a new
#category 'Doesn't have'.
NAs<- c("Alley", "BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1",
        "BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual",
        "GarageCond","PoolQC","Fence","MiscFeature")

for (i in NAs){
  data_train[[i]]<- ifelse(is.na(data_train[[i]]), "Doesn't have", data_train[[i]])
  data_test[[i]]<-ifelse(is.na(data_test[[i]]), "Doesn't have", data_test[[i]])
}

#Change character to factor
character_columns <- sapply(data_train, is.character)
data_train[, character_columns] <- lapply(data_train[, character_columns], as.factor)
data_test[, character_columns] <- lapply(data_test[, character_columns], as.factor)



#Hay variables discretas que tenemos que transformar a categóricas al tener
#un rango muy pequeño. También hay variables categóricas que ha tomado como
#numéricas cuando no lo son. Vamos a transformarlas a factores definiendo 
#distinas categorías para cada variable y teniendo en cuenta que el rango en
#algunas variables es distinto en el test que en el train.

#MSSubClass  es una variable categorica que ya tiene definidas las distintas clases 

#data_train$MSSubClass<- as.factor(data_train$MSSubClass)
#data_test$MSSubClass<- as.factor(data_test$MSSubClass) 

#la categoria 150 no aparece en el train, hay que arreglar esto para que no de problemas
#al crear el modelo. **La categoria 150 solo tiene una observación 
#El salto entre los levels es acorde a los que salen en la descripción de las variables
data_train$MSSubClass <- factor(data_train$MSSubClass, levels = c(20,30,40,45,50,60,70,75,80,85,90,120,150,160,180,190), labels = c(20,30,40,45,50,60,70,75,80,85,90,120,150,160,180,190))
data_test$MSSubClass <- factor(data_test$MSSubClass, levels = c(20,30,40,45,50,60,70,75,80,85,90,120,150,160,180,190), labels = c(20,30,40,45,50,60,70,75,80,85,90,120,150,160,180,190))
summary(data_train$MSSubClass)
summary(data_test$MSSubClass)

#OverallQual y OverallCond son variables ordinales 
data_train$OverallQual<- as.factor(data_train$OverallQual)
data_test$OverallQual<-as.factor(data_test$OverallQual)
summary(data_train$OverallQual)
summary(data_test$OverallQual)

#Las categorias 1 y 2 tienen muy pocas observaciones, igual podemos emparejar categorias?
data_train$OverallCond<-as.factor(data_train$OverallCond)
data_test$OverallCond<-as.factor(data_test$OverallCond)
summary(data_train$OverallCond)
summary(data_test$OverallCond)

#Variables discretas que refieren al nº de habitaciones, baños, coches y chimeneas
#hay que tener cuidado con las variables cuyo rango es diferente en el test y en
#el train (FullBath, BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageCars)

#Separamos en intervalos: 0, 1, 2 o más
data_train$BsmtFullBath<-cut(data_train$BsmtFullBath, breaks = c(-1,0, 1, Inf), labels=c("0", "1","2 or more"))
data_test$BsmtFullBath<-cut(data_test$BsmtFullBath, breaks = c(-1,0, 1, Inf), labels=c("0", "1","2 or more"))
summary(data_train$BsmtFullBath)
summary(data_test$BsmtFullBath) #2 NA's: obs 661 y 729
barplot(table(data_train$BsmtFullBath))

#Agrupar como 0 y 1 o más ?
data_train$BsmtHalfBath<-cut(data_train$BsmtHalfBath, breaks=c(-1,0,Inf), labels= c("0", "1 or more"))
data_test$BsmtHalfBath<-cut(data_test$BsmtHalfBath, breaks=c(-1,0,Inf), labels= c("0", "1 or more"))
summary(data_train$BsmtHalfBath)
summary(data_test$BsmtHalfBath) #2 NA's: obs 661 y 729
barplot(table(data_train$BsmtHalfBath))


#agrupar: 0,1, 2, 3 o más?
data_train$FullBath<-cut(data_train$FullBath, breaks = c(-1,0,1,2,Inf), labels = c("0","1","2","3 or more"))
data_test$FullBath<-cut(data_test$FullBath,breaks = c(-1,0,1,2,Inf), labels = c("0","1","2","3 or more") )
summary(data_train$FullBath)
summary(data_test$FullBath)
barplot(table(data_train$FullBath))

#agrupar: 0 y 1 o más
data_train$HalfBath<-cut(data_train$HalfBath, breaks = c(-1,0,Inf), labels=c("0", "1 or more"))
data_test$HalfBath<-cut(data_test$HalfBath, breaks = c(-1,0,Inf), labels=c("0", "1 or more"))
summary(data_train$HalfBath)
summary(data_test$HalfBath)
barplot(table(data_train$HalfBath))

#Esta podemos separar tambien en intervalos: menos de 2, 2, 3, 4, 5 o mas (por ejemplo) 
data_train$BedroomAbvGr<-cut(data_train$BedroomAbvGr, breaks = c(-1,1,2,3,4,Inf), labels = c("less than 2", "2", "3", "4", "5 or more"))
data_test$BedroomAbvGr<-cut(data_test$BedroomAbvGr,breaks = c(-1,1,2,3,4,Inf), labels = c("less than 2", "2", "3", "4", "5 or more"))
summary(data_train$BedroomAbvGr)
summary(data_test$BedroomAbvGr)
barplot(table(data_train$BedroomAbvGr))


#Hacer dos categorias: 1 o menos, 2 o más
data_train$KitchenAbvGr<-cut(data_train$KitchenAbvGr, breaks = c(-1,1, Inf), labels = c("1 or less", "2 or more"))
data_test$KitchenAbvGr<-cut(data_test$KitchenAbvGr,breaks = c(-1,1,Inf), labels = c("1 or less", "2 or more"))
summary(data_train$KitchenAbvGr)
summary(data_test$KitchenAbvGr)
barplot(table(data_train$KitchenAbvGr))

#Separar en intervalos mejor: 3 o menos, 4,5,6,7,8,9,10, 11 o más 
data_train$TotRmsAbvGrd<-cut(data_train$TotRmsAbvGrd, breaks = c(-1,3,4,5,6,7,8,9,10,Inf), labels = c("3 or less", "4", "5", "6", "7", "8", "9", "10", "11 or more"))
data_test$TotRmsAbvGrd<-cut(data_test$TotRmsAbvGrd,breaks = c(-1,3,4,5,6,7,8,9,10,Inf), labels = c("3 or less", "4", "5", "6", "7", "8", "9", "10", "11 or more"))
summary(data_train$TotRmsAbvGrd)
summary(data_test$TotRmsAbvGrd)
barplot(table(data_train$TotRmsAbvGrd))


#separar en intervalos: 0, 1, 2, 3 o más
data_train$Fireplaces<-cut(data_train$Fireplaces, breaks = c(-1,0,1,2,Inf), labels = c("0","1", "2","3 or more"))
data_test$Fireplaces<-cut(data_test$Fireplaces,breaks = c(-1,0,1,2,Inf), labels = c("0","1", "2","3 or more"))
summary(data_train$Fireplaces)
summary(data_test$Fireplaces)
barplot(table(data_train$Fireplaces))

#separar en intervalos: 0, 1, 2, 3 o mas 
data_train$GarageCars<-cut(data_train$GarageCars, breaks = c(-1,0,1,2,Inf), labels = c("0","1", "2","3 or more"))
data_test$GarageCars<-cut(data_test$GarageCars, breaks = c(-1,0,1,2,Inf), labels = c("0","1", "2","3 or more"))
summary(data_train$GarageCars)
summary(data_test$GarageCars) #1 NA
barplot(table(data_train$GarageCars))

#Variables numéricas discretas que refieren al mes y año de venta
data_train$MoSold<-as.factor(data_train$MoSold)
data_test$MoSold<-as.factor(data_test$MoSold)
summary(data_train$MoSold)
summary(data_test$MoSold)
barplot(table(data_train$MoSold))


data_train$YrSold<-as.factor(data_train$YrSold)
data_test$YrSold<-as.factor(data_test$YrSold)
summary(data_train$YrSold)
summary(data_test$YrSold)
barplot(table(data_train$YrSold))




##################### DATA_TRAIN ##########################
###### Colocar numérica, binarias y multicategóricas

# Identify numerical, binary, and multicategorical variables
numerical_vars <- names(data_train)[sapply(data_train, is.numeric)]
binary_vars <- names(data_train)[sapply(data_train, function(x) is.factor(x) && length(levels(x)) == 2)]
multicategorical_vars <- names(data_train)[sapply(data_train, function(x) is.factor(x) && length(levels(x)) > 2)]
categorical_vars <- names(data_train)[sapply(data_train,function(x) is.factor(x) && length(levels(x))>=2)]
# Rearrange columns based on variable types
data_train <- data_train %>%
  select(c(sort(numerical_vars), sort(binary_vars), sort(multicategorical_vars)))
# Print the first few rows of the rearranged dataset
head(data_train)
summary(data_train) #81 NA's en GarageYrBlt, 1 NA en Electrical, 8 NA's en MasVnrArea
#y MasVnrType, 259 LotFrontage


#### GARAGEYRBLT (make categories)
summary(data_train$GarageYrBlt)
data_train$GarageYrBlt
#We make categories corresponding to each decade and another category for na's called no tiene
breaks <- c(seq(1900, 2000, 10), Inf)
labels <- paste(seq(1900, 1990, 10), seq(1909, 1999, 10), sep = "-")
labels <- c(labels, "2000-2010")
# Create a new categorical variable for GarageYrBlt
data_train$GarageYrBlt <- cut(
  data_train$GarageYrBlt,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = FALSE
)
# Add "Doesn't have" to the factor levels
levels(data_train$GarageYrBlt) <- c(levels(data_train$GarageYrBlt), "Doesn't have")
# Assign "Doesn't have" category to missing values
data_train$GarageYrBlt[is.na(data_train$GarageYrBlt)] <- "Doesn't have"
#Let's check
summary(data_train$GarageYrBlt)
if (any(is.na(data_train$GarageYrBlt))) {
  print("There are missing values in 'GarageYrBlt'")
} else {
  print("There are no missing values in 'GarageYrBlt'")
}


#### ELECTRICAL (find the category for the missing na)
#Hacemos (knn?) para obtener la categoria de electrical mas adecuada para la observacion del NA
rows_with_na <- which(is.na(data_train$Electrical))
categoria_NA <- data_train[rows_with_na, "Electrical"]
# Inicializar una lista para almacenar el recuento de categorías compartidas por observación
conteo_categorias <- vector("numeric", length = nrow(data_train))
# Calcular el recuento de categorías compartidas en las variables categóricas
for (i in 1:nrow(data_train)) {
  categorias_en_comun <- sum(data_train[i, categorical_vars] == data_train[rows_with_na, categorical_vars])
  conteo_categorias[i] <- categorias_en_comun
}
# Encontrar la observación con más categorías en común
observacion_mas_categorias <- which.max(conteo_categorias)
# Mostrar la observación con más categorías en común
data_train[observacion_mas_categorias, ]
data_train$Electrical[c(1,9,17,25,33,41,49,57,65,73)]
data_train$Electrical[1380]="SBrkr"
#Let's check
if (any(is.na(data_train$Electrical))) {
  print("There are missing values in 'Electrical'")
} else {
  print("There are no missing values in 'Electrical'")
}
data_train$Electrical


#######KNN
# Replace NA values in numerical variables using knn
# Impute missing values using kNN from the 'VIM' package
# Loop through each numerical variable and perform kNN imputation
for (var in numerical_vars) {
  data_train[[var]] <- impute(data_train[[var]], k = 5)
}
# Print the first few rows of the updated dataset
head(data_train)
summary(data_train)

#Check the remaining Na's
sapply(data_train, function(x) sum(is.na(x)))




####ELIMINATE NA'S
variables_con_NA <- colSums(is.na(data_train)) > 0
variables_con_NA <- names(variables_con_NA)[variables_con_NA]
numerical_vars <- names(data_train)[sapply(data_train, is.numeric)]
multicategorical_vars <- names(data_train)[sapply(data_train, function(x) is.factor(x) && length(levels(x)) >=2 )]
lista=list()
for (variable in 'MasVnrType') {#variables_con_NA
  # Encuentra las observaciones con NA en la variable actual
  lista=append(lista,variable)
  frecuencias_variable <- table(data_train[,variable])
  moda <- names(frecuencias_variable)[which.max(frecuencias_variable)]
  lista=append(lista,'moda_variable')
  lista=append(lista,max(frecuencias_variable)/1460)
  observaciones_con_NA <- which(is.na(data_train[[variable]]))
  
  for (observacion in observaciones_con_NA) {#observaciones_con_NA
    conteo_categorias <- numeric()
    
    for (i in 1:nrow(data_train)) {#nrow(data_train)
      
      if (!(i %in% observaciones_con_NA))  {
        categorias_en_comun <- sum(data_train[i, multicategorical_vars] == data_train[observacion, multicategorical_vars], na.rm = TRUE)
        
        conteo_categorias[i] <- categorias_en_comun
        
        #me devuelve NA pq no conoce la 1380 al haberla ignorado
      }
      else {
        conteo_categorias[i]=0
      }
    }
    # Encuentra las observaciones con la misma moda
    indices_maximos <- which(conteo_categorias == max(conteo_categorias))
    electrical_values=data_train[indices_maximos,variable]
    frecuencias <- table(electrical_values)
    moda <- names(frecuencias)[which.max(frecuencias)]
    data_train[observacion,variable]=moda
    lista=append(lista,observacion)
    lista=append(lista,'observaciones_parecidas')
    lista=append(lista,indices_maximos)
    lista=append(lista,'numero_coincidencias')
    lista=append(lista,conteo_categorias[indices_maximos])
    lista=append(lista,length(indices_maximos))
    lista=append(lista,max(frecuencias)/length(indices_maximos))
    # Realiza comparaciones o acciones con las observaciones encontradas
    # Realiza otras acciones según sea necesario con 'indice' y 'observacion'
  }
}


saveRDS(data_train,file='datos_limpios.rds')


#############Error in data
###MascVnr problems
# Check for observations where MasVnrArea is 0 and MasVnrType is not "None"
sum(data_train$MasVnrArea == 0 & data_train$MasVnrType != "None")
obs11=which(data_train$MasVnrArea == 0 & data_train$MasVnrType != "None")
obs
#Check for observations where MasVnrType is 'None' and MasVnrArea y >0
sum(data_train$MasVnrArea > 0 & data_train$MasVnrType == "None")
obs12=which(!is.na(data_train$MasVnrArea) & data_train$MasVnrArea > 0 & !is.na(data_train$MasVnrType) & data_train$MasVnrType == "None")
obs12
###Make the wrong data as NA's
data_train[obs1,c("MasVnrArea", "MasVnrType")]
obs1=c(obs11,obs12)
for (i in obs1){
  data_train[i, "MasVnrType"]<-NA 
  data_train[i, "MasVnrArea"]<- NA
}


###Pool problems
# Check for observations where PoolArea is 0 and PoolQC is not "NoPool"
sum(data_train$PoolArea == 0 & data_train$PoolQC != "Doesn't have")
#All good
#Check for observations whwere PoolQC is ' NoPool' and PoolArea is >0
sum(data_train$PoolArea > 0 & data_train$PoolQC == "Doesn't have")
#All good
sapply(data_train, function(x) sum(is.na(x)))



###Misc problems
#Check for observations where MiscFeature is 'Doesn't have' and MiscValue >0
sum(data_train$MiscVal > 0 & data_train$MiscFeature == "Doesn't have")
#All good
# Check for observations where MiscValue is 0 and MiscFeature is not "Doesn't have"
sum(data_train$MiscVal == 0 & data_train$MiscFeature!= "Doesn't have")
obs2=which(!is.na(data_train$MiscVal) & data_train$MiscVal == 0 & !is.na(data_train$MiscFeature) & data_train$MiscFeature != "Doesn't have")
data_train[which(!is.na(data_train$MiscVal) & data_train$MiscVal == 0 & !is.na(data_train$MiscFeature) & data_train$MiscFeature != "Doesn't have"),c("MiscVal", "MiscFeature")]
#Set them as NA's
for (i in obs2){
  data_train[i, "MiscVal"]<-NA 
  data_train[i, "MiscFeature"]<- NA
}


#Data errors on garage variables
# Define the categorical and numerical variables
categorical_gar <- c("GarageType", "GarageFinish", "GarageQual", "GarageCond")
numerical_gar <- c("GarageCars", "GarageArea")
# Initialize a counter for observations that don't meet the conditions
counter <- 0
# Initialize a vector to store indices of observations that add one to the counter
counter_indices <- c()
# Loop through each observation
for (i in 1:nrow(data_train)) {
  # Check condition 1: If any categorical variable is "Doesn't have," all must be, and numerical variables must be 0
  if (any(data_train[i, categorical_gar] == "Doesn't have") &&
      all(data_train[i, categorical_gar] == "Doesn't have") &&
      all(data_train[i, numerical_gar] == 0)) {
    next  # Move to the next observation
  }
  # Check condition 2: If either numerical variable is 0, both must be, and categorical variables must be "Doesn't have"
  if ((any(data_train[i, numerical_gar] == 0) && all(data_train[i, numerical_gar] == 0)) &&
      all(data_train[i, categorical_gar] == "Doesn't have")) {
    next  # Move to the next observation
  }
  # New condition: If all categorical variables are not "Doesn't have" and all numerical variables are not 0
  if (all(data_train[i, categorical_gar] != "Doesn't have") &&
      all(data_train[i, numerical_gar] != 0)) {
    next  # Move to the next observation
  }
  # If neither condition is met, increment the counter and store the index
  counter <- counter + 1
  counter_indices <- c(counter_indices, i)
}
# Print the number of observations that don't meet the conditions
counter
# Print the indices of observations that add one to the counter
counter_indices
#All good




####COMPUTE THE NA'S COMING FROM THE ERRONEOUS OBSERVATIONS
variables_con_NA <- colSums(is.na(data_train)) > 0
variables_con_NA <- names(variables_con_NA)[variables_con_NA]
numerical_vars <- names(data_train)[sapply(data_train, is.numeric)]
multicategorical_vars <- names(data_train)[sapply(data_train, function(x) is.factor(x) && length(levels(x)) >=2 )]
for (variable in variables_con_NA ) {#LotFrontage
  # Encuentra las observaciones con NA en la variable actual
  
  observaciones_con_NA <- which(is.na(data_train[[variable]]))
  
  for (observacion in observaciones_con_NA) {#observaciones_con_NA
    conteo_categorias <- numeric()
    
    for (i in 1:nrow(data_train)) {#nrow(data_train)
      
      if (!(i %in% observaciones_con_NA))  {
        categorias_en_comun <- sum(data_train[i, multicategorical_vars] == data_train[observacion, multicategorical_vars], na.rm = TRUE)
        
        conteo_categorias[i] <- categorias_en_comun
        
        #me devuelve NA pq no conoce la 1380 al haberla ignorado
      }
      else {
        conteo_categorias[i]=0
        
      }
      
    }
    # Encuentra las observaciones con la misma moda
    indices_maximos <- which(conteo_categorias == max(conteo_categorias))
    electrical_values=data_train[indices_maximos,variable]
    frecuencias <- table(electrical_values)
    indices_segundos_maximos=which(conteo_categorias == max(conteo_categorias)-1)
    indices_terceros_maximos=which(conteo_categorias == max(conteo_categorias)-2)
    electrical_values=data_train[c(indices_maximos,indices_segundos_maximos,indices_terceros_maximos),variable]
    frecuencias <- table(electrical_values)
    moda <- names(frecuencias)[which.max(frecuencias)]
    if(variable %in% multicategorical_vars){
      data_train[observacion,variable]=moda
      
    } 
    else{
      data_train[observacion,variable]=mean(data_train[c(indices_maximos,indices_segundos_maximos,indices_terceros_maximos),variable])
      
    }
  }
}


class(data_train$LotFrontage)
data_train$MasVnrArea=as.numeric(data_train$MasVnrArea)

save



################### DATA_TEST ###################
#Reordenar test
numerical_vars2 <- names(data_test)[sapply(data_test, is.numeric)]
binary_vars2 <- names(data_test)[sapply(data_test, function(x) is.factor(x) && length(levels(x)) == 2)]
multicategorical_vars2 <- names(data_test)[sapply(data_test, function(x) is.factor(x) && length(levels(x)) > 2)]

# Rearrange columns based on variable types
data_test <- data_test %>%
  select(c(sort(numerical_vars2), sort(binary_vars2), sort(multicategorical_vars2)))

#Check where we have missing values
sapply(data_test, function(x) sum(is.na(x)))


# Print the first few rows of the rearranged dataset
head(data_test)
summary(data_test) #1 NA en SaleType, 1 NA en GarageArea, 78 en GarageYrBlt, 
#2 Functional, 1 KitchenQual, 2 en BsmtHalfBath y BsmtFullBath, 1 en TotalBsmtSF,
#BsmtUnfSF, BsmtFinSF2 y BsmtFinSF1, 15 en MASVnrArea,, 16 en MasVnrType, 
#1 Exterior2nd, 1 Exterior1st, 2 Utilities, 227 LotFrontge, 4 MSZoning

#### GARAGEYRBUILT
#We make categories corresponding to each decade and another category for na's called no tiene
breaks <- c(seq(1900, 2000, 10), Inf)
labels <- paste(seq(1900, 1990, 10), seq(1909, 1999, 10), sep = "-")
labels <- c(labels, "2000-2010")
# Create a new categorical variable for GarageYrBlt
data_test$GarageYrBlt <- cut(
  data_test$GarageYrBlt,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = FALSE
)
# Add "Doesn't have" to the factor levels
levels(data_test$GarageYrBlt) <- c(levels(data_test$GarageYrBlt), "Doesn't have")
# Assign "Doesn't have" category to missing values
data_test$GarageYrBlt[is.na(data_test$GarageYrBlt)] <- "Doesn't have"
#Let's check
summary(data_test$GarageYrBlt)
if (any(is.na(data_test$GarageYrBlt))) {
  print("There are missing values in 'GarageYrBlt'")
} else {
  print("There are no missing values in 'GarageYrBlt'")
}


#GarageCars
sum(is.na(data_test$GarageCars))
# Find the indices of rows with NA values in GarageCars
na_garagecars <- which(is.na(data_test[, "GarageCars"]))
data_test[na_garagecars, "GarageCars"]
#Fix the NA
# Impute missing values using kNN from the 'Hmisc' package
data_test[na_garagecars, "GarageCars"] <- impute(data_test[na_garagecars, "GarageCars"], k = 5)
# Verify that missing values are imputed
print(sum(is.na(data_test[, "GarageCars"])))



#Numerical NA's
for (var in numerical_vars2) {
  data_test[[var]] <- impute(data_test[[var]], k = 5)
}
# Print the first few rows of the updated dataset
head(data_test)
summary(data_test)

#Check the remaining Na's
sapply(data_test, function(x) sum(is.na(x)))


variables_con_NA <- colSums(is.na(data_test)) > 0
variables_con_NA <- names(variables_con_NA)[variables_con_NA]
numerical_vars <- names(data_test)[sapply(data_test, is.numeric)]
multicategorical_vars <- names(data_test)[sapply(data_test, function(x) is.factor(x) && length(levels(x)) >=2 )]
lista=list()
for (variable in variables_con_NA) {#variables_con_NA
  # Encuentra las observaciones con NA en la variable actual
  lista=append(lista,variable)
  frecuencias_variable <- table(data_test[,variable])
  moda <- names(frecuencias_variable)[which.max(frecuencias_variable)]
  lista=append(lista,'moda_variable')
  lista=append(lista,max(frecuencias_variable)/1460)
  observaciones_con_NA <- which(is.na(data_test[[variable]]))
  
  for (observacion in observaciones_con_NA) {#observaciones_con_NA
    conteo_categorias <- numeric()
    
    for (i in 1:nrow(data_test)) {#nrow(data_train)
      
      if (!(i %in% observaciones_con_NA))  {
        categorias_en_comun <- sum(data_test[i, multicategorical_vars] == data_test[observacion, multicategorical_vars], na.rm = TRUE)
        
        conteo_categorias[i] <- categorias_en_comun
        
        #me devuelve NA pq no conoce la 1380 al haberla ignorado
      }
      else {
        conteo_categorias[i]=0
      }
    }
    # Encuentra las observaciones con la misma moda
    indices_maximos <- which(conteo_categorias == max(conteo_categorias))
    electrical_values=data_test[indices_maximos,variable]
    frecuencias <- table(electrical_values)
    moda <- names(frecuencias)[which.max(frecuencias)]
    data_test[observacion,variable]=moda
    lista=append(lista,observacion)
    lista=append(lista,'observaciones_parecidas')
    lista=append(lista,indices_maximos)
    lista=append(lista,'numero_coincidencias')
    lista=append(lista,conteo_categorias[indices_maximos])
    lista=append(lista,length(indices_maximos))
    lista=append(lista,max(frecuencias)/length(indices_maximos))
    # Realiza comparaciones o acciones con las observaciones encontradas
    # Realiza otras acciones según sea necesario con 'indice' y 'observacion'
  }
}





#Error in data
###Vnr problems
# Check for observations where MasVnrArea is 0 and MasVnrType is not "None"
sum(data_test$MasVnrArea == 0 & data_test$MasVnrType != "None")
obs0=which(!is.na(data_test$MasVnrArea) & data_test$MasVnrArea == 0 & !is.na(data_test$MasVnrType) & data_test$MasVnrType != "None")
data_test[which(!is.na(data_test$MasVnrArea) & data_test$MasVnrArea == 0 & !is.na(data_test$MasVnrType) & data_test$MasVnrType != "None"),c("MasVnrArea", "MasVnrType")]
#Check for observations where MasVnrType is 'None' and MasVnrArea y >0
sum(data_test$MasVnrArea > 0 & data_test$MasVnrType == "None")
obs3=which(!is.na(data_test$MasVnrArea) & data_test$MasVnrArea > 0 & !is.na(data_test$MasVnrType) & data_test$MasVnrType == "None")
data_test[obs3,c("MasVnrArea", "MasVnrType")]
#Set them as NA's
for (i in obs3){
  data_test[i,'MasVnrArea'] = NA
  data_test[i,'MasVnrType'] = NA
}


###Pool problems
# Check for observations where PoolArea is 0 and PoolQC is not "NoPool"
sum(data_test$PoolArea == 0 & data_test$PoolQC != "Doesn't have")
#All good
#Check for observations whwere PoolQC is ' NoPool' and PoolArea is >0
sum(data_test$PoolArea > 0 & data_test$PoolQC == "Doesn't have")
obs4=which(!is.na(data_test$PoolArea) & data_test$PoolArea > 0 & !is.na(data_test$PoolQC) & data_test$PoolQC == "Doesn't have")
data_test[obs4,c("PoolQC", "PoolArea")]
#Set as NA's
for (i in obs4){
  data_test[i,'PoolQC']=NA
  data_test[i,'PoolArea']=NA
}


###Misc problems
#Check for observations where MiscFeature is 'Doesn't have' and MiscValue >0
sum(data_test$MiscVal > 0 & data_test$MiscFeature == "Doesn't have")
obs5_1=which(!is.na(data_test$MiscVal) & data_test$MiscVal > 0 & !is.na(data_test$MiscFeature) & data_test$MiscFeature == "Doesn't have")
data_test[obs5_1,c("MiscVal", "MiscFeature")]
# Check for observations where MiscValue is 0 and MiscFeature is not "Doesn't have"
sum(data_test$MiscVal == 0 & data_test$MiscFeature!= "Doesn't have")
obs5=c(obs5_1,which(!is.na(data_test$MiscVal) & data_test$MiscVal == 0 & !is.na(data_test$MiscFeature) & data_test$MiscFeature != "Doesn't have"))
data_test[obs5,c("MiscVal", "MiscFeature")]
for (i in obs5){
  data_test[i,'MiscFeature']=NA
  data_test[i,'MiscVal']=NA
}






#Data errors on garage variables
# Define the categorical and numerical variables
categorical_gar <- c("GarageType", "GarageFinish", "GarageQual", "GarageCond")
numerical_gar <- c("GarageCars", "GarageArea")
# Initialize a counter for observations that don't meet the conditions
counter <- 0
counter_indices = c()
# Loop through each observation
for (i in 1:nrow(data_test)) {
  # Check condition 1: If any categorical variable is "Doesn't have," all must be, and numerical variables must be 0
  if (any(data_test[i, categorical_gar] == "Doesn't have") &&
      all(data_test[i, categorical_gar] == "Doesn't have") &&
      all(data_test[i, numerical_gar] == 0)) {
    next  # Move to the next observation
  }
  # Check condition 2: If either numerical variable is 0, both must be, and categorical variables must be "Doesn't have"
  if ((any(data_test[i, numerical_gar] == 0) && all(data_test[i, numerical_gar] == 0)) &&
      all(data_test[i, categorical_gar] == "Doesn't have")) {
    next  # Move to the next observation
  }
  # New condition: If all categorical variables are not "Doesn't have" and all numerical variables are not 0
  if (all(data_test[i, categorical_gar] != "Doesn't have") &&
      all(data_test[i, numerical_gar] != 0)) {
    next  # Move to the next observation
  }
  # If neither condition is met, increment the counter
  counter <- counter + 1
  counter_indices=c(counter_indices,i)
}
counter
counter_indices
data_test[counter_indices,c(categorical_gar,numerical_gar)]

for (i in counter_indices){
  data_test[i,categorical_gar]=NA
  data_test[i,numerical_gar]=NA
}


data_test[counter_indices,c(categorical_gar,numerical_gar)]


sapply(data_test, function(x) sum(is.na(x)))


####COMPUTE THE NA'S COMING FROM THE ERRONEOUS OBSERVATIONS
variables_con_NA <- colSums(is.na(data_test)) > 0
variables_con_NA <- names(variables_con_NA)[variables_con_NA]
numerical_vars <- names(data_test)[sapply(data_test, is.numeric)]
multicategorical_vars <- names(data_test)[sapply(data_test, function(x) is.factor(x) && length(levels(x)) >=2 )]
for (variable in variables_con_NA ) {#LotFrontage
  # Encuentra las observaciones con NA en la variable actual
  
  observaciones_con_NA <- which(is.na(data_test[[variable]]))
  
  for (observacion in observaciones_con_NA) {#observaciones_con_NA
    conteo_categorias <- numeric()
    
    for (i in 1:nrow(data_test)) {#nrow(data_train)
      
      if (!(i %in% observaciones_con_NA))  {
        categorias_en_comun <- sum(data_test[i, multicategorical_vars] == data_test[observacion, multicategorical_vars], na.rm = TRUE)
        
        conteo_categorias[i] <- categorias_en_comun
        
        #me devuelve NA pq no conoce la 1380 al haberla ignorado
      }
      else {
        conteo_categorias[i]=0
        
      }
      
    }
    # Encuentra las observaciones con la misma moda
    indices_maximos <- which(conteo_categorias == max(conteo_categorias))
    electrical_values=data_test[indices_maximos,variable]
    frecuencias <- table(electrical_values)
    indices_segundos_maximos=which(conteo_categorias == max(conteo_categorias)-1)
    indices_terceros_maximos=which(conteo_categorias == max(conteo_categorias)-2)
    electrical_values=data_test[c(indices_maximos,indices_segundos_maximos,indices_terceros_maximos),variable]
    frecuencias <- table(electrical_values)
    moda <- names(frecuencias)[which.max(frecuencias)]
    if(variable %in% multicategorical_vars){
      data_test[observacion,variable]=moda
      
    } 
    else{
      data_test[observacion,variable]=mean(data_test[c(indices_maximos,indices_segundos_maximos,indices_terceros_maximos),variable])
      
    }
  }
}

sapply(data_test, function(x) sum(is.na(x)))
sum(sapply(data_test, function(x) sum(is.na(x))))


saveRDS(data_test,file='data_test_limpio.rds')


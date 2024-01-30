data_train=read.csv('train.csv')
vars_with_na <- names(which(colSums(is.na(data_train)) > 0))
NAs<- c("Alley", "BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1",
        "BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual",
        "GarageCond","PoolQC","Fence","MiscFeature")

for (i in NAs){
  data_train[[i]]<- ifelse(is.na(data_train[[i]]), "No tiene", data_train[[i]])
  }
character_columns <- sapply(data_train, is.character)
data_train[, character_columns] <- lapply(data_train[, character_columns], as.factor)
#hasta aquí es lo que hicimos juntos el primer día 
datos_sin_NA <- na.omit(data_train) #me quito los NA y listo, se pierden unas 300 observaciones. 
datos_sin_NA=datos_sin_NA[,-1] #la columna id no aporta nada, la quito 

modall=lm(SalePrice~.,data=datos_sin_NA)
#da error porque tengo una variable categótica con un solo factor
#el siguiente código lo uso para descubrir qué variable es la que tiene un solo factor
for (col in names(datos_sin_NA)) {
  if (is.factor(datos_sin_NA[[col]]) && length(unique(datos_sin_NA[[col]])) <= 1) {
    print(paste("Variable con un solo factor:", col))
    num_columna <- which(colnames(datos_sin_NA) == col)
    print(num_columna)
  }
}
#debe ser qu solo se me quedan ALLPUb al quitar los NA, por ahora la elimino y listo.
datos_sin_utilities <- datos_sin_NA[,-9] #quito la variable utilities, que es la que me ha salido con un factor
library(caret)

set.seed(100)
#set.seed(1234)
#set.seed(1014)
#probar 3 semillas por ejemplo comunes a todos para garantizar las mejoras de los modelos 
trainRowNumbers = createDataPartition(datos_sin_utilities$SalePrice, p=0.9, list=FALSE)
#Create the training dataset
trainData = datos_sin_utilities[trainRowNumbers,]
#Create the test dataset
testData = datos_sin_utilities[-trainRowNumbers,]
#esta division, si profe no deja usar nada que no sea esttrictamente de su curso
#se puede hacer a mano fácilmente como lo hace ella en el ejemplo de los coches
#(hoja 6 del PDF de la tarea )
modall=lm(SalePrice~.,data=trainData)
summary(modall)
# RMSE of train dataset
RMSE(pred = (modall$fitted.values), obs = (trainData$SalePrice))
# RMSE in the test dataset
mod_pred <- predict(modall, newdata = testData)
RMSE(pred = (mod_pred), obs = (testData$SalePrice))
#IMPORTANTE: SI SELECCIONO p MAS PEQUEÑO EN TRAINROWNUMBERS, LO QUE PASA ES QUE,p SEGUN EL SEED 
#COGIDO, HAY CIERTAS CATEGORIAS DE LAS VARIABLES CATEGORICAS QUE NO SE COGÍAN 
#EN EL TRAIN DATASET Y QUE RESULTA QUE SI APARECEN EN EL TEST DATASET.
#Y POR TANTO NO PUEDE PREDECIR. HAY QUE ARREGLAR ESO.
#YO PARA IR RAPIDO HE METIDO MAS EN EL TRAIN Y MENOS EN EL TEST PARA QUE ASÍ SEA
#CASI IMPOSIBLE QUE ESTO ME OCURRA, PERO NO CREO QUE SEA  EFICIENTE. 
#EN TODO CASO, COMO EL MODELO QUE NOSTOROS UTILIZAREMOS EMPLEA TODOS LOS DATOS PARA 
#ENTRENAR, CUANDO ELLA TESTEE CON LOS SUYOS NO LE APARECERAN CATEGORÍAS NUEVAS,
#PORQUE NOSOTROS COGIMOS TODAS, POR LO QUE ESTE PROBLEMA NO SE DARÁ.
#PODRÍA DARSE SI AL ELIMINAR LOS NA HAY ALGUNA CATEGORÍA QUE NO VUELVE A SALIR
#SI EN VEZ DE QUITAR LOS NA LOS SUSTITUIMOS POR OTRA COSA ESTO NO NOS PASARÁ 
#Y EL PROBLEMA QUEDA SOLUCIONADO.


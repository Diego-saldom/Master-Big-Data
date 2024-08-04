#En primer lugar importamos los datos

#Fuente: insideairbnb.com

df <- read.csv("C:/Users/Administrador/Documents/data_science/databases/listings.csv")
geojson <- geojson_read("C:/Users/Administrador/Documents/data_science/databases/neighbourhoods.geojson", what = "sp")

#definimos la funcion de moda

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

#tratamiento de datos

set.seed(1) #protegemos la reproducibilidad del modelo

df <- df[c( "neighbourhood", "latitude", "longitude" , "room_type", "price", "minimum_nights", "number_of_reviews",
            "availability_365")] #tomamos las columnas con valor predicitivo
df <- na.omit(df) #omitimos las filas que contienen valores desconocidos


sf_geojson <- st_as_sf(geojson) #convertimos las coordenadas geojson a un objeto simple feature
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84") #conjunto de datos como simple feature


#visualización de datos

mapa_vacio  <- leaflet() %>% addTiles()  %>%  setView(lng = -5.97,  lat = 37.38, zoom = 11)

mapa_distritos <- mapa_vacio %>%  addPolygons(data = geojson, color = "#444444", weight = 2, opacity = 1)

mapa_plano <- ggplot() +
  geom_polygon(data = geojson, aes(x = long, y = lat, group = group), fill = "#69b3a2", color = "blue") +
  theme_void() +
  coord_map()

mapa_airbn <- mapa_distritos %>%  addCircleMarkers(  lng = df$longitude, 
                                                     lat = df$latitude,
                                                     radius = 2, 
                                                     stroke = FALSE,
                                                     color = "#0e0e70",
                                                     fillOpacity = 0.5, 
                                                     group = "otras")


### I: Estimación por el método de voronoi

#realizamos la particion de datos de testeo y entrenamiento

voro <- df %>% st_as_sf(coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84") 
particiones <- createDataPartition(df$price, times=1, list=FALSE, p=0.8)
train_voro1 <- df[particiones,]
train2_voro1<-df[particiones,] %>% st_as_sf(coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84") 
test_voro1 <- df[-particiones,] %>% st_as_sf(coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84") 


#agregamos las variables por barrio

df_cont <- train_voro1[c( "neighbourhood", "latitude", "longitude", "price", 
                 "minimum_nights", "number_of_reviews", "availability_365")] #variables cotinuas (+ barrios)
df_dis <-train_voro1[c("neighbourhood", "room_type")]  #variables discretas

cont_agg <- aggregate(df_cont, list(df_cont$neighbourhood), FUN = mean ) 
cont_agg <- cont_agg[-2]

dis_agg <- aggregate(df_dis, list(df_dis$neighbourhood), FUN = Mode )
dis_agg <- dis_agg[-2]

agg_df <- left_join(dis_agg, cont_agg, by="Group.1") 
colnames(agg_df)[1] <- "neighbourhood"

agg_sf <-  st_as_sf(agg_df, coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84") 

#igual para los datos de entrenamiento

df_cont <- test_voro1[c( "neighbourhood", "latitude", "longitude", "price", 
                          "minimum_nights", "number_of_reviews", "availability_365")] #variables cotinuas (+ barrios)
df_dis <-test_voro1[c("neighbourhood", "room_type")]  #variables discretas

cont_agg <- aggregate(df_cont, list(df_cont$neighbourhood), FUN = mean ) 
cont_agg <- cont_agg[-2]

dis_agg <- aggregate(df_dis, list(df_dis$neighbourhood), FUN = Mode )
dis_agg <- dis_agg[-2]

agg_df2 <- left_join(dis_agg, cont_agg, by="Group.1") 
colnames(agg_df)[1] <- "neighbourhood"

agg_sf2 <-  st_as_sf(agg_df, coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84") 

#creamos los polígonos de voronoi por barrio

vect_sf <- vect(agg_sf) #convertimos las coordenadas de los barrios en objetos de vector

voro1 <- voronoi(vect_sf)   #puntos de voronoi formados por los barrios
voro1 <- st_as_sf(voro1)

#igual para los datos de entrenamiento

vect_sf <- vect(agg_sf2) #convertimos las coordenadas de los barrios en objetos de vector

voro2 <- voronoi(vect_sf)   #puntos de voronoi formados por los barrios
voro2 <- st_as_sf(voro2)

#y también los datos originales

vect_sf <- vect(voro)
voro <- voronoi(vect_sf)
voro <- st_as_sf(voro)


#visualizamos el mapa de las curvas de voronoi
voro_map <- mapa_vacio %>%  addPolygons(data = voro, color = "#444444", weight = 2, opacity = 1)
voro_ma_barrios <- mapa_vacio %>%  addPolygons(data = voro1, color = "#444444", weight = 2, opacity = 1)


#funcion de estimacion del precio por Voronoi

voro_price <- function(data, sample){
  indice <- c(1:length(sample$geometry))
  precios <- c()
  for(i in indice) { 
    conts <- st_contains(data$geometry, sample$geometry[i]) %>% as.character()
    pos <- which(conts != "integer(0)")
    precios[i]<-data$price[pos]
  }
    result <- data.frame( precios)
    return(result)
}


### II: Estimación K-Nearest neighbours

coorp<- df[c("longitude", "latitude", "price")] 

train_knn <- coorp[particiones,]
test_knn <- coorp[-particiones,]

KNN<- function(data, sample,k){
  precios <- c()
  for(i in 1:dim(sample)[1]){
    dists <- distGeo(data[c("longitude", "latitude")], sample[i,c("longitude", "latitude")]) #matriz distancias
    Knear <- sort(dists)[1:k]
    pos <- match(Knear, dists)
    
    precio <- data$price[pos] %>% mean()
    precios[i] <- precio

  }
  return(precios)
}

### III: Inverse distance weighting

train_IDW<- df[particiones,]
test_IDW<-df[-particiones,]

IDW <- function(data, sample){
  precios <- c()
  data <- as_tibble(data)
  for(i in 1:length(sample[,1])){
    dists <- distGeo(data[c("longitude", "latitude")], sample[i,c("longitude", "latitude")]) #matriz distancias
    dists[dists==0] <- 0.00000000000000000000001 #no dividir por 0
    num <- sum(data$price/dists) 
    den <- sum(1/dists)
    precio <- num/den
    precios[i] <-  precio
    }
  return(precios)
}

### IV: Support vector machines

#preprocesamos los datos 
dummy_roomtypes <- dummy_cols(df$room_type)
dummy_barrios <- dummy_cols(df$neighbourhood)
df_model  <- bind_cols(df,dummy_roomtypes)
df_model  <- bind_cols(df_model,dummy_barrios)
for(col in 1:length( colnames(df_model))){
  colnames(df_model)[col]<- gsub ("data_","",colnames(df_model)[col])
  colnames(df_model)[col]<- gsub ("[.,]","",colnames(df_model)[col])
  colnames(df_model)[col]<- gsub ("[/ ]","_",colnames(df_model)[col])
}
df_model <- df_model[colnames(df_model)[-c(1,2,3,4,9,14)]]
df_model[,-1] <- preProcess(df_model[,-1], method= "range") %>% predict(newdata=df_model[,-1])

train_model <- df_model[particiones,]
test_model  <- df_model[-particiones,]


#construimos el modelo
trcontrol <- trainControl( #parámetros de entrenamiento
  method = "cv",
  number = 5 )

lm_model <- caret::train(price~.,data=train_model, trcontrol=trcontrol, method="lm", preprocess="null")
SVM_model <- caret::train(price~.,data=train_model, trcontrol=trcontrol, method="svmLinear", preprocess="null")
M5_hiperparams <- expand.grid(pruned="Yes",smoothed="Yes", rules="No")
M5_model <- caret::train(price~.,data=data.frame(train_model), method="M5", tuneGrid=M5_hiperparams)

### V: Evaluación de modelos

#predicciones
pred_voronoi1 <- voro_price(voro1, test_voro1)

k <- length(train_knn$price)^0.5/2 #regla de oro para estimar el valor de K

pred_KNN <- KNN(train_knn, test_knn,k)
pred_IDW <- IDW(train_IDW,test_IDW)

pred_lm <- predict(lm_model, newdata = test_model)
pred_SVM <- predict(SVM_model, newdata= test_model)
pred_M5 <- predict(M5_model, newdata= test_model)

preds <- data.frame(pred_voronoi1,pred_KNN,pred_IDW,#resumen de predicciones (+ground truth)
                    pred_lm,pred_SVM,pred_M5, df$price[-particiones])
colnames(preds) <- c("VORONOI", "KNN", "IDW", "LINEARES", "SVM", "M5", "GROUND_TRUTH")
rownames(corpreds) <- colnames(corpreds)
corpreds <- cor(preds) %>% as_tible()
corpreds_plot <- preds %>% cor()  %>%  ggcorrplot() #matriz de correlación de predicciones

# calculamos el coeficiente determinación/cuadrados ordinarios

r2_voronoi1 <- R2(test_voro1$price, pred_voronoi1)
r2_KNN <- R2(test_knn$price, pred_KNN)
r2_IDW <- R2(test_IDW$price, pred_IDW)
r2_lm <- R2(test_model$price, pred_lm)
r2_SVM <- R2(test_model$price,pred_SVM)
r2_M5 <- R2(test_model$price, pred_M5)



### VI: Creamos un modelo de Stacking

#predecimos las instancias de entrenamiento con los modelos hijos

pred2_voro <- voro_price(voro1, train2_voro1)
pred2_knn <- KNN(train_knn, train_knn, k)
pred2_IDW <- IDW(train_knn, train_knn)
pred2_lm <- predict(lm_model, newdata = train_model)
pred2_SVM <- predict(SVM_model, newdata = train_model)
pred2_M5 <- predict(M5_model, newdata = train_model)


train_stack <- data.frame(pred2_voro,pred2_knn,pred2_IDW,pred2_lm,
                     pred2_SVM,pred2_M5, df$price[particiones])

colnames(train_stack) <-  c("VORONOI", "KNN", "IDW", "LINEARES", "SVM", "M5", "GROUND_TRUTH")

corpreds2 <- cor(train_stack) %>% as_tibble()  #comporbamos que no hay fallas de colinealidad
stacking <- lm("GROUND_TRUTH~.", train_stack) #entrenamos el modelo de stacking con dichas predicciones


pred_stack <- predict(stacking, newdata= preds) # predecimos la muestra de testeo con el stacking
r2_stack <- R2(test_model$price, pred_stack) #testeamos el modelo

r2_resumen <- list( voro1 = r2_voronoi1, 
                    KNN = r2_KNN, 
                    IDW= r2_IDW, 
                    linear_regression = r2_lm, 
                    SVM= r2_SVM,
                    M5 = r2_M5,
                    Stack = r2_stack)
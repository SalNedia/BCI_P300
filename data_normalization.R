#my_data= lista contenete training e test ottenuti 
#utilizzando la funzione split

data_normalization <- function(my_data,scale_values){
  
  data_test <- my_data$test_set[,1:(ncol(my_data$test_set)-2)]
  label_Y_test <- my_data$test_set[,ncol(my_data$test_set)]
  label_C_test <- my_data$test_set[,ncol(my_data$test_set)-1]
  
  data_train <- my_data$training_set[,1:(ncol(my_data$training_set)-2)]
  label_Y_train <- my_data$training_set[,ncol(my_data$training_set)]
  label_C_train <- my_data$training_set[,ncol(my_data$training_set)-1]
  
  scaled_training <- scale(data_train,center = T,scale = T) #se lo scale è FALSE faccio x_i - la media
  
  scale_values <- rbind(attr(scaled_training, "scaled:center"),
                               attr(scaled_training, "scaled:scale"))
  
  scaled_test <- scale(data_test, scale_values[1,], scale_values[2,])
  
  output <- list(scaled_training,label_C_train,label_Y_train,scaled_test,label_C_test,label_Y_test)
  names(output) <- c("training_set","C_train","Y_train","test_set","C_test","Y_test")
  return(output)
}



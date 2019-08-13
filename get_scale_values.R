get_scale_values <- function(my_data)
{
  data_train <- my_data$training_set[,1:(ncol(my_data$training_set)-2)]
  label_Y_train <- my_data$training_set[,ncol(my_data$training_set)]
  label_C_train <- my_data$training_set[,ncol(my_data$training_set)-1]
  
  
  scaled_training <- scale(data_train, center = T, scale = T)
  
  return(rbind(attr(scaled_training, "scaled:center"),
               attr(scaled_training, "scaled:scale")))
}
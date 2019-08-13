#Risplitto il mio dataset dividendo il dataset in training e label cosi da poter applicare 
#facilmente le funzioni di cross validation 

split_label <- function(my_data){
  data_train <- my_data$training_set[,1:(ncol(my_data$training_set)-2)]
  label_Y_train <- my_data$training_set[,ncol(my_data$training_set)]
  label_C_train <- my_data$training_set[,ncol(my_data$training_set)-1]
  
  data_test <- my_data$test_set[,1:(ncol(my_data$test_set)-2)]
  label_Y_test <- my_data$test_set[,ncol(my_data$test_set)]
  label_C_test <- my_data$test_set[,ncol(my_data$test_set)-1]
  
  
  output <- list(data_train,label_C_train,label_Y_train,data_test,label_C_test,label_Y_test)
  names(output) <- c("training_set","C_train","Y_train","test_set","C_train","Y_train")
  return(output)
}
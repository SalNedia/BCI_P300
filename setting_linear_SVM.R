setting_linear_SVM <- function(my_list,char_train,dim_char,n_char,dimension,type="full"){
  
  if (all(type == "full")){
    dim_fold = n_char* dim_char * dimension
  }else if (all(type == "mean")){
    dim_fold = n_char*dim_char
  }

  #Supponiamo di voler provare tre diversi valori di c
  c_vector <- c(10,1,0.1,100,0.001)
  n_cost <- 1:5
  iteration <- floor(char_train/n_char)

  #definiamo una lista contenente 3 matrici, in ognuna delle quali salveremo 
  #accuracy e percentuale di istanze classificate correttamente per ogni classe 
  #per ogni run della cross validation (righe della matrice = run della croos validation,
  #colonne = 4 parametri)
  
  classification_parameter <- lapply(n_cost,function(n_classifiers)matrix(0, nrow = iteration, ncol = 1))
  names(classification_parameter) <- c("c_10","c_1","c_0.1","c_100","c_0.001")
  for(j in 1:length(c_vector)){
    for (i in 0:(iteration-1)) {
      #scorriamo il train scorrendo di n_char * dim_char per volta
      k <- ((dim_fold*i+1): ((dim_fold)*(i+1)))
      train <- my_list$training_set[-k, ]
      validation <- my_list$training_set[k, ]
      
      #parte train rimane la stessa tranne il fold selezionato ad ogni iterazione
      Y_train <- my_list$Y_train[-k]
      C_train <- my_list$C_train[-k]
      
      #costruisco validation_test sui caratteri all'interno del fold selezionato
      Y_validation <- my_list$Y_train[k]
      C_validation <- my_list$C_train[k]
      
      classification_parameter[[j]][i+1,] <- c_linear(train,Y_train,C_train,validation,Y_validation,C_validation,c_vector[j],n_char,dimension,type)
      
    }
  }
  #per ogni matrice con i risultati,
  #sostituiamo eventuali nan con 0 e calcoliamo la media per ciascuno dei parametri

  mean_parameter <- lapply(classification_parameter, function(x){
    x <- replace(x, is.na(x), 0)
    parameter_mean <- apply(x,2,mean)
    return(parameter_mean)
  })
  my_output <- do.call("rbind",mean_parameter)
  my_output <- cbind(my_output,c_vector)
  colnames(my_output) <- c("Avg_Accuracy","c_values")
  return(my_output) 
}
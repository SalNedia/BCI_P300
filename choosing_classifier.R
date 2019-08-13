choosing_classifier <- function(my_list,char_train,dim_char,n_char,dimension,type){
  set.seed(123)
  
  #Supponiamo di voler provare tre classificatori: 
  #SVM con Kernel lineare,SVM con kernel gaussiano,
  #e una SVM con kernel polinomiale utilizzando la 
  #cross validation sui dati di training per individuare il miglior 
  #classificatore

  n_classifiers <- 1:3
  
  # numero di fold su cui testare 
  n_folds <- floor(char_train/n_char)
 
  
  #creiamo una lista contenente 3 matrici, in ognuna delle quali salveremo 
  #accuracy per ogni run della cross validation (righe della matrice = run della croos validation,
  #colonne = 1 parametri)
  
  classification_parameter <- lapply(n_classifiers,function(n_classifiers)matrix(0, nrow = n_folds, ncol = 1))
  #costruisco 3 matrici una per ogni classificatore attraverso le quali mi calcolo l'accuratezza complessiva.
  
  names(classification_parameter) <- c("Linear_SVM","Gaussian_SVM","Polinomial_SVM")
  
  rows_char = n_char*dim_char*dimension
  
  for (j in 0:(n_folds-1)) {
      #scorriamo il train scorrendo di n_char * dim_char per volta
      k <- ((rows_char*j+1): ((rows_char)*(j+1)))
      train <- my_list$training_set[-k, ]
      validation <- my_list$training_set[k, ]
      
      #parte train rimane la stessa tranne il fold selezionato ad ogni iterazione
      Y_train <- my_list$Y_train[-k]
      C_train <- my_list$C_train[-k]
    
      #costruisco validation_test sui caratteri all'interno del fold selezionato
      Y_validation <- my_list$Y_train[k]
      C_validation <- my_list$C_train[k]
      
      #Dopo aver visto le migliori prestazioni del linear e la migliore sicurezza nella scelta dei target di quest'ultimo, nel caso "full" abbiamo
      #deciso di fare cross-validation solo sull'SVM lineare. Se si vuole vedere le prestazioni con tutti e 3 i classificatori usare type "mean" 
      #precisando che il gaussian e il polinomial sono stati implementati con il segno data la difficoltà di ottenere il max. infatti abbiamo 
      #notato nel polinomial l'assoluta incapacità della macchina di predire in maniera efficace i target, ponendo tutti i decision value a 1 nella
      #maggior parte dei casi.
      
      if (all(type == "full")){
        classification_parameter$Linear_SVM[j+1,] <- linear_SVM(train,Y_train,C_train,validation,Y_validation,C_validation,n_char,dimension,type)
        #classification_parameter$Gaussian_SVM[j+1,] <- gaussian_SVM(train,Y_train,C_train,validation,Y_validation,C_validation,n_char,dimension,type)
        #classification_parameter$Polinomial_SVM[j+1,] <- polinomial_SVM(train,Y_train,C_train,validation,Y_validation,C_validation,n_char,dimension,type)
      }else if (all(type == "mean")){
        classification_parameter$Linear_SVM[j+1,] <- linear_SVM(train,Y_train,C_train,validation,Y_validation,C_validation,n_char,dimension,type)
        classification_parameter$Gaussian_SVM[j+1,] <- gaussian_SVM(train,Y_train,C_train,validation,Y_validation,C_validation,n_char,dimension,type)
        classification_parameter$Polinomial_SVM[j+1,] <- polinomial_SVM(train,Y_train,C_train,validation,Y_validation,C_validation,n_char,dimension,type)
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
  colnames(my_output) <- c("Avg_Accuracy")
  return(my_output)
}

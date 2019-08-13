split <- function(my_data,tot_char,test_char,dimension,splitType){
  set.seed(123)
  
  min_test = sample(1:(tot_char - test_char), 1, replace=T)       # scelgo un indice di minimo per il test_set
  max_test = (min_test + (test_char-1))                           # in base al numero di caratteri che voglio nel test calcolo l'indice di max del test
  train_min = 1:(min_test-1)                                      # train_min va da 1 al min del test -1, non considero il primo carattere del test cosi 
  train_max = (max_test+1):tot_char                               # train_max va dal min al max del test +11, non considero l'ultimo carattere del test cosi 
  
  #caso "full": equivalente con la dimension da considerare 
  if (all(splitType == "full")){
    min_test = (min_test * dimension)+1
    max_test = (max_test * dimension)+ dimension
    train_min = 1:(min_test-1)
    train_max = (max_test+1):(tot_char*dimension)
  }
  
  #le istanze vengono raggruppate in base al valore della classe
  #per ognuno dei gruppi cosi creato si procede allo split training test
  output <- by(my_data, as.factor(my_data$C) ,function(x){
    test <- x[(min_test):max_test, ]
    train_1 <- x[(train_min), ] # train1 va da 1 al carattere prima del min di test
    train_2 <- x[(train_max), ] # va dal carattere successivo al max di test fino alla fine 
    train <- rbind(train_1,train_2) #unione dei due train per formare il train finale
    my_split <- list(train,test)
    names(my_split) <- c("train","test")
    return(my_split)
  })
  
  #Raggruppo e splitto per C
  training_set <- rbind(output[[1]]$train,output[[2]]$train,output[[3]]$train,
                        output[[4]]$train,output[[5]]$train,output[[6]]$train,
                        output[[7]]$train,output[[8]]$train,output[[9]]$train,
                        output[[10]]$train,output[[11]]$train,output[[12]]$train)
  
  test_set <-  rbind(output[[1]]$test,output[[2]]$test,output[[3]]$test,
                           output[[4]]$test,output[[5]]$test,output[[6]]$test,
                           output[[7]]$test,output[[8]]$test,output[[9]]$test,
                           output[[10]]$test,output[[11]]$test,output[[12]]$test)
  
  
  data_split <- list(training_set,test_set)
  names(data_split) <- c("training_set","test_set") 
  return(data_split)
}

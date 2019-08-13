test_to_execute <- function(our_model,our_c,pathX,pathY,pathC,scale_values,type){
  
  #Caricamento dati del test
  C <- read.csv(pathC,header = F, stringsAsFactors = F)
  Y <- read.csv(pathY,header = F, stringsAsFactors = F)
  X <-  read.csv(pathX,header = F, stringsAsFactors = F)
  
  #Parametri utilizzati
  tot_char = 30
  test_char = 5
  dim_char = 12
  
  if (all(type == "full")){
    dimension= 10
    tot_char = tot_char*dimension
    char_test = test_char * dimension
  }else if (all(type == "mean")){
    dimension =1
    tot_char = tot_char*dimension
    char_test = test_char * dimension
    dim=60
  }
  
  #dataframe ordinato rispetto a C mediato
  test <- mean_and_order(X,C,Y,type,dim)
  
  #riordino lo split per carattere
  #matrice associata al test set
  mat_test <- matrix(, (nrow = char_test * dim_char), ncol = 1634)
  mat_test <-test
  
  #matrice per calcoli test_set
  mat_TS <- matrix(, nrow =  char_test  * dim_char, ncol = 1634)
  
  #creo un dataset di test in cui ordino i caratteri con le rispettive colonne e righe illuminate
  #indentificare il carattere
  df_TS <- data.frame(mat_TS)
  
  # prendo le righe e le colonne per carattere rispettivo del test set
  for (ind in 0:(char_test-1)){
    for (i in 0:(dim_char-1)){
      order_characters <- mat_test[ind+char_test*i+1,]
      df_TS[i+dim_char*ind+1, ] <- rbind(order_characters)
    }
  }
  
  #rinominazione
  colnames(df_TS)[1633] <- "C" 
  colnames(df_TS)[1634] <- "Y"
  
  #normalizzaazione test
  #prendo i dati normalizzati del train e li associo al test set 
  data_test <- df_TS[,1:(ncol(df_TS)-2)]
  label_Y_test <- df_TS[,ncol(df_TS)]
  label_C_test <- df_TS[,ncol(df_TS)-1]
  scaled_test <- scale(data_test, scale_values[1,], scale_values[2,])
  df_scaled_test <-data.frame(scaled_test)
  df_scaled_test <- list(df_scaled_test,label_C_test,label_Y_test)
  names(df_scaled_test) <- c("test_set","C_test","Y_test")
  
  #Pulizia strutture d'appoggio
  rm(order_characters)
  rm(mat_test)
  rm(df_TS)
  rm(mat_TS)
  
  
  #calcolo Accuratezza
  your_accuracy = test_prediction(our_model,df_scaled_test$test_set,df_scaled_test$Y_test,df_scaled_test$C_test,c=our_c,test_char,dimension,type)
  return(your_accuracy)
}
  
  
  
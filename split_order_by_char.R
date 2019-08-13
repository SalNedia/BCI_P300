#Riordino il mio split(training e test) per carattere ponendo le colonne nel giusto ordine relative ad ogni 
#carattere e in parallelo viene fatto lo stesso lavoro con le y target

split_order_by_char <- function(my_data,tot_char,char_train,dim_char,order_type,dimension){

i <- dimension

if (all(order_type == "full")){
  tot_char = tot_char*i
  char_train = char_train*i
  char_test = tot_char - char_train
  
}else if (all(order_type == "mean")){
  
  char_test = tot_char - char_train
  
}
  
#matrice associata al training set
mat_training <- matrix(, nrow = char_train * dim_char, ncol = 1634)
mat_training <- my_data$training_set #training_set

#matrice associata al test set
mat_test <- matrix(, (nrow = char_test * dim_char), ncol = 1634)
mat_test <- my_data$test_set

#matrice per calcoli training
mat_TR <- matrix(, nrow = char_train * dim_char, ncol = 1634)

#creo un dataset di training in cui ordino i caratteri con le rispettive colonne e righe illuminate
#indentificare il carattere
df_TR <- data.frame(mat_TR) 



# prendo le righe e le colonne per carattere rispettivo del training set 
for (ind in 0:(char_train-1)){
  for (i in 0:(dim_char-1)){
    order_characters <- mat_training[ind+char_train*i+1,]
    df_TR[i+(dim_char)*ind+1, ] <- rbind(order_characters)
  }
}


#rinominazione
colnames(df_TR)[1633] <- "C"
colnames(df_TR)[1634] <- "Y"

#matrice per calcoli test_set
mat_TS <- matrix(, nrow = char_test  * dim_char, ncol = 1634)

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

my_data$training_set <- df_TR
my_data$test_set <- df_TS

#Pulizia Global Environment
rm(order_characters)
rm(mat_TR)
rm(mat_training)
rm(mat_test)
rm(df_TR)
rm(df_TS)
rm(mat_TS)

return(my_data)
}


#Funzioni utilizzate & Implementate

library("dplyr")
library("LiblineaR")
library("e1071")
source("split.R")
source("data_normalization.R")
source("choosing_classifier.R")
source("linear_SVM.R")
source("gaussian_SVM.R")
source("polinomial_SVM.R")
source("setting_linear_SVM.R")
source("c_linear.R")
source("opt_classifier.R")
source("mean_and_order.R")
source("split_label.R")
source("split_order_by_char.R")
source("test_prediction.R")
source("test_to_execute.R")
source("get_scale_values.R")


#Caricamento Dati dei dataset
C <- read.csv("./C.txt",header = F, stringsAsFactors = F)
Y <- read.csv("./Y.txt",header = F, stringsAsFactors = F)
X <-  read.csv("./X.txt",header = F, stringsAsFactors = F)

#      type è scelto a seconda di come si vuole lavorare:
# "mean" se si vuole lavorare sulla matrice mediata e ridotta
# "full" se si vuole lavorare sull'intero dataset non mediato

#OSS:Si è dimostrato che lavorando sull'intero dataset (type = "full") invece che mediarlo per iterazione 
#i risultati sono notevolmente migliori.

type = "full" #configurabile

if (all(type == "full")){
  dimension= 10                          #            dimension -> numero di iterazioni per carattere: 
}else if (all(type == "mean")){          # 10 se le si considerano tutte, 1 se sono state mediate sul singolo carattere                            
  dimension =1
}

#PARAMETRI UTILIZZATI

tot_char = 30         #tot_char -> caratteri totali del dataset
char_train = 21      #char_train -> caratteri assegnati per il training_set
test_char = 9        #test_char -> caratteri assegnati al test_set
dim_char = 12         #dim_char -> sono le 6 righe e 6 colonne con cui è identificato un carattere
n_char = 3           #n_char -> numero di caratteri che assegneremo al validation_test
size =  360           #size: dimensione della matrice dopo aver mediato le iterazioni


#dataframe ordinato rispetto a C(prima tutte le righe 1 , poi 2..) e in più nel caso mean si procede a mediare
#per ogni carattere le iterazioni così da considerarne come fosse una una sola
df_order <- mean_and_order(X,C,Y,type,size)

#splitto in training_set e test_set
my_split <- split(df_order,tot_char,test_char,dimension,type)

#riordino lo split per carattere
split_order <- split_order_by_char(my_split,tot_char,char_train,dim_char,type,dimension)

#normalizzazione split su train e test
#get_scale_values(param) <- ottieni i valori ottenuti tramite la normalizzazione del training su media e varianza
#data_normalization(param,param) <- normalizzo il training su media e varianza e li associo al test set.
scale_values <- get_scale_values(split_order)
normalized_split <- data_normalization(split_order,scale_values)

#Risplitto lo split non normalizzato suddivendolo nelle diverse partition(train,y,c) 
split_with_label <- split_label(split_order)

#OSS:
#Tramite cross_validation si è dimostrato che usare kernel lineare è più robusto e più efficiente 
#tuttavia per vedere la cosa abbiamo sfruttato il caso con "mean" meno dispendiso di lavorare sull'intero dataset.
#Non abbiamo notato grandi differenze a livello di accuratezza tra split non normalizzato e normalizzato 
#abbiamo notato solamente essere più stabile il normalizzato e abbiamo preferito utilizzate quest'ultimo per l'addestramento effettivo

#Cross validation su split normalizzato
cross_validation_output_N <- choosing_classifier(normalized_split,char_train ,dim_char,n_char,dimension,type)
cross_validation_output_N

#Cross validation su split non normalizzato 
cross_validation_output_L <- choosing_classifier(split_with_label,char_train,dim_char,n_char,dimension,type)
cross_validation_output_L


#C calcolata sullo split normalizzato scelta tra 5 diversi parametri
c_setting_N <-setting_linear_SVM(normalized_split,char_train,dim_char,n_char,dimension,type)
c_setting_N
index_C <- which.max(c_setting_N[,1]) #indice del migliore c 
c_optima_N <- c_setting_N[index_C,2]  #seleziono il migliore c tra quelli proposti


#c calcolata sullo split non normalizzato scelta tra 5 diversi parametri
c_setting_L <-setting_linear_SVM(split_with_label,char_train,dim_char,n_char,dimension,type)
c_setting_L
index_C <- which.max(c_setting_N[,1]) #indice del migliore c 
c_optima_L <- c_setting_N[index_C,2]  #seleziono il migliore c tra quelli proposti


#test_result su normalizzato -> ritorna modello su addestramento con normalizzazione dei dati
our_model_N <- opt_classifier(normalized_split$training_set,normalized_split$Y_train,normalized_split$C_train,
                              normalized_split$test_set,normalized_split$Y_test,normalized_split$C_test,c = c_optima_N,n_char,dimension,type)

#test_result su non normalizzato -> ritorna modello su addestramento senza normalizzazione dei dati
our_model_L <- opt_classifier(split_order$training_set[,1:1632],split_order$training_set$Y,split_order$training_set$C,
                              split_order$test_set[,1:1632],split_order$test_set$Y,split_order$test_set$C, c = c_optima_L,n_char,dimension,type)


                                                #QUi SI PUO PROVARE IL TEST
#1. Inserire nella funzione il path del test

#2. type configurabile all'inizio dell'applicazione:
#i).utilizzare "full" per ottenere i risultati di accuratezza migliori, ii). "mean" meno efficiente ma piu rapido

#3. modello utilizzato infine è stato ottenuto su dataset normalizzato
#4. c migliore ottenuta dalla normalizzazione utilizzando funzione setting linear è c_optima_N
#5. Sosituire a "./Xtest.txt","./Ytest.txt","./Ctest.txt" i path relativi del test effettivo

your_accuracy <- test_to_execute(our_model_N,c_optima_N,"./Xtest.txt","./Ytest.txt","./Ctest.txt",scale_values,type) 
your_accuracy


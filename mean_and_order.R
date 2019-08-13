#in questa funzione ordino il dataset rispetto al vettore C e medio cosi da non avere piu iterazioni 
#ma solo 12 *30 selezioni di righe o colonne, dove 12 sono le righe e/o colonne e 30 sono i caratteri

mean_and_order <- function(X,C,Y,type,size){
  if (all(type == "full")){
    #Rinominazione
    names(C) <- c("C")
    names(Y) <- c("Y")
    
    #unione dataset X C Y
    d_order <- cbind(X,C,Y)
    
    #dataset ordinato rispetto alla C (1,2,3..)
    d_order <- d_order[order(d_order$C),]
  }else if (all(type == "mean")){

    #Rinominazione
    names(C) <- c("C")
    names(Y) <- c("Y")
  
    #unione dataset X C Y
    d_order <- cbind(X,C,Y)
    
    #dataset ordinato rispetto alla C (1,2,3..)
    d_order <- d_order[order(d_order$C),]

    #creo una matrice in cui faccio una media delle righe per iterazione --> dopo 
    #questo calcolo non ho piu iterazioni praticamente e ho solo la riga/colonna i-esima che si
    #illumina/non illumina per ogni carattere(12 * 30 = 360 dove 12 sono le righe colonne e 30 i caratteri delle 6 words)

    mat2 <- matrix(, nrow = size, ncol = 1634)
    for (j in 0:(size-1)){
      media<- d_order[(10*j+1):(10*(j+1)),]
      colsum <- (apply(media,2,sum))/10
      mat2[j+1,] <- colsum
    }
  
    #converto mat2 in un dataframe  cosi da poter effettuare le operazioni sui dataframe e successivamente la
    #rimuovo dal mio environment dato che non la utilizzo piu 
    df_order <- data.frame(mat2)
    rm(mat2)
    rm(colsum)
    rm(d_order)
  
    #Rinominazione
    colnames(df_order)[1633] <- "C" #rinomino colonna in C
    colnames(df_order)[1634] <- "Y" #rinomino colonna uscite
  
    return (df_order)
  }
}

#TR=dati di training
#YTR=classe dati di training
#CTR = indice di colonna e/o riga del carattere del train
#TS=dati di test
#YTS=classe dati di test
#CTs = indice di colonna e/o riga del carattere del test(validation)

#questo classificatore si trova nel package LibLineaR
linear_SVM <- function(TR,YTR,CTR,TS,YTS,CTS,n_char,dimension,type){
  #set.seed(123)
  rows_char = n_char * dimension
  dim_test = dim_char * n_char
  dim_char_f = dimension*dim_char
  
  #addestramento utilizzando la migliore c individuata mediante cross validation
  linear_model<-LiblineaR(data=TR,target=YTR,type=1,cost=0.001,bias=TRUE,verbose=FALSE) #
  #predizione sui dati di test
  test_prediction <- predict(linear_model,TS,decisionValues = TRUE)
  
  dim_test=nrow(TS)
  
  #Calcolo la W e la b dell'SVM
  W<-linear_model$W[,1:ncol(linear_model$W)-1]
  b<-linear_model$W[,ncol(linear_model$W)]
  
  #creo matrix per prediction e converto in dataframe
  y_pred <- matrix(, nrow = dim_test, ncol = 1)
  y_pred<-data.frame(y_pred)
  
  #Calcolo il massimo e setto i target corretti predetti a 1 
  for(i in 0:(2*rows_char-1)){
    tmp<- matrix(, nrow = dim_test, ncol = 1)
    for(j in (6*i+1):(6*(i+1))){
      tmp[j]=(crossprod(as.vector(TS[j,],mode='numeric'),W))+b
    }
    target=which.max(tmp[(6*i+1):(6*(i+1))])
    y_pred[(6*i+1):(6*(i+1)),]=c(-1,-1,-1,-1,-1,-1)
    y_pred[6*i+target,]=1
  }

  
  #caso "full": Per ogni carattere caratterizzato l'uno da 10 iterazioni vado a vedere quali sono le righe e le colonne 
  #a cui inoltre sono associati il maggior numero di target. Ogni 6 la riga/colonna con piu target (in proporzione) sarà 
  #effettivamente il mio target finale.
  
  if (all(type == "full")){
    count = 0
    y_pred_char <- c()
    counts <- matrix(, nrow = 12, ncol = 1)
    for (k in 0:(n_char-1)){
      for (i in 1:dim_char){
        for (j in 0:(dimension-1)){
          row_pred <- y_pred[(k*dim_char_f)+12*j+i,]
          if (row_pred == 1){
            count = count+1
          }
          counts[i] <- count
        }
        count = 0
      }
      y_pred_char <- append(y_pred_char,counts)
    }
  
    y_pred_char <- data.frame(y_pred_char)
  
    for(i in 0:(2*n_char-1)){
      target=which.max(y_pred_char[(6*i+1):(6*(i+1)),])
      y_pred_char[(6*i+1):(6*(i+1)),]=c(-1,-1,-1,-1,-1,-1)
      y_pred_char[6*i+target,]=1
    }

  
  yts_char <- c()
  for (h in 0:(n_char-1)){
    y_char <- YTS[(10*dim_char*h+1):(dim_char*h*10 + dim_char)] 
    yts_char <- append(yts_char,y_char)
  }
  
  yts_char <- data.frame(yts_char)
  
  table <- cbind(CTS[1:(n_char*dim_char)],yts_char,y_pred_char)
  
  correct_trials=0
  for(i in 0:(n_char-1)){
    for(j in 1:12){
      if(table[j+(12*i),2]+table[j+(12*i),3] ==2 ){
        correct_trials=correct_trials+1
      }
    }
  }
  
  
  }else if (all(type == "mean")){
    table <- cbind(CTS[1:dim_test],YTS[1:dim_test],y_pred)
    
    correct_trials=0
    for(i in 0:(n_char-1)){
      for(j in 1:12){
        if(table[j+(12*i),2]+table[j+(12*i),3] ==2 ){
          correct_trials=correct_trials+1
        }
      }
    }
    
  }
  
  accuracy=(correct_trials/2)/n_char
  
  return(accuracy)
}
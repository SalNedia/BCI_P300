#TR=dati di training
#YTR=classe dati di training
#CTR = indice di colonna e/o riga del carattere del train
#TS=dati di test
#YTS=classe dati di test
#CTs = indice di colonna e/o riga del carattere del test(validation)

#questo classificatore si trova nel package e1071
gaussian_SVM <- function(TR,YTR,CTR,TS,YTS,CTS,n_char,dimension,type){
  set.seed(123)
  
  #addestramento
  gaussian_model<-svm(x=TR,y=YTR,scale=F,type="C-classification",kernel="radial")  #predizione sui dati di test
  #predizione dei target
  test_prediction <- predict(gaussian_model,TS,decisionValues = TRUE)
  y_pred <- test_prediction
  
  table <- cbind(CTS,YTS,y_pred)
  
  if (all(type == "full")){
    
    correct_trials=0 #numero trial correttamente classificati
    
    for(i in 0:((n_char*dimension) - 1)){
      for(j in 1:12){
        if(table[j+(12*i),2]+table[j+(12*i),3] ==2 ){
          correct_trials=correct_trials+1
        }
      }
    }
    accuracy=(correct_trials/2)/(n_char*dimension)
  }else if (all(type == "mean")){
    
    correct_trials=0 #numero trial correttamente classificati
    
    for(i in 0:(n_char-1)){
     for(j in 1:12){
      if(table[j+(12*i),2]+table[j+(12*i),3] ==2 ){
        correct_trials=correct_trials+1
      }
     }
    }
    accuracy=(correct_trials/2)/n_char
  }
  
  return(accuracy)
}
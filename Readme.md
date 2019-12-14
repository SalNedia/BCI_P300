# Progetto per il corso di Metodi di Ottimizzazione per Big Data
## Implemetazione di un classificatore, tramite l'utilizzo di un SVM (Support Vector Machine) che sia in grado di predire i caratteri osservati dal paziente soggetto a SLA e che sta utilizzando il tool BCI (Brain-Computer Interface)  
### (Lanciotti - Nedia)

Inizialmente è possibile scegliere la modalità 
	- "mean" se si vuole lavorare sulla matrice mediata e ridotta
	- "full" se si vuole lavorare sull'intero dataset non mediato
configurabile tramite il parametro type.	

PARAMETRI UTILIZZATI:
tot_char -> caratteri totali del dataset
char_train -> caratteri assegnati per il training_set
test_char -> caratteri assegnati al test_set
dim_char -> sono le 6 righe e 6 colonne con cui è identificato un carattere
n_char -> numero di caratteri che assegneremo al validation_test
size -> dimensione della matrice dopo aver mediato le iterazioni

Alla fine dell'applicazione si troverà la funzione:

test_to_execute(our_model_N,c_optima_N,"./Xtest.txt","./Ytest.txt","./Ctest.txt",type) 
                                              
dove bisognerà:
1. Sostituire a "./Xtest.txt","./Ytest.txt","./Ctest.txt" i path relativi del test effettivo
2. type: configurabile all'inizio dell'applicazione:
			i). "full" per ottenere i risultati di accuratezza migliori in generale,
		  ii). "mean" meno efficiente ma piu rapido
3. our_model: modello utilizzato infine è stato ottenuto su dataset normalizzato con i parametri settati.
4. c_optima_N: migliore parametro c ottenuto dalla normalizzazione utilizzando funzione setting linear è c_optima_N 
# Progetto Metaeuristiche 

## Istanze
**Le insanze utilizzate per testare gli algoritmi si trovano all'interno della cartella files**

## Tecnologia Utilizzata

Per la realizzazione di questo progetto si e' utilissato il linguaggio haskell. 
Si e' scelto di utilizzare questa tecnologia unicamente come esercizio rispetto ad altre tecnologie come java o csharp dove ho gia' esperienza. 
Di conseguenza mi scuso per evetuali errori o performance scadenti proprio perche' si tratta del mio primo programma con questa tecnologia.

## Strategia Generale e specifica per GA e ACO

All'esecuzione del programma viene chiesto all'utente quale algoritmo si vuole utilizzare, e' possibile anche richiedere la terminazione del programma, e in seguito su quale/i istanza/e si vuole far eseguire il programma. Appena inseriti questi dati il programma procede con il parsing dell'istanza scelta, se piu' di una si parte dalla prima, poi la secona e cosi via. Nel caso nell'istanza ci sia un limite nella capacita' del veicolo i nodi vengono suddivisi in sottogruppi e l'algoritmo viene eseguito per ogniuno di questi, dando cosi piu' risultati per quella istanza, altrimenti se la capacita' e' sufficiente per tutti i nodi, questa suddivisione non avviene.

Importante, Visto che si tratta di un caso di minimizzazione si e' deciso, una volta calcolata la fitness di esporre i valori invertiti: quello con distanza minore avra' possibilita' maggiore in montecarlo o in calcoli di probabilita' di venire scelto rispetto a quello con distanza piu' grande. Semplicemente vengono calcolati normalmente e poi si scambiano il maggiore con il minore e via cosi, i varoli centrali non venogono toccati.

### Genetico 

Per la realizzazione dell'algoritmo dei genetici prima viene generato causalmente una soluzione che viene utilizzata come best iniziale e allo stesso modo viene creata una popolazione di grandezza pari a quanto specificato nei parametri. (Parameters.hs, attenzione e' un file che deve essere compilato.) 

Fatto cio' parte una chiamata ricorsiva che termina solo dopo un certo numero di iterazioni.
Di seguito i passi dell'algoritmo:
1.  All'inizio di ogni ciclo si ha un controllo sul numero di iterazioni che sono avvenute senza l'aggiornamento dell'individuo migliore, se una certa soglia viene superata, non si utilizza la popolazione del ciclo precedende ma questa viene rigenerata a caso, questo per evitare minimi locali. l'elemento migliore chiaramente viene mantenuto nella ricorsione ma non nella nuova popolazione.
2.  Si attiva una selezione a montecarlo a roulette, sulla base della fitness in modo da eliminare gli individui peggiori
3.  Si ha una selezione degli individui che dovranno andare a riprodursi
4.  Si effettua il two point crossover ponendo attenzione a mantenere tutti gli elementi differenti all'interno dell'individuo generato.
5.  Si sostituiscono i figli ai padri nel caso questi siano effettivamente migliori dei precedenti, altrimenti viene sostituito il peggiore della popolazione con il nuovo figlio.
6.  Si applica la mutazione alla popolazione e si reintroduce il migliore del ciclo precedente.
7.  Si controlla la terminazione e se e' emerso un nuovo caso migliore. Se la terminazione non avviene si procede con un nuovo ciclo.

### Ant Colony Optimization

All'inizio dell'algoritmo si crea una struttura dati che accoppia tutti i nodi e gli associa il loro grado di pheromone e la distanza tra loro, questa verra' poi utilizzata come attrattivita', calcolo di probabilita' della mossa etc. Nella Struttura dati le coppie solo senza ripetizione (a,b) == (b,a). Questo consente di ridurre un minimo la grandezza della struttura.

Per ogni ciclo:
1. Le formiche effettuano una costruzione della loro soluzione sulla base della formula vista a lezione e generano una soluzione che ha all'interno tutti i nodi.
2. Viene aggiornato il pheromone
3. Si calcola la soluzione migliore
4. Si controlla la terminazione. Anche in questo caso se il miglior risultato non e' cambiato da molto tempo viene rigenerata la struttura dati iniziale.

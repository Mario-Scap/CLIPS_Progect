(defmodule MAIN (export ?ALL))

;;******************
;;* STATO INIZIALE *
;;******************

(deftemplate MAIN::attribute
   (slot nome)
   (slot value)
    (slot certainty (default 100.0))
)

(defrule MAIN::start
  (declare (salience 10000))
  =>
  (set-strategy depth)
  (set-fact-duplication TRUE)
  (focus GENERA-PREZZI DOMANDE HOME-QUALITIES QUARTIERI CASE PRINT-RESULTS DOMANDE2 HOME-QUALITIES CASE PRINT-RESULTS DOMANDE3 MODIFICA-PREFERENZE CASE PRINT-RESULTS)
)

(defrule MAIN::combine-certainties ""
  (declare (salience 200)
           (auto-focus TRUE))
  ?rem1 <- (attribute (nome ?rel) (value ?val) (certainty ?per1))
  ?rem2 <- (attribute (nome ?rel) (value ?val) (certainty ?per2))
  (test (neq ?rem1 ?rem2))
  =>
  (retract ?rem1)
  (modify ?rem2 (certainty (/ (- (* 100 (+ ?per1 ?per2)) (* ?per1 ?per2))
   100))))
;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction MAIN::ask-question (?question ?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)

;;******************
;;* REGOLE DOMANDE *
;;******************

(defmodule DOMANDE (import MAIN ?ALL) (export ?ALL))

(deftemplate DOMANDE::domanda
   (slot attribute (default ?NONE))
   (slot giro (default ?NONE))
   (slot the-question (default ?NONE))
   (multislot valid-answers (default ?NONE))
   (slot already-asked (default FALSE))
   (multislot precursors (default ?DERIVE)))
   
(defrule DOMANDE::fai-domanda
   ?f <- (domanda (already-asked FALSE)
                   (giro 1)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
   =>
   (modify ?f (already-asked TRUE))
   (assert (attribute (nome ?the-attribute)
                      (value (ask-question ?the-question ?valid-answers))))
)

(defrule DOMANDE::rifai-domanda
   ?f <- (domanda (already-asked TRUE)
                   (giro 1)
                   (precursors)
                   (attribute ?the-attribute))
         (attribute (nome ?the-attribute) (value qualsiasi))
   =>
   (modify ?f (already-asked REDO))
)

(defrule DOMANDE::precursore-or-continua
   ?f <- (domanda (already-asked FALSE)
                   (precursors ?name is ?value $?rest))
         (attribute (nome ?name) (value ~?value))
   =>
   (if (eq (nth$ 1 ?rest) or) 
    then (modify ?f (precursors (rest$ ?rest))))
)

(defrule DOMANDE::precursore-soddisfatto
   ?f <- (domanda (already-asked FALSE)
                   (precursors ?name is ?value $?rest))
         (attribute (nome ?name) (value ?value))
   =>
   (if (eq (nth$ 1 ?rest) and) 
    then (modify ?f (precursors (rest$ ?rest)))
    else (modify ?f (precursors ?rest)))
   (if (eq (nth$ 1 ?rest) or) 
    then (modify ?f (precursors)))
)

(defrule DOMANDE::precursore-non-soddisfatto
   ?f <- (domanda (already-asked FALSE)
                   (precursors ?name is-not ?value $?rest))
         (attribute (nome ?name) (value ~?value))
   =>
   (if (eq (nth$ 1 ?rest) and) 
    then (modify ?f (precursors (rest$ ?rest)))
    else (modify ?f (precursors ?rest))))

;;****************
;;* CASE DOMANDE *
;;****************

(defmodule CASE-DOMANDE (import DOMANDE ?ALL) (export ?ALL))

(deffacts CASE-DOMANDE::domande-attributi
        (domanda (attribute modifiche)
                (giro 3)
                (the-question "Vuoi modificare una delle risposte che hai inserito precedentemente? ")
                (valid-answers 
                        zona-centro zona-periferia zona-prima-cintura
                        metri-30 metri-40 metri-50 metri-60 metri-70 metri-80 metri-90 metri-100 metri-120 metri-140 metri-160 metri-180 metri-200 metri-250 metri-300 metri-400 metri-500
                        indipendente-si indipendente-no
                        piano-alto piano-basso piano-terra
                        ascensore-si ascensore-no
                        bagni-1 bagni-2 bagni-3 bagni-4
                        balcone-si balcone-no
                        boxauto-si boxauto-no
                        deposito-si deposito-no
                        no
                )
        )
        
        (domanda (attribute casa-animale)
                (giro 2)
                (the-question "Hai un animale? ")
                (valid-answers si no))
        (domanda (attribute fa-shopping)
            (giro 2)
            (the-question "Ti piace fare shopping? ")
            (valid-answers si no ogni-tanto))
        (domanda (attribute casa-mezzi)
                (giro 2)
                (the-question "Sei automunito/a? ")
                (valid-answers si no))
        (domanda (attribute casa-pendolare)
                (giro 2)
                (precursors casa-studente is no)
                (the-question "Sei invece un pendolare? ")
                (valid-answers si no))
        (domanda (attribute casa-studente)
                (giro 2)
                (the-question "Sei uno studente fuorisede? ")
                (valid-answers si no))
        (domanda (attribute casa-bambini)
                (giro 2)
                (the-question "Hai dei bambini? ")
                (valid-answers si no))
        (domanda (attribute casa-fidanzata)
                (giro 2)
                (the-question "Sei per caso fidanzata/o ? ")
                (valid-answers si no))
        (domanda (attribute casa-anziano)
                (giro 2)
                (the-question "Sei una persona anziana? ")
                (valid-answers si no))


        (domanda (attribute casa-prezzo)
                (giro 1)
                (the-question "Quanto vuoi spendere come massimo? ")
                (valid-answers 50000 80000 100000 120000 150000 180000 200000 250000 300000 400000 500000 600000 700000 800000 900000 1000000))
        (domanda (attribute casa-deposito)
                (giro 1)
                (precursors casa-box is no or casa-box is preferisco-no)
                (the-question "Invece un deposito? ")
                (valid-answers si no preferisco-si preferisco-no qualsiasi))
        (domanda (attribute casa-box)
                (giro 1)
                (the-question "Desideri un box per l'auto? ")
                (valid-answers si no preferisco-si preferisco-no qualsiasi))
        (domanda (attribute casa-balcone)
                (giro 1)
                (the-question "E un balcone? ")
                (valid-answers si no preferisco-si preferisco-no qualsiasi))
        (domanda (attribute casa-bagno)
                (giro 1)
                (the-question "Quanti bagni vorresti avere in casa? ")
                (valid-answers 1 2 3 4))
        (domanda (attribute casa-ascensore)
                (giro 1)
                (precursors casa-piano is alto or casa-piano is preferisco-alto)
                (the-question "Vorresti che ci sia un ascensore? ")
                (valid-answers si no preferisco-si preferisco-no qualsiasi))
        (domanda (attribute casa-piano)
                (giro 1)
                (precursors casa-indipendente is no or casa-indipendente is preferisco-no)
                (the-question "A che piano desideri avere la casa? ")
                (valid-answers alto basso terra preferisco-alto preferisco-basso preferisco-terra qualsiasi))
        (domanda (attribute casa-indipendente)
                (giro 1)
                (the-question "Preferisci avere una casa indipendente? ")
                (valid-answers si no preferisco-si preferisco-no qualsiasi))
        (domanda (attribute casa-metriquadri)
                (giro 1)
                (the-question "Di quanti metri quadri vuoi la casa? ")
                (valid-answers 30 40 50 60 70 80 90 100 120 140 160 180 200 250 300 400 500))
        (domanda (attribute casa-zona)
                (giro 1)
                (the-question "In che zona vuoi comprare la casa? ")
                (valid-answers centro periferia prima-cintura preferisco-centro preferisco-prima-cintura preferisco-periferia qualsiasi))
        (domanda (attribute casa-citta)
                (giro 1)
                (the-question "In che citta' stai cercando la casa? ")
                (valid-answers torino milano roma bari))
)

;;*******************************
;;* REGOLE DOMANDE SECONDO GIRO *
;;*******************************

(defmodule DOMANDE2     (import MAIN ?ALL) (import DOMANDE ?ALL) (export ?ALL))

(defrule DOMANDE2::prima-domanda-refresh
   ?f <- (domanda (already-asked FALSE)
                   (giro 2)
                   (precursors)
                   (the-question "Sei una persona anziana? "))
=>
      (refresh PRINT-RESULTS::header)
      (refresh PRINT-RESULTS::print-casa)
      (refresh PRINT-RESULTS::remove-poor-casa-choices)
      (refresh PRINT-RESULTS::end-spaces)
)

(defrule DOMANDE2::fai-domanda2
   ?f <- (domanda (already-asked FALSE)
                   (giro 2)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
   =>
   (modify ?f (already-asked TRUE))
   (assert (attribute (nome ?the-attribute)
                      (value (ask-question ?the-question ?valid-answers))))
)

;;*******************************
;;* REGOLE DOMANDE TERZO GIRO   *
;;*******************************

(defmodule DOMANDE3     (import MAIN ?ALL) (import DOMANDE ?ALL) (export ?ALL))

(defrule DOMANDE3::fai-domanda3
   ?f <- (domanda (already-asked FALSE)
                   (giro 3)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
   =>
   (assert (attribute (nome ?the-attribute)
                      (value (ask-question ?the-question ?valid-answers))))
)

(defrule DOMANDE3::rifai-domanda-finale
   ?f <- (domanda (already-asked FALSE)
                   (giro 3)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
         (attribute (nome ?the-attribute) (value ~no))
   =>
   (assert (attribute (nome ?the-attribute)
                      (value (ask-question ?the-question ?valid-answers))))
)

(defrule DOMANDE3::stop-domanda-finale
   ?f <- (domanda (already-asked FALSE)
                   (giro 3)
                   (the-question ?the-question)
                   (attribute ?the-attribute))
   ?a <- (attribute (nome ?the-attribute) (value no))
   =>
   (modify ?f (already-asked TRUE))
   (retract ?a)
   (refresh PRINT-RESULTS::header)
   (refresh PRINT-RESULTS::print-casa)
   (refresh PRINT-RESULTS::remove-poor-casa-choices)
   (refresh PRINT-RESULTS::end-spaces)
   (refresh CASE::genera-case2)
   (set-strategy breadth)
)

(defrule DOMANDE3::rifai-domanda3
        (declare (salience 10))
        ?f <- (domanda (already-asked REDO)
                        (the-question ?the-question)
                        (attribute ?the-attribute)
                        (valid-answers $?valid-answers))
        ?a <- (attribute (nome ?the-attribute))

        =>
        (modify ?f (already-asked TRUE))
        (assert (attribute (nome (sym-cat modifiche- ?the-attribute))
                           (value (ask-question ?the-question ?valid-answers)))
        )
)

;;*************************
;;* REGOLE QUARTIERI      *
;;*************************

(defmodule QUARTIERI (export ?ALL))

(deftemplate QUARTIERI::quartiere
  (slot nome (default ?NONE))
  (slot citta (default ?NONE))
  (slot costo-mq (default any))
  (multislot servizi (default any))
)

(deffacts QUARTIERI::lista-quartieri
    (quartiere (citta torino) (nome parella) (costo-mq 1800) (servizi parco scuola ospedale metro pullman stazione supermercato))
    (quartiere (citta torino) (nome barriera-milano) (costo-mq 1000) (servizi parco scuola ospedale pullman supermercato centro-commerciale metro stazione))
    (quartiere (citta torino) (nome crocetta) (costo-mq 2500) (servizi ospedale pullman metro supermercato palestra bar ristoranti pizzerie centro-commerciale))
    (quartiere (citta torino) (nome mirafiori-nord) (costo-mq 2000) (servizi parco pullman metro stazione supermercato palestra ristoranti pizzerie bar))
    (quartiere (citta torino) (nome centro) (costo-mq 5000) (servizi parco pullman metro supermercato ristoranti bar universita centro-commerciale pizzerie))
    (quartiere (citta torino) (nome san-salvario) (costo-mq 2000) (servizi parco pullman metro supermercato ospedale))

    (quartiere (citta roma) (nome roma-sud) (costo-mq 1800) (servizi parco scuola ospedale metro pullman stazione supermercato))
    (quartiere (citta roma) (nome san-basilio) (costo-mq 1000) (servizi parco scuola ospedale pullman supermercato centro-commerciale metro stazione))
    (quartiere (citta roma) (nome roma-ovest) (costo-mq 2500) (servizi ospedale pullman metro supermercato palestra bar ristoranti pizzerie universita centro-commerciale))
    (quartiere (citta roma) (nome municipio) (costo-mq 2000) (servizi parco pullman metro stazione supermercato palestra ristoranti pizzerie bar))
    (quartiere (citta roma) (nome centro) (costo-mq 5000) (servizi parco pullman metro supermercato ristoranti bar universita centro-commerciale pizzerie))
    (quartiere (citta roma) (nome ostiense) (costo-mq 2000) (servizi parco pullman metro supermercato ospedale))

    (quartiere (citta milano) (nome bicocca) (costo-mq 1800) (servizi parco scuola ospedale metro pullman stazione supermercato))
    (quartiere (citta milano) (nome rozzano) (costo-mq 1000) (servizi parco scuola ospedale pullman supermercato centro-commerciale metro stazione))
    (quartiere (citta milano) (nome citylife) (costo-mq 2500) (servizi ospedale pullman metro supermercato palestra bar ristoranti pizzerie universita))
    (quartiere (citta milano) (nome navigli) (costo-mq 2000) (servizi parco pullman metro stazione supermercato palestra ristoranti pizzerie bar centro-commerciale))
    (quartiere (citta milano) (nome centro) (costo-mq 5000) (servizi parco pullman metro supermercato ristoranti bar universita centro-commerciale pizzerie))
    (quartiere (citta milano) (nome loreto) (costo-mq 2000) (servizi parco pullman metro supermercato ospedale))

    (quartiere (citta bari) (nome japigia) (costo-mq 1800) (servizi parco scuola ospedale metro pullman stazione supermercato))
    (quartiere (citta bari) (nome san-paolo) (costo-mq 1000) (servizi parco scuola ospedale pullman supermercato centro-commerciale metro stazione))
    (quartiere (citta bari) (nome poggiofranco) (costo-mq 2500) (servizi ospedale pullman metro supermercato palestra bar ristoranti pizzerie universita centro-commerciale))
    (quartiere (citta bari) (nome palese) (costo-mq 2000) (servizi parco pullman metro stazione supermercato palestra ristoranti pizzerie bar))
    (quartiere (citta bari) (nome centro) (costo-mq 5000) (servizi parco pullman metro supermercato ristoranti bar universita centro-commerciale pizzerie))
    (quartiere (citta bari) (nome liberta') (costo-mq 2000) (servizi parco pullman metro supermercato ospedale))
)

;;*******************************
;;* SCEGLI QUALITA' CASE        *
;;*******************************


(defmodule HOME-QUALITIES (import MAIN ?ALL) (import QUARTIERI ?ALL))

; Scelta zona migliore

(defrule HOME-QUALITIES::best-zona-preferisco
        (attribute (nome casa-zona) (value ?value & ~qualsiasi & ~centro & ~periferia & ~prima-cintura))

      =>
            (if (eq ?value preferisco-centro)
             then (assert (attribute (nome best-zona) 
                          (value centro)
                          (certainty 80.0)))
                  (assert (attribute (nome best-zona) 
                          (value periferia)
                          (certainty 30.0)))
                  (assert (attribute (nome best-zona) 
                          (value prima-cintura)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-periferia)
             then (assert (attribute (nome best-zona) 
                          (value periferia)
                          (certainty 80.0)))
                  (assert (attribute (nome best-zona) 
                          (value centro)
                          (certainty 30.0)))
                  (assert (attribute (nome best-zona) 
                          (value prima-cintura)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-prima-cintura)
             then (assert (attribute (nome best-zona) 
                          (value prima-cintura)
                          (certainty 80.0)))
                  (assert (attribute (nome best-zona) 
                          (value centro)
                          (certainty 30.0)))
                  (assert (attribute (nome best-zona) 
                          (value periferia)
                          (certainty 30.0)))
            )
)

(defrule HOME-QUALITIES::best-zona-sicuro
            (attribute (nome casa-zona) (value ?value & ~qualsiasi & ~preferisco-prima-cintura & ~preferisco-centro & ~preferisco-periferia))
      =>
            (assert (attribute (nome best-zona) 
                     (value ?value)))
)

(defrule HOME-QUALITIES::best-zonaqualsiasi
            (attribute (nome casa-zona) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-zona) 
                     (value centro)
                     (certainty 30.0)))
            (assert (attribute (nome best-zona) 
                     (value periferia)
                     (certainty 30.0)))
            (assert (attribute (nome best-zona) 
                     (value prima-cintura)
                     (certainty 30.0)))
)

; Scelta metri quadri migliore
(defrule HOME-QUALITIES::best-metriquadri
            (attribute (nome casa-metriquadri) (value ?value))
      =>
            (assert (attribute (nome best-metriquadri) 
                     (value ?value)
                     (certainty 100.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value (- ?value 10))
                     (certainty 70.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value (+ ?value 10))
                     (certainty 70.0)))
)

(defrule HOME-QUALITIES::best-metriquadri-qualsiasi
            (attribute (nome casa-metriquadri) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-metriquadri) 
                     (value 30)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 40)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 50)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 60)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 70)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 80)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 90)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 100)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 120)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 140)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 160)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 180)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 200)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 250)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 300)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 400)
                     (certainty 30.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 500)
                     (certainty 30.0)))
)

; Scelta casa indipendente o no

(defrule HOME-QUALITIES::best-indipendente-preferisco
            (attribute (nome casa-indipendente) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-indipendente) 
                     (value si)
                     (certainty 80.0)))
                  (assert (attribute (nome best-indipendente) 
                     (value no)
                     (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                     (value terra)
                     (certainty 50.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 50.0)))
                  (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 50.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value si)
                        (certainty 50.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value no)
                        (certainty 50.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-indipendente) 
                     (value no)
                     (certainty 80.0)))
                  (assert (attribute (nome best-indipendente) 
                     (value si)
                     (certainty 30.0)))
            )              
)

(defrule HOME-QUALITIES::best-indipendente-sicuro
            (attribute (nome casa-indipendente) (value ?value & ~qualsiasi & ~preferisco-si & ~preferisco-no))
      =>
            (assert (attribute (nome best-indipendente) 
                     (value ?value)))
            (if (eq ?value si)
             then (assert (attribute (nome best-piano) 
                     (value terra)
                     (certainty 50.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 50.0)))
                  (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 50.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value si)
                        (certainty 50.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value no)
                        (certainty 50.0)))
            )
)

(defrule HOME-QUALITIES::best-indipendentequalsiasi
            (attribute (nome casa-indipendente) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-indipendente) 
                     (value si)
                     (certainty 30.0)))
            (assert (attribute (nome best-indipendente) 
                     (value no)
                     (certainty 30.0)))
            (assert (attribute (nome best-piano) 
                     (value terra)
                     (certainty 50.0)))
            (assert (attribute (nome best-piano) 
                     (value alto)
                     (certainty 50.0)))
            (assert (attribute (nome best-piano) 
                     (value basso)
                     (certainty 50.0)))
            (assert (attribute (nome best-ascensore) 
                     (value si)
                     (certainty 50.0)))
            (assert (attribute (nome best-ascensore) 
                     (value no)
                     (certainty 50.0)))
)

; Scelta piano della casa
(defrule HOME-QUALITIES::best-piano-preferisco
            (attribute (nome casa-piano) (value ?value & ~qualsiasi & ~alto & ~basso & ~terra))
      =>
            (if (eq ?value preferisco-alto)
             then (assert (attribute (nome best-piano) 
                          (value alto)
                          (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                          (value terra)
                          (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                          (value basso)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-basso)
             then (assert (attribute (nome best-piano) 
                          (value basso)
                          (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                          (value terra)
                          (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                          (value alto)
                          (certainty 30.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 80.0)))
            )
            (if (eq ?value preferisco-terra)
             then (assert (attribute (nome best-piano) 
                          (value terra)
                          (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                          (value basso)
                          (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                          (value alto)
                          (certainty 30.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 80.0)))
            )
)

(defrule HOME-QUALITIES::best-piano-sicuro
            (attribute (nome casa-piano) (value ?value & ~qualsiasi & ~preferisco-alto & ~preferisco-basso & ~preferisco-terra))
      =>
            (assert (attribute (nome best-piano) 
                     (value ?value)))
            (if (eq ?value basso)
                  then  (assert (attribute (nome best-ascensore) 
                              (value si)
                              (certainty 80.0)))
                        (assert (attribute (nome best-ascensore) 
                              (value no)
                              (certainty 80.0)))
            )
            (if (eq ?value terra)
                  then  (assert (attribute (nome best-ascensore) 
                              (value si)
                              (certainty 80.0)))
                        (assert (attribute (nome best-ascensore) 
                              (value no)
                              (certainty 80.0)))
            )
)

(defrule HOME-QUALITIES::best-piano-qualsiasi
            (attribute (nome casa-piano) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-piano) 
                     (value basso)
                     (certainty 30.0)))
            (assert (attribute (nome best-piano) 
                     (value alto)
                     (certainty 30.0)))
            (assert (attribute (nome best-piano) 
                     (value terra)
                     (certainty 30.0)))
            (assert (attribute (nome best-ascensore) 
                     (value si)
                     (certainty 30.0)))
            (assert (attribute (nome best-ascensore) 
                     (value no)
                     (certainty 30.0)))
)

; Scelta ascensore
(defrule HOME-QUALITIES::best-ascensore-preferisco
            (attribute (nome casa-ascensore) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 30.0)))
            )
)

(defrule HOME-QUALITIES::best-ascensore-sicuro
            (attribute (nome casa-ascensore) (value ?value & ~qualsiasi & ~preferisco-si & ~preferisco-no))
      =>
            (assert (attribute (nome best-ascensore) 
                          (value ?value)))
)

(defrule HOME-QUALITIES::best-ascensore-qualsiasi
            (attribute (nome casa-ascensore) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-ascensore) 
                     (value si)
                     (certainty 30.0)))
            (assert (attribute (nome best-ascensore) 
                     (value no)
                     (certainty 30.0)))
)

; Scelta bagni
(defrule HOME-QUALITIES::best-bagni
            (attribute (nome casa-bagno) (value ?value))
      =>
            (assert (attribute (nome best-bagni) 
                     (value ?value)))
            (if (> ?value 1)
             then (assert (attribute (nome best-bagni) 
                     (value (- ?value 1))
                     (certainty 60.0)))
            )
            (if (< ?value 4)
             then (assert (attribute (nome best-bagni) 
                     (value (+ ?value 1))
                     (certainty 90.0)))
            )
)

; Scelta balcone

(defrule HOME-QUALITIES::best-balcone-preferisco
            (attribute (nome casa-balcone) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-balcone) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-balcone) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-balcone) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-balcone) 
                          (value si)
                          (certainty 30.0)))
            )
)

(defrule HOME-QUALITIES::best-balcone-sicuro
            (attribute (nome casa-balcone) (value ?value & ~qualsiasi & ~preferisco-si & ~preferisco-no))
      =>
            (assert (attribute (nome best-balcone) 
                          (value ?value)))
)

(defrule HOME-QUALITIES::best-balcone-qualsiasi
            (attribute (nome casa-balcone) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-balcone) 
                     (value si)
                     (certainty 30.0)))
            (assert (attribute (nome best-balcone) 
                     (value no)
                     (certainty 30.0)))
)

; Scelta box auto
(defrule HOME-QUALITIES::best-boxauto-preferisco
            (attribute (nome casa-box) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-boxauto) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-boxauto) 
                          (value no)
                          (certainty 40.0)))
                  (assert (attribute (nome best-deposito) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-deposito) 
                          (value si)
                          (certainty 80.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-boxauto) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-boxauto) 
                          (value si)
                          (certainty 40.0)))
            )
)

(defrule HOME-QUALITIES::best-boxauto-sicuro
            (attribute (nome casa-box) (value ?value & ~qualsiasi & ~preferisco-si & ~preferisco-no))
      =>
            (assert (attribute (nome best-boxauto) 
                          (value ?value)))
            (if (eq ?value si)
             then (assert (attribute (nome best-deposito) 
                          (value si)
                          (certainty 100.0)))
                  (assert (attribute (nome best-deposito) 
                          (value no)
                          (certainty 80.0)))
            )
)

(defrule HOME-QUALITIES::best-boxauto-qualsiasi
            (attribute (nome casa-box) (value ?value & qualsiasi))
      =>
            (assert  (attribute (nome best-boxauto) 
                     (value si)
                     (certainty 30.0)))
            (assert  (attribute (nome best-boxauto) 
                     (value no)
                     (certainty 30.0)))
            (assert  (attribute (nome best-deposito) 
                     (value si)
                     (certainty 30.0)))
            (assert  (attribute (nome best-deposito) 
                     (value no)
                     (certainty 30.0)))
)

; Scelta deposito
(defrule HOME-QUALITIES::best-deposito-preferisco
            (attribute (nome casa-deposito) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-deposito) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-deposito) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-deposito) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-deposito) 
                          (value si)
                          (certainty 30.0)))
            )
)

(defrule HOME-QUALITIES::best-deposito-sicuro
            (attribute (nome casa-deposito) (value ?value & ~qualsiasi & ~preferisco-si & ~preferisco-no))
      =>
            (assert (attribute (nome best-deposito) 
                          (value ?value)))
)

(defrule HOME-QUALITIES::best-deposito-qualsiasi
            (attribute (nome casa-deposito) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-deposito) 
                     (value si)
                     (certainty 30.0)))
            (assert (attribute (nome best-deposito) 
                     (value no)
                     (certainty 30.0)))
)

;;**************************************
;;* REGOLE PER DOMANDE PIU' SPECIFICHE *
;;**************************************

; Scelta anziani
(defrule HOME-QUALITIES::best-anziani
        (attribute (nome casa-anziano) (value ?value))
        (not (attribute (nome casa-piano) (value ?value2 & alto | basso | terra)))
        (not (attribute (nome casa-ascensore) (value ?value3 & si | no)))
      =>
            (if (eq ?value si)
             then (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 90.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value si)
                        (certainty 70.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value no)
                        (certainty 30.0)))
        
             else (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 70.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value si)
                        (certainty 70.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value no)
                        (certainty 70.0)))
            )
            
)

(defrule HOME-QUALITIES::best-anziani-asce
        (attribute (nome casa-anziano) (value ?value))
        (not (attribute (nome casa-piano) (value ?value2 & alto | basso | terra)))
        (attribute (nome casa-ascensore) (value ?value3 & si | no))
        
      =>
            (if (eq ?value si)
             then (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 90.0)))
                
             else (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 70.0)))
                
            )
            
)

(defrule HOME-QUALITIES::best-anziani-ospedale
        (attribute (nome casa-anziano) (value ?value))
        (not (attribute (nome casa-piano) (value ?value2 & alto | basso | terra)))
        (not (attribute (nome casa-ascensore) (value ?value3 & si | no)))
        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? ospedale $?))
      =>
            (if (eq ?value si)
             then (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 90.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value si)
                        (certainty 70.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value no)
                        (certainty 30.0)))
                (assert (attribute (nome best-quartiere)
                        (value ?nq)
                        (certainty 80.0)))
        
             else (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 70.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value si)
                        (certainty 70.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value no)
                        (certainty 70.0)))
                (assert (attribute (nome best-quartiere)
                        (value ?nq)
                        (certainty 30.0)))
            
        )
)


(defrule HOME-QUALITIES::best-anziani-osp-sicuro
            (attribute (nome casa-anziano) (value ?value))
            (not (attribute (nome casa-piano) (value ?value2 & alto | basso | terra)))
            (attribute (nome casa-ascensore) (value ?value3 & si | no))
            (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? ospedale $?))
      =>
            (if (eq ?value si)
             then (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 90.0)))
                (assert (attribute (nome best-quartiere)
                        (value ?nq)
                        (certainty 80.0)))
             else (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 70.0)))
                (assert (attribute (nome best-quartiere)
                        (value ?nq)
                        (certainty 30.0)))
            )
            
)

; Scelta bambini
(defrule HOME-QUALITIES::best-bambini
            (attribute (nome casa-bambini) (value ?value & si))
            (attribute (nome casa-citta) (value ?casaCitta))
            (or   
                (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? parco $?))
                (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? scuola $? ))
            )
            (not (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? parco scuola $?)))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 70.0))
            ) 
)

(defrule HOME-QUALITIES::best-bambini-parco-scuola
            (attribute (nome casa-bambini) (value ?value & si))
            (attribute (nome casa-citta) (value ?casaCitta))
            (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? parco scuola $?))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 85.0))
            ) 
)

(defrule HOME-QUALITIES::best-bambini-zona
            (attribute (nome casa-bambini) (value ?value & si))
            (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))

      =>
            (assert (attribute (nome best-zona) 
                        (value periferia)
                        (certainty 80.0))
            )  
            (assert (attribute (nome best-zona) 
                        (value centro)
                        (certainty 40.0))
            ) 
            (assert (attribute (nome best-zona) 
                        (value prima-cintura)
                        (certainty 70.0))
            )  
)

(defrule HOME-QUALITIES::best-bambini-indip
            (attribute (nome casa-bambini) (value ?value & si))
            (not (attribute (nome casa-indipendente) (value ?value2 & si | no)))
      =>
            (assert (attribute (nome best-indipendente) 
                        (value si)
                        (certainty 80.0))
            )
            (assert (attribute (nome best-indipendente) 
                        (value no)
                        (certainty 30.0))
            )
)

(defrule HOME-QUALITIES::best-bambini-no
            (attribute (nome casa-bambini) (value ?value & no))
            (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))
      =>
            (assert (attribute (nome best-zona) 
                        (value centro)
                        (certainty 80.0))
            )  
            (assert (attribute (nome best-zona) 
                        (value periferia)
                        (certainty 50.0))
            ) 
            (assert (attribute (nome best-zona) 
                        (value prima-cintura)
                        (certainty 50.0))
            )  
)

(defrule HOME-QUALITIES::best-bambini-indip-no
            (attribute (nome casa-bambini) (value ?value & no))
            (not (attribute (nome casa-indipendente) (value ?value2 & si | no)))
      =>
            (assert (attribute (nome best-indipendente) 
                        (value si)
                        (certainty 40.0))
            )
            (assert (attribute (nome best-indipendente) 
                        (value no)
                        (certainty 50.0))
            )  
)

; Scelta studente
(defrule HOME-QUALITIES::scelta-studente-si
	(attribute (nome casa-studente) (value ?value & si))
	(attribute (nome casa-citta) (value ?casaCitta))
        (or
                (quartiere (citta ?casaCitta) (nome ?nq)(servizi $? universita $?))
                (quartiere (citta ?casaCitta) (nome ?nq)(servizi $? pullman $?))
                (quartiere (citta ?casaCitta) (nome ?nq)(servizi $? metro $?))
        )
        
        (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))

	=>
		(assert (attribute (nome best-quartiere)
				(value ?nq)
				(certainty 90.0)))
                (assert (attribute (nome best-zona)
                                (value centro)
                                (certainty 70.0)))
                (assert (attribute (nome best-zona)
                                (value periferia)
                                (certainty 60.0)))
                (assert (attribute (nome best-zona)
                                (value prima-cintura)
                                (certainty 60.0)))
)

(defrule HOME-QUALITIES::scelta-studente-sicuro
	(attribute (nome casa-studente) (value ?value & si))
	(attribute (nome casa-citta) (value ?casaCitta))
        (or
                (quartiere (citta ?casaCitta) (nome ?nq)(servizi $? universita $?))
                (quartiere (citta ?casaCitta) (nome ?nq)(servizi $? pullman $?))
                (quartiere (citta ?casaCitta) (nome ?nq)(servizi $? metro $?))
        )

        (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura))

	=>
		(assert (attribute (nome best-quartiere)
				(value ?nq)
				(certainty 90.0)))
)

;check pendolare
(defrule HOME-QUALITIES::scelta-pendolare-si
        (attribute (nome casa-pendolare) (value ?value & si))
        (attribute (nome casa-citta) (value ?casaCitta))
        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? stazione $?))
        (not (attribute (nome casa-zona)(value ?value2 & centro | periferifa | prima-cintura)))

        => 
        (assert (attribute (nome best-quartiere)
                        (value ?nq)
                        (certainty 90.0)))
        (assert (attribute (nome best-zona)
                        (value centro)
                        (certainty 50.0)))
        (assert (attribute (nome best-zona)
                        (value periferia)
                        (certainty 85.0)))
        (assert (attribute (nome best-zona)
                        (value prima-cintura)
                        (certainty 60.0)))
)

(defrule HOME-QUALITIES::scelta-pendolare-sicuro
        (attribute (nome casa-pendolare) (value ?value & si))
        (attribute (nome casa-citta) (value ?casaCitta))
        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? stazione $?))
        (attribute (nome casa-zona)(value ?value2 & centro | periferifa | prima-cintura))

        => 
        (assert (attribute (nome best-quartiere)
                        (value ?nq)
                        (certainty 90.0)))
        
)

(defrule HOME-QUALITIES::scelta-pendolare-no
        (attribute (nome casa-pendolare) (value ?value & no))
        (attribute (nome casa-citta) (value ?casaCitta))
        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? stazione $?))
        (not (attribute (nome casa-zona)(value ?value2 & centro | periferifa | prima-cintura)))

        => 
        (assert (attribute (nome best-quartiere)
                        (value ?nq)
                        (certainty 30.0)))
        (assert (attribute (nome best-zona)
                        (value centro)
                        (certainty 30.0)))
        (assert (attribute (nome best-zona)
                        (value periferia)
                        (certainty 30.0)))
        (assert (attribute (nome best-zona)
                        (value prima-cintura)
                        (certainty 30.0)))
)

(defrule HOME-QUALITIES::scelta-pendolare-no-sicuro
        (attribute (nome casa-pendolare) (value ?value & no))
        (attribute (nome casa-citta) (value ?casaCitta))
        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? stazione $?))
        (attribute (nome casa-zona)(value ?value2 & centro | periferifa | prima-cintura))

        => 
        (assert (attribute (nome best-quartiere)
                        (value ?nq)
                        (certainty 30.0)))
)

;scelta automunito e se prima non ha inserito casa-box no o si
(defrule HOME-QUALITIES::scelta-mezzi
        (attribute (nome casa-mezzi) (value ?value & si))
        (not (attribute (nome casa-box) (value ?value2 & si | no)))
        =>
        (assert (attribute (nome best-boxauto)
                (value si)
                (certainty 80.0))
        )
        (assert (attribute (nome best-boxauto)
                (value no)
                (certainty 30.0))
        )

)

(defrule HOME-QUALITIES::scelta-mezzi-sicuro
        (attribute (nome casa-mezzi) (value ?value & si))
        (attribute (nome casa-box) (value ?value2 & si | no))

        =>
        (assert (attribute (nome best-boxauto)
                (value si)
                (certainty 80.0))
        )
        (assert (attribute (nome best-boxauto)
                (value no)
                (certainty 30.0))
        )

)

;se non e automunito 
(defrule HOME-QUALITIES::scelta-mezzi
        (attribute (nome casa-mezzi)(value ?value & no))
        (attribute (nome casa-citta) (value ?casaCitta))
        (attribute (nome casa-box) (value ?value2 & si | no))
        (or 
                (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? pullman $?))
                (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? metro $?))
                (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? stazione $?))
        )
        (not (attribute (nome casa-zona) (value ?value3 & centro | periferifa | prima-cintura)))

        =>
        (assert (attribute (nome best-quartiere)
			(value ?nq)
			(certainty 90.0)))
        (assert (attribute (nome best-zona)
                        (value periferia)
                        (certainty 70.0)))
         (assert (attribute (nome best-zona)
                        (value centro)
                        (certainty 50.0)))
         (assert (attribute (nome best-zona)
                        (value prima-cintura)
                        (certainty 60.0)))
        (assert (attribute (nome best-boxauto)
                (value si)
                (certainty 30.0)))
        (assert (attribute (nome best-boxauto)
                (value no)
                (certainty 30.0)))

)

(defrule HOME-QUALITIES::scelta-mezzi-sicuro
        (attribute (nome casa-mezzi)(value ?value & no))
        (attribute (nome casa-citta) (value ?casaCitta))
        (not (attribute (nome casa-box) (value ?value2 & si | no)))
        (or 
                (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? pullman $?))
                (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? metro $?))
                (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? stazione $?))
        )
        (attribute (nome casa-zona)(value ?value3 & centro | periferifa | prima-cintura))

        =>
        (assert (attribute (nome best-quartiere)
			(value ?nq)
			(certainty 90.0)))
        (assert (attribute (nome best-boxauto)
                        (value si)
                        (certainty 30.0)))
        (assert (attribute (nome best-boxauto)
                        (value no)
                        (certainty 30.0)))
)

; Scelta fidanzata
(defrule HOME-QUALITIES::scelta-fidanzata-ottima
		(attribute (nome casa-fidanzata) (value ?value & si))
		(attribute (nome casa-citta) (value ?casaCitta))
		(or 
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? pizzerie $?))
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? bar $?))
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? ristoranti $?))
                )        
                (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura))
	=>
		(assert (attribute (nome best-quartiere)
				(value ?nq)
				(certainty 90.0)))
                
)

(defrule HOME-QUALITIES::scelta-fidanzata-si
		(attribute (nome casa-fidanzata) (value ?value & si))
		(attribute (nome casa-citta) (value ?casaCitta))
		(or 
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? pizzerie $?))
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? bar $?))
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? ristoranti $?))
                )  
                (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))
		
	=>
		(assert (attribute (nome best-quartiere)
				(value ?nq)
				(certainty 90.0)))
                (assert (attribute (nome best-zona)
				(value centro)
				(certainty 85.0)))
                (assert (attribute (nome best-zona)
				(value periferia)
				(certainty 50.0)))
                (assert (attribute (nome best-zona)
				(value prima-cintura)
				(certainty 70.0)))
)

(defrule HOME-QUALITIES::scelta-fidanzata-no-sicuro
		(attribute (nome casa-fidanzata) (value ?value & no))
		(attribute (nome casa-citta) (value ?casaCitta))
		(or 
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? pizzerie $?))
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? bar $?))
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? ristoranti $?))
                )  
                (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura))
	=>
		(assert (attribute (nome best-quartiere)
				(value ?nq)
				(certainty 30.0)))
)

(defrule HOME-QUALITIES::scelta-fidanzata-no
		(attribute (nome casa-fidanzata) (value ?value & no))
		(attribute (nome casa-citta) (value ?casaCitta))
		(or 
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? pizzerie $?))
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? bar $?))
                        (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? ristoranti $?))
                )  
                (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))
		
	=>
		(assert (attribute (nome best-quartiere)
				(value ?nq)
				(certainty 30.0)))
                (assert (attribute (nome best-zona)
				(value centro)
				(certainty 30.0)))
                (assert (attribute (nome best-zona)
				(value periferia)
				(certainty 30.0)))
                (assert (attribute (nome best-zona)
				(value prima-cintura)
				(certainty 30.0)))
)

; Scelta shopping
(defrule HOME-QUALITIES::best-shopping
            (attribute (nome fa-shopping) (value ?value & si))
            (attribute (nome casa-citta) (value ?casaCitta))
            (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? centro-commerciale $?))
            (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 70.0))
            )
            (assert (attribute (nome best-zona) 
                        (value centro)
                        (certainty 85.0))
            )
            (assert (attribute (nome best-zona) 
                        (value periferia)
                        (certainty 40.0))
            ) 
            (assert (attribute (nome best-zona) 
                        (value prima-cintura)
                        (certainty 50.0))
            )  
)

(defrule HOME-QUALITIES::best-shopping-sicuro
            (attribute (nome fa-shopping) (value ?value & si))
            (attribute (nome casa-citta) (value ?casaCitta))
            (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? centro-commerciale $?))
            (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 70.0))
            )
)

(defrule HOME-QUALITIES::best-shopping-ogni-tanto
            (attribute (nome fa-shopping) (value ?value & ogni-tanto))
            (attribute (nome casa-citta) (value ?casaCitta))
            (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? centro-commerciale $?))
            (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 55.0))
            )
            (assert (attribute (nome best-zona) 
                        (value centro)
                        (certainty 55.0))
            )
             (assert (attribute (nome best-zona) 
                        (value periferia)
                        (certainty 55.0))
            )  
             (assert (attribute (nome best-zona) 
                        (value prima-cintura)
                        (certainty 55.0))
            )    
)

(defrule HOME-QUALITIES::best-shopping-ogni-tanto-sicuro
            (attribute (nome fa-shopping) (value ?value & ogni-tanto))
            (attribute (nome casa-citta) (value ?casaCitta))
            (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? centro-commerciale $?))
            (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 55.0))
            )
)

(defrule HOME-QUALITIES::best-shopping-no
            (attribute (nome fa-shopping) (value ?value & no))
            (attribute (nome casa-citta) (value ?casaCitta))
            (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? centro-commerciale $?))
            (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 30.0))
            )
            (assert (attribute (nome best-zona) 
                        (value centro)
                        (certainty 30.0))
            )
            (assert (attribute (nome best-zona) 
                        (value periferia)
                        (certainty 30.0))
            )
            (assert (attribute (nome best-zona) 
                        (value prima-cintura)
                        (certainty 30.0))
            )      
)

(defrule HOME-QUALITIES::best-shopping-no-sicuro
            (attribute (nome fa-shopping) (value ?value & no))
            (attribute (nome casa-citta) (value ?casaCitta))
            (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? centro-commerciale $?))
            (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 30.0))
            ) 
)


; Scelta cane
(defrule HOME-QUALITIES::best-cane-si
            (attribute (nome casa-animale) (value ?value & si))
            (not (attribute (nome casa-indipendente) (value ?value2 & si | no)))
      =>
            (assert (attribute (nome best-indipendente) 
                        (value si)
                        (certainty 80.0))
            )
            (assert (attribute (nome best-indipendente) 
                        (value no)
                        (certainty 30.0))
            )
)

(defrule HOME-QUALITIES::best-cane-si-quart
            (attribute (nome casa-animale) (value ?value & si))
            (attribute (nome casa-citta) (value ?casaCitta))
            (quartiere (citta ?casaCitta) (nome ?nq) (servizi $? parco $?))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 70.0))
            )
)

;;*************************
;;* MODIFICA PREFERENZE   *
;;*************************

(defmodule MODIFICA-PREFERENZE (import MAIN ?ALL))

; Modifiche preferenze zona

(defrule MODIFICA-PREFERENZE::modifica-zona-cancella
      (declare (salience 20))
           (or    (attribute (nome modifiche) (value ?value & zona-centro | zona-periferia | zona-prima-cintura))
                  (attribute (nome modifiche-casa-zona) (value ?value & ~qualsiasi))
           )
      ?a <- (attribute (nome best-zona) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-zona-inserisci
      (declare (salience 10))
       (or  ?a <-  (attribute (nome modifiche) (value ?value & zona-centro | zona-periferia | zona-prima-cintura))
            ?a <-  (attribute (nome modifiche-casa-zona) (value ?value & centro | periferia | prima-cintura))
       )
      =>
            (if (or (eq ?value zona-centro) (eq ?value centro))
             then (assert (attribute (nome best-zona) 
                     (value centro)))
            )
            (if (or (eq ?value zona-periferia) (eq ?value periferia))
             then (assert (attribute (nome best-zona) 
                     (value periferia)))
            )
            (if (or (eq ?value zona-prima-cintura) (eq ?value prima-cintura))
             then (assert (attribute (nome best-zona) 
                     (value prima-cintura)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-zona-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-casa-zona) (value ?value & preferisco-centro | preferisco-periferia | preferisco-prima-cintura))
      =>
            (if (eq ?value preferisco-centro)
             then (assert (attribute (nome best-zona) 
                          (value centro)
                          (certainty 80.0)))
                  (assert (attribute (nome best-zona) 
                          (value periferia)
                          (certainty 30.0)))
                  (assert (attribute (nome best-zona) 
                          (value prima-cintura)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-periferia)
             then (assert (attribute (nome best-zona) 
                          (value periferia)
                          (certainty 80.0)))
                  (assert (attribute (nome best-zona) 
                          (value centro)
                          (certainty 30.0)))
                  (assert (attribute (nome best-zona) 
                          (value prima-cintura)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-prima-cintura)
             then (assert (attribute (nome best-zona) 
                          (value prima-cintura)
                          (certainty 80.0)))
                  (assert (attribute (nome best-zona) 
                          (value centro)
                          (certainty 30.0)))
                  (assert (attribute (nome best-zona) 
                          (value periferia)
                          (certainty 30.0)))
            )
            (retract ?a)
)

; Modifiche preferenze metri quadri

(defrule MODIFICA-PREFERENZE::modifica-metri-quadri-cancella
      (declare (salience 20))
            (attribute (nome modifiche) (value ?value & metri-30 | metri-40 | metri-50 | metri-60 | metri-70 | metri-80 | metri-90 | metri-100 | metri-120 | metri-140 | metri-160 | metri-180 | metri-200 | metri-250 | metri-300 | metri-400 | metri-500))
      ?a <- (attribute (nome best-metriquadri) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-metri-quadri-inserisci
      (declare (salience 10))
      ?a <- (attribute (nome modifiche) (value ?value & metri-30 | metri-40 | metri-50 | metri-60 | metri-70 | metri-80 | metri-90 | metri-100 | metri-120 | metri-140 | metri-160 | metri-180 | metri-200 | metri-250 | metri-300 | metri-400 | metri-500))
      =>
            (if (eq ?value metri-30)
             then (assert (attribute (nome best-metriquadri) 
                     (value 30)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 40)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 20)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-40)
             then (assert (attribute (nome best-metriquadri) 
                     (value 40)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 50)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 30)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-50)
             then (assert (attribute (nome best-metriquadri) 
                     (value 50)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 60)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 40)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-60)
             then (assert (attribute (nome best-metriquadri) 
                     (value 60)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 70)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 50)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-70)
             then (assert (attribute (nome best-metriquadri) 
                     (value 70)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 80)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 60)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-80)
             then (assert (attribute (nome best-metriquadri) 
                     (value 80)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 90)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 70)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-90)
             then (assert (attribute (nome best-metriquadri) 
                     (value 90)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 100)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 80)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-100)
             then (assert (attribute (nome best-metriquadri) 
                     (value 100)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 110)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 90)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-120)
             then (assert (attribute (nome best-metriquadri) 
                     (value 120)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 130)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 110)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-140)
             then (assert (attribute (nome best-metriquadri) 
                     (value 140)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 150)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 130)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-160)
             then (assert (attribute (nome best-metriquadri) 
                     (value 160)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 170)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 150)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-180)
             then (assert (attribute (nome best-metriquadri) 
                     (value 180)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 190)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 170)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-200)
             then (assert (attribute (nome best-metriquadri) 
                     (value 200)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 210)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 190)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-250)
             then (assert (attribute (nome best-metriquadri) 
                     (value 250)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 260)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 240)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-300)
             then (assert (attribute (nome best-metriquadri) 
                     (value 300)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 310)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 290)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-400)
             then (assert (attribute (nome best-metriquadri) 
                     (value 400)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 410)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 390)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-500)
             then (assert (attribute (nome best-metriquadri) 
                     (value 500)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 510)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 490)
                     (certainty 70.0)))
            )
            (retract ?a)
)

; Modifiche preferenza indipendente
(defrule MODIFICA-PREFERENZE::modifica-indipendente-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & indipendente-si | indipendente-no))
            (attribute (nome modifiche-casa-indipendente) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-indipendente) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-indipendente-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & indipendente-si | indipendente-no))
            ?a <- (attribute (nome modifiche-casa-indipendente) (value ?value & si | no))
      )
      =>
            (if (or (eq ?value indipendente-si) (eq ?value si))
             then (assert (attribute (nome best-indipendente) 
                     (value si)))
             else (assert (attribute (nome best-indipendente) 
                     (value no)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-indipendente-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-casa-indipendente) (value ?value & preferisco-si | preferisco-no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-indipendente) 
                     (value si)
                     (certainty 80.0)))
                  (assert (attribute (nome best-indipendente) 
                     (value no)
                     (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-indipendente) 
                     (value no)
                     (certainty 80.0)))
                  (assert (attribute (nome best-indipendente) 
                     (value si)
                     (certainty 30.0)))
            )
            (retract ?a)
)

; Modifiche preferenza indipendente

(defrule MODIFICA-PREFERENZE::modifica-piano-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & piano-alto | piano-basso | piano-terra))
            (attribute (nome modifiche-casa-piano) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-piano) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-piano-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & piano-alto | piano-basso | piano-terra))
            ?a <- (attribute (nome modifiche-casa-piano) (value ?value & alto | basso | terra))
      )
      =>
            (if (or (eq ?value piano-alto) (eq ?value alto))
             then (assert (attribute (nome best-piano) 
                     (value alto)))
            )
            (if (or (eq ?value piano-basso) (eq ?value basso))
             then (assert (attribute (nome best-piano) 
                     (value basso)))
            )
            (if (or (eq ?value piano-terra) (eq ?value terra))
             then (assert (attribute (nome best-piano) 
                     (value terra)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-piano-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-casa-piano) (value ?value & preferisco-alto | preferisco-basso | preferisco-terra))
      =>
            (if (eq ?value preferisco-alto)
             then (assert (attribute (nome best-piano) 
                          (value alto)
                          (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                          (value terra)
                          (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                          (value basso)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-basso)
             then (assert (attribute (nome best-piano) 
                          (value basso)
                          (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                          (value terra)
                          (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                          (value alto)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-terra)
             then (assert (attribute (nome best-piano) 
                          (value terra)
                          (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                          (value basso)
                          (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                          (value alto)
                          (certainty 30.0)))
            )
            (retract ?a)
)

; Modifiche preferenza ascensore

(defrule MODIFICA-PREFERENZE::modifica-ascensore-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & ascensore-si | ascensore-no))
            (attribute (nome modifiche-casa-ascensore) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-ascensore) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-ascensore-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & ascensore-si | ascensore-no))
            ?a <- (attribute (nome modifiche-casa-ascensore) (value ?value & si | no))
      )
      =>
            (if (or (eq ?value ascensore-si) (eq ?value si))
             then (assert (attribute (nome best-ascensore) 
                     (value si)))
             else (assert (attribute (nome best-ascensore) 
                     (value no)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-ascensore-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-casa-ascensore) (value ?value & preferisco-si | preferisco-no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 30.0)))
            )
            (retract ?a)
)

; Modifiche preferenza bagni

(defrule MODIFICA-PREFERENZE::modifica-bagni-cancella
      (declare (salience 20))
            (attribute (nome modifiche) (value ?value & bagni-1 | bagni-2 | bagni-3 | bagni-4))
      ?a <- (attribute (nome best-bagni) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-bagni-inserisci
      (declare (salience 10))
      ?a <- (attribute (nome modifiche) (value ?value & bagni-1 | bagni-2 | bagni-3 | bagni-4))
      =>
            (if (eq ?value bagni-1)
             then (assert (attribute (nome best-bagni) 
                     (value 1)))
                  (assert (attribute (nome best-bagni) 
                     (value 2)
                     (certainty 90.0)))
            )
            (if (eq ?value bagni-2)
             then (assert (attribute (nome best-bagni) 
                     (value 2)))
                  (assert (attribute (nome best-bagni) 
                     (value 3)
                     (certainty 90.0)))
                  (assert (attribute (nome best-bagni) 
                     (value 1)
                     (certainty 60.0)))
            )
            (if (eq ?value bagni-3)
             then (assert (attribute (nome best-bagni) 
                     (value 3)))
                  (assert (attribute (nome best-bagni) 
                     (value 4)
                     (certainty 90.0)))
                  (assert (attribute (nome best-bagni) 
                     (value 2)
                     (certainty 60.0)))
            )
            (if (eq ?value bagni-4)
             then (assert (attribute (nome best-bagni) 
                     (value 4)))
                  (assert (attribute (nome best-bagni) 
                     (value 3)
                     (certainty 60.0)))
            )
            (retract ?a)
)

; Modifiche preferenza balcone
(defrule MODIFICA-PREFERENZE::modifica-balcone-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & balcone-si | balcone-no))
            (attribute (nome modifiche-casa-balcone) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-balcone) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-balcone-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & balcone-si | balcone-no))
            ?a <- (attribute (nome modifiche-casa-balcone) (value ?value & si | no))
      )
      =>
            (if (or (eq ?value balcone-si) (eq ?value si))
             then (assert (attribute (nome best-balcone) 
                     (value si)))
             else (assert (attribute (nome best-balcone) 
                     (value no)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-balcone-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-casa-balcone) (value ?value & preferisco-si | preferisco-no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-balcone) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-balcone) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-balcone) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-balcone) 
                          (value si)
                          (certainty 30.0)))
            )
            (retract ?a)
)

; Modifiche preferenza boxauto
(defrule MODIFICA-PREFERENZE::modifica-boxauto-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & boxauto-si | boxauto-no))
            (attribute (nome modifiche-casa-box) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-boxauto) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-boxauto-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & boxauto-si | boxauto-no))
            ?a <- (attribute (nome modifiche-casa-box) (value ?value & si | no))
      )
      =>
            (if (or (eq ?value boxauto-si) (eq ?value si))
             then (assert (attribute (nome best-boxauto) 
                     (value si)))
             else (assert (attribute (nome best-boxauto) 
                     (value no)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-boxauto-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-casa-box) (value ?value & preferisco-si | preferisco-no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-boxauto) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-boxauto) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-boxauto) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-boxauto) 
                          (value si)
                          (certainty 30.0)))
            )
            (retract ?a)
)

; Modifiche preferenza deposito
(defrule MODIFICA-PREFERENZE::modifica-deposito-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & deposito-si | deposito-no))
            (attribute (nome modifiche-casa-deposito) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-deposito) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-deposito-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & deposito-si | deposito-no))
            ?a <- (attribute (nome modifiche-casa-deposito) (value ?value & si | no))
      )
      =>
            (if (or (eq ?value deposito-si) (eq ?value si))
             then (assert (attribute (nome best-deposito) 
                     (value si)))
             else (assert (attribute (nome best-deposito) 
                     (value no)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-deposito-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-casa-deposito) (value ?value & preferisco-si | preferisco-no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-deposito) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-deposito) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-deposito) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-deposito) 
                          (value si)
                          (certainty 30.0)))
            )
            (retract ?a)
)

;;*************************
;;* REGOLE SELEZIONE CASE *
;;*************************

(defmodule CASE (import MAIN ?ALL)
                (import QUARTIERI ?ALL)
                (export ?ALL)
)

(deffacts any-attributes
  (attribute (nome best-metriquadri) (value any))
  (attribute (nome best-vani) (value any))
  (attribute (nome best-servizi) (value any))
  (attribute (nome best-piano) (value any))
  (attribute (nome best-citta) (value any))
  (attribute (nome best-zona) (value any))
  (attribute (nome best-quartiere) (value any))
  (attribute (nome best-ascensore) (value any))
  (attribute (nome best-boxauto) (value any))
  (attribute (nome best-balcone) (value any))
  (attribute (nome best-indipendente) (value any))
  (attribute (nome best-bagni) (value any))
)

(deftemplate CASE::casa
  (slot nome (default ?NONE))
  (slot metriquadri (default any))
  (slot vani (default any))
  (slot servizi (default any))
  (multislot piano (default any))
  (slot citta (default any))
  (slot zona (default any))
  (slot quartiere (default any))
  (slot ascensore (default any))
  (multislot boxauto (default any))
  (slot deposito (default any))
  (slot balcone (default any))
  (slot prezzo (default any))
  (slot indipendente (default any))
  (slot bagni (default any))
)


(deffacts CASE::casa-lista 

;TORINO
    (casa (nome "Appartamento in Via Arcivescovado, 9") (metriquadri 50) (vani 2) (piano basso 1) (citta torino) (zona centro) (quartiere centro) 
            (ascensore no) (boxauto no) (deposito no) (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Casa in Via Sesia, 18") (metriquadri 70) (vani 2) (piano alto 6) (citta torino) (zona periferia) (quartiere barriera-milano) 
            (ascensore si) (boxauto si 15) (deposito no)  (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Villa in Via Antonio Bertola, 11") (metriquadri 250) (vani 7) (citta torino) (zona centro) (quartiere centro) 
            (boxauto no) (deposito si)  (balcone si) (indipendente si) (bagni 3))
    (casa (nome "Casa indipendente in Corso Telesio, 1 ") (metriquadri 60) (vani 5) (piano basso 2) (citta torino) (zona prima-cintura) (quartiere parella) 
            (ascensore no) (boxauto si 15) (deposito no) (balcone si) (indipendente si) (bagni 1))
    (casa (nome "Casa indipendente in Corso Telesio, 22") (metriquadri 100) (vani 2) (citta torino) (zona prima-cintura) (quartiere parella) 
            (deposito si) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Appartamento in Via Saluzzo, 7") (metriquadri 80) (vani 2) (piano alto 5) (citta torino) (zona periferia) (quartiere san-salvario) 
            (deposito no) (boxauto si 25) (balcone si)  (indipendente no) (bagni 1))
    (casa (nome "Casa indipendente in Corso Luigi Settembrini, 132") (metriquadri 140) (vani 3) (citta torino) (zona prima-cintura) (quartiere mirafiori-nord) 
            (deposito si) (balcone si)  (indipendente si) (bagni 2))
    (casa (nome "Appartamente in Corso Luigi Settembrini, 19") (metriquadri 40) (vani 2) (piano terra 0) (citta torino) (zona prima-cintura) (quartiere mirafiori-nord) 
            (deposito no) (balcone si)  (indipendente no) (bagni 1))
    (casa (nome "Casa in Piazza Castello") (metriquadri 70) (vani 1) (citta torino) (zona centro) (quartiere centro) 
            (deposito no) (boxauto si 20) (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Casa indipendente in Corso Massimo D'Azeglio, 55") (metriquadri 200) (vani 4) (citta torino) (zona periferia) (quartiere san-salvario) 
            (deposito si) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Casa indipendente in Corso Giulio Cesare, 44") (metriquadri 80) (vani 2) (citta torino) (zona periferia) (quartiere barriera-milano) 
            (deposito no) (boxauto si 35) (indipendente si) (bagni 2))
    (casa (nome "Appartamento indipendente in Corso Galileo Ferraris, 56") (metriquadri 90) (vani 3) (citta torino) (zona prima-cintura) (quartiere crocetta) 
            (deposito si) (boxauto no) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Casa in Corso Re Umberto, 43") (metriquadri 70) (vani 2) (piano basso 1) (citta torino) (zona prima-cintura) (quartiere crocetta) 
            (deposito si) (boxauto no) (balcone no) (indipendente no) (bagni 1))


;ROMA
    (casa (nome "Appartamento in Via Catania, 9") (metriquadri 50) (vani 2) (piano basso 1) (citta roma) (zona centro) (quartiere centro) 
            (ascensore no) (boxauto no) (deposito no) (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Casa in Via Dei Ramni, 18") (metriquadri 70) (vani 2) (piano alto 6) (citta roma) (zona periferia) (quartiere san-basilio) 
            (ascensore si) (boxauto si 15) (deposito no)  (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Villa in Via Nomentana, 11") (metriquadri 250) (vani 7) (citta roma) (zona centro) (quartiere centro) 
            (boxauto no) (deposito si)  (balcone si) (indipendente si) (bagni 3))
    (casa (nome "Casa indipendente in Via Salaria, 1 ") (metriquadri 60) (vani 5) (piano basso 2) (citta roma) (zona prima-cintura) (quartiere roma-sud) 
            (ascensore no) (boxauto si 15) (deposito no) (balcone si) (indipendente si) (bagni 1))
    (casa (nome "Casa indipendente Via Isonzo, 22") (metriquadri 100) (vani 2) (citta roma) (zona prima-cintura) (quartiere roma-sud) 
            (deposito si) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Appartamento in Via Savoia, 7") (metriquadri 80) (vani 2) (piano alto 5) (citta roma) (zona periferia) (quartiere ostiense) 
            (deposito no) (boxauto si 25) (balcone si)  (indipendente no) (bagni 1))
    (casa (nome "Casa indipendente in Corso Trieste, 132") (metriquadri 140) (vani 3) (citta roma) (zona prima-cintura) (quartiere municipio) 
            (deposito si) (balcone si)  (indipendente si) (bagni 2))
    (casa (nome "Appartamente in Corso Trieste, 19") (metriquadri 40) (vani 2) (piano terra 0) (citta roma) (zona prima-cintura) (quartiere municipio) 
            (deposito no) (balcone si)  (indipendente no) (bagni 1))
    (casa (nome "Casa in Piazza del Colosseo") (metriquadri 70) (vani 1) (citta roma) (zona centro) (quartiere centro) 
            (deposito no) (boxauto si 20) (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Casa indipendente in Villa di Patrizi, 55") (metriquadri 200) (vani 4) (citta roma) (zona periferia) (quartiere ostiense) 
            (deposito si) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Casa indipendente in Via Goito, 44") (metriquadri 80) (vani 2) (citta roma) (zona periferia) (quartiere san-basilio) 
            (deposito no) (boxauto si 35) (indipendente si) (bagni 2))
    (casa (nome "Appartamento indipendente in Via Sicilia, 56") (metriquadri 90) (vani 3) (citta roma) (zona prima-cintura) (quartiere roma-ovest) 
            (deposito si) (boxauto no) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Casa in Corso Via Reno, 43") (metriquadri 70) (vani 2) (piano basso 1) (citta roma) (zona prima-cintura) (quartiere roma-ovest) 
            (deposito si) (boxauto no) (balcone no) (indipendente no) (bagni 1))

;MILANO
    (casa (nome "Appartamento in Via Pietro Mascagni, 9") (metriquadri 50) (vani 2) (piano basso 1) (citta milano) (zona centro) (quartiere centro) 
            (ascensore no) (boxauto no) (deposito no) (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Casa in Via Fontana, 18") (metriquadri 70) (vani 2) (piano alto 6) (citta milano) (zona periferia) (quartiere rozzano) 
            (ascensore si) (boxauto si 15) (deposito no)  (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Villa in Via Antonio Kramer, 11") (metriquadri 250) (vani 7) (citta milano) (zona centro) (quartiere centro) 
            (boxauto no) (deposito si)  (balcone si) (indipendente si) (bagni 3))
    (casa (nome "Casa indipendente in Via Pisacane, 1 ") (metriquadri 60) (vani 5) (piano basso 2) (citta milano) (zona prima-cintura) (quartiere bicocca) 
            (ascensore no) (boxauto si 15) (deposito no) (balcone si) (indipendente si) (bagni 1))
    (casa (nome "Casa indipendente in Via Carlo Poerio, 22") (metriquadri 100) (vani 2) (citta milano) (zona prima-cintura) (quartiere bicocca) 
            (deposito si) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Appartamento in Via Cellini, 7") (metriquadri 80) (vani 2) (piano alto 5) (citta milano) (zona periferia) (quartiere loreto) 
            (deposito no) (boxauto si 25) (balcone si)  (indipendente no) (bagni 1))
    (casa (nome "Casa indipendente in Via Emilio Caldara, 132") (metriquadri 140) (vani 3) (citta milano) (zona prima-cintura) (quartiere navigli) 
            (deposito si) (balcone si)  (indipendente si) (bagni 2))
    (casa (nome "Appartamente in Via Emilio Caldara, 19") (metriquadri 40) (vani 2) (piano terra 0) (citta milano) (zona prima-cintura) (quartiere navigli) 
            (deposito no) (balcone si)  (indipendente no) (bagni 1))
    (casa (nome "Casa in Piazza Duomo") (metriquadri 70) (vani 1) (citta milano) (zona centro) (quartiere centro) 
            (deposito no) (boxauto si 20) (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Casa indipendente in Via Antonio Fogazzaro, 55") (metriquadri 200) (vani 4) (citta milano) (zona periferia) (quartiere loreto) 
            (deposito si) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Casa indipendente in Via Spartaco, 44") (metriquadri 80) (vani 2) (citta milano) (zona periferia) (quartiere rozzano) 
            (deposito no) (boxauto si 35) (indipendente si) (bagni 2))
    (casa (nome "Appartamento indipendente in Via Cadore, 56") (metriquadri 90) (vani 3) (citta milano) (zona prima-cintura) (quartiere citylife) 
            (deposito si) (boxauto no) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Casa in Corso Via Marcona, 43") (metriquadri 70) (vani 2) (piano basso 1) (citta milano) (zona prima-cintura) (quartiere citylife) 
            (deposito si) (boxauto no) (balcone no) (indipendente no) (bagni 1))

;BARI
    (casa (nome "Appartamento in Viale Luigi Einaudi, 9") (metriquadri 50) (vani 2) (piano basso 1) (citta bari) (zona centro) (quartiere centro) 
        (ascensore no) (boxauto no) (deposito no) (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Casa in Via Sacchi, 18") (metriquadri 70) (vani 2) (piano alto 6) (citta bari) (zona periferia) (quartiere san-paolo) 
            (ascensore si) (boxauto si 15) (deposito no)  (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Villa in Via Amendola, 11") (metriquadri 250) (vani 7) (citta bari) (zona centro) (quartiere centro) 
            (boxauto no) (deposito si)  (balcone si) (indipendente si) (bagni 3))
    (casa (nome "Casa indipendente in Via Monte Grappa, 1 ") (metriquadri 60) (vani 5) (piano basso 2) (citta bari) (zona prima-cintura) (quartiere japigia) 
            (ascensore no) (boxauto si 15) (deposito no) (balcone si) (indipendente si) (bagni 1))
    (casa (nome "Casa indipendente in Viale Unita' D'italia, 22") (metriquadri 100) (vani 2) (citta bari) (zona prima-cintura) (quartiere japigia) 
            (deposito si) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Appartamento in Via San Lorenzo, 7") (metriquadri 80) (vani 2) (piano alto 5) (citta bari) (zona periferia) (quartiere liberta') 
            (deposito no) (boxauto si 25) (balcone si)  (indipendente no) (bagni 1))
    (casa (nome "Casa indipendente in Via Piave, 132") (metriquadri 140) (vani 3) (citta bari) (zona prima-cintura) (quartiere palese) 
            (deposito si) (balcone si)  (indipendente si) (bagni 2))
    (casa (nome "Appartamente in Via Piave, 19") (metriquadri 40) (vani 2) (piano terra 0) (citta bari) (zona prima-cintura) (quartiere palese) 
            (deposito no) (balcone si)  (indipendente no) (bagni 1))
    (casa (nome "Casa in Piazza Del Ferrarese") (metriquadri 70) (vani 1) (citta bari) (zona centro) (quartiere centro) 
            (deposito no) (boxauto si 20) (balcone si) (indipendente no) (bagni 1))
    (casa (nome "Casa indipendente in Via Martin L. King, 55") (metriquadri 200) (vani 4) (citta bari) (zona periferia) (quartiere liberta') 
            (deposito si) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Casa indipendente in Corso Alcide De Gasperi, 44") (metriquadri 80) (vani 2) (citta bari) (zona periferia) (quartiere san-paolo) 
            (deposito no) (boxauto si 35) (indipendente si) (bagni 2))
    (casa (nome "Appartamento indipendente in Via Petroni, 56") (metriquadri 90) (vani 3) (citta bari) (zona prima-cintura) (quartiere poggiofranco) 
            (deposito si) (boxauto no) (balcone si) (indipendente si) (bagni 2))
    (casa (nome "Casa in Corso Alcide De Gasperi, 43") (metriquadri 70) (vani 2) (piano basso 1) (citta bari) (zona prima-cintura) (quartiere poggiofranco) 
            (deposito si) (boxauto no) (balcone no) (indipendente no) (bagni 1))
)
  
(defrule CASE::genera-case
  (attribute (nome casa-prezzo) (value ?prezzoMax))
  (attribute (nome casa-citta) (value ?casaCitta))
  (casa (nome ?nome)
        (metriquadri ?mq)
        (vani ?vani)
        (servizi ?serv)
        (piano $? ?pianoAltezza $?)
        (citta ?citta & ?casaCitta)
        (zona ?zona)
        (quartiere ?quart)
        (ascensore ?asce)
        (boxauto $? ?box $?)
        (deposito ?deposito)
        (balcone ?balcone)
        (prezzo ?prezzo &:(<= (integer ?prezzo) (integer ?prezzoMax)))
        (indipendente ?indip)
        (bagni ?bagni)
  )

  (attribute (nome best-metriquadri) (value ?mq) (certainty ?certainty-1))
  (attribute (nome best-piano) (value ?pianoAltezza) (certainty ?certainty-2))
  (attribute (nome best-zona) (value ?zona) (certainty ?certainty-3))
  (attribute (nome best-boxauto) (value ?box) (certainty ?certainty-7))
  (attribute (nome best-indipendente) (value ?indip) (certainty ?certainty-10))
  (attribute (nome best-deposito) (value ?deposito) (certainty ?certainty-8))
  (attribute (nome best-balcone) (value ?balcone) (certainty ?certainty-9))
  (attribute (nome best-ascensore) (value ?asce) (certainty ?certainty-6))
  (attribute (nome best-bagni) (value ?bagni) (certainty ?certainty-11))
  (not (attribute (nome best-quartiere) (value ?quart) (certainty ?certainty-5)))
  =>
  (assert (attribute (nome casa) (value ?nome)
                (certainty (min ?certainty-1 ?certainty-2 ?certainty-3
                                ?certainty-7 ?certainty-10 ?certainty-8
                                ?certainty-9  ?certainty-6 ?certainty-11
                          ))))
  )

(defrule CASE::genera-case2
  (attribute (nome casa-prezzo) (value ?prezzoMax))
  (attribute (nome casa-citta) (value ?casaCitta))

  (casa (nome ?nome)
        (metriquadri ?mq)
        (vani ?vani)
        (servizi ?serv)
        (piano $? ?pianoAltezza $?)
        (citta ?citta & ?casaCitta)
        (zona ?zona)
        (quartiere ?quart)
        (ascensore ?asce)
        (boxauto $? ?box $?)
        (deposito ?deposito)
        (balcone ?balcone)
        (prezzo ?prezzo &:(<= (integer ?prezzo) (integer ?prezzoMax)))
        (indipendente ?indip)
        (bagni ?bagni)
  )

  (attribute (nome best-metriquadri) (value ?mq) (certainty ?certainty-1))
  (attribute (nome best-piano) (value ?pianoAltezza) (certainty ?certainty-2))
  (attribute (nome best-zona) (value ?zona) (certainty ?certainty-3))
  (attribute (nome best-boxauto) (value ?box) (certainty ?certainty-7))
  (attribute (nome best-indipendente) (value ?indip) (certainty ?certainty-10))
  (attribute (nome best-deposito) (value ?deposito) (certainty ?certainty-8))
  (attribute (nome best-balcone) (value ?balcone) (certainty ?certainty-9))
  (attribute (nome best-ascensore) (value ?asce) (certainty ?certainty-6))
  (attribute (nome best-bagni) (value ?bagni) (certainty ?certainty-11))
  (attribute (nome best-quartiere) (value ?quart) (certainty ?certainty-5))
  =>
  (assert (attribute (nome casa) (value ?nome)
                (certainty (min ?certainty-1 ?certainty-2 ?certainty-3
                                ?certainty-5 ?certainty-7 ?certainty-10 ?certainty-8
                                ?certainty-9  ?certainty-6 ?certainty-11 
                          ))))
)

;;*********************************
;;* REGOLE MODIFICHE DOMANDE REDO *
;;*********************************

(defmodule GENERA-PREZZI    (import MAIN ?ALL)
                            (import QUARTIERI ?ALL)
                            (import CASE ?ALL)
)

(defrule GENERA-PREZZI::genera-prezzo
   ?f <- (casa (prezzo any) (metriquadri ?mq) (quartiere ?q) (boxauto $? ?b ?bmq $?) (deposito ?g))
         (quartiere (nome ?q) (costo-mq ?cmq) (servizi $?s))
   =>    (modify ?f (prezzo (+ (+ (+ (* ?cmq ?mq) 30000) (* (length$ $?s) 5000)) (* ?bmq ?cmq))))
         (modify ?f (servizi (length$ $?s)))         
)

(defrule GENERA-PREZZI::genera-prezzo-any
   ?f <- (casa (prezzo any) (metriquadri ?mq) (quartiere ?q) (boxauto $? ?b $?) (deposito ?g))
         (quartiere (nome ?q) (costo-mq ?cmq) (servizi $?s))
   =>    (if (eq ?g si)
          then (modify ?f (prezzo (+ (+ (+ (* ?cmq ?mq) 30000) (* (length$ $?s) 5000)) 40000)))
         )
         (if (eq ?g no)
          then (modify ?f (prezzo (+ (+ (* ?cmq ?mq) 30000) (* (length$ $?s) 5000))))
         )
         (modify ?f (servizi (length$ $?s)))
)

;;*****************************
;;* STAMPA RISULTATI CASE     *
;;*****************************

(defmodule PRINT-RESULTS (import MAIN ?ALL))

(defrule PRINT-RESULTS::header ""
   (declare (salience 10))
   =>
   ;(printout t t)
   (printout t " ---------------------------------- SELECTED APARTMENT ------------------------------------------")
   ;(assert (phase print-casa))
)

(defrule PRINT-RESULTS::print-casa ""
  ?rem <- (attribute (nome casa) (value ?nome) (certainty ?per))		  
  (not (attribute (nome casa) (certainty ?per1&:(> ?per1 ?per))))
  =>
  (retract ?rem)
  (format t "%n %-24s %2d%%%n" ?nome ?per))

(defrule PRINT-RESULTS::remove-poor-casa-choices ""
  ?rem <- (attribute (nome casa) (certainty ?per&:(< ?per 20)))
  =>

  (retract ?rem))

(defrule PRINT-RESULTS::end-spaces ""
   (not (attribute (nome casa)))
   =>
   (format t "-------------------------------------------------------------------------------------------------------")
   (format t "%n%n")
   ;(assert (attribute (nome fine-ciclo) (value "Fine") (certainty 100.0)))

)
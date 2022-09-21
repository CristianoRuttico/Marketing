#definire clienti attivi e non, soglia: numero di giorni medi tali che  80 % clienti riacquistano entro tale soglia
active <- finale %>% filter(avg_next_purchase <= 60)

#faccio un check
tail(active)


#alternativa mia fatta con finale(quella scelta)
pre_rfm_data <- final %>%
  filter(avg_next_purchase %in% active$avg_next_purchase)

rfm_data_1 <- df_7_tic_clean_final %>%
  filter (ID_CLI %in% pre_rfm_data$ID_CLI)
 
#via standard che avrei dovuto fare con totcli (non scelta)
rfm_data<-df_7_tic_clean_final %>%
  filter (ID_CLI %in% active$ID_CLI)

#impostazione rfm

#calcolo la data corrente dell'analisi
DataAnalisi <- df_7_tic_clean_final %>% summarize(DataAnalisi = max(TIC_DATE))
DataAnalisi

#trovo l'ultima data possibile e la imposto come data di analisi
analysis_date <- as.Date('2019-04-30')

#filtro per prendere solo i purchase
rfm_data_fin <- rfm_data_1 %>% filter(DIREZIONE ==1) %>% mutate(IMPORTO_NETTO = IMPORTO_LORDO-SCONTO) 
rfm_data_fin 

#Calcoliamo gli score tramite un modello di rfm presente nella libreria di R rfm
rfm_result <- rfm_table_order(rfm_data_fin , ID_CLI, TIC_DATE, IMPORTO_NETTO, analysis_date,
                              recency_bins = 3, frequency_bins = 3, monetary_bins = 3) 
rfm_result

rfm_result_fin<-rfm_result$rfm %>%
  mutate(segment=0)


#Creazione segmenti
Diamond<-c(333, 233)
Gold<-c(232, 332, 133)
Silver<-c(231, 331, 132, 223, 323)
Bronze<-c(131, 222, 322, 113, 123)
Copper<-c(221, 321, 112, 122, 213, 313)
Tin<-c(111, 121, 212, 312)
Cheap<-c(211, 311)


#assegnazione segmenti
rfm_result_fin$segment[which(rfm_result_fin$rfm_score %in% Diamond)] = "Diamond"
rfm_result_fin$segment[which(rfm_result_fin$rfm_score %in% Gold)] = "Gold"
rfm_result_fin$segment[which(rfm_result_fin$rfm_score %in% Silver)] = "Silver"
rfm_result_fin$segment[which(rfm_result_fin$rfm_score %in% Bronze)] = "Bronze"
rfm_result_fin$segment[which(rfm_result_fin$rfm_score %in% Copper)] = "Copper"
rfm_result_fin$segment[which(rfm_result_fin$rfm_score %in% Tin)] = "Tin"
rfm_result_fin$segment[which(rfm_result_fin$rfm_score %in% Cheap)] = "Cheap"

rfm_result_fin

#distribuzione segmenti
distr_seg <- rfm_result_fin %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segmento = segment, Clienti = n)

distr_seg

#plot distribuzione segmenti
plot_distr_seg <-ggplot(data=distr_seg, aes(x=Segmento, y=Clienti)) + geom_bar(stat ='identity', fill='#cced00') +theme_classic()+ theme(panel.background = element_rect(fill='#1b1a1a')) 
plot_distr_seg

#palcuni grafici a barre per rappresentare la distribuzione dei customers tra i segmenti con riferimento alla mediana
rfm_plot_median_recency(rfm_result_fin)
rfm_plot_median_frequency(rfm_result_fin)
rfm_plot_median_monetary(rfm_result_fin)

#vediamo la distribuzione di frequency, recency, monetary, per ciascun cliente
rfm_histograms(rfm_result)


# La heatmap ci mostra il valore medio di monetary, per diverse categorie di recency e frequency
rfm_heatmap(rfm_result)

#Distribuzione punteggi monetary per diverse combinazioni punteggi recency e frequency.
rfm_bar_chart(rfm_result)


#clienti per numero di ordini, il grafico è stato tagliato ai fini di interpretabilità
rfm_order_dist(rfm_result) +
  coord_fixed(ratio = 1/80, xlim =c(0, 30), expand=FALSE)

#scatterplots
rfm_rm_plot(rfm_result)
rfm_fm_plot(rfm_result)
rfm_rf_plot(rfm_result)





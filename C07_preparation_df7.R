#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 #### OK

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  
  )
#### EXPLORE VARIABLES in df_7 #### OK

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)  + labs(title="Distribuzione scontrini per orario",
          x ="Orario scontrino", y = "Clienti") 


plot_df7_dist_hour

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
) + labs(title="Distribuzione percentuale scontrini per orario",
         x ="Orario scontrino", y = "Clienti") 


plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
) + labs(title="Distribuzione clienti per reparto",
         x ="Identificativo reparto", y = "Clienti") 


plot_df7_dist_dep

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
) + labs(title="Distribuzione percentuale clienti per reparto",
         x ="Identificativo reparto", y = "Clienti") 

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
) + labs(title="Distribuzione clienti per tipo di giorno",
         x ="Tipo giorno ", y = "Clienti") 

plot_df7_dist_datetyp

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
) + labs(title="Distribuzione percentuale clienti per tipo di giorno",
         x ="Tipo giorno ", y = "Clienti") 


plot_df7_dist_datetyp_percent

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

#### ???? TO DO df_7 ???? ####
# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO
df7_importo_sconto <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , SCONTO = mean(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_importo_sconto

df7_purchase_medie <- df7_importo_sconto %>% filter(DIREZIONE == 1)
df7_purchase_medie

df7_refunde_medie <- df7_importo_sconto %>% filter(DIREZIONE == -1)
df7_refunde_medie

#plot purchase importo lordo
plot_df7purchase_medie_importo <- 
  ggplot(data=df7_purchase_medie
         , aes(x=COD_REPARTO, y=IMPORTO_LORDO)
  )+
  geom_bar(stat="identity"
           , fill="cyan") +
  theme_minimal() + labs(title="Distribuzione importo lordo acquisti medio per reparto",
                         x ="Identificativo reparto", y = "Importo lordo medio") 



plot_df7purchase_medie_importo

#plot purchase sconto
plot_df7_purchase_medie_sconto <- 
  ggplot(data=df7_purchase_medie
         , aes(x=COD_REPARTO, y=SCONTO)
  )+
  geom_bar(stat="identity"
           , fill="cyan") +
  theme_minimal() + labs(title="Distribuzione sconto medio acquisti per reparto",
                         x ="Identificativo reparto", y = "sconto medio")

plot_df7_purchase_medie_sconto

#plot refund importo 
plot_df7_refund_medie_importo <- 
  ggplot(data=df7_refunde_medie
         , aes(x=COD_REPARTO, y=IMPORTO_LORDO)
  )+
  geom_bar(stat="identity"
           , fill="cyan") +
  theme_minimal() + labs(title="Distribuzione importo lordo medio resi per reparto",
                         x ="Identificativo reparto", y = "Importo lordo medio")

plot_df7_refund_medie_importo

#plot refund sconto
plot_df7_refund_medie_sconto <- 
  ggplot(data=df7_refunde_medie
         , aes(x=COD_REPARTO, y=SCONTO)
  )+
  geom_bar(stat="identity"
           , fill="cyan") +
  theme_minimal() + labs(title="Distribuzione sconto medio resi per reparto",
                         x ="Identificativo reparto", y = "Sconto medio")

plot_df7_refund_medie_sconto



# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO) OK
df7_dist_IDaRTICOLO <- df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO) %>%
  summarize(num_TICs = n_distinct(ID_SCONTRINO)
  ) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_IDaRTICOLO



##EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI
df7_importo_sconto_per_IDcliente <- df_7_tic_clean_final %>%
  group_by(ID_CLI, DIREZIONE
  ) %>%
  summarize(IMPORTO_LORDO = mean(IMPORTO_LORDO)
            ,SCONTO = mean(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_importo_sconto_per_IDcliente

df7_purchase_medie_IDCLIENTE <- df7_importo_sconto_per_IDcliente %>% filter(DIREZIONE == 1)


df7_refunde_medie_IDCLIENTE <- df7_importo_sconto_per_IDcliente %>% filter(DIREZIONE == -1)



#PLOTTING THINGS
#plot purchase importo lordo
plot_df7_purchase_medie_importo_PER_iDcliente <- 
  ggplot(data=df7_purchase_medie_IDCLIENTE %>% filter(IMPORTO_LORDO < 1400)
         , aes(x=IMPORTO_LORDO)
  ) +
  geom_histogram(fill="red", alpha=0.7, bins=20) + 
  
  theme_minimal() + labs(title="Distribuzione importo lordo medio acquisti",
                         x ="Importo lordo medio", y = "Clienti")

plot_df7_purchase_medie_importo_PER_iDcliente

#plot purchase sconto
plot_df7_purchase_medie_sconto_PERIDCLIENTE <- 
  ggplot(data=df7_purchase_medie_IDCLIENTE %>% filter(SCONTO < 250)
         , aes(x=SCONTO)
  )+
  geom_histogram(fill="red", alpha=0.7, bins=10) +
  theme_minimal() + labs(title="Distribuzione sconto medio acquisti",
                         x ="Sconto medio", y = "Clienti")

plot_df7_purchase_medie_sconto_PERIDCLIENTE

#plot refund importo 
plot_df7_refund_medie_importo_PERIDCLIENTE <- 
  ggplot(data=df7_refunde_medie_IDCLIENTE  %>% filter(IMPORTO_LORDO > -1000)
         , aes(x=IMPORTO_LORDO)
  )+
  geom_histogram(fill="red", alpha=0.7, bins=10) +
  theme_minimal() + labs(title="Distribuzione importo lordo medio resi",
                         x ="Importo lordo medio", y = "Clienti")

plot_df7_refund_medie_importo_PERIDCLIENTE

#plot refund sconto
plot_df7_refund_medie_sconto_PERIDCLIENTE <- 
  ggplot(data=df7_refunde_medie_IDCLIENTE %>% filter((SCONTO > - 100) & (SCONTO < 25) )
         , aes(x=SCONTO)
  )+
  geom_histogram(fill="red", alpha=0.7, bins=10) +
  theme_minimal() + labs(title="Distribuzione sconto medio resi",
                         x ="Sconto medio", y = "Clienti")

plot_df7_refund_medie_sconto_PERIDCLIENTE

df_7_tic_clean_final

# compute the distribution of customers by number of purchases (as described in the slides)
df7_customers_by_acquisti<- df_7_tic_clean_final %>%
  group_by(ID_CLI) %>%
  summarize(num_acq = n_distinct(ID_SCONTRINO)) %>%
  group_by(num_acq) %>%
  summarize(NUM_cli =  n_distinct(ID_CLI)) %>%
  mutate(num_acq_clean = if_else(num_acq < 6, num_acq, 6L)) %>%
  group_by(num_acq_clean) %>%
  summarise(NUM_cli=sum(NUM_cli)) %>%
  arrange(desc(num_acq_clean)) %>%
  mutate(SUM_cli=cumsum(NUM_cli)) %>%
  arrange(num_acq_clean) %>%
  mutate(num_acq = c(paste0(num_acq_clean, " o pi?"))) %>%
  mutate(SUM_CLI_difference=c(0, SUM_cli[-1]/lag(SUM_cli)[-1]))

df7_customers_by_acquisti[-1,]

#plotting clienti per numero di acquisti
plot_df7_customers_by_acquisti <- (
  ggplot() +
    geom_bar(data=df7_customers_by_acquisti,
             aes(x=num_acq_clean, y=NUM_cli), stat="identity", fill="#cced00") +
    geom_point(data=df7_customers_by_acquisti[-1,],
               aes(x=num_acq_clean, y=SUM_CLI_difference*70599), size = 2, color = "black")+ 
    
    scale_y_continuous(sec.axis=sec_axis(~./70599, name="% confermati")) + 
    
    labs(title='Clienti per numero acqusti', x='Numero acquisti', y='Clienti')
) +theme_light()


plot_df7_customers_by_acquisti 

# compute the days for next purchase curve (as described in the slides)

#Consideriamo solo i clienti che hanno almeno 2 scontrini
Pre <- df_7_tic_clean_final %>%
  group_by(ID_CLI) %>%  
  distinct(TIC_DATE) %>%
  summarise(acquisti = n()) %>%
  filter(acquisti > 1)

df_7_tic_clean_final_piu_acq <- df_7_tic_clean_final %>% filter(ID_CLI %in% Pre$ID_CLI) 

df_for_next_purch <- df_7_tic_clean_final_piu_acq %>%
  filter(DIREZIONE == 1) %>% select(ID_CLI,TIC_DATE, DIREZIONE) %>%
  arrange(ID_CLI)

#prendo un campione, perch? computazionalmente non riesce a calcolare la differenza per ogni cliente
#qua o campioniamo 
##############

pre_ter_alt <- sample(unique(df_for_next_purch$ID_CLI), size=130000, replace=FALSE) %>% 
  as_tibble() %>% 
  rename(ID_CLI = value) 

ter_alt <- df_for_next_purch %>% filter(ID_CLI %in% pre_ter_alt$ID_CLI)

##

df_next_purch <- ter_alt   %>%
  group_by(ID_CLI) %>%
  distinct(TIC_DATE) %>%
  mutate(DifferencesD= TIC_DATE - lag(TIC_DATE), Differences = as.numeric(DifferencesD)) %>%
  ungroup() 


df_next_purch

#sistemo na delle differenze
df_next_purch_clean <- df_next_purch%>% na.omit()
df_next_purch_clean

df_next_purch_medie <- df_next_purch_clean %>% group_by(ID_CLI) %>% 
  summarize(avg_next_purchase = trunc(mean(Differences))) %>%
  arrange(avg_next_purchase) %>%
  ungroup() 


final <- df_next_purch_medie %>% mutate(clienti =c(1))


#calcolo il numero totale dei clienti
totcli <- final  %>% summarise(TOTCLI = n_distinct(ID_CLI))

#calcolo le cumulate percentuali
finale <- final  %>% group_by(avg_next_purchase) %>% summarise(totcli = n()) %>% 
  mutate( cumulata = cumsum(totcli), perc = cumulata/127814)

finale
#indago per vedere quali sono i giorni medi di riacquisto per l'80% dei clienti
tail(finale, 301)

#plot
plot_curva_riacquisto <- ggplot(finale, aes(x=avg_next_purchase, y=perc)) + 
  geom_line(colour='#cced00') + geom_point(colour='#cced00') +theme_minimal()+ labs(x="Giorni medi acquisto successivo", y = "Numero clienti in percentuale", title='Curva di riacquisto') +
  coord_fixed(ratio = 100, xlim =c(0, 100), expand=FALSE)

plot_curva_riacquisto
#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)

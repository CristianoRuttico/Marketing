#### FIRST LOOK of df_6 ####

str(df_6_camp_event)
summary(df_6_camp_event)

#### START CLEANING df_6 ####

df_6_camp_event_clean <- df_6_camp_event

#### CLEANING DATA TYPES in df_6 ####

## formatting dates and times ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATETIME = as.POSIXct(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S")) %>%
  mutate(EVENT_HOUR = hour(EVENT_DATETIME)) %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATETIME))

#### CONSISTENCY CHECK ID_CLI in df_1/df_6 ####

cons_idcli_df1_df6 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_6) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df6

#!!! NOTE: all ID_CLI in df_6 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_6 !!!#  

#### CONSISTENCY CHECK ID_CAMP in df_5/df_6 ####

cons_idcamp_df5_df6 <- df_5_camp_cat_clean %>%
  select(ID_CAMP) %>%
  distinct() %>%
  mutate(is_in_df_5 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CAMP) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CAMP"
  ) %>%
  group_by(is_in_df_5, is_in_df_6) %>%
  summarize(NUM_ID_CAMPs = n_distinct(ID_CAMP)) %>%
  as.data.frame()

cons_idcamp_df5_df6

#!!! NOTE: all ID_CAMP in df_6 are mapped in df_5, but not all ID_CAMP in df_5 are mapped in df_6 !!!#

#### RESHAPING df_6 ####

## remapping TYPE_EVENT values "E" [ERROR] and "B" [BOUNCE] into a level "F" [FAILURE] ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))

## adding type from df_5 ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")

## organize the data adding to each sending event the corresponding opens/clicks/fails

# sends
df_sends <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE = EVENT_DATE) %>%
  as.data.frame()

df_sends
# opens
# there could be multiple opens of the same communication
# 1- count the open events
# 2- consider explicitely only the first open

df_opens_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , OPEN_DATETIME = EVENT_DATETIME
         , OPEN_DATE = EVENT_DATE)

total_opens <- df_opens_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarize(NUM_OPENs = n_distinct(ID_EVENT_O))

df_opens <- df_opens_prep %>%
  left_join(total_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(OPEN_DATETIME == min(OPEN_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_opens_prep

# clicks
# there could be multiple clicks of the same communication
# 1- count the click events
# 2- consider explicitely only the first click

df_clicks_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , CLICK_DATETIME = EVENT_DATETIME
         , CLICK_DATE = EVENT_DATE)

total_clicks <- df_clicks_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarize(NUM_CLICKs = n_distinct(ID_EVENT_C))

df_clicks <- df_clicks_prep %>%
  left_join(total_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(CLICK_DATETIME == min(CLICK_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# fails
df_fails <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , FAIL_DATETIME = EVENT_DATETIME
         , FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATETIME == min(FAIL_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# combine sends opens clicks and fails
df_6_camp_event_clean_final <- df_sends %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | OPEN_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(OPENED = !is.na(ID_EVENT_O)) %>%
  mutate(CLICKED = !is.na(ID_EVENT_C)) %>%
  mutate(FAILED = !is.na(ID_EVENT_F)) %>%
  mutate(DAYS_TO_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  select(ID_EVENT_S
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE
         
         , OPENED
         , OPEN_DATE
         , DAYS_TO_OPEN
         , NUM_OPENs
         
         , CLICKED
         , CLICK_DATE
         , NUM_CLICKs
         
         , FAILED
  )

df_6_camp_event_clean_final

#### EXPLORE VARIABLES in df_6 ####

### GENERAL OVERVIEW ###

## compute aggregate
df6_overview <- df_6_camp_event_clean_final %>% 
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overview

### GENERAL OVERVIEW by TYP_CAMP ###

## compute aggregate
df6_overviewbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP) %>%
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overviewbytyp

## plot aggregate
plot_df6_overviewbytyp <- (
  ggplot(data=df6_overviewbytyp
         , aes(x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)  + labs(title="Distribuzione del tipo di campagna",
          x ="Tipo di campagna", y = "Eventi")


plot_df6_overviewbytyp

### Variable OPENED ###

## compute aggregate
df6_dist_opened <- df_6_camp_event_clean_final %>%
  group_by(OPENED) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_opened

## plot aggregate
plot_df6_dist_opened <- (
  ggplot(data=df6_dist_opened
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    theme_minimal()
)  + labs(title="Distribuzione apertura o meno campagna da parte del cliente",
          x ="Tutte le campagne", y = "Eventi")

plot_df6_dist_opened

### Variable OPENED by TYP_CAMP ###

## compute aggregate
df6_dist_openedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, OPENED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , OPENED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_openedbytyp

## plot aggregate
plot_df6_dist_openedbytyp <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
) + labs(title="Distribuzione apertura o meno in base al tipo di campagna",
         x ="Tipo di campagna", y = "Eventi")

plot_df6_dist_openedbytyp

## plot aggregate percent
plot_df6_dist_openedbytyp_percent <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
) + labs(title="Distribuzione del tipo di campagna in percentuale",
         x ="Tipo di campagna", y = "Eventi in percentuale")

plot_df6_dist_openedbytyp_percent

### Variable DAYS_TO_OPEN

## compute aggregate
df6_dist_daystoopen <- df_6_camp_event_clean_final %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarize(AVG_DAYS_TO_OPEN = floor(mean(DAYS_TO_OPEN))) %>%
  ungroup() %>%
  group_by(AVG_DAYS_TO_OPEN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_daystoopen

## plot aggregate
plot_df6_dist_daystoopen <- (
  ggplot(data=df6_dist_daystoopen %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
) + labs(title="Distribuzione clienti in base a giorni medi di apertura",
         x ="Giorni medi apertura campagna", y = "Clienti")

plot_df6_dist_daystoopen

### DAYS_TO_OPEN vs CUMULATE PERCENT ###

## compute aggregate
df6_dist_daystoopen_vs_cumulate <- df6_dist_daystoopen %>%
  arrange(AVG_DAYS_TO_OPEN) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

## plot aggregate
plot_df6_dist_daystoopen_vs_cumulate <- (
  ggplot(data=df6_dist_daystoopen_vs_cumulate %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=PERCENT_COVERED)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) +
    theme_minimal()
) + labs(title="Distribuzione cumulata percentuale apertura campagna da parte clienti",
         x ="Giorni medi apertura campagna", y = "percentuale coperta")


#in pratica curva ci dice quanti giorni in media servono perch? tutti i clienti aprano la campagna
plot_df6_dist_daystoopen_vs_cumulate

#### ???? TO DO df_6 ???? ####
# EXPLORE the following relevant variables in df_6_camp_event_clean_final:
df_6_camp_event_clean_final

# - CLICKED/CLICKED by TYP_CAMP

#distribuzione clicked
df6_dist_clicked<- df_6_camp_event_clean_final %>%
  group_by(CLICKED) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_clicked

# plot distribuzione percentuale clicked

plot_df6_dist_clicked <- (
  ggplot(data=df6_dist_clicked
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    theme_minimal()
)  + labs(title="Distribuzione click o meno su campagna da parte del cliente",
          x ="Tutte le campagne", y = "Eventi")

plot_df6_dist_clicked

#CLICKED by TYP_CAMP

df6_dist_clickedbytypcamp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, CLICKED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , CLICKED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_clickedbytypcamp

# plot distribuzione clicked per tipo di campagna

plot_df6_dist_clickedbytypcamp <- (
  ggplot(data=df6_dist_clickedbytypcamp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)  + labs(title="Distribuzione click o meno per campagna da parte del cliente",
          x ="Campagne", y = "Eventi")

plot_df6_dist_clickedbytypcamp  

# plot distribuzione percentuali clicked per tipo di campagna

plot_df6_dist_clickedbytypcamp_percent <- (
  ggplot(data=df6_dist_clickedbytypcamp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
) + labs(title="Distribuzione percentuale click o meno per campagna da parte del cliente",
         x ="Campagne", y = "Eventi")

plot_df6_dist_clickedbytypcamp_percent

# FAILED/FAILED by TYP_CAMP

#failed
df6_dist_failed<- df_6_camp_event_clean_final %>%
  group_by(FAILED) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_failed

## plot distribuzione percentuale campagna fallita o meno
plot_df6_dist_failed <- (
  ggplot(data=df6_dist_failed
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    theme_minimal()
) + labs(title="Distribuzione percentuale ricezione campagna fallita o meno",
         x ="Campagne", y = "Eventi")


plot_df6_dist_failed

#FAILED by TYP_CAMP
df6_dist_failedbytypcamp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, FAILED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , FAILED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_failedbytypcamp

#plot failed by TYP_CAMP
plot_df6_dist_failedbycamp <- (
  ggplot(data=df6_dist_failedbytypcamp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
) + labs(title="Distribuzione ricezione campagna per tipo di campagna",
         x ="Campagne", y = "Eventi")

plot_df6_dist_failedbycamp 

#plot failed by TYP_CAMP percentuale 
plot_df6_dist_failedbycamp_perc <- (
  ggplot(data=df6_dist_failedbytypcamp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    theme_minimal()
) + labs(title="Distribuzione percentuale ricezione campagna per tipo di campagna",
         x ="Campagne", y = "Eventi")

plot_df6_dist_failedbycamp_perc 

# - NUM_OPENs

df_6_camp_event_clean_final_no_na <-df_6_camp_event_clean_final %>% replace_na(list(NUM_OPENs=0, NUM_CLICKs=0 ))


#numero totali degli opens
num_opens <- df_6_camp_event_clean_final_no_na %>%
  summarise(TOT_NUM_OPENS = sum(NUM_OPENs))


#numero totale opens per tipo di campagna
num_opens_camp <- df_6_camp_event_clean_final_no_na %>% 
  group_by(TYP_CAMP) %>%
  summarise(totale_opens = sum(NUM_OPENs))%>%
  mutate(perc = totale_opens/num_opens$TOT_NUM_OPENS )
  

#Potrebbe avere senso numero aperture a campagna media
# - NUM_CLICKs

#numero totali dei clicks
num_clicks <- df_6_camp_event_clean_final_no_na %>%
  summarise(TOT_NUM_clicks = sum(NUM_CLICKs) )

#numero totale clicks per tipo di campagna
num_click_camp <- df_6_camp_event_clean_final_no_na %>% 
  group_by(TYP_CAMP) %>%
  summarise(totale_clicks = sum(NUM_CLICKs))%>%
  mutate(perc = totale_clicks/num_clicks$TOT_NUM_clicks )

#check per verificare che eveti hanno id singolo
df_6_camp_event_clean_final%>%
  summarise(tot =n_distinct( ID_EVENT_S))


#Plot dei opens e per tipo di campagna 
plot_df6_dist_num_opens_camp <- (
  ggplot(data=num_opens_camp 
         , aes( x=TYP_CAMP,   y=perc))+
    geom_bar(stat="identity") +
    theme_minimal()
) + labs(title="Distribuzione opens per tipo di campagna",
         x ="Campagne", y = "Tot opens") +theme_minimal()

plot_df6_dist_num_opens_camp

#Plot dei clicks e per tipo di campagna 

plot_df6_dist_num_click_camp <- (
  ggplot(data=num_click_camp 
         , aes( x=TYP_CAMP,   y=perc))+
    geom_bar(stat="identity") +
    theme_minimal()
) + labs(title="Distribuzione clicks per tipo di campagna",
         x ="Campagne", y = "Tot clicks") +theme_minimal()

plot_df6_dist_num_click_camp


#### FINAL REVIEW df_6_clean ####

str(df_6_camp_event_clean_final)
summary(df_6_camp_event_clean_final)
### CHURN MODEL ###

#Analisi esplorative e creazione della variabile target
rec_date <- max(df_7_tic_clean_final$TIC_DATE)
#ultima data: 30/04/2019

# Reference date come data più recente meno 60 gg (hold out period da elaborazione data set 7)

ref_date <- rec_date - days (60) 
# Conflitto tra chron e lubridate ma esegue correttamente

# Si filtra per lookback period 7 mesi dalla reference date 

lback <- ref_date - months (7)

df<- subset(df_7_tic_clean_final, TIC_DATE > lback)

df <- df[order(df$ID_CLI,rev(df$TIC_DATE)),]
dft2 <- df %>%
  group_by(ID_SCONTRINO) %>% 
  summarise(ID_CLI = max(ID_CLI),DATETIME=max(TIC_DATE))
dft2 <- dft2[order(dft2$ID_CLI,rev(dft2$DATETIME)),]

#Calcoliamo il numero di acquisti per ogni cliente
dft3 <- dft2 %>% group_by(ID_CLI) %>% summarise(tot = n()) %>% filter(tot>1)

#Si aggiunge id_scontrino & datetime
dft3 <- left_join(dft3,dft2,by="ID_CLI") 
dft4 <- dft3 %>% 
  arrange(desc(DATETIME)) %>% 
  group_by(ID_CLI) %>% 
  summarise(last=nth(DATETIME,1),secondl=nth(DATETIME,2))

#Si crea quindi la variabile target da inserire all'interno del dataframe

df_churn <- df_7_tic_clean_final %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE),
            TOTAL_PURCHASE = sum(IMPORTO_LORDO),
            NUMBER_OF_PURCHASE=n())   %>%
  mutate(CHURN = as.numeric(LAST_PURCHASE_DATE < as.Date("2019-03-01"))) %>%
  select(CHURN,ID_CLI,LAST_PURCHASE_DATE,TOTAL_PURCHASE,NUMBER_OF_PURCHASE)

table(df_churn$CHURN)

# Aggiungiamo una variabile in riferimento alla caratterizzazione del giorno ed estrapoliamo
# i mesi e l'anno dalla data, in modo da rimuoverla come variabile per l'analisi e iniziare
# un primo approccio in modo tale da avere abbastanza informazione e dettaglio

df_churn <- df_churn %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(LAST_PURCHASE_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", LAST_PURCHASE_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )

df_churn$Mese <- as.factor(months(df_churn$LAST_PURCHASE_DATE))
df_churn$Anno <- as.factor(year(df_churn$LAST_PURCHASE_DATE))

# Si crea un data set con un maggior numero di variabili attraverso la join con gli altri data set e l'RFM

df_tot <- df_1_cli_fid_clean %>%
  select(ID_CLI, FIRST_ID_NEG,LAST_TYP_CLI_FID, LAST_COD_FID, LAST_STATUS_FID) %>%
  left_join(df_2_cli_account_clean 
            , by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, PRV, REGION)
            , by = "ID_ADDRESS") %>%
  left_join(df_4_cli_privacy_clean
            , by = "ID_CLI") %>%
  select(-ID_ADDRESS)

df_tot_churn <- df_churn %>%
  left_join(df_tot, by="ID_CLI")%>%
  mutate(PRV = fct_explicit_na(PRV)) %>%
  mutate(REGION = fct_explicit_na(REGION))

colnames(rfm_result_fin)[1] <- "ID_CLI"

df_tot_churn_fin<- df_tot_churn%>%
              left_join(rfm_result_fin %>%
              select(ID_CLI, recency_score, frequency_score, monetary_score), Churn, by = "ID_CLI")


#controllo dati mancanti
sapply(df_tot_churn_fin, function(x)(sum(is.na(x)/nrow(df_tot_churn_fin))))

#rimuoviamo le variabili non significative per l'analisi 
df_tot_churn_fin <- df_tot_churn_fin[,c('CHURN', "TOTAL_PURCHASE","NUMBER_OF_PURCHASE",
                                "LAST_TYP_CLI_FID", "LAST_COD_FID", "FIRST_ID_NEG",
                                "LAST_STATUS_FID", "W_PHONE","TYP_CLI_ACCOUNT","FLAG_PRIVACY_2",
                                "FLAG_DIRECT_MKT", "monetary_score", "recency_score", "frequency_score")] 


#trasformazione delle variabili char in factor

df_tot_churn_fin$monetary_score <- as.factor(df_tot_churn_fin$monetary_score)
df_tot_churn_fin$recency_score <- as.factor(df_tot_churn_fin$recency_score)
df_tot_churn_fin$frequency_score <- as.factor(df_tot_churn_fin$frequency_score)

df_tot_churn_fin$LAST_COD_FID <- as.factor(df_tot_churn_fin$LAST_COD_FID)
df_tot_churn_fin$FIRST_ID_NEG <- as.factor(df_tot_churn_fin$FIRST_ID_NEG)

df_tot_churn_fin$CHURN <- as.factor(df_tot_churn_fin$CHURN)
str(df_tot_churn_fin)

isfactor <- sapply(df_tot_churn_fin, function(x) is.factor(x))
isfactor
factordata <- df_tot_churn_fin[, isfactor]
str(factordata)
numeric <- sapply(df_tot_churn_fin, function(x) is.numeric(x))
numeric <-df_tot_churn_fin[, numeric]
str(numeric)

#analisi della correlazione

correlatedPredictors = findCorrelation(cor(numeric), cutoff = 0.8, names = TRUE)
correlatedPredictors


corrgram(numeric, lower.panel = panel.cor, cex=1, cex.labels = 1)
# Non ci sono variabili significativamente correlate

#si procede creando la partizione tra train e test

check_ch <-table(df_tot_churn_fin$CHURN)
check_ch["0"]/(check_ch["0"] + check_ch["1"])

#dataset sbilanciato, 0.3629339 (churn = 0) vs  0.6370661 (churn = 1)

split <- createDataPartition(df_tot_churn_fin$CHURN, 
                             p = .70, 
                             list = FALSE, 
                             times = 1)
train <- df_tot_churn_fin[split,]
test <- df_tot_churn_fin[-split,]

#undersampling per bilanciare il campione

churn0 <- train %>% filter(CHURN == 0) # 53869 
churn1 <- train %>% filter(CHURN == 1) # 94618

bilancia <- churn1[sample(nrow(churn1), nrow(churn0)),]

train <- rbind(bilancia, churn0)
table(train$CHURN)
table(test$CHURN)
##MODELLI
# Decision Tree
# Si valuta contestualmente l'importanza delle variabili e si valuta se
# eliminarne alcune per dimuinuire la dimensione del dataset o perchè inutili
# ai fini dell'analisi

level_key <- c('1' = "c1", '0' = "c0")
train$CHURN <- recode_factor(train$CHURN, !!!level_key)
test$CHURN <- recode_factor(test$CHURN, !!!level_key)

train_mod <- na.exclude(train) # si eliminano i missing values che creano problemi nel training dei modelli

###

dec_tree <- rpart(CHURN ~., data = train_mod)
pred_tree <- predict(dec_tree, test[,-1],type = "class") #0.8009
confusionMatrix(pred_tree, test$CHURN)

Var_imp <- varImp(dec_tree, scale = FALSE)
Var_imp
# Si nota che le variabili importanti sono: 

#Overall
#FIRST_ID_NEG         398.0281
#frequency_score     1574.8342
#NUMBER_OF_PURCHASE  3247.1713
#recency_score      15869.7366
#TOTAL_PURCHASE      1178.6970

# e pertanto si selezionano le variabili significative

df_tot_churn_fin <- df_tot_churn_fin[,c('CHURN', "TOTAL_PURCHASE","NUMBER_OF_PURCHASE","FIRST_ID_NEG",
                                         "recency_score", "frequency_score")] 




#Naive Bayes V
nb <- naiveBayes(CHURN ~ ., train_mod )
print(nb)

pred_nb <- predict(nb, test[,-1],type = "class")
confusionMatrix(pred_nb, test$CHURN) # Accuracy = 0.7906


#Logistic model 

logistic_model <- glm(CHURN ~ . , data = train_mod, family="binomial")
print(logistic_model)
pred_lm <- predict(logistic_model, test[-1], type='response')
pred_lm <- as.factor(if_else(pred_lm > 0.5,'c0','c1')) #soglia di default
confusionMatrix(pred_lm, test$CHURN) # Accuracy = 0.8955


# Random Forest 

tree_rf <- randomForest(CHURN ~ ., data= train_mod, ntree = 100)
print(tree_rf)
pred_rf <- predict(tree_rf, test[,-1],type = "class") #0.8856
confusionMatrix(pred_rf, test$CHURN)

#previsti
B <- data.frame(c(1:63637) ,0)
B$pred_tree <- as.numeric(pred_tree)
B$pred_nb <- as.numeric(pred_nb)
B$pred_lm <- as.numeric(pred_lm)
B$pred_rf <- as.numeric(pred_rf)



# confronto
length1=roc(test$CHURN ~ pred_tree, data = B) # Decision Tree
length2=roc(test$CHURN ~ pred_lm, data = B) # Naive Bayes
length3=roc(test$CHURN ~ pred_nb, data = B) # Logistic Model
length4=roc(test$CHURN ~ pred_rf, data = B) # Random Forest

cat(" AUC Decision Treet\n",length1$auc,"\n\n","AUC Logistic Model\n",length2$auc,"\n\n","AUC Naive Bayes\n",
   length3$auc, "\n\n","AUC Bagging\n",length4$auc )

plot(length1, col = "aquamarine") #dec tree
plot(length2,add=T,col="blue") # logistic
plot(length3,add=T,col="orange") # nb
plot(length4,add=T,col="green") # random forest


# misure modelli

dtree_t<-table(Predicted = pred_tree, Actual = test$CHURN)

dtree_accuracy<-(dtree_t["c1","c1"]+dtree_t["c0","c0"])/(sum(dtree_t))
dtree_precision<-(dtree_t["c1","c1"])/(dtree_t["c1","c1"]+dtree_t["c1","c0"])
dtree_recall<-(dtree_t["c1","c1"])/(dtree_t["c1","c1"]+dtree_t["c0","c1"])
dtree_Fmeasure<-(2*dtree_precision*dtree_recall)/(dtree_precision+dtree_recall)

nb_t<-table(Predicted = pred_nb, Actual = test$CHURN)

nb_accuracy<-(nb_t["c1","c1"]+nb_t["c0","c0"])/(sum(nb_t))
nb_precision<-(nb_t["c1","c1"])/(nb_t["c1","c1"]+nb_t["c1","c0"])
nb_recall<-(nb_t["c1","c1"])/(nb_t["c1","c1"]+nb_t["c0","c1"])
nb_Fmeasure<-(2*nb_precision*nb_recall)/(nb_precision+nb_recall)

log_t<-table(Predicted = pred_lm, Actual = test$CHURN)

log_accuracy<-(log_t["c1","c1"]+log_t["c0","c0"])/(sum(log_t))
log_precision<-(log_t["c1","c1"])/(log_t["c1","c1"]+log_t["c1","c0"])
log_recall<-(log_t["c1","c1"])/(log_t["c1","c1"]+log_t["c0","c1"])
log_Fmeasure<-(2*log_precision*log_recall)/(log_precision+log_recall)

rf_t<-table(Predicted = pred_rf, Actual = test$CHURN)

rf_accuracy<-(rf_t["c1","c1"]+rf_t["c0","c0"])/(sum(rf_t))
rf_precision<-(rf_t["c1","c1"])/(rf_t["c1","c1"]+rf_t["c1","c0"])
rf_recall<-(rf_t["c1","c1"])/(rf_t["c1","c1"]+rf_t["c0","c1"])
rf_Fmeasure<-(2*rf_precision*rf_recall)/(rf_precision+rf_recall)

names_mod <- c("Decision Tree", "Naive Bayes", "Logistic Regression", "Random Forest")

acc <- c(dtree_accuracy, nb_accuracy, log_accuracy, rf_accuracy)
pre <- c(dtree_precision, nb_precision, log_precision, rf_precision)
rcl <- c(dtree_recall, nb_recall, log_recall, rf_recall)
fmsr <- c(dtree_Fmeasure, nb_Fmeasure, log_Fmeasure, rf_Fmeasure)


# ACCURACY

acc_plot <- barplot(acc, names.arg = names_mod, xlab = "Model",ylab = "Accuracy", col = "#cced00")


# PRECISION

pre_plot <- barplot(pre, names.arg = names_mod, xlab = "Model",ylab = "Precision", col = "#cced00")


# RECALL

rec_plot <- barplot(rcl, names.arg = names_mod, xlab = "Model",ylab = "Recall", col = "#cced00", ylim=c(0,1)) 


# F-MEASURE

fmsr_plot <- barplot(fmsr, names.arg = names_mod, xlab = "Model",ylab = "F-Measure", col = "#cced00")


#### FIRST LOOK of df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting boolean as factor ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

#### EXPLORE COLUMNS of df_4 ####

#### ???? TO DO df_4 ???? ####
# EXPLORE the df_4_cli_privacy_clean relevant variables

#distribuzione dei clienti in base a flag_privacy_1

df4_dist_flag_privacy1 <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarise(TOT_FLAG_PRIVACY_1 = n())

df4_dist_flag_privacy1 

#plot distribuzione dei clienti in base a flag_privacy_1
plot_df4_dist_flag_privacy1 <- (
  ggplot(data=df4_dist_flag_privacy1 
         , aes(x=FLAG_PRIVACY_1, y=TOT_FLAG_PRIVACY_1)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
) + labs(title="Numero clienti per flag-privacy-1",
         x ="flag-privacy-1", y = "clienti") 

plot_df4_dist_flag_privacy1

#distribuzione dei clienti in base a flag_privacy_2

df4_dist_flag_privacy2 <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarise(TOT_FLAG_PRIVACY_2 = n())

df4_dist_flag_privacy2 

#plot distribuzione dei clienti in base a flag_privacy_2
plot_df4_dist_flag_privacy2 <- (
  ggplot(data=df4_dist_flag_privacy2 
         , aes(x=FLAG_PRIVACY_2, y=TOT_FLAG_PRIVACY_2)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
) + labs(title="Numero clienti per flag-privacy-2",
         x ="flag-privacy-2", y = "clienti") 

plot_df4_dist_flag_privacy2

#distribuzione dei clienti in base a direct e-mail marketing
df4_dist_direct_mkt <- df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarise(TOT_DIRECT_MKT = n())

df4_dist_direct_mkt

#plot distribuzione dei clienti in base a flag_privacy_2
plot_df4_dist_direct_mkt <- (
  ggplot(data=df4_dist_direct_mkt
         , aes(x=FLAG_DIRECT_MKT, y=TOT_DIRECT_MKT)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
) + labs(title="Numero clienti per flag-direct-mkt",
         x ="flag-direct-mkt", y = "clienti") 

plot_df4_dist_direct_mkt

#### FINAL REVIEW df_4_clean ####
str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)
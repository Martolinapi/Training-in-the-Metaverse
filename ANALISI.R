library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)


#test di normalità#
norm <- apply(data[,3:59],2,shapiro.test)

# Estrai i p-value dai risultati del test di Shapiro
p_values <- sapply(norm, function(x) x$p.value)


# Conta quanti p-value sono minori di 0.05
num_significant <- sum(p_values < 0.05)

# Ottieni i nomi delle colonne con p-value minore di 0.05
significant_columns <- names(p_values[p_values < 0.05])

# Stampa il numero di test significativi e i nomi delle colonne corrispondenti
print(paste("Numero di colonne con p-value < 0.05:", num_significant))
print("Colonne con p-value < 0.05:")

print(significant_columns)

#TABELLE
library(gtsummary)

data_long <- data %>%
  pivot_longer(
    cols = -c(Code, Condizione), # Escludi le colonne che non devono essere trasformate
    names_to = c(".value", "Condizione_2"), # Divide il nome della colonna in due nuove colonne
    names_pattern = "(.*)_(.*)" # Usa un'espressione regolare per separare i nomi delle colonne
  )

data_long$Condizione_2 <- factor(data_long$Condizione_2,
                                 levels = c("PRE","POST","FOLLOWUP"))

# SPCC
data_long %>% select(starts_with("SPCC"), Condizione,Condizione_2) %>% 
  filter(Condizione_2 %in% c("PRE","POST","FOLLOWUP")) %>% 
  tbl_strata(
    strata = Condizione_2, # Definisce le stratificazioni
    .tbl_fun = 
      ~ .x %>%
      tbl_summary(
        by = Condizione, # Raggruppamento all'interno di ogni stratificazione
        type = list(contains("_") ~ "continuous"), # Definisce il tipo di variabili
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        missing = "no"
      ) %>% 
      modify_header(label = "**Variable**")
  )

# SPCC solo POST
data_long %>% select(starts_with("SPCC"), Condizione,Condizione_2) %>% 
  filter(Condizione_2 %in% c("POST")) %>% 
  tbl_strata(
    strata = Condizione_2, # Definisce le stratificazioni
    .tbl_fun = 
      ~ .x %>%
      tbl_summary(
        by = Condizione, # Raggruppamento all'interno di ogni stratificazione
        type = list(contains("_") ~ "continuous"), # Definisce il tipo di variabili
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        missing = "no"
      ) %>% 
      modify_header(label = "**Variable**")
  )

# PANAS
data_long %>% select(starts_with("PANAS"), Condizione,Condizione_2) %>% 
  filter(Condizione_2 %in% c("PRE","POST")) %>% 
  tbl_strata(
    strata = Condizione_2, # Definisce le stratificazioni
    .tbl_fun = 
      ~ .x %>%
      tbl_summary(
        by = Condizione, # Raggruppamento all'interno di ogni stratificazione
        type = list(contains("_") ~ "continuous"), # Definisce il tipo di variabili
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        missing = "no"
      )%>% 
      modify_header(label = "**Variable**")
  )

# IRI
data_long %>% select(starts_with("IRI"), Condizione,Condizione_2) %>% 
  filter(Condizione_2 %in% c("PRE","FOLLOWUP")) %>% 
  tbl_strata(
    strata = Condizione_2, # Definisce le stratificazioni
    .tbl_fun = 
      ~ .x %>%
      tbl_summary(
        by = Condizione, # Raggruppamento all'interno di ogni stratificazione
        type = list(contains("_") ~ "continuous"), # Definisce il tipo di variabili
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        missing = "no"
      )%>% 
      modify_header(label = "**Variable**")
  )

# ICT
data_long %>% select(starts_with("ICT"), Condizione,Condizione_2) %>% 
  filter(Condizione_2 == "POST") %>% 
  tbl_strata(
    strata = Condizione_2, # Definisce le stratificazioni
    .tbl_fun = 
      ~ .x %>%
      tbl_summary(
        by = Condizione, # Raggruppamento all'interno di ogni stratificazione
        type = list(contains("_") ~ "continuous"), # Definisce il tipo di variabili
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        missing = "no"
      ) %>% 
      add_p()%>% 
      modify_header(label = "**Variable**")
  )


# Embodiment

data_long %>% select(starts_with("Embodiment"), Condizione,Condizione_2) %>% 
  filter(Condizione_2 == "POST") %>% 
  tbl_strata(
    strata = Condizione_2, # Definisce le stratificazioni
    .tbl_fun = 
      ~ .x %>%
      tbl_summary(
        by = Condizione, # Raggruppamento all'interno di ogni stratificazione
        type = list(contains("_") ~ "continuous"), # Definisce il tipo di variabili
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        missing = "no"
      ) %>% 
      add_p()%>% 
      modify_header(label = "**Variable**")
  )

# ELS
data_long %>% select(starts_with("ELS"), Condizione,Condizione_2) %>% 
  filter(Condizione_2 == "POST") %>% 
  tbl_strata(
    strata = Condizione_2, # Definisce le stratificazioni
    .tbl_fun = 
      ~ .x %>%
      tbl_summary(
        by = Condizione, # Raggruppamento all'interno di ogni stratificazione
        type = list(contains("_") ~ "continuous"), # Definisce il tipo di variabili
        statistic = list(all_continuous() ~ "{mean} ({sd})"),
        missing = "no"
      )%>% 
      modify_header(label = "**Variable**")
  )


# MIXED ANOVA
library(lmerTest)
data_SPCC <- data_long %>%
  filter(Condizione_2 %in% c("PRE","POST","FOLLOWUP")) %>% 
  select(Code,Condizione,Condizione_2,starts_with("SPCC"))

data_SPCC[,4:11] <- apply(data_SPCC[,4:11],2,log2) #effettuato trasformazione logaritmica

model_SPCC_Public <- lmer(SPCC_Public ~ Condizione_2 * Condizione + (1 | Code), data = data_SPCC)


summary(model_SPCC_Public)
#non ci sono effetti statisticamente significativi tranne per un'indicazione di marginalità nella condizione principale e nella sua interazione in FOLLOWUP, che sono vicine alla soglia di significatività.

### Correlazione

data_cor <- data_long %>% filter(Condizione_2 == "POST") %>% 
  select(starts_with(c("SPCC","ELS")))

cor <- cor(data_cor, method="spearman")
ggcorrplot::ggcorrplot(cor)

#Nessuna correlazione rilevata!

data_cor <- data_long %>% filter(Condizione_2 == "POST") %>%
  select(starts_with(c("Embodiment","ELS")))

cor <- cor(data_cor, method="spearman")
ggcorrplot::ggcorrplot(cor)

#Nessuna correlazione rilevata!

data_cor <- data_long %>% filter(Condizione_2 == "POST") %>%
  select(starts_with(c("ICT","ELS")))

cor <- cor(data_cor, method="spearman")
ggcorrplot::ggcorrplot(cor)



# REGRESSIONE LINEARE

mod_0 <- lm(SPCC_totalscore_POST ~ ELS_Global_POST, data = data)
summary(mod_0)

#                  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)     72.95208   12.49047   5.841 6.23e-07 ***
#  ELS_Global_POST  0.05269    0.07079   0.744    0.461 NESSUNA SIGNIFICATIVITA'

#Istogrammi

# Calcolo delle medie per ogni sottoscala ITC e per condizione
data_means <- data_long %>%
  filter(Condizione_2 == "POST") %>%
  group_by(Condizione) %>%
  summarise(
    Media_ITC_Spatial_Presence = mean(ITC_Spatial_Presence, na.rm = TRUE),
    SE_ITC_Spatial_Presence = sd(ITC_Spatial_Presence, na.rm = TRUE) / sqrt(n()),
    Media_ITC_Engagement = mean(`ITC_Engagement `, na.rm = TRUE),
    SE_ITC_Engagement = sd(`ITC_Engagement `, na.rm = TRUE) / sqrt(n()),
    Media_ITC_Ecological_Validity = mean(ITC_Ecological_Validity, na.rm = TRUE),
    SE_ITC_Ecological_Validity = sd(ITC_Ecological_Validity, na.rm = TRUE) / sqrt(n()),
    Media_ITC_Negative_Effects = mean(`ITC_Negative_Effects `, na.rm = TRUE),
    SE_ITC_Negative_Effects = sd(`ITC_Negative_Effects `, na.rm = TRUE) / sqrt(n())
  )

# Calcolo delle medie per ogni sottoscala Embodiment e per condizione

# Verifica i dati aggregati
print(data_means)


# Trasforma in formato lungo per il plotting
data_long_plot <- data_means %>%
  pivot_longer(
    cols = c(Media_ITC_Spatial_Presence, Media_ITC_Engagement, Media_ITC_Ecological_Validity,Media_ITC_Negative_Effects),
    names_to = "Sottoscala",
    values_to = "Media",
    names_prefix = "Media_"
  ) %>%
  pivot_longer(
    cols = c(SE_ITC_Spatial_Presence, SE_ITC_Engagement, SE_ITC_Ecological_Validity, SE_ITC_Negative_Effects),
    names_to = "Sottoscala_SE",
    values_to = "SE",
    names_prefix = "SE_"
  )

# Verifica i dati trasformati
print(data_long_plot)

# Grafico a barre che mostra la media di ITC per Condizione e Sottoscala
ggplot(data_long_plot, aes(x = Sottoscala, y = Media, fill = as.factor(Condizione))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                position = position_dodge(width = 0.8), width = 0.25) +
  facet_wrap(~ Sottoscala) +
  labs(title =,
       x = "ITC-Subscales",
       y = "Mean") +
  scale_fill_brewer(palette = "Set1", name = "Condition",
labels = c("Immersive", "Non-Immersive"))+
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Nasconde le etichette dell'asse X
      axis.ticks.x = element_blank(), # Opzionale: rimuove anche i ticks dell'asse X
      panel.grid.major.x = element_blank())  # Rimuove le linee verticali principali
                  

# Calcolo delle medie per ogni sottoscala Embodiment e per condizione
data_means <- data_long %>%
  filter(Condizione_2 == "POST") %>%
  group_by(Condizione) %>%
  summarise(
    Media_Embodiment_Appearance = mean(Embodiment_Appearance, na.rm = TRUE),
    SE_Embodiment_Appearance = sd(Embodiment_Appearance, na.rm = TRUE) / sqrt(n()),
    Media_Embodiment_Response = mean(Embodiment_Response, na.rm = TRUE),
    SE_Embodiment_Response = sd(Embodiment_Response, na.rm = TRUE) / sqrt(n()),
    Media_Embodiment_Ownership = mean(Embodiment_Ownership, na.rm = TRUE),
    SE_Embodiment_Ownership = sd(Embodiment_Ownership, na.rm = TRUE) / sqrt(n()),
    Media_Embodiment_MultySensory = mean(`Embodiment_Multi-Sensory`, na.rm = TRUE),
    SE_Embodiment_MultySensory = sd(`Embodiment_Multi-Sensory`, na.rm = TRUE) / sqrt(n()),
    Media_Embodiment_Agency = mean(Embodiment_Agency, na.rm = TRUE),
    SE_Embodiment_Agency = sd(Embodiment_Agency, na.rm = TRUE) / sqrt(n()),
    Media_Embodiment_Tot = mean(Embodiment_Tot, na.rm = TRUE),
    SE_Embodiment_Tot = sd(Embodiment_Tot, na.rm = TRUE) / sqrt(n())
  )

# Calcolo delle medie per ogni sottoscala Embodiment e per condizione

# Verifica i dati aggregati
print(data_means)


# Trasforma in formato lungo per il plotting
data_long_plot <- data_means %>%
  pivot_longer(
    cols = c(Media_Embodiment_Appearance, Media_Embodiment_Response, Media_Embodiment_Ownership,Media_Embodiment_MultySensory, Media_Embodiment_Agency, Media_Embodiment_Tot),
    names_to = "Sottoscala",
    values_to = "Media",
    names_prefix = "Media_"
  ) %>%
  pivot_longer(
    cols = c(SE_Embodiment_Appearance, SE_Embodiment_Response, SE_Embodiment_Ownership, SE_Embodiment_MultySensory, SE_Embodiment_Agency, SE_Embodiment_Tot),
    names_to = "Sottoscala_SE",
    values_to = "SE",
    names_prefix = "SE_"
  )

# Verifica i dati trasformati
print(data_long_plot)

# Grafico a barre che mostra la media di ITC per Condizione e Sottoscala
ggplot(data_long_plot, aes(x = Sottoscala, y = Media, fill = as.factor(Condizione))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.95) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                position = position_dodge(width = 0.8), width = 0.25) +
  facet_wrap(~ Sottoscala) +
  labs(title =,
       x = "Embodiment-Subscales",
       y = "Mean") +
  scale_fill_brewer(palette = "Set1", name = "Condition",
                    labels = c("Immersive", "Non-Immersive"))+
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Nasconde le etichette dell'asse X
        axis.ticks.x = element_blank(), # Opzionale: rimuove anche i ticks dell'asse X
        panel.grid.major.x = element_blank())  # Rimuove le linee verticali principali

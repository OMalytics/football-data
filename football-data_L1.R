#Chargement des librairies requises pour faire fonctionner le script
require(tidyverse)
require(ggrepel)
require(scales)

####Récupération des données####

#Initiation des identifiants saison sur lesquels on va itérer
season <- c("1617", "1516", "1415", "1314", "1213", "1112", "1011", "0910", "0809", "0708", "0607", "0506")

#Création d'une fonction qui récupère chaque saison au sein d'une liste, 
#Sélectionne les colonnes intéressantes 
#et ajoute l'identifiant saison

data <- season %>% 
  map(function(i) {read_csv(paste0("http://www.football-data.co.uk/mmz4281/", i ,"/F1.csv")) %>% 
      select(Date:AST) %>%
      mutate(season = i)})

#Fusion des éléments de la liste en une seule table
data <- bind_rows(data)

#Suppression des lignes vides
data <- data[complete.cases(data[ , 1:3]), ]

####Mise au format des données####

#Sélection des équipes "domicile" et de leurs résultats
tmp1 <- data %>% 
  select(season, HomeTeam, FTHG:FTR,HS:AST) %>%
  rename(BP = FTHG,
         BC = FTAG,
         TP = HS,
         TC = AS,
         TCP = HST,
         TCC = AST,
         team = HomeTeam)%>%
  mutate(Pts = ifelse(FTR == "H", 3, ifelse(FTR == "A", 0, 1)), 
         Terrain = "Domicile")


#Sélection des équipes "extérieur" et de leurs résultats
tmp2 <- data %>% 
  select(season, AwayTeam, FTHG:FTR, HS:AST) %>%
  rename(BP = FTAG,
         BC = FTHG,
         TP = AS,
         TC = HS,
         TCP = AST,
         TCC = HST,
         team = AwayTeam)%>%
  mutate(Pts = ifelse(FTR == "A", 3 ,ifelse(FTR == "H", 0 , 1)), 
         Terrain = "Extérieur")

#Fusion des résultats domicile et extérieur
tmp3 <- bind_rows(tmp1, tmp2)

#Création des classements de chaque saison
l1_0517 <- tmp3 %>%
  group_by(season, team)%>%
  summarise(j = n(),
            pts = sum(Pts),
            diff_but = (sum(BP) - sum(BC)),
            diff_t_ca = (sum(TCP, na.rm = T) - sum(TCC, na.rm = T)),
            diff_t = (sum(TP, na.rm = T) - sum(TC, na.rm = T)), 
            but_p = sum(BP),
            but_c = sum(BC),
            tir_ca_p = sum(TCP, na.rm = T),
            tir_ca_c = sum(TCC, na.rm = T),
            tir_p = sum(TP, na.rm = T),
            tir_c = sum(TC, na.rm = T)) %>%
  ungroup() %>%
  arrange(season, desc(pts), desc(diff_but))


#Exemple de visuel, on appréciera le record de tirs tentés en une saison par l'équipe d'Eric Gerets
l1_0517 %>%
  ggplot(aes(x = tir_p, y = but_p / tir_p, label = paste0(team," ",season))) +
  geom_vline(xintercept = median(l1_0517$tir_p), color = "grey50", linetype = 2) +
  geom_hline(yintercept = median(l1_0517$but_p / l1_0517$tir_p), color = "grey50", linetype = 2) +
  geom_label(data = . %>% filter(team != "Marseille"), color = "grey") +
  geom_label(data = . %>% filter(team == "Marseille"), color = "blue") +
  scale_y_continuous(labels = scales::percent, name = "Taux de conversion") +
  xlab("Tirs pour") +
  labs(title = "Volume de tirs tentés et Taux de conversion",
       subtitle = "Ligue 1 - Saisons 2015/2016 à 2016/2017",
       caption = "Source : football-data.co.uk - @OMalytics") +
  theme(plot.title = element_text(face = "bold"))
ggsave("vol_tdc.png",width=29,height=21,units=c("cm"),dpi=1000)

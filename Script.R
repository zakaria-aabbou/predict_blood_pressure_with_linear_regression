#################################################################
#                     Mini-projet R:                            #
#           Prédiction de la pression sanguine                  #
#################################################################

# Avant de commencer, installez les packages nécessaires:
# install.packages(c("olsrr","lmtest","car"))
# install.packages("tidyverse")

#----------------------------------------------------------------
#  #1. Importation des données
#----------------------------------------------------------------
library(tidyverse)
health <- read_csv("health.csv")

# Visualiser les données importées.
glimpse(health)

# Convertir les variables 'diabetes' et 'smoker' en facteurs (catégoriques).
health$diabetes= as.factor(health$diabetes)
health$smoker=as.factor(health$smoker)


#----------------------------------------------------------------
#  #2. Explorer les données
#----------------------------------------------------------------
# Obteneir un résumé statistique.
summary(health)

## Visualiser la variable expliquée (variable de réponse).
ggplot() +
  geom_histogram(mapping=aes(x=health$systolic), fill="lightblue", color="black") +
  theme_minimal() +
  theme(text = element_text(size=14))

## Visualiser les variables explicatives.
# L'opérateur (%>%) est défini par le package 'magrittr' et il'est fortement utilisé par 'dplyr'.
# Exemple: iris %>% head() %>% summary() équivaut à summary(head(iris)).
health %>%
  select(-systolic) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size=14))

## Les corrélations.
# Représentation graphique des corrélations.
pairs(systolic~weight+height+bmi+waist+age,data=health , pch=20)
# Valeures de corrélations entre les variables continues.
cor(health[c("systolic","weight","height","bmi","waist","age","fastfood")])


#----------------------------------------------------------------
#  #3. Ajustement du modèle de régression linéaire simple
#----------------------------------------------------------------
# Ajuster le modèle en utilisant AGE comme seule variable explicative.
health_mod1 <- lm(data=health, systolic~age)
summary(health_mod1)

# Visualiser la regression linéaire du modele.
age = health$age
systolic =health$systolic
ggplot(health, aes(x = age, y = systolic)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


#----------------------------------------------------------------
#  #4. Ajustement du modèle de régression linéaire multiple
#----------------------------------------------------------------
# Ajuster un modèle en utilisant toutes les variables.
health_mod2 <- lm(data=health, systolic~.)
summary(health_mod2)

#----------------------------------------------------------------
#  #5. Tests de diagnostic du modèle
#----------------------------------------------------------------
## Moyenne nulle des résidus.
mean(health_mod2$residuals)

library(olsrr) # Chargement de la bibliothèque olsrr
## Normalité des résidus
# Avec l'histogramme des résidus
ols_plot_resid_hist(health_mod2)
# Avec le Q-Q plot des résidus
ols_plot_resid_qq(health_mod2)

## Homoscédasticité des résidus
ols_plot_resid_fit(health_mod2)

## Autocorrélation des résidus
# La fonction set.seed() garantit que nous pouvons obtenir la même p-value à chaque fois.
library(car)
set.seed(123)
durbinWatsonTest(health_mod2)

## Analyse des points aberrants
# Créer un graphique des points aberrants en fonction de la distance de Cook.
cooksd_chart = ols_plot_cooksd_chart(health_mod2)

# Comparer un échantillon de point aberrant et le résumé statistique pour l'ensemble des données.
health[1358,]
summary(health)

# Récupérer les points aberrants
params2 = cooksd_chart$plot_env
result1 = params2$f[,"observation"]

# Lister les valeurs aberrantes du graphique ci-dessus.
outlier_index <- as.numeric(unlist(result1))
outlier_index

# Comparer le résumé statistique entre l'ensemble des valeurs aberrantes et l'ensemble sans valeurs aberrantes.
summary(health[outlier_index,])
summary(health[-outlier_index,])

# Créer un nouvel ensemble de données sans les valeurs aberrantes.
health2 <- health[-outlier_index,]

## Multicollinéarité
ols_vif_tol(health_mod2)

#----------------------------------------------------------------
#  #6. Construire les nouveaux modèles
#----------------------------------------------------------------
# Créer un nouveau modèle avec les nouvelles données et un sous-ensemble de prédicteurs.
health_mod3 <- lm(data=health2, systolic ~ weight+age+diabetes)
summary(health_mod3)

# Tester si la variable 'diabetes' est significatif dans le modèle
drop1(health_mod3,.~.,test="F")


#----------------------------------------------------------------
#  #7.  Tests de diagnostic de nouveaux modèle
#----------------------------------------------------------------
## Moyenne nulle des résidus.
mean(health_mod3$residuals)

## Normalité des résidus
# Avec l'histogramme des résidus
ols_plot_resid_hist(health_mod3)
# Avec le Q-Q plot des résidus
ols_plot_resid_qq(health_mod3)

## Homoscédasticité des résidus
ols_plot_resid_fit(health_mod3)

## Autocorrélation des résidus
set.seed(123)
durbinWatsonTest(health_mod3)

## Analyse des points aberrants
cooksd_chart2 = ols_plot_cooksd_chart(health_mod3)

# Récupérer les points aberrants
params2 = cooksd_chart2$plot_env
result1 = params2$f[,"observation"]

# Lister les valeurs aberrantes du graphique ci-dessus.
outlier_index <- as.numeric(unlist(result1))
outlier_index

# Créer un nouvel ensemble de données sans les valeurs aberrantes.
health3 <- health2[-outlier_index,]

## Multicollinéarité
ols_vif_tol(health_mod3)

# Tester est qu'il ya un effet d'interaction.
model_interaction1 = lm(
  data = health3,
  systolic ~ weight * diabetes + age * diabetes
)
summary(model_interaction1)


#----------------------------------------------------------------
#  #8. Construire le modèle final
#----------------------------------------------------------------
# Créer le modèle final avec les nouvelles données.
health_mod4 <- lm(data=health3, systolic ~ weight+age+diabetes)
summary(health_mod4)




#################################################################
#                     Mini-projet R:                            #
#           Pr�diction de la pression sanguine                  #
#################################################################

# Avant de commencer, installez les packages n�cessaires:
# install.packages(c("olsrr","lmtest","car"))
# install.packages("tidyverse")

#----------------------------------------------------------------
#  #1. Importation des donn�es
#----------------------------------------------------------------
library(tidyverse)
health <- read_csv("health.csv")

# Visualiser les donn�es import�es.
glimpse(health)

# Convertir les variables 'diabetes' et 'smoker' en facteurs (cat�goriques).
health$diabetes= as.factor(health$diabetes)
health$smoker=as.factor(health$smoker)


#----------------------------------------------------------------
#  #2. Explorer les donn�es
#----------------------------------------------------------------
# Obteneir un r�sum� statistique.
summary(health)

## Visualiser la variable expliqu�e (variable de r�ponse).
ggplot() +
  geom_histogram(mapping=aes(x=health$systolic), fill="lightblue", color="black") +
  theme_minimal() +
  theme(text = element_text(size=14))

## Visualiser les variables explicatives.
# L'op�rateur (%>%) est d�fini par le package 'magrittr' et il'est fortement utilis� par 'dplyr'.
# Exemple: iris %>% head() %>% summary() �quivaut � summary(head(iris)).
health %>%
  select(-systolic) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size=14))

## Les corr�lations.
# Repr�sentation graphique des corr�lations.
pairs(systolic~weight+height+bmi+waist+age,data=health , pch=20)
# Valeures de corr�lations entre les variables continues.
cor(health[c("systolic","weight","height","bmi","waist","age","fastfood")])


#----------------------------------------------------------------
#  #3. Ajustement du mod�le de r�gression lin�aire simple
#----------------------------------------------------------------
# Ajuster le mod�le en utilisant AGE comme seule variable explicative.
health_mod1 <- lm(data=health, systolic~age)
summary(health_mod1)

# Visualiser la regression lin�aire du modele.
age = health$age
systolic =health$systolic
ggplot(health, aes(x = age, y = systolic)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


#----------------------------------------------------------------
#  #4. Ajustement du mod�le de r�gression lin�aire multiple
#----------------------------------------------------------------
# Ajuster un mod�le en utilisant toutes les variables.
health_mod2 <- lm(data=health, systolic~.)
summary(health_mod2)

#----------------------------------------------------------------
#  #5. Tests de diagnostic du mod�le
#----------------------------------------------------------------
## Moyenne nulle des r�sidus.
mean(health_mod2$residuals)

library(olsrr) # Chargement de la biblioth�que olsrr
## Normalit� des r�sidus
# Avec l'histogramme des r�sidus
ols_plot_resid_hist(health_mod2)
# Avec le Q-Q plot des r�sidus
ols_plot_resid_qq(health_mod2)

## Homosc�dasticit� des r�sidus
ols_plot_resid_fit(health_mod2)

## Autocorr�lation des r�sidus
# La fonction set.seed() garantit que nous pouvons obtenir la m�me p-value � chaque fois.
library(car)
set.seed(123)
durbinWatsonTest(health_mod2)

## Analyse des points aberrants
# Cr�er un graphique des points aberrants en fonction de la distance de Cook.
cooksd_chart = ols_plot_cooksd_chart(health_mod2)

# Comparer un �chantillon de point aberrant et le r�sum� statistique pour l'ensemble des donn�es.
health[1358,]
summary(health)

# R�cup�rer les points aberrants
params2 = cooksd_chart$plot_env
result1 = params2$f[,"observation"]

# Lister les valeurs aberrantes du graphique ci-dessus.
outlier_index <- as.numeric(unlist(result1))
outlier_index

# Comparer le r�sum� statistique entre l'ensemble des valeurs aberrantes et l'ensemble sans valeurs aberrantes.
summary(health[outlier_index,])
summary(health[-outlier_index,])

# Cr�er un nouvel ensemble de donn�es sans les valeurs aberrantes.
health2 <- health[-outlier_index,]

## Multicollin�arit�
ols_vif_tol(health_mod2)

#----------------------------------------------------------------
#  #6. Construire les nouveaux mod�les
#----------------------------------------------------------------
# Cr�er un nouveau mod�le avec les nouvelles donn�es et un sous-ensemble de pr�dicteurs.
health_mod3 <- lm(data=health2, systolic ~ weight+age+diabetes)
summary(health_mod3)

# Tester si la variable 'diabetes' est significatif dans le mod�le
drop1(health_mod3,.~.,test="F")


#----------------------------------------------------------------
#  #7.  Tests de diagnostic de nouveaux mod�le
#----------------------------------------------------------------
## Moyenne nulle des r�sidus.
mean(health_mod3$residuals)

## Normalit� des r�sidus
# Avec l'histogramme des r�sidus
ols_plot_resid_hist(health_mod3)
# Avec le Q-Q plot des r�sidus
ols_plot_resid_qq(health_mod3)

## Homosc�dasticit� des r�sidus
ols_plot_resid_fit(health_mod3)

## Autocorr�lation des r�sidus
set.seed(123)
durbinWatsonTest(health_mod3)

## Analyse des points aberrants
cooksd_chart2 = ols_plot_cooksd_chart(health_mod3)

# R�cup�rer les points aberrants
params2 = cooksd_chart2$plot_env
result1 = params2$f[,"observation"]

# Lister les valeurs aberrantes du graphique ci-dessus.
outlier_index <- as.numeric(unlist(result1))
outlier_index

# Cr�er un nouvel ensemble de donn�es sans les valeurs aberrantes.
health3 <- health2[-outlier_index,]

## Multicollin�arit�
ols_vif_tol(health_mod3)

# Tester est qu'il ya un effet d'interaction.
model_interaction1 = lm(
  data = health3,
  systolic ~ weight * diabetes + age * diabetes
)
summary(model_interaction1)


#----------------------------------------------------------------
#  #8. Construire le mod�le final
#----------------------------------------------------------------
# Cr�er le mod�le final avec les nouvelles donn�es.
health_mod4 <- lm(data=health3, systolic ~ weight+age+diabetes)
summary(health_mod4)




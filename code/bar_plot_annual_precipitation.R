library(zoo)
library(dplyr)
library(ggplot2)
library(readxl)
library(scales)
library(trend)
library(fitdistrplus)
library(evd)
library(writexl)


df_dk <- read_excel("data/Pluvio.xlsx")

cumul <- df_dk$`Cumul annuel (mm)`
annees <- df_dk$Année

# les distributions
fit_norm  <- fitdist(cumul, "norm")
fit_lnorm <- fitdist(cumul, "lnorm")
fit_gamma <- fitdist(cumul, "gamma")
fit_weibull <- fitdist(cumul, "weibull")

# Fonctions nécessaires pour fitdistrplus
dgumbel <- function(x, loc, scale) evd::dgumbel(x, loc, scale)
pgumbel <- function(q, loc, scale) evd::pgumbel(q, loc, scale)
qgumbel <- function(p, loc, scale) evd::qgumbel(p, loc, scale)

fit_gumbel <- fitdist(cumul, "gumbel", 
                      start = list(loc = mean(cumul), scale = sd(cumul)))

resultats_gof <- gofstat(list(fit_norm, fit_lnorm, fit_gamma, fit_weibull, fit_gumbel),
                         fitnames = c("Normale", "Log-normale", "Gamma", "Weibull", "Gumbel"))

# AIC/BIC (plus bas = meilleur)
print(resultats_gof$aic)
print(resultats_gof$bic)

# Tests d'ajustement (p-value > 0.05 = bon ajustement)
print(resultats_gof$kstest)
print(resultats_gof$adtest)


resultats_gof <- gofstat(list(fit_norm, fit_lnorm, fit_gamma, fit_weibull, fit_gumbel),
                         fitnames = c("Normale", "Log-normale", "Gamma", "Weibull", "Gumbel"))

# Construction du tableau récapitulatif
tableau_gof <- data.frame(
  Distribution = c("Normale", "Log-normale", "Gamma", "Weibull", "Gumbel"),
  KS           = resultats_gof$ks,
  CvM          = resultats_gof$cvm,
  AD           = resultats_gof$ad,
  AIC          = resultats_gof$aic,
  BIC          = resultats_gof$bic
)

# Paramètres ajustés sur la base Gamma (meilleure loi)
shape <- fit_gamma$estimate["shape"]
rate  <- fit_gamma$estimate["rate"]

# Périodes de retour classiques
periodes_retour <- c(2, 5, 10, 20, 50, 100)
probs_non_depassement <- 1 - 1/periodes_retour  # ex: T=10 -> p=0.90

quantiles_secheresse <- qgamma(1/periodes_retour, shape = shape, rate = rate)
quantiles_humide     <- qgamma(1 - 1/periodes_retour, shape = shape, rate = rate)

tableau_frequentiel <- data.frame(
  `Période de retour (ans)` = periodes_retour,
  `Année sèche` = round(quantiles_secheresse, 1),   # années sèches
  `Année humide`= round(quantiles_humide, 1)        # années humides
)

print(tableau_frequentiel)

# Générer la courbe théorique sur une plage de probabilités
p_seq <- seq(0.01, 0.99, by = 0.01)
courbe_theorique <- data.frame(
  prob = p_seq,
  periode_retour = 1 / p_seq,
  quantile = qgamma(p_seq, shape = shape, rate = rate)
)

# Position des points empiriques (positions de Weibull, standard en hydrologie)
donnees_triees <- sort(cumul)
n <- length(donnees_triees)
prob_empirique <- (1:n) / (n + 1)

points_empiriques <- data.frame(
  quantile = donnees_triees,
  periode_retour = 1 / prob_empirique
)

# Seuils tirés du tableau fréquentiel
seuil_secheresse_10 <- 218.8
seuil_humide_10     <- 623.7

# Trouver l'année la plus humide et la moyenne
annee_max <- df_dk$Année[which.max(df_dk$`Cumul annuel (mm)`)]
valeur_max <- max(df_dk$`Cumul annuel (mm)`, na.rm = TRUE)
moyenne_generale <- mean(df_dk$`Cumul annuel (mm)`, na.rm = TRUE)

# Le graphique
ggplot(df_dk, aes(x = Année, y = `Cumul annuel (mm)`)) +

  geom_hline(yintercept = moyenne_generale, linetype = "dotted", 
             color = "navy", linewidth = 0.9) +
  geom_hline(yintercept = seuil_secheresse_10, linetype = "dashed", 
             color = "#D73027", linewidth = 0.8) +
  geom_hline(yintercept = seuil_humide_10, linetype = "dashed", 
             color = "#2E6E45", linewidth = 0.8) +
  geom_col(aes(fill = case_when(
    `Cumul annuel (mm)` <= seuil_secheresse_10 ~ "Année sécheresse",
    `Cumul annuel (mm)` >= seuil_humide_10     ~ "Année humide",
    TRUE ~ "Année normale"
  )), color = "black", alpha = 0.75, width = 0.7) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Année sécheresse"   = "#D73027",
      "Année humide" = "#2E6E45",
      "Année normale"  = "white"
    )
  ) +
  geom_curve(
    aes(x = annee_max + 9, y = valeur_max - 55, 
        xend = annee_max+0.5, yend = valeur_max + 5),
    curvature = 0.25,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40",
    linewidth = 0.4
  ) +
  # Étiquettes des seuils
  annotate("text", x = max(df_dk$Année) + 0.5, y = seuil_secheresse_10,
           label = "Seuil sécheresse\n(T=10 ans)", color = "#D73027",
           size = 3, hjust = 0, fontface = "bold") +
  annotate("text", x = max(df_dk$Année) + 0.5, y = seuil_humide_10,
           label = "Seuil humide\n(T=10 ans)", color = "#2E6E45",
           size = 3, hjust = 0, fontface = "bold") +
  annotate("label",
           x = annee_max + 14.5,
           y = valeur_max - 85,
           label = paste0("Année record :", "\n", annee_max, " (",
                          round(valeur_max, 1), " mm)"),
           color = "grey10",
           fill = "white",
           fontface = "bold",
           size = 4,
           hjust = 1,
           label.size = 0.6,
           label.padding = unit(0.4, "lines"),
           label.r = unit(0.15, "lines")
  )+
  scale_x_continuous(breaks = seq(1960, 2025, by = 1), 
                     expand = expansion(mult = c(0.01, 0.12))) +
  scale_y_continuous(labels = label_number(suffix = " mm"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Précipitations annuelles à Dakar (1960–2025) et seuils fréquentiels",
    subtitle = "Basé sur l'ajustement à la loi Gamma — seuils T=10 ans (tirets) | Ligne pointillée bleue : moyenne de la période ",
    x = "Année",
    y = "Cumul de précipitations",
    caption = "Source de données : ANACIM | © SMC"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey30", size = 10),
    plot.caption = element_text(color = "grey50", size = 9, hjust = 0),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.4, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)  # marge droite pour les étiquettes de seuil
  )
ggsave("plot_dakar.png", width = 13, height = 7, dpi = 400)

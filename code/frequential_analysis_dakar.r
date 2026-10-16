# =============================================================================
# Analyse des pluies journalières maximales annuelles - Dakar
# Complément à l'analyse des cumuls annuels
# Données : ANACIM | Fichier Excel : 3 colonnes (année, cumul annuel, max journalier)
# =============================================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)
library(trend)
library(fitdistrplus)
library(evd)
library(writexl)
library(ggrepel)  

# -----------------------------------------------------------------------------
# 0. PARAMÈTRES 
# -----------------------------------------------------------------------------
fichier         <- "cumul_pluie_dakar_1950_2020.xlsx"   # chemin du fichier Excel
annee_debut     <- 1960
annee_fin       <- 2020                     # passer à 2025 pour rester cohérent avec le post précédent
periodes_retour <- c(2, 5, 10, 20, 50, 100)
n_boot          <- 1000                     # nombre de rééchantillonnages bootstrap
dir.create("sorties", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. IMPORT ET PRÉPARATION
# -----------------------------------------------------------------------------

df <- read_excel(fichier)[, 1:3]
names(df) <- c("annee", "cumul", "max_jour")

df <- df %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(annee >= annee_debut, annee <= annee_fin) %>%
  arrange(annee) %>%
  mutate(part_max = 100 * max_jour / cumul)   # part du cumul tombée en 1 seul jour (%)

# Contrôle qualité
cat("\n--- Contrôle des données ---\n")
cat("Période :", min(df$annee), "-", max(df$annee), "(", nrow(df), "années )\n")
cat("Valeurs manquantes :\n"); print(colSums(is.na(df)))
annees_manquantes <- setdiff(annee_debut:annee_fin, df$annee)
if (length(annees_manquantes) > 0) cat("Années absentes :", annees_manquantes, "\n")
if (any(df$max_jour > df$cumul, na.rm = TRUE)) warning("Incohérence : max journalier > cumul annuel pour certaines années")

df <- df %>% filter(!is.na(max_jour), !is.na(cumul))
x  <- df$max_jour

# -----------------------------------------------------------------------------
# 2. STATISTIQUES DESCRIPTIVES
# -----------------------------------------------------------------------------
asymetrie <- function(v) mean((v - mean(v))^3) / sd(v)^3

cat("\n--- Statistiques descriptives du maximum journalier annuel (mm) ---\n")
print(summary(x))
cat("Écart-type :", round(sd(x), 1), "\n")
cat("Coefficient de variation :", round(sd(x) / mean(x), 3), "\n")
cat("Asymétrie :", round(asymetrie(x), 3), "\n")

annee_max <- df$annee[which.max(x)]
valeur_max <- max(x)
cat("Record :", annee_max, "->", round(valeur_max, 1), "mm en 24 h\n")

cat("\nTop 10 des plus fortes pluies journalières :\n")
print(df %>% arrange(desc(max_jour)) %>% dplyr::select(annee, max_jour, cumul, part_max) %>% head(10))

cat("\nCorrélation cumul annuel / max journalier (Spearman) :",
    round(cor(df$cumul, df$max_jour, method = "spearman"), 3), "\n")
cor.test(df$cumul, df$max_jour, method = "spearman")
# Moyennes par décennie
par_decennie <- df %>%
  mutate(decennie = paste0(floor(annee / 10) * 10, "s")) %>%
  group_by(decennie) %>%
  summarise(n = n(),
            cumul_moyen    = round(mean(cumul), 1),
            max_moyen      = round(mean(max_jour), 1),
            max_maximorum  = round(max(max_jour), 1),
            part_moyenne   = round(mean(part_max), 1),
            .groups = "drop")
print(par_decennie)

# -----------------------------------------------------------------------------
# 3. TENDANCES ET RUPTURES (Mann-Kendall, pente de Sen, Pettitt)
# -----------------------------------------------------------------------------
tendance <- function(valeurs, annees, nom) {
  mk  <- mk.test(valeurs)
  sen <- sens.slope(valeurs)
  pet <- pettitt.test(valeurs)
  data.frame(
    Variable          = nom,
    MK_p_value        = round(mk$p.value, 4),
    MK_tendance       = ifelse(mk$p.value < 0.05, "significative", "non significative"),
    Pente_Sen_par_an  = round(unname(sen$estimates), 3),
    Pente_Sen_10ans   = round(unname(sen$estimates) * 10, 2),
    Pettitt_annee     = annees[pet$estimate[1]],
    Pettitt_p_value   = round(pet$p.value, 4)
  )
}

tab_tendances <- bind_rows(
  tendance(df$max_jour, df$annee, "Max journalier (mm)"),
  tendance(df$cumul,    df$annee, "Cumul annuel (mm)"),
  tendance(df$part_max, df$annee, "Part du max dans le cumul (%)")
)
cat("\n--- Tests de tendance et de rupture ---\n")
print(tab_tendances)

# Moyennes avant / après la rupture détectée sur le max journalier
pet_max <- pettitt.test(df$max_jour)
i_rup   <- pet_max$estimate[1]
cat("\nRupture (max journalier) :", df$annee[i_rup], "| p =", round(pet_max$p.value, 4), "\n")
cat("Moyenne avant :", round(mean(df$max_jour[1:i_rup]), 1), "mm\n")
cat("Moyenne après :", round(mean(df$max_jour[(i_rup + 1):nrow(df)]), 1), "mm\n")

# Graphique 1 : série des maxima journaliers + tendance de Sen + moyenne mobile 10 ans
pluie_2026 <- 135.5

# Années à mettre en évidence : maxima >= pluie de 2026
df_haut <- df %>% filter(max_jour >= pluie_2026)

p1 <- ggplot(df, aes(x = annee, y = max_jour)) +
  
  # Repère de la pluie du 4 octobre 2026
  geom_hline(yintercept = pluie_2026, color = "#D73027", linewidth = 0.9, linetype = "dashed") +
  annotate("text", x = max(df$annee)-7.5, y = pluie_2026, hjust = 0, vjust = -0.6,
           color = "#D73027", fontface = "bold", size = 3.8,
           label = "4 oct. 2026 (Yoff) : 135.5 mm") +
  
  # Série observée : ligne + points
  geom_line(color = "black", linewidth = 0.9, alpha = 0.8) +
  geom_point(color = "darkgreen", size = 2.4) +
  
  # Années plus intenses que 2026 : points mis en évidence et étiquetés
  geom_point(data = df_haut, color = "#B23A48", size = 3.6) +
  geom_text_repel(data = df_haut, aes(label = paste0(round(max_jour,2))), color = "#B23A48",
                  fontface = "bold", size = 4, seed = 1, min.segment.length = 0,
                  segment.color = "grey60") +
  scale_x_continuous(breaks = seq(1960, 2020, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = label_number(suffix = " mm"),
                     expand = expansion(mult = c(0.02, 0.12))) +
  
  labs(title = "Pluies journalières maximales annuelles à Dakar",
       subtitle = paste0("Points rouges : années plus intenses que le 4 octobre 2026  |  ",
                         "Tirets rouges : pluie du 04 octobre 2026"),
       x = NULL, y = "Maximum journalier",
       caption = "Source de données : ANACIM") +
  
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(color = "grey50", hjust = 0),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        #panel.grid.major.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.2))

print(p1)
ggsave("sorties/max_journalier_tendance.png", p1, width = 15, height = 7.5, dpi = 300)



# -----------------------------------------------------------------------------
# 4. AJUSTEMENT DE LOIS (GEV, Gumbel, Log-normale, Gamma, Weibull)
# -----------------------------------------------------------------------------
# Fonctions nécessaires à fitdistrplus (argument log requis)
dgumbel <- function(x, loc, scale, log = FALSE) evd::dgumbel(x, loc, scale, log = log)
pgumbel <- function(q, loc, scale, lower.tail = TRUE, log.p = FALSE) evd::pgumbel(q, loc, scale, lower.tail = lower.tail)
qgumbel <- function(p, loc, scale, lower.tail = TRUE, log.p = FALSE) evd::qgumbel(p, loc, scale, lower.tail = lower.tail)
dgev    <- function(x, loc, scale, shape, log = FALSE) evd::dgev(x, loc, scale, shape, log = log)
pgev    <- function(q, loc, scale, shape, lower.tail = TRUE, log.p = FALSE) evd::pgev(q, loc, scale, shape, lower.tail = lower.tail)
qgev    <- function(p, loc, scale, shape, lower.tail = TRUE, log.p = FALSE) evd::qgev(p, loc, scale, shape, lower.tail = lower.tail)

# Valeurs initiales (méthode des moments pour Gumbel)
sc0 <- sd(x) * sqrt(6) / pi
lc0 <- mean(x) - 0.5772 * sc0

fit_gumbel  <- fitdist(x, "gumbel", start = list(loc = lc0, scale = sc0))
fit_gev     <- tryCatch(fitdist(x, "gev", start = list(loc = lc0, scale = sc0, shape = 0.1)),
                        error = function(e) { message("GEV non convergente : ", e$message); NULL })
fit_lnorm   <- fitdist(x, "lnorm")
fit_gamma   <- fitdist(x, "gamma")
fit_weibull <- fitdist(x, "weibull")

fits <- list(Gumbel = fit_gumbel, GEV = fit_gev, `Log-normale` = fit_lnorm,
             Gamma = fit_gamma, Weibull = fit_weibull)
fits <- fits[!sapply(fits, is.null)]

gof <- gofstat(fits, fitnames = names(fits))
tableau_gof <- data.frame(Distribution = names(fits),
                          KS = round(gof$ks, 4), CvM = round(gof$cvm, 4), AD = round(gof$ad, 4),
                          AIC = round(gof$aic, 2), BIC = round(gof$bic, 2)) %>%
  arrange(AIC)
cat("\n--- Comparaison des lois (plus bas AIC/BIC = meilleur) ---\n")
print(tableau_gof)

nom_best <- tableau_gof$Distribution[1]
best     <- fits[[nom_best]]
cat("\nLoi retenue (AIC minimal) :", nom_best, "\n")
print(summary(best))

# Diagnostics visuels (QQ, PP, densité, fonction de répartition)
png("sorties/diagnostic_ajustement.png", width = 2400, height = 1800, res = 250)
par(mfrow = c(2, 2))
leg <- names(fits)
denscomp(fits, legendtext = leg); qqcomp(fits, legendtext = leg)
cdfcomp(fits, legendtext = leg);  ppcomp(fits, legendtext = leg)
par(mfrow = c(1, 1)); dev.off()

# Test d'une GEV non stationnaire (position variable dans le temps)
g0 <- fgev(x)
g1 <- fgev(x, nsloc = data.frame(t = df$annee - mean(df$annee)))
lr   <- g0$deviance - g1$deviance
p_lr <- pchisq(lr, df = 1, lower.tail = FALSE)
cat("\n--- GEV stationnaire vs non stationnaire (test du rapport de vraisemblance) ---\n")
cat("Statistique :", round(lr, 3), "| p-value :", round(p_lr, 4), "\n")
cat(ifelse(p_lr < 0.05,
           "-> La position de la loi évolue significativement dans le temps : la stationnarité est à remettre en cause.\n",
           "-> Pas d'évidence statistique d'une non-stationnarité sur cette période.\n"))

# -----------------------------------------------------------------------------
# 5. PÉRIODES DE RETOUR AVEC INTERVALLES DE CONFIANCE (bootstrap paramétrique)
# -----------------------------------------------------------------------------
set.seed(123)
boot_best <- bootdist(best, niter = n_boot, bootmethod = "param")
q_ret <- quantile(boot_best, probs = 1 - 1 / periodes_retour)

tableau_retour <- data.frame(
  `Période de retour (ans)` = periodes_retour,
  `Pluie journalière (mm)`  = round(as.numeric(q_ret$quantiles), 1),
  `IC 95% inf`              = round(as.numeric(q_ret$quantCI[1, ]), 1),
  `IC 95% sup`              = round(as.numeric(q_ret$quantCI[2, ]), 1),
  check.names = FALSE
)
cat("\n--- Pluies journalières de période de retour T (loi", nom_best, ") ---\n")
print(tableau_retour)

# Période de retour des événements observés (record et top 5)
pdist <- function(q) do.call(paste0("p", best$distname), c(list(q), as.list(best$estimate)))
top5 <- df %>% arrange(desc(max_jour)) %>% head(5) %>%
  mutate(`Période de retour estimée (ans)` = round(1 / (1 - pdist(max_jour)), 1)) %>%
  dplyr::select(Année = annee, `Max journalier (mm)` = max_jour, `Période de retour estimée (ans)`)
cat("\n--- Période de retour estimée des plus fortes pluies observées ---\n")
print(top5)

# Graphique 3 : courbe des niveaux de retour
T_seq <- exp(seq(log(1.5), log(200), length.out = 60))
q_seq <- quantile(boot_best, probs = 1 - 1 / T_seq)
courbe <- data.frame(T = T_seq,
                     est = as.numeric(q_seq$quantiles),
                     inf = as.numeric(q_seq$quantCI[1, ]),
                     sup = as.numeric(q_seq$quantCI[2, ]))

emp <- data.frame(max_jour = sort(x, decreasing = TRUE)) %>%
  mutate(rang = row_number(), T = (length(x) + 1) / rang)   # position de tracé de Weibull

p3 <- ggplot() +
  geom_ribbon(data = courbe, aes(x = T, ymin = inf, ymax = sup), fill = "#4C82AF", alpha = 0.2) +
  geom_line(data = courbe, aes(x = T, y = est), color = "#4C82AF", linewidth = 1.2) +
  geom_point(data = emp, aes(x = T, y = max_jour), color = "#B23A48", size = 2.5) +
  scale_x_log10(breaks = c(2, 5, 10, 20, 50, 100, 200)) +
  scale_y_continuous(labels = label_number(suffix = " mm")) +
  labs(title = "Pluies journalières maximales : niveaux de retour à Dakar",
       subtitle = paste0("Loi ", nom_best, " | bande : IC 95 % (bootstrap) | points : observations ",
                         min(df$annee), "-", max(df$annee)),
       x = "Période de retour (ans, échelle log)", y = "Pluie journalière maximale",
       caption = "Source de données : ANACIM") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.caption = element_text(color = "grey50", hjust = 0),
        panel.grid.minor = element_blank(), axis.title = element_text(face = "bold"))
print(p3)

# -----------------------------------------------------------------------------
# 5b. ÉVÉNEMENT DU 4 OCTOBRE 2026 (Yoff) : COMPARAISON À LA SÉRIE HISTORIQUE
# -----------------------------------------------------------------------------

# Rang dans la série historique
rang <- sum(df$max_jour > pluie_2026) + 1
cat("\n--- Événement du 4 octobre 2026 ---\n")
cat("Rang parmi les", nrow(df), "maxima annuels :", rang, "\n")
cat("Maximum historique de la série :", round(valeur_max, 1), "mm en", annee_max, "\n")
cat("Rapport 2026 / record :", round(100 * pluie_2026 / valeur_max), "%\n")

# Période de retour estimée (loi ajustée sur 1960-annee_fin, SANS 2026)
T_2026 <- 1 / (1 - pdist(pluie_2026))
cat("Période de retour estimée :", round(T_2026, 1), "ans\n")

# Intervalle de confiance sur cette période de retour (bootstrap)
T_boot <- apply(boot_best$estim, 1, function(par) {
  p <- do.call(paste0("p", best$distname), c(list(pluie_2026), as.list(par)))
  1 / (1 - p)
})
cat("IC 95 % :", round(quantile(T_boot, c(0.025, 0.975), na.rm = TRUE), 1), "ans\n")

# Ajout au graphique des niveaux de retour
p3 <- p3 +
  geom_hline(yintercept = pluie_2026, linetype = "dashed", color = "darkgreen", linewidth = 1) +
  annotate("label", x = 2, y = pluie_2026, hjust = 0, vjust = -0.3, fontface = "bold",
           color = "darkgreen", fill = "white", size = 4,
           label = "4 oct. 2026 (Yoff) : 135,5 mm")
print(p3)
ggsave("sorties/niveaux_retour_2026.png", p3, width = 12, height = 7, dpi = 300)

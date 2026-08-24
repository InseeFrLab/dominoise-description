# =============================================================================
# Construction des tableaux de démonstration du guide dominoise.
#
# On simule un univers d'entreprises (chiffre d'affaires, région, secteur, clé
# individuelle), puis on en tire trois tableaux agrégés qui partagent le même
# univers : par région, par secteur, et croisé région x secteur.
#
# Cette cohérence est essentielle au chapitre 3 : une cellule composée des mêmes
# contributeurs dans deux tableaux porte la même clé, donc reçoit exactement le
# même bruit. La région R12, volontairement mono-sectorielle, en fournit
# l'illustration.
#
# Lancé automatiquement en `pre-render` ; ne recalcule que si le .rds manque.
# =============================================================================

out <- file.path("data", "tables_demo.rds")
if (file.exists(out) && !isTRUE(as.logical(Sys.getenv("REBUILD_DATA")))) {
  message("data/tables_demo.rds déjà présent — reconstruction ignorée.")
} else {

  set.seed(20260824)

  n_ent    <- 6000
  regions  <- sprintf("R%02d", 1:12)
  secteurs <- sprintf("S%d", 1:8)

  # Effectifs déséquilibrés : quelques régions et secteurs très petits, donc
  # des cellules fortement dominées, indispensables pour illustrer le risque.
  poids_reg <- c(30, 22, 15, 10, 7, 5, 4, 3, 2, 1, 0.6, 0.4)
  poids_sec <- c(28, 20, 15, 12, 10, 8, 5, 2)

  ent <- data.frame(
    id      = seq_len(n_ent),
    region  = sample(regions,  n_ent, TRUE, prob = poids_reg),
    secteur = sample(secteurs, n_ent, TRUE, prob = poids_sec),
    # chiffre d'affaires log-normal : queue épaisse, donc dominance réaliste
    ca      = round(stats::rlnorm(n_ent, meanlog = 6.5, sdlog = 1.8)),
    rk      = stats::runif(n_ent)          # clé individuelle, tirée une fois
  )

  # R12 n'a qu'un seul secteur : la cellule "région R12" et la cellule
  # "R12 x S1" ont exactement les mêmes contributeurs.
  ent$secteur[ent$region == "R12"] <- "S1"

  agreger <- function(df, by) {
    parts <- split(df, df[by], drop = TRUE)
    res <- lapply(parts, function(g) {
      tri <- sort(g$ca, decreasing = TRUE)
      cle <- sum(g$rk)
      out <- data.frame(
        n_ent = nrow(g),
        ca    = sum(g$ca),
        ca_x1 = tri[1],
        ca_x2 = if (length(tri) >= 2) tri[2] else 0,
        ck    = cle - floor(cle)           # partie fractionnaire (déf. 6)
      )
      cbind(g[1, by, drop = FALSE], out)
    })
    res <- do.call(rbind, res)
    rownames(res) <- NULL
    res[order(res[[by[1]]]), ]
  }

  tables <- list(
    region          = agreger(ent, "region"),
    secteur         = agreger(ent, "secteur"),
    region_secteur  = agreger(ent, c("region", "secteur"))
  )

  dir.create("data", showWarnings = FALSE)
  saveRDS(tables, out)
  saveRDS(ent,    file.path("data", "entreprises_demo.rds"))

  message(sprintf("Écrit : %s (%s cellules)",
                  out, paste(vapply(tables, nrow, integer(1)), collapse = " / ")))
}

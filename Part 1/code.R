# ============================================================
# PART 1 - STEP 1: DATA EXPLORATION (NHANES 2021-2023)
# Volgt Les 2: STEP 1.1 know your variables
#              STEP 1.2 describe your data
#              STEP 1.3 missing data
# ============================================================

# ---- 0. Voorbereiding ----------------------------------------
library(tidyverse)
setwd("~/Documents/taak large scale")      # pas aan naar jullie map
dir.create("out", showWarnings = FALSE)    # map voor tabellen en figuren

# ---- 1. Data inlezen -----------------------------------------
# guess_max: read_csv gokt het type van elke kolom op basis van de eerste rijen.
# Zonder guess_max kijkt het maar naar 1000 rijen. Kolommen die bijna leeg zijn
# worden dan als "logical" gelezen en echte waarden verdwijnen (-> NA).
dat <- read_csv("data/NHANES_20212023_merged_filtered 1.csv", guess_max = 20000)
codebook <- read_csv("data/NHANES_20212023_codebook_filtered 1.csv",
                     locale = locale(encoding = "latin1"))

# ============================================================
# STEP 1.1  KNOW YOUR VARIABLES
# ============================================================

# Hoeveel samples (rijen = personen) en features (kolommen)?
dim(dat)
nrow(dat)
ncol(dat)

# Is elke rij een unieke persoon? (moet 0 geven)
sum(duplicated(dat$SEQN))

# Ontbreekt er een ID? (moet 0 geven)
sum(is.na(dat$SEQN))

# Zijn er volledig identieke rijen? (moet 0 geven)
sum(duplicated(dat))

# Eerste blik op de data
head(dat[, 1:10])
glimpse(dat[, 1:20])

# Welke R-datatypes? (bijna alles is numeriek, 1 kolom is tekst)
table(sapply(dat, class))

# Welke soort variabelen zijn er? Zie codebook:
count(codebook, component)

# Aantal verschillende waarden per variabele (lege waarden tellen niet mee)
n_unique <- sapply(dat, n_distinct, na.rm = TRUE)

# Variabelen die nooit veranderen of leeg zijn -> geen informatie
constant_vars <- names(n_unique)[n_unique <= 1]
length(constant_vars)                              # aantal (verwacht: 51)
codebook |> filter(variable %in% constant_vars)    # wat zijn het? (label)

# Variabelen met 2 waarden = binair (ja/nee)
binary_vars <- names(n_unique)[n_unique == 2]
length(binary_vars)

# Variabelen met 3 tot 12 waarden = categorisch of tellingen
# BEKIJK DEZE ZELF: lees het label en beslis nominaal / ordinaal / discreet
few_vars <- names(n_unique)[n_unique >= 3 & n_unique <= 12]
length(few_vars)
print(codebook |> filter(variable %in% few_vars), n = Inf)

# Variabelen met veel waarden (> 12) = continu of discreet
many_vars <- names(n_unique)[n_unique > 12]
length(many_vars)

# ============================================================
# STEP 1.2  DESCRIBE YOUR DATA
# ============================================================

# Kies een paar belangrijke variabelen
key_vars <- c("RIDAGEYR", "BMXBMI", "BMXWAIST", "BPXOSY1", "LBDHDD",
              "LBXTLG", "LBXGLU", "LBXGH", "LBXHSCRP", "LBXHGB")
codebook |> filter(variable %in% key_vars)     # wat betekenen ze?

# Beschrijvende statistiek: min, kwartielen, mediaan, gemiddelde, max, aantal NA
summary(dat[, key_vars])

# Standaarddeviatie van 1 variabele
sd(dat$BMXBMI, na.rm = TRUE)

# Histogrammen (ruw): kijk naar de vorm van de verdeling
png("out/fig_histograms_raw.png", width = 1400, height = 700)
par(mfrow = c(2, 5))
for (v in key_vars) hist(dat[[v]], main = v, xlab = "")
dev.off()

# Histogrammen na log10-transformatie: wordt de vorm symmetrischer?
png("out/fig_histograms_log10.png", width = 1400, height = 700)
par(mfrow = c(2, 5))
for (v in key_vars) hist(log10(dat[[v]]), main = paste("log10", v), xlab = "")
dev.off()

# Boxplot (de punten buiten de whiskers zijn mogelijke outliers)
boxplot(dat$BMXBMI, main = "BMI")

# Categorische variabelen: frequentietabel (useNA toont ontbrekende waarden!)
table(dat$RIAGENDR, useNA = "ifany")      # 1 = man, 2 = vrouw
table(dat$RIDRETH3, useNA = "ifany")
table(dat$DMDEDUC2, useNA = "ifany")      # let op code 9 = "weet niet"

# Staafdiagram
barplot(table(dat$RIDRETH3), main = "Race/ethnicity")

# Valt er iets op aan leeftijd? (afgekapt op 80)
table(dat$RIDAGEYR == 80)

# ============================================================
# STEP 1.3  MISSING DATA
# ============================================================

# Totaal aantal ontbrekende waarden
sum(is.na(dat))

# Per variabele: % ontbrekend
pct_NA <- colMeans(is.na(dat)) * 100
hist(pct_NA, breaks = 40, xlab = "% NA per variabele",
     main = "Missing values per variabele")

# Welke 15 variabelen hebben het meeste ontbrekend?
round(sort(pct_NA, decreasing = TRUE)[1:15], 1)

# Mediaan % NA over alle variabelen
median(pct_NA)

# WAAROM ontbreekt het? Voorbeeld: sommige mensen zijn enkel geinterviewd.
table(dat$RIDSTATR)    # 1 = enkel interview, 2 = interview + onderzoek
# Mensen met RIDSTATR == 1 hebben geen lab- en onderzoeksdata:
dat |> group_by(RIDSTATR) |>
  summarise(pct_NA_glucose = mean(is.na(LBXGLU)) * 100,
            pct_NA_BMI     = mean(is.na(BMXBMI)) * 100)

# Speciale codes: in NHANES betekent 7 / 9 / 77 / 99 soms "geweigerd" of "weet niet".
# Voorbeeld: opleiding (DMDEDUC2): 9 = weet niet -> moet NA worden (in stap 2)
sum(dat$DMDEDUC2 == 9, na.rm = TRUE)
# LET OP: dit mag NIET voor alle variabelen. In RIDRETH3 is 7 een echte categorie
# en in labowaarden zijn 77 of 99 gewoon metingen.

# # ============================================================
# PART 1 - STEP 2: DATA PREPROCESSING (NHANES 2021-2023)
# Volgt Les 2: a. missing values / imputation
#              b. outliers and filtering
#              c. normalisation of omics data
#              d. transformation
#              e. standardization
# Elke beslissing staat in de tekst en kan je overnemen in het verslag.
# ============================================================

# ---- 0. Voorbereiding ----------------------------------------
library(tidyverse)
setwd("~/Documents/taak large scale")      # pas aan naar jullie map
dir.create("out", showWarnings = FALSE)

dat <- read_csv("data/NHANES_20212023_merged_filtered 1.csv", guess_max = 20000)
codebook <- read_csv("data/NHANES_20212023_codebook_filtered 1.csv",
                     locale = locale(encoding = "latin1"))

# Instellingen: pas deze grenzen aan en draai opnieuw om het effect te zien
max_NA   <- 40     # bewaar variabelen met hoogstens 40% NA (bij de onderzochte personen)
cor_cut  <- 0.999  # twee variabelen met |correlatie| > 0.999 zijn dezelfde meting (andere eenheid)
skew_cut <- 1      # log-transformeer variabelen met skewness > 1

# Hulpfunctie: skewness (0 = symmetrisch, > 1 = sterk rechtsscheef)
skew <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / sd(x)^3 }

# Logboek: hoeveel variabelen na elke stap
log_steps <- tibble(stap = "start", n_variabelen = ncol(dat), n_personen = nrow(dat))
add_log <- function(stap, d) {
  log_steps <<- bind_rows(log_steps, tibble(stap = stap, n_variabelen = ncol(d), n_personen = nrow(d)))
}

# ============================================================
# a. MISSING VALUES / IMPUTATION
# ============================================================

# --- a1. Hoeveel ontbreekt er? (controle van het overzicht uit stap 1) ---
pct_NA   <- colMeans(is.na(dat)) * 100
n_unique <- sapply(dat, n_distinct, na.rm = TRUE)
sum(pct_NA == 0)                    # variabelen zonder NA            (12)
sum(n_unique <= 10)                 # hoogstens 10 waarden            (206)
sum(pct_NA > 90)                    # meer dan 90% NA                 (35)
sum(pct_NA > 75 & pct_NA <= 90)     # tussen 75 en 90% NA             (121)

# --- a2. Speciale codes ("geweigerd" / "weet niet") worden NA ---
# BESLISSING: alleen bij variabelen waar de code echt een niet-antwoord is.
# In lab-metingen zijn 77 of 99 gewoon waarden, en in RIDRETH3 is 7 een categorie.
special <- list(
  list(vars = c("DMQMILIZ", "DMDEDUC2", "WHQ030E", "MCQ080E", "ALQ111", "ALQ151",
                "DBQ010", "DBQ330", "DBQ360", "DBQ370", "DBQ390", "DBQ400",
                "DBQ421", "DBQ424", "DBQ930", "DBQ935", "DBQ940", "DBQ945",
                "ECQ080", "ECQ090"), codes = c(7, 9)),
  list(vars = c("DMDYRUSR", "DMDMARTZ", "ALQ121", "ALQ142", "ALQ270", "ALQ280"),
       codes = c(77, 99)),
  list(vars = c("ALQ130", "ALQ170"), codes = c(777, 999)),
  list(vars = c("DBD381", "DBD411", "ECD070A", "ECD070B"), codes = c(7777, 9999))
)
na_before <- sum(is.na(dat))
for (g in special) {
  for (v in g$vars) dat[[v]][dat[[v]] %in% g$codes] <- NA
}
sum(is.na(dat)) - na_before         # aantal nieuwe NA's door de speciale codes

# --- a3. Welke personen houden we? ---
# BESLISSING: enkel de onderzochte personen (RIDSTATR == 2). De 3.073 personen met
# enkel een interview hebben geen onderzoeks- of labodata (structureel ontbrekend).
# Wil je alleen volwassenen? Voeg dan toe: filter(RIDAGEYR >= 18)
table(dat$RIDSTATR)
dat <- dat |> filter(RIDSTATR == 2)
add_log("alleen onderzochte personen", dat)

# --- a4. Variabelen zonder informatie verwijderen ---
# BESLISSING: constante of lege variabelen (na de vorige stappen opnieuw bepaald)
n_unique <- sapply(dat, n_distinct, na.rm = TRUE)
constant_vars <- names(n_unique)[n_unique <= 1]
length(constant_vars)
# BESLISSING: ID en survey-gewichten zijn geen kenmerken van de persoon -> apart bewaren
design_vars <- c("SEQN", names(dat)[str_detect(names(dat), "^WT")], "SDMVSTRA", "SDMVPSU")
# BESLISSING: de 'below detection limit'-vlaggen (...LC) zijn geen metingen
flag_vars <- names(dat)[str_detect(names(dat), "^(LBD|URD).*LC$")]
ids <- dat |> select(SEQN, any_of(c("WTMEC2YR", "SDMVSTRA", "SDMVPSU")))
dat <- dat |> select(-any_of(c(constant_vars, design_vars, flag_vars)))
add_log("constante, ID/gewicht en vlag-variabelen weg", dat)

# --- a5. Variabelen met te veel NA verwijderen ---
# BESLISSING: grens max_NA (%). Gemeten bij de onderzochte personen.
# Afweging: een hogere grens houdt meer variabelen (bv. glucose, LDL) maar vraagt
# meer imputatie. Pas aan naargelang jullie onderzoeksvraag.
pct_NA <- colMeans(is.na(dat)) * 100
hist(pct_NA, breaks = 40, xlab = "% NA per variabele (onderzochte personen)",
     main = "Missing values na selectie van personen")
sum(pct_NA > max_NA)                # aantal variabelen dat wegvalt
dat <- dat |> select(all_of(names(pct_NA)[pct_NA <= max_NA]))
add_log(paste0("variabelen met > ", max_NA, "% NA weg"), dat)

# --- a6. Opsplitsen: categorische annotatie versus numerieke kenmerken ---
# Variabelen met hoogstens 12 waarden = categorisch (blijven als annotatie voor QC).
# Variabelen met meer waarden = numeriek (gaan de analyse in).
n_unique <- sapply(dat, n_distinct, na.rm = TRUE)
annot <- dat |> select(all_of(names(n_unique)[n_unique <= 12]))
feat  <- dat |> select(all_of(names(n_unique)[n_unique > 12]))
dim(annot); dim(feat)

# --- a7. Imputatie ---
# BESLISSING: kNN-imputatie gebeurt helemaal op het einde (stap f), op getransformeerde
# en gestandaardiseerde data. Reden: kNN gebruikt afstanden tussen personen, en die zijn
# alleen eerlijk als alle variabelen op dezelfde schaal staan.
# De categorische annotatie wordt niet geimputeerd (NA blijft NA).

# ============================================================
# b. OUTLIERS AND FILTERING
# ============================================================

# --- b1. Outliers met de IQR-regel (zoals in de les) ---
count_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE); i <- q[2] - q[1]
  sum(x < q[1] - 1.5 * i | x > q[2] + 1.5 * i, na.rm = TRUE)
}
n_out   <- sapply(feat, count_outliers)
pct_out <- round(n_out / colSums(!is.na(feat)) * 100, 1)
round(sort(pct_out, decreasing = TRUE)[1:10], 1)  # variabelen met de meeste outliers
# Voorbeeld BMI: boxplot en z-score
boxplot(feat$BMXBMI, main = "BMI")
sum(abs(scale(feat$BMXBMI)) > 3, na.rm = TRUE)    # outliers volgens z-score > 3
# BESLISSING: outliers worden NIET verwijderd. Het zijn echte metingen (bv. zwaarlijvigheid,
# ziekte) en geen meetfouten. De log-transformatie (d) verkleint hun invloed.

# --- b2. Variabelen zonder spreiding ---
# BESLISSING: IQR == 0 betekent dat meer dan de helft van de waarden identiek is
# (vaak "onder de detectielimiet") -> te weinig variatie.
iqr0 <- names(feat)[sapply(feat, IQR, na.rm = TRUE) == 0]
iqr0
feat <- feat |> select(-all_of(iqr0))

# --- b3. Dubbele metingen (zelfde meting in twee eenheden) ---
# BESLISSING: per paar behouden we de CONVENTIONELE eenheid en verwijderen we de SI-variant.
# De 40 SI-varianten komen uit de paartabel van stap 1 (paren met |r| > 0.999).
# Let op: we verwijderen NIET alle namen die op "SI" eindigen. Veel variabelen
# (bv. LBXWBCSI, LBXPLTSI) hebben geen conventionele tegenhanger en blijven.
# Cholesterol en triglyceriden uit twee aparte labtests (LBXTC/LBXSCH, LBXTLG/LBXSTR)
# zijn verschillende metingen: alleen de eenheidsparen binnen dezelfde test zijn duplicaten.
si_vars <- c("URXUMS", "URXCRS", "LBDHDDSI", "LBDTRSI", "LBDLDLSI", "LBDLDMSI", "LBDLDNSI",
             "LBDTCSI", "LBDFERSI", "LBDRFOSI", "LBDINSI", "LBDBPBSI", "LBDBCDSI", "LBDTHGSI",
             "LBDBSESI", "LBDBMNSI", "LBDIHGSI", "LBDBGMSI", "LBDGLUSI", "LBDFOTSI", "LBD17HSI",
             "LBDANDSI", "LBDAMHSI", "LBDDHESI", "LBDESTSI", "LBDESOSI", "LBDES1SI", "LBDPG4SI",
             "LBDTSTSI", "LBDSALSI", "LBDSBUSI", "LBDSCRSI", "LBDSGBSI", "LBDSPHSI", "LBDSTBSI",
             "LBDSCHSI", "LBDSTPSI", "LBDSTRSI", "LBDSUASI", "LBDTFRSI")
length(si_vars)                                   # 40
dup_vars <- intersect(si_vars, names(feat))       # enkel de SI-varianten die nog aanwezig zijn
length(dup_vars)                                  # hoeveel zijn er nog over na de vorige filters?
feat <- feat |> select(-all_of(dup_vars))
ncol(feat)

# Controle: zijn er nog paren met |r| > cor_cut (bijna identieke variabelen)?
cm <- cor(feat, use = "pairwise.complete.obs")
cm[lower.tri(cm, diag = TRUE)] <- NA
rest <- which(abs(cm) > cor_cut, arr.ind = TRUE)
data.frame(var1 = rownames(cm)[rest[, 1]], var2 = colnames(cm)[rest[, 2]])   # liefst leeg
log_steps <- bind_rows(log_steps, tibble(stap = "numerieke kenmerken na filtering (b)",
                                         n_variabelen = ncol(feat), n_personen = nrow(feat)))

# ============================================================
# c. NORMALISATION OF OMICS DATA
# ============================================================
# BESLISSING: niet van toepassing. Dit is geen omics-data (geen counts, geen sequencing
# depth, geen spike-ins). De labowaarden zijn al gekalibreerde concentraties of tellingen.
# Er is dus geen correctie voor bibliotheekgrootte of technische variatie zoals bij RNA-seq.

# ============================================================
# d. TRANSFORMATION (log)
# ============================================================
sk_before <- sapply(feat, skew)
round(sort(sk_before, decreasing = TRUE)[1:10], 1)    # meest scheve variabelen
sum(sk_before > skew_cut)                              # hoeveel worden getransformeerd

# BESLISSING: log10 voor variabelen met skewness > skew_cut (rechtsscheef).
# Variabelen met waarden <= 0 krijgen eerst een kleine verschuiving zodat log10 mogelijk is.
log_vars <- names(sk_before)[sk_before > skew_cut]
feat_log <- feat
for (v in log_vars) {
  x <- feat[[v]]
  m <- min(x, na.rm = TRUE)
  if (m <= 0) x <- x - m + (min(x[x > m], na.rm = TRUE) - m) / 2
  feat_log[[v]] <- log10(x)
}
sk_after <- sapply(feat_log, skew)
summary(abs(sk_before[log_vars]))    # skewness voor
summary(abs(sk_after[log_vars]))     # skewness na (moet kleiner zijn)

# Figuur: de 4 meest scheve variabelen, ruw (boven) en na log10 (onder)
plot_vars <- names(sort(sk_before[log_vars], decreasing = TRUE))[1:4]
png("out/fig_log_before_after.png", width = 1200, height = 600)
par(mfrow = c(2, 4))
for (v in plot_vars) hist(feat[[v]],     main = paste(v, "(ruw)"),   xlab = "")
for (v in plot_vars) hist(feat_log[[v]], main = paste(v, "(log10)"), xlab = "")
dev.off()

# ============================================================
# e. STANDARDIZATION
# ============================================================
# BESLISSING: z-score per variabele (gemiddelde 0, sd 1), zodat variabelen met grote
# eenheden (bv. cholesterol in mg/dL) niet domineren in PCA, clustering en heatmaps.
feat_scaled <- scale(feat_log)
round(head(colMeans(feat_scaled, na.rm = TRUE)), 3)         # moet ~0 zijn
round(head(apply(feat_scaled, 2, sd, na.rm = TRUE)), 3)     # moet 1 zijn

png("out/fig_scaling_before_after.png", width = 1200, height = 500)
par(mfrow = c(1, 2))
boxplot(feat_log[, 1:20],    las = 2, main = "voor standaardisatie", cex.axis = 0.6)
boxplot(feat_scaled[, 1:20], las = 2, main = "na standaardisatie",   cex.axis = 0.6)
dev.off()

# ============================================================
# f. IMPUTATIE (kNN) op de getransformeerde en gestandaardiseerde data
# ============================================================
# BESLISSING: kNN met k = 10: een ontbrekende waarde wordt aangevuld met het gemiddelde van
# de 10 meest gelijkende personen. Enkel voor de numerieke kenmerken.
library(impute)

# BESLISSING: personen waarvan meer dan de helft van de numerieke kenmerken ontbreekt,
# verwijderen we eerst. Voor hen is kNN niet betrouwbaar: R vult dan gewoon het gemiddelde
# in (zonder deze stap waarschuwt impute.knn hiervoor).
person_NA <- rowMeans(is.na(feat_scaled))
sum(person_NA > 0.5)                            # aantal personen dat wegvalt
keep_p      <- person_NA <= 0.5
feat_scaled <- feat_scaled[keep_p, ]
annot       <- annot[keep_p, ]
ids         <- ids[keep_p, ]
log_steps <- bind_rows(log_steps, tibble(stap = "personen met > 50% NA in kenmerken weg",
                                         n_variabelen = ncol(feat_scaled), n_personen = nrow(feat_scaled)))

sum(is.na(feat_scaled))                         # NA voor imputatie
round(100 * mean(is.na(feat_scaled)), 1)        # % NA in de matrix
set.seed(123)
feat_imp <- impute.knn(as.matrix(feat_scaled), k = 10)$data
sum(is.na(feat_imp))                            # moet 0 zijn

# ============================================================
# OPSLAAN
# ============================================================
write_csv(bind_cols(ids, annot, as_tibble(feat_imp)), "out/nhanes_clean_imputed.csv")
write_csv(bind_cols(ids, annot, as_tibble(feat_scaled)), "out/nhanes_clean_not_imputed.csv")
write_csv(log_steps, "out/preprocessing_log.csv")
print(log_steps)

# ============================================================
# PART 1 - STEP 3: QUALITY CONTROL / EXPLORATION VIA VISUALIZATION
# Volgt Les 2 STEP 3: correlaties, heatmap, PCA
# Vraag: clusteren de personen volgens biologie of volgens een technische factor?
# Input: de uitvoer van stap 2 (out/nhanes_clean_imputed.csv)
# ============================================================

# ---- 0. Voorbereiding ----------------------------------------
library(tidyverse)
library(pheatmap)      # eerste keer: install.packages("pheatmap")
setwd("~/Documents/taak large scale")      # pas aan naar jullie map
dir.create("out", showWarnings = FALSE)

imp     <- read_csv("out/nhanes_clean_imputed.csv")        # na kNN-imputatie
not_imp <- read_csv("out/nhanes_clean_not_imputed.csv")    # voor imputatie (met NA)
raw     <- read_csv("data/NHANES_20212023_merged_filtered 1.csv", guess_max = 20000)

# Opsplitsen: ID/gewichten, categorische annotatie, numerieke kenmerken
id_cols   <- c("SEQN", "WTMEC2YR", "SDMVSTRA", "SDMVPSU")
n_unique  <- sapply(imp, n_distinct)
annot_cols <- setdiff(names(imp)[n_unique <= 12], id_cols)
feat_cols  <- setdiff(names(imp), c(id_cols, annot_cols))
length(annot_cols); length(feat_cols)               # 34 en 133

feat <- as.matrix(imp[, feat_cols])                 # rijen = personen, kolommen = variabelen
rownames(feat) <- imp$SEQN
dim(feat)

# Originele (niet-gestandaardiseerde) leeftijd erbij: in feat is leeftijd een z-score
meta <- imp |> select(SEQN, all_of(annot_cols)) |>
  left_join(raw |> select(SEQN, age = RIDAGEYR), by = "SEQN") |>
  mutate(
    sex        = factor(RIAGENDR, labels = c("man", "vrouw")),
    age_group  = cut(age, c(-1, 17, 39, 59, 79, 80),
                     labels = c("0-17", "18-39", "40-59", "60-79", "80+")),
    race       = factor(RIDRETH3),
    period     = factor(RIDEXMON),              # TECHNISCH: periode van het jaar van het onderzoek
    arm        = factor(BPAOARM),               # TECHNISCH: arm van de bloeddrukmeting
    n_imputed  = rowSums(is.na(not_imp[, feat_cols]))   # TECHNISCH: hoeveel waarden zijn ingevuld
  )

# Vaste kleuren, zodat dezelfde groep in alle figuren dezelfde kleur heeft
col_map <- list(
  sex       = c("man" = "#0072B2", "vrouw" = "#D55E00"),
  age_group = c("0-17" = "#FDE725", "18-39" = "#5DC863", "40-59" = "#21908C",
                "60-79" = "#3B528B", "80+" = "#440154"),
  period    = c("1" = "#999999", "2" = "#CC79A7"),
  arm       = c("L" = "#F0E442", "R" = "#009E73")
)

# ============================================================
# 3.1 UNIVARIAAT: zien de variabelen er na preprocessing goed uit?
# ============================================================
png("out/fig3_boxplot_all_features.png", width = 1400, height = 500)
boxplot(feat, las = 2, cex.axis = 0.3, outline = FALSE,
        main = "Alle numerieke variabelen na preprocessing (gestandaardiseerd)")
dev.off()

# ============================================================
# 3.2 BIVARIAAT/MULTIVARIAAT: correlaties tussen variabelen
# ============================================================
cm <- cor(feat)
png("out/fig3_correlation_heatmap.png", width = 1000, height = 1000)
pheatmap(cm, clustering_method = "ward.D2", show_rownames = FALSE, show_colnames = FALSE,
         main = "Correlatie tussen de numerieke variabelen")
dev.off()
# Blokken met hoge correlatie = variabelen die samen horen (bv. bloeddruk, vetten, bloedcellen)

# ============================================================
# 3.3 HEATMAP personen x variabelen
# ============================================================
# Een heatmap toont maximaal ~1000 rijen leesbaar -> willekeurige steekproef van 500 personen
set.seed(1)
idx <- sample(nrow(feat), 500)
ann_row <- data.frame(sex = meta$sex[idx], age_group = meta$age_group[idx],
                      period = meta$period[idx], arm = meta$arm[idx],
                      row.names = rownames(feat)[idx])
mat <- feat[idx, ]
mat[mat > 3]  <- 3      # kleurschaal afkappen op -3 .. 3 zodat extremen niet alles overheersen
mat[mat < -3] <- -3
png("out/fig3_heatmap_persons.png", width = 1500, height = 1000, res = 110)
pheatmap(mat, clustering_method = "ward.D2", show_rownames = FALSE, show_colnames = FALSE,
         annotation_row = ann_row, annotation_colors = col_map,
         treeheight_row = 40, treeheight_col = 40, fontsize = 12,
         main = "500 willekeurige personen x numerieke variabelen")
dev.off()

# ============================================================
# 3.4 PCA
# ============================================================
pca <- prcomp(feat, center = TRUE, scale. = TRUE)
var_expl <- summary(pca)$importance[2, ] * 100          # % variantie per component
round(var_expl[1:10], 1)

png("out/fig3_screeplot.png", width = 700, height = 500)
barplot(var_expl[1:20], names.arg = 1:20, xlab = "Principal component",
        ylab = "% verklaarde variantie", main = "Screeplot")
dev.off()

scores <- as_tibble(pca$x[, 1:5]) |> bind_cols(meta)

# PC1 versus PC2, gekleurd per biologische en per technische factor
pc_lab <- labs(x = sprintf("PC1 (%.1f%% van de variatie)", var_expl[1]),
               y = sprintf("PC2 (%.1f%% van de variatie)", var_expl[2]))
pc_plot <- function(var, title) {
  ggplot(scores, aes(PC1, PC2, color = .data[[var]])) +
    geom_point(alpha = 0.4, size = 0.8) + labs(title = title) + pc_lab +
    scale_color_manual(values = col_map[[var]]) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
    theme_minimal(base_size = 13)
}
p_bio  <- list(pc_plot("sex", "biologie: geslacht"), pc_plot("age_group", "biologie: leeftijd"))
p_tech <- list(pc_plot("period", "techniek: periode"), pc_plot("arm", "techniek: arm bloeddruk"))
ggsave("out/fig3_pca_sex.png",    p_bio[[1]],  width = 6, height = 5, bg = "white")
ggsave("out/fig3_pca_age.png",    p_bio[[2]],  width = 6, height = 5, bg = "white")
ggsave("out/fig3_pca_period.png", p_tech[[1]], width = 6, height = 5, bg = "white")
ggsave("out/fig3_pca_arm.png",    p_tech[[2]], width = 6, height = 5, bg = "white")
ggsave("out/fig3_pca_nimputed.png",
       ggplot(scores, aes(PC1, PC2, color = n_imputed)) + geom_point(alpha = 0.4, size = 0.8) +
         scale_color_viridis_c() + labs(title = "techniek: aantal ingevulde waarden") + pc_lab +
         theme_minimal(base_size = 13),
       width = 6, height = 5, bg = "white")

# ---- Hoeveel van elke PC verklaart een factor? (R-kwadraat) ----
# Hoge waarde = die PC volgt die factor. Vergelijk biologie met techniek.
r2 <- function(pc, f) {
  d <- tibble(y = scores[[pc]], f = scores[[f]]) |> drop_na()
  round(summary(lm(y ~ f, data = d))$r.squared, 3)
}
factors <- c("sex", "age_group", "race", "period", "arm", "n_imputed")
r2_table <- expand_grid(PC = paste0("PC", 1:5), factor = factors) |>
  mutate(R2 = map2_dbl(PC, factor, r2)) |>
  pivot_wider(names_from = PC, values_from = R2)
print(r2_table)
write_csv(r2_table, "out/fig3_pca_R2_table.csv")

# ---- Welke variabelen sturen PC1 en PC2? ----
load_pc <- as_tibble(pca$rotation[, 1:2], rownames = "variable")
load_pc |> arrange(desc(abs(PC1))) |> head(10)
load_pc |> arrange(desc(abs(PC2))) |> head(10)

# ============================================================
# 3.5 AFWIJKENDE PERSONEN (samples die anders zijn dan de rest)
# ============================================================
dist_pc <- sqrt(rowSums(pca$x[, 1:5]^2))                # afstand tot het centrum in PC1-5
thr <- mean(dist_pc) + 4 * sd(dist_pc)
sum(dist_pc > thr)                                       # aantal sterk afwijkende personen
out_ids <- imp$SEQN[dist_pc > thr]
meta |> filter(SEQN %in% out_ids) |> select(SEQN, sex, age, n_imputed) |> head(15)
# Beslissing: eerst kijken WAT er aan is (extreme meting, veel ingevulde waarden, ziek?)
# voor je iemand verwijdert. Een extreem maar echt profiel is interessante biologie.
summary(meta$n_imputed[dist_pc > thr]); summary(meta$n_imputed)

# ============================================================
# 3.6 HIERARCHISCHE CLUSTERING (personen)
# ============================================================
# Een afstandsmatrix van 7.000 personen is te groot (7000 x 7000) -> steekproef van 1000
set.seed(2)
idx_h <- sample(nrow(feat), 1000)
d_h   <- dist(feat[idx_h, ], method = "euclidean")      # afstand tussen alle paren personen
hc    <- hclust(d_h, method = "ward.D2")                # Ward: compacte clusters (zoals in de les)

png("out/fig3_dendrogram.png", width = 1200, height = 600)
plot(hc, labels = FALSE, hang = -1, xlab = "1000 willekeurige personen", sub = "",
     ylab = "afstand (Ward)", main = "Hierarchische clustering (1000 personen, ward.D2)")
rect.hclust(hc, k = 3, border = c("red", "blue", "darkgreen"))
dev.off()

hc_cl <- cutree(hc, k = 3)                              # knip de boom in 3 clusters
table(hc_cl)
# Vergelijk de clusters met biologie en techniek
table(cluster = hc_cl, age_group = meta$age_group[idx_h])
table(cluster = hc_cl, sex       = meta$sex[idx_h])
table(cluster = hc_cl, period    = meta$period[idx_h])

# ============================================================
# 3.7 K-MEANS CLUSTERING (personen)
# ============================================================
# Hoeveel clusters? Elleboogplot: totale variatie binnen de clusters voor k = 1..10
set.seed(3)
wss <- sapply(1:10, function(k) kmeans(feat, centers = k, nstart = 10, iter.max = 50)$tot.withinss)
png("out/fig3_kmeans_elbow.png", width = 700, height = 500)
plot(1:10, wss, type = "b", xlab = "aantal clusters k", ylab = "totale within-cluster variatie",
     main = "Elleboogplot k-means")
dev.off()

# Silhouette (hoe goed zit elke persoon in zijn cluster?) op de steekproef van 1000
library(cluster)
sil <- sapply(2:6, function(k) {
  km <- kmeans(feat[idx_h, ], centers = k, nstart = 10, iter.max = 50)
  mean(silhouette(km$cluster, d_h)[, "sil_width"])
})
names(sil) <- 2:6
round(sil, 3)                                           # hoger = betere clusters (max 1)

# k-means met k = 3 op alle personen (nstart = 25: 25 random starts, beste behouden)
set.seed(4)
km <- kmeans(feat, centers = 3, nstart = 25, iter.max = 50)
table(km$cluster)
table(cluster = km$cluster, age_group = meta$age_group)
table(cluster = km$cluster, sex       = meta$sex)
table(cluster = km$cluster, period    = meta$period)

scores$kmeans <- factor(km$cluster)
ggsave("out/fig3_pca_kmeans.png",
       ggplot(scores, aes(PC1, PC2, color = kmeans)) + geom_point(alpha = 0.4, size = 0.8) +
         labs(title = "k-means (k = 3) op PCA") + pc_lab +
         guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) + theme_minimal(base_size = 13),
       width = 6, height = 5, bg = "white")

# ============================================================
# 3.8 T-SNE (dimensiereductie voor lokale structuur)
# ============================================================
# t-SNE is traag en tekent geen globale afstanden -> steekproef van 2000 personen
library(Rtsne)       # eerste keer: install.packages("Rtsne")
set.seed(5)
idx_t <- sample(nrow(feat), 2000)
tsne  <- Rtsne(feat[idx_t, ], perplexity = 30, check_duplicates = FALSE, pca = TRUE)
tsne_df <- tibble(tSNE1 = tsne$Y[, 1], tSNE2 = tsne$Y[, 2],
                  age_group = meta$age_group[idx_t], sex = meta$sex[idx_t],
                  period = meta$period[idx_t])
for (v in c("age_group", "sex", "period")) {
  ggsave(paste0("out/fig3_tsne_", v, ".png"),
         ggplot(tsne_df, aes(tSNE1, tSNE2, color = .data[[v]])) + geom_point(alpha = 0.5, size = 0.8) +
           labs(title = paste("t-SNE, gekleurd per", v)) +
           scale_color_manual(values = col_map[[v]]) +
           guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
           theme_minimal(base_size = 13),
         width = 6, height = 5, bg = "white")
}
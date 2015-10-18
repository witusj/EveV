library(dplyr)

setwd("~/GitHub/Evelien")

# read in data and merge data frames on "klant_nr"
klant_file <- "klant.csv"
trace_file <- "trace.csv"
geo_file <- "postcode_NL_en.csv"

klant_data <- read.csv(klant_file, stringsAsFactors = FALSE)
klant_data$klant_postcode <- gsub(" ", "", klant_data$klant_postcode)
trace_data <- read.csv(trace_file, stringsAsFactors = FALSE)
trace_data <- merge(trace_data, klant_data, by = "klant_nr", all.x = TRUE)

# Naar wat voor categorieen klanten kijkt de werknemer het vaakst?
views_klant_cat <- as.data.frame.matrix(table(klant_cat = trace_data$CGC_OMSCHRIJVING, screen = trace_data$SCREENNAME))
views_klant_cat$totaal <- rowSums(views_klant_cat)
views_klant_cat <- cbind(klant_cat = as.character(rownames(views_klant_cat)), views_klant_cat)
row.names(views_klant_cat) <- NULL
views_klant_cat$klant_cat <- as.character(views_klant_cat$klant_cat)
views_klant_cat <- arrange(views_klant_cat, desc(totaal))
pref_cat <- grep("Pref.+", views_klant_cat$klant_cat)
views_klant_cat[pref_cat,1] <- "Preferred Banking"

views_klant_cat_prop <- views_klant_cat %>%
  group_by(klant_cat) %>%
  summarise_each(funs(sum)) %>%
  mutate(prop=totaal/sum(totaal)) %>%
  arrange(desc(prop))

write.csv(views_klant_cat_prop, "aantal_vws_per_cat.csv")

# Uit de data blijkt dat de werknemer het vaakst naar retail klanten kijkt (zijn eigen doelgroep),
# maar ook vaak naar klanten uit de categorieen:
# Preferred Banking, Jongeren Retail en Sports & Entertainment Desk (SED). 

# Onderzoek laatste drie categorieen.
sel1 <- head(views_klant_cat_prop[-1,]$klant_cat, 3)
trace_data1 <- trace_data
pref_cat <- grep("Pref.+", trace_data1$CGC_OMSCHRIJVING)
trace_data1[pref_cat,]$CGC_OMSCHRIJVING <- "Preferred Banking"
trace_data_sel1 <- trace_data1 %>%
  filter(CGC_OMSCHRIJVING %in% sel1) %>%
  group_by(CGC_OMSCHRIJVING, klant_nr, klant) %>%
  summarise(aantal_views = n())

trace_data_sel1 <- merge(trace_data_sel1, unique(trace_data1[c(1,6:9)]), by = "klant_nr", all.x = TRUE)

trace_data_sel1 <- arrange(trace_data_sel1, CGC_OMSCHRIJVING,  -aantal_views)
#trace_data_sel1 <- trace_data_sel1[order(trace_data_sel1$CGC_OMSCHRIJVING, -trace_data_sel1$aantal_views),]
write.csv(trace_data_sel1, "aantal_vws_per_cat_top3.csv")

# Hoe vaak bekijkt hij gegevens van familieleden?
werkn <- "Grondkoekoek Californische"
trace_data_sel2 <- trace_data %>%
  filter(klant == werkn) %>%
  group_by(klant_nr, klant) %>%
  summarise(aantal_views = n())

trace_data_sel2 <- merge(trace_data_sel2, unique(trace_data1[c(1,6:9)]), by = "klant_nr", all.x = TRUE)
trace_data_sel2 <- arrange(trace_data_sel2, -aantal_views)
write.csv(trace_data_sel2, "aantal_vws_fam.csv")

# Bekijkt hij gegevens van buurtbewonders?
postc <- "0137AA"
trace_data_sel3 <- trace_data %>%
  filter(klant_postcode == postc) %>%
  group_by(klant_nr, klant) %>%
  summarise(aantal_views = n())

trace_data_sel3 <- merge(trace_data_sel3, unique(trace_data1[c(1,4,6:9)]), by = "klant_nr", all.x = TRUE)
trace_data_sel3 <- arrange(trace_data_sel3, -aantal_views)
write.csv(trace_data_sel3, "aantal_vws_buurt.csv")

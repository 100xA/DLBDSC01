# Ben<U+00F6>tigte Bibliotheken laden
library(dplyr)
library(tidyr)
library(wbstats)
library(ggplot2)

# Definition der Weltbank-Indikatoren
indicators <- c(
    "NY.GDP.PCAP.CD", # BIP pro Kopf
    "SP.DYN.TFRT.IN", # Fertilitätsrate
    "SP.DYN.LE00.IN", # Lebenserwartung
    "SE.SEC.CUAT.PO.ZS", # Prozentsatz mit postsekundärer Bildung (25+)
    "SL.TLF.ACTI.ZS", # Erwerbsquote
    "SH.XPD.CHEX.PC.CD" # Gesundheitsausgaben pro Kopf
)

# Daten von der Weltbank abrufen
data <- wb_data(indicators, start_date = 2000, end_date = 2019)

# Berechnung der Gesamtjahre und des Schwellenwerts f<U+00FC>r die Datenqualit<U+00E4>t
total_years <- 2019 - 2000 + 1
threshold <- ceiling(0.3 * total_years)

# Identifizierung der Spalten, die Indikatoren repr<U+00E4>sentieren
indicator_cols <- setdiff(names(data), c("country", "date", "iso2c", "iso3c"))

# Z<U+00E4>hlen der nicht-NA Werte f<U+00FC>r jedes Land und jeden Indikator
grouped_data <- group_by(data, country)
data_counts <- summarise(grouped_data, across(all_of(indicator_cols),
    function(x) sum(!is.na(x)),
    .names = "count_{.col}"
))

# Identifizierung der L<U+00E4>nder, die den Schwellenwert f<U+00FC>r alle Indikatoren erf<U+00FC>llen
valid_countries_df <- filter(
    data_counts,
    if_all(
        starts_with("count_"),
        function(x) x >= threshold
    )
)
valid_countries <- pull(valid_countries_df, country)

# Filterung des urspr<U+00FC>nglichen Datensatzes auf g<U+00FC>ltige L<U+00E4>nder
data_filtered <- filter(data, country %in% valid_countries)

# Funktion zur linearen Interpolation
interpolate_linear <- function(x) {
    nas <- is.na(x)
    x[nas] <- approx(x = which(!nas), y = x[!nas], xout = which(nas))$y
    return(x)
}

# Anwendung der Interpolation auf alle Indikatoren
data_filtered <- group_by(data_filtered, country)
data_filtered <- arrange(data_filtered, date)
data_interpolated <- mutate(data_filtered, across(all_of(indicator_cols), interpolate_linear))

# Beispiel: Visualisierung f<U+00FC>r einen spezifischen Indikator in einem Land
country_example <- "Armenia"
indicator_example <- "SE.SEC.CUAT.PO.ZS"

# Daten vor der Interpolation extrahieren
data_before <- filter(data_filtered, country == country_example)
data_before <- select(data_before, date, !!indicator_example)

# Daten nach der Interpolation extrahieren
data_after <- filter(data_interpolated, country == country_example)
data_after <- select(data_after, date, !!indicator_example)

title_before <- "Prozentsatz_Ausbildung_Armenien_25+_vor_Interpolation.png"

# Grafik vor der Interpolation erstellen
p_before <- ggplot(data_before, aes(x = date, y = !!sym(indicator_example))) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(
        title = paste("Prozentsatz mit postsekundären Ausbildung 25+", country_example, "- Vor Interpolation"),
        x = "Jahr",
        y = "Prozentsatz"
    )

# Grafik speichern
ggsave(title_before, plot = p_before)


title_after <- "Prozentsatz_Ausbildung_Armenien_25+_nach_Interpolation.png"

# Grafik nach der Interpolation erstellen
p_after <- ggplot(data_after, aes(x = date, y = !!sym(indicator_example))) +
    geom_line(color = "red") +
    geom_point(color = "red") +
    labs(
        title = paste("Prozentsatz mit postsekundären Ausbildung 25+", country_example, "- Nach Interpolation"),
        x = "Jahr",
        y = "Prozentsatz"
    )

# Grafik speichern und mit der vorherigen Grafik vergleichen
ggsave(title_after, plot = p_after)

# Daten bereinigen und Korrelationen berechnen
data_clean <- data %>%
    filter(!is.na(SP.DYN.LE00.IN), !is.na(NY.GDP.PCAP.CD), !is.na(SP.DYN.TFRT.IN)) %>%
    na.omit()

data_interpolated <- data_interpolated %>% na.omit()
selected_data <- data_interpolated[indicators]
correlations <- cor(selected_data)
print(correlations)

# Streudiagramm erstellen
p <- ggplot(data_clean, aes(x = SE.SEC.CUAT.PO.ZS, y = NY.GDP.PCAP.CD)) +
    geom_point() +
    stat_smooth(method = "lm", col = "blue") +
    labs(
        title = "Prozentsatz mit postsekundärer Bildung vs. BIP pro Kopf",
        x = "Prozentsatz mit postsekundärer Bildung",
        y = "BIP pro Kopf"
    )

# Grafik anzeigen
print(p)

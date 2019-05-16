sau2canonical <- function(country) {
    if (country == "China (Hong Kong)")
        return("Hong Kong")
    if (country == "Comoros Isl.")
      return("Comoros")
    if (country == "Côte d'Ivoire")
      return("Cote d'Ivoire")
    if (country == "Denmark (Greenland)")
      return("Greenland")
    if (country == "India (mainland)")
      return("India")
    if (country == "Viet Nam")
        return("Vietnam")
    if (country == "Andaman &amp; Nicobar Isl. (India)")
        return("Andaman & Nicobar")
    if (country == "Ascension Isl. (UK)")
        return("Ascension")
    if (country == "Azores Isl. (Portugal)")
        return("Azores")
    if (country == "Brunei Darussalam")
        return("Brunei")
    if (country == "Congo (ex-Zaire)")
        return("Democratic Republic of the Congo")
    if (country == "Congo, R. of")
        return("Republique du Congo")
    if (country == "Réunion (France)")
        return("Reunion")
    
    country <- strsplit(as.character(country), " \\(")[[1]][1]
    
    return(gsub("&amp;", "&", country))
}

border2canonical <- function(country) {
    if (country == "Amsterdam Island and Saint Paul Island")
        return("Amsterdam & St Paul Isl.")
    if (country == "Comoro Islands")
        return("Comoros")
    if (country == "R\xe9publique du Congo")
        return("Republique du Congo")
    if (country == "R\xe9union")
        return("Reunion")
    if (country == "Ile Tromelin")
        return("Tromelin Isl.")

    country <- gsub(" and ", " & ", country)
    country <- gsub("Islands", "Isl.", country)
    country <- gsub("Island", "Isl.", country)
    return(country)
}

# Identify any missing and duplicated names
for (sau in countryinfo$name) {
  canonical.sao <- sau2canonical(sau)

  found <- c()
  for (border in unique(attributes(one)$PolyData$Country)) {
    canonical.border <- border2canonical(border)
    if (canonical.sao == canonical.border) {
      found <- c(found, border)
    }
  }

  if (length(found) == 0) {
    print(paste("No matches for", sau))
  } else if (length(found) > 1) {
    print(paste("Multiple matches for ", sau, ": ", paste(found, collapse=", "), sep=""))
  }
}

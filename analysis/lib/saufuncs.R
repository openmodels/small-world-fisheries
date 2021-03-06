## Construct the value (in 2005 USD)/catch (in metric tonnes) stream coming out of a collection of EEZs
## Result has rows by species and columns by year
combine.regions <- function(values, regionids) {
    results <- data.frame(V1=c())
    for (regionid in regionids) {
        start <- which(values$V1 == regionid)[1]
        numvs <- which(values$V1[start:nrow(values)] == "Total")[1]
        myvalues <- values[start+(1:(numvs-2)),]

        for (ii in 1:nrow(myvalues)) {
            if (myvalues$V1[ii] %in% results$V1)
                results[results$V1 == myvalues$V1[ii], -1] <- results[results$V1 == myvalues$V1[ii], -1] + myvalues[ii, -1]
            else
                results <- rbind(results, myvalues[ii,])
        }
    }

    results
}

## Return list of SAU names
eez2sau <- function(eez) {
  if (eez == "Alaska")
      return(c("Alaska (USA)", "USA (Alaska, Arctic)", "USA (Alaska, Subarctic)"))
  if (eez == "Amsterdam Island and Saint Paul Island")
    return("St Paul & Amsterdam Isl. (France)")
  if (eez == "Andaman and Nicobar")
    return(c("Andaman & Nicobar Isl. (India)", "Andaman &amp; Nicobar Isl. (India)"))
  if (eez == "Anguilla")
    return("Anguilla (UK)")
  if (eez == "Antigua and Barbuda")
    return(c("Antigua & Barbuda", "Antigua &amp; Barbuda"))
  if (eez == "Aruba")
    return(c("Aruba (Leeward Netherlands Antilles)", "Aruba (Netherlands)"))
  if (eez == "Ascension")
    return("Ascension Isl. (UK)")
  if (eez == "Australia")
    return(c("Australia", "Lord Howe Isl. (Australia)"))
  if (eez == "Azores")
    return("Azores Isl. (Portugal)")
  if (eez == "Bermuda")
    return("Bermuda (UK)")
  if (eez == "Bonaire")
    return("Bonaire (Netherlands)")
  if (eez == "Bosnia and Herzegovina")
    return("Bosnia & Herzegovina")
  if (eez == "Bouvet Island")
    return("Bouvet Isl. (Norway)")
  if (eez == "Brazil")
    return(c("Brazil (mainland)", "St Paul and St. Peter Archipelago (Brazil)", "Fernando de Noronha (Brazil)"))
  if (eez == "British Virgin Islands")
    return("British Virgin Isl. (UK)")
  if (eez == "Brunei")
    return("Brunei Darussalam")
  if (eez == "Canada")
    return(c("Canada (Arctic)", "Canada (East Coast)", "Canada (Pacific)"))
  if (eez == "Canary Islands")
    return("Canary Isl. (Spain)")
  if (eez == "Cayman Islands")
    return("Cayman Isl. (UK)")
  if (eez == "Chile")
    return(c("Chile (mainland)", "Juan Fernandez Islands (Chile)", "Desventuradas Isl. (Chile)"))
  if (eez == "China")
    return(c("China", "Hong Kong (China)"))
  if (eez == "Christmas Island")
    return("Christmas Isl. (Australia)")
  if (eez == "Clipperton Island")
    return(c("Clipperton Isl. (France)", "Clipperton Isl.  (France)"))
  if (eez == "Cocos Islands")
    return("Cocos (Keeling) Isl. (Australia)")
  if (eez == "Colombia")
    return(c("Colombia (Caribbean)", "Colombia (Pacific)"))
  if (eez == "Comoro Islands")
    return(c("Comoros", "Comoros Isl."))
  if (eez == "Costa Rica")
    return(c("Costa Rica (Caribbean)", "Costa Rica (Pacific)"))
  if (eez == "Crozet Islands")
    return("Crozet Isl. (France)")
  if (eez == "Cura\xe7ao")
    return("Curacao (Netherlands)")
  if (eez == "Cyprus")
    return(c("Cyprus (North)", "Cyprus (South)"))
  if (eez == "Denmark")
    return(c("Denmark (Baltic Sea)", "Denmark (North Sea)"))
  if (eez == "East Timor")
    return("Timor Leste")
  if (eez == "Easter Island")
    return("Easter Isl. (Chile)")
  if (eez == "Ecuador")
    return("Ecuador (mainland)")
  if (eez == "Egypt")
    return(c("Egypt (Mediterranean)", "Egypt (Red Sea)"))
  if (eez == "Faeroe Islands")
    return("Faeroe Isl. (Denmark)")
  if (eez == "Falkland Islands")
    return("Falkland Isl. (UK)")
  if (eez == "France")
    return(c("France (Atlantic Coast)", "France (Mediterranean)", "Corsica (France)", "St Barthelemy (France)"))
  if (eez == "French Guiana")
    return(c("French Guiana (France)", "French Guiana"))
  if (eez == "Galapagos Islands")
    return("Galapagos Isl. (Ecuador)")
  if (eez == "Germany")
    return(c("Germany (Baltic Sea)", "Germany (North Sea)"))
  if (eez == "Greece")
    return(c("Greece (without Crete)", "Crete (Greece)"))
  ## if (eez == "Greenland")
  ##   return("Greenland (Denmark)")
  if (eez == "Glorioso Islands")
    return("Glorieuse Islands (France)")
  if (eez == "Guadeloupe")
    return(c("Guadeloupe (France)", "Guadeloupe  (France)"))
  if (eez == "Guatemala")
    return(c("Guatemala (Caribbean)", "Guatemala (Pacific)"))
  if (eez == "Guinea Bissau")
    return("Guinea-Bissau")
  if (eez == "Hawaii")
    return(c("Hawaii Main Islands (USA)", "Hawaii Northwest Islands (USA)"))
  if (eez == "Heard and McDonald Islands")
    return("Heard & McDonald Isl. (Australia)")
  if (eez == "Honduras")
    return(c("Honduras (Caribbean)", "Honduras (Pacific)"))
  if (eez == "Howland Island and Baker Island")
    return(c("Howland & Baker Isl. (USA)", "Howland &amp; Baker Isl. (USA)"))
  if (eez == "Ile Tromelin")
    return("Tromelin Isl. (France)")
  if (eez == "India")
    return("India")
  if (eez == "Indonesia")
    return(c("Indonesia (Eastern)", "Indonesia (Western)", "Indonesia (Central)", "Indonesia (Indian Ocean)"))
  if (eez == "Iran")
    return(c("Iran (Persian Gulf)", "Iran (Sea of Oman)"))
  if (eez == "Israel")
    return(c("Israel (Mediterranean)", "Israel (Red Sea)", "Gaza Strip"))
  if (eez == "Italy")
    return(c("Italy (mainland)", "Sardinia (Italy)", "Sicily (Italy)"))
  if (eez == "Ivory Coast")
    return("Côte d'Ivoire")
  if (eez == "Japan")
    return(c("Japan (main islands)", "Japan (outer islands)", "Japan (Daito Islands)", "Japan (Ogasawara Islands)"))
  if (eez == "Jarvis Island")
    return("Jarvis Isl. (USA)")
  if (eez == "Johnston Atoll")
    return("Johnston Atoll (USA)")
  if (eez == "Jan Mayen")
    return("Jan Mayen Isl. (Norway)")
  if (eez == "Kerguelen Islands")
    return("Kerguelen Isl. (France)")
  if (eez == "Kiribati")
    return(c("Kiribati (Gilbert Islands)"))
  if (eez == "Macquarie Island")
    return("Macquarie Isl. (Australia)")
  if (eez == "Madeira")
    return("Madeira Isl. (Portugal)")
  if (eez == "Malaysia")
    return(c("Malaysia (Peninsula East)", "Malaysia (Peninsula West)", "Malaysia (Sabah)", "Malaysia (Sarawak)"))
  if (eez == "Marshall Islands")
    return("Marshall Isl.")
  if (eez == "Martinique")
    return("Martinique (France)")
  if (eez == "Mayotte")
    return("Mayotte (France)")
  if (eez == "Mexico")
    return(c("Mexico (Atlantic)", "Mexico (Pacific)"))
  if (eez == "Micronesia")
    return("Micronesia (Federated States of)")
  if (eez == "Montserrat")
    return("Montserrat (UK)")
  if (eez == "Morocco")
    return(c("Morocco (Central)", "Morocco (Mediterranean)", "Morocco (South)"))
  if (eez == "New Caledonia")
    return("New Caledonia (France)")
  if (eez == "New Zealand")
    return(c("New Zealand", "Kermadec Isl. (New Zealand)"))
  if (eez == "Nicaragua")
    return(c("Nicaragua (Caribbean)", "Nicaragua (Pacific)"))
  if (eez == "Niue")
    return("Niue (New Zealand)")
  if (eez == "Norfolk Island")
    return("Norfolk Isl. (Australia)")
  if (eez == "Northern Mariana Islands and Guam")
    return(c("Northern Marianas (USA)", "Guam (USA)"))
  if (eez == "North Korea")
    return("Korea (North)")
  if (eez == "Oman")
    return(c("Oman", "Oman (Musandam)"))
  if (eez == "Palmyra Atoll")
    return("Palmyra Atoll & Kingman Reef (USA)")
  if (eez == "Panama")
    return(c("Panama (Caribbean)", "Panama (Pacific)"))
  if (eez == "Pitcairn")
    return("Pitcairn (UK)")
  if (eez == "Portugal")
    return("Portugal (mainland)")
  if (eez == "Prince Edward Islands")
    return("Prince Edward Isl. (South Africa)")
  if (eez == "Puerto Rico")
    return("Puerto Rico (USA)")
  if (eez == "R\xe9publique du Congo")
      return("Congo, R. of")
    if (eez == "Democratic Republic of the Congo")
        return("Congo (ex-Zaire)")
  if (eez == "R\xe9union")
    return(c("Réunion (France)", "Réunion (France)"))
  if (eez == "R_union")
    return(c("Réunion (France)", "Réunion (France)"))
  if (eez == "Russia")
    return(c("Russia (Baltic Sea, Kaliningrad)", "Russia (Baltic Sea, St. Petersburg)", "Russia (Barents Sea)", "Russia (Black Sea)", "Russia (Pacific)", "Russia (Siberia)", "Russia (Baltic Sea)", "Russia (Far East)", "Russia (Kara Sea)", "Russia (Laptev to Chukchi Sea)"))
  if (eez %in% c("Saba", "Sint-Eustasius"))
    return(c("Saba (Windward Netherlands Antilles)", "Saba and Sint Eustaius (Netherlands)"))
  if (eez == "Saint Helena")
    return("Saint Helena (UK)")
  if (eez == "Saint Kitts and Nevis")
    return(c("Saint Kitts & Nevis", "Saint Kitts &amp; Nevis"))
  if (eez =="Northern Saint-Martin")
    return("St Martin (France)")
  if (eez == "Saint Pierre and Miquelon")
    return(c("Saint Pierre & Miquelon (France)", "Saint Pierre &amp; Miquelon (France)"))
  if (eez == "Saint Vincent and the Grenadines")
    return(c("Saint Vincent & the Grenadines", "Saint Vincent &amp; the Grenadines"))
  if (eez == "Sao Tome and Principe")
    return("Sao Tome & Principe")
  if (eez == "Saudi Arabia")
    return(c("Saudi Arabia (Persian Gulf)", "Saudi Arabia (Red Sea)"))
  if (eez == "Sint-Maarten")
    return("Sint Maarten (Netherlands)")
  if (eez == "Solomon Islands")
    return("Solomon Isl.")
  if (eez == "South Africa")
    return(c("South Africa (Atlantic and Cape)", "South Africa (Indian Ocean Coast)"))
  if (eez == "South Georgia and the South Sandwich Islands")
    return("South Georgia & Sandwich Isl. (UK)")
  if (eez == "South Korea")
    return("Korea (South)")
  if (eez == "Spain")
    return(c("Spain (mainland, Med and Gulf of Cadiz)", "Spain (Northwest)", "Balearic Island (Spain)"))
  if (eez == "Svalbard")
    return("Svalbard Isl. (Norway)")
  if (eez == "Sweden")
    return(c("Sweden (Baltic)", "Sweden (West Coast)"))
  if (eez == "Thailand")
    return(c("Thailand (Andaman Sea)", "Thailand (Gulf of Thailand)"))
  if (eez == "Tokelau")
    return("Tokelau (New Zealand)")
  if (eez == "Trindade")
    return("Trindade & Martim Vaz Isl. (Brazil)")
  if (eez == "Trinidad and Tobago")
    return(c("Trinidad & Tobago", "Trinidad &amp; Tobago"))
  if (eez == "Tristan da Cunha")
    return("Tristan da Cunha Isl. (UK)")
  if (eez == "Turkey")
    return(c("Turkey (Black Sea)", "Turkey (Mediterranean Sea)", "Turkey (Marmara Sea)"))
  if (eez == "Turks and Caicos Islands")
    return(c("Turks & Caicos Isl. (UK)", "Turks &amp; Caicos Isl. (UK)"))
  if (eez == "United Arab Emirates")
    return(c("United Arab Emirates (Fujairah)", "United Arab Emirates"))
  if (eez == "United Kingdom")
    return(c("United Kingdom (UK)", "South Orkney Islands (UK)", "Channel Isl. (UK)"))
  if (eez == "United States")
    return(c("USA (East Coast)", "USA (West Coast)", "USA (Gulf of Mexico)"))
  if (eez == "Virgin Islands of the United States")
    return(c("US Virgin Isl. (USA)", "US Virgin Isl."))
  ## if (eez == "Vietnam")
  ##   return("Viet Nam")
  if (eez == "Wake Island")
    return("Wake Isl. (USA)")
  if (eez == "Wallis and Futuna")
    return(c("Wallis & Futuna Isl. (France)", "Wallis &amp; Futuna Isl. (France)"))
  if (eez == "Western Sahara")
    return("Western Sahara (Morocco)")
  if (eez == "Yemen")
    return(c("Yemen (Arabian Sea)", "Yemen (Red Sea)"))
    if (eez == "Line Group")
        return(c("Kiribati (Line Islands)"))
    if (eez == "Phoenix Group")
        return(c("Kiribati (Phoenix Islands)"))
    if (eez == "Bassas da India")
        return(c("Mozambique Channel Isl. (France)")) # Also includes Ile Europa, Juan de Nova Island
    if (eez == "British Indian Ocean Territory")
        return(c("Chagos Archipelago (UK)"))

    as.character(eez)
}


## Compute the average landed value and MT catch for the last 10 years for the given species
## Note:
## groups <- read.csv("../groups.csv")
## myvalues <- combine.regions(values, regionids)
## mycatches <- combine.regions(catches, regionids)
species.averages <- function(specie, mygroups, myvalues, mycatches) {
    specgroups <- mygroups[mygroups$scientific == as.character(specie),][1,]
    if (nrow(specgroups) == 0){
        return(c(0,0))
    }

    common_name <- as.character(specgroups$common_name)
    commercial_group <- as.character(specgroups$commercial_group)

    if (common_name %in% as.character(myvalues$V1)){
        value <- mean(as.numeric(myvalues[myvalues$V1 == specgroups$common_name, (ncol(values)-9):ncol(values)]), na.rm=T)
    } else if (commercial_group %in% myvalues$V1){
        value <- mean(as.numeric(myvalues[myvalues$V1 == specgroups$commercial_group, (ncol(values)-9):ncol(values)]), na.rm=T)
    } else {
        value <- 0 # This falls into an un-tracked mixed group
    }

    if (common_name %in% as.character(mycatches$V1)){
        catch <- mean(as.numeric(mycatches[as.character(mycatches$V1) == specgroups$common_name, (ncol(catches)-9):ncol(catches)]), na.rm=T)
    } else if (commercial_group %in% as.character(mycatches$V1)) {
        catch <- mean(as.numeric(mycatches[as.character(mycatches$V1) == specgroups$commercial_group, (ncol(catches)-9):ncol(catches)]), na.rm=T)
    } else {
        catch <- 0 # This falls into an un-tracked mixed group
    }
    return(c(value, catch))
}

## Version of species.averages that uses saudata-new, and uses exact SAU keys
species.averages.keyed <- function(specie, mygroups, myvalues, mycatches, fromyear=-9, toyear=0) {
    specgroups <- mygroups[mygroups$scientific == as.character(specie),]
    if (nrow(specgroups) == 0){
        return(c(0,0))
    }
    specgroups <- specgroups[1,]

    key <- as.character(specgroups$key)

    value <- mean(as.numeric(myvalues[myvalues$V1 == key, (ncol(values)+fromyear):(ncol(values)+toyear)]), na.rm=T)
    catch <- mean(as.numeric(mycatches[mycatches$V1 == key, (ncol(catches)+fromyear):(ncol(catches)+toyear)]), na.rm=T)

    return(c(value, catch))
}

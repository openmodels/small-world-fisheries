canonical2worldbank <- function(country) {
    if (country == "Bahamas")
        return("Bahamas, The")
    if (country %in% c("Bosnia & Herzegovina", "Bosnia Herzegovina"))
        return("Bosnia and Herzegovina")
    if (country %in% c("Egypt", "Egypt (Mediterranean)", "Egypt (Red Sea)"))
        return("Egypt, Arab Rep.")
    if (country %in% c("Iran", "Iran (Persian Gulf)", "Iran (Sea of Oman)"))
        return("Iran, Islamic Rep.")
    if (country == "Saint Lucia")
        return("St. Lucia")
    if (country == "Venezuela")
        return("Venezuela, RB")
    if (country == "Viet Nam")
        return("Vietnam")
    if (country == "Fmr Sudan")
        return("Sudan")
    if (country == "United Rep. of Tanzania")
        return("Tanzania")
    if (country %in% c("Yemen", "Yemen (Arabian Sea)", "Yemen (Red Sea)"))
        return("Yemen, Rep.")
    if (country == "Cape Verde")
      return("Cabo Verde")
    if (country %in% c("Congo, R. of", "Congo (Rep.)"))
        return("Congo, Rep.")
    if (country %in% c("Congo (ex-Zaire)", "Congo (DemRep)", "Congo"))
        return("Congo, Dem. Rep.")
    if (country == "Gambia")
        return("Gambia, The")
    if (country %in% c("Hong Kong", "China, Hong Kong SAR"))
      return("Hong Kong SAR, China")
    if (country == "Marshall Isl.")
      return("Marshall Islands")
    if (country %in% c("Micronesia", "Micronesia (Federated States of)", "FS Micronesia"))
        return("Micronesia, Fed. Sts.")
    if (country %in% c("Saint Kitts & Nevis", "Saint Kitts and Nevis"))
        return("St. Kitts and Nevis")
    if (country %in% c("Saint Vincent & the Grenadines", "St Vincent", "Saint Vincent and the Grenadines"))
        return("St. Vincent and the Grenadines")
    if (country %in% c("Sao Tome & Principe", "Sao Tome Prn"))
        return("Sao Tome and Principe")
    if (country %in% c("Solomon Isl.", "Solomon Isds"))
      return("Solomon Islands")
    if (country == "Syria")
        return("Syrian Arab Republic")
    if (country == "Dominican Rep.")
        return("Dominican Republic")
    if (country %in% c("Timor Leste", "East Timor"))
        return("Timor-Leste")
    if (country == "Trinidad & Tobago")
        return("Trinidad and Tobago")
    if (country == "Antigua & Barbuda")
        return("Antigua and Barbuda")
    if (country %in% c("Andaman &amp; Nicobar Isl. (India)", "Andaman & Nicobar Isl. (India)", "Andaman and Nicobar"))
      return("India")
    if (country %in% c("Australia (Christmas Isl.)", "Australia (Cocos (Keeling) Isl.)", "Australia (Heard &amp; McDonald Isl.)",
                       "Australia (Lord Howe Isl.)", "Australia (Macquarie Isl.)", "Christmas Isl. (Australia)",
                       "Australia (Cocos (Keeling) Isl.)", "Heard & McDonald Isl. (Australia)",
                       "Lord Howe Isl. (Australia)", "Macquarie Isl. (Australia)", "Norfolk Isl. (Australia)", "Cocos Islands", "Christmas Island",
                       "Cocos (Keeling) Isl. (Australia)"))
        return("Australia")
    if (country %in% c("Trindade & Martin Vaz Isl. (Brazil)", "Brazil (mainland)", "Fernando de Noronha (Brazil)", "St Paul and St. Peter Archipelago (Brazil)", "Trindade & Martim Vaz Isl. (Brazil)"))
        return("Brazil")
    if (country %in% c("Chile (Easter Isl.)", "Chile (J. Fernandez, Felix and Ambrosio Isl.)", "Desventuradas Isl. (Chile)",
                       "Easter Isl. (Chile)", "J. Fernandez, Felix and Ambrosio Isl. (Chile)",
                       "Chile (mainland)", "Juan Fernandez Islands (Chile)"))
        return("Chile")
    if (country == "Bolivia (Plurinational State of)")
        return("Bolivia")
    if (country %in% c("Taiwan"))
      return("China")
    if (country %in% c("Greenland", "Faeroe Isl. (Denmark)", "Denmark (Baltic Sea)", "Denmark (North Sea)"))
      return("Denmark")
    if (country %in% c("Ecuador (Galapagos Isl.)", "Galapagos Isl. (Ecuador)", "Ecuador (mainland)"))
      return("Ecuador")
    if (country %in% c("France (French Guiana)", "France (French Polynesia)", "France (Guadeloupe)",
                       "Guadeloupe", "France (Martinique)", "Martinique",
                       "France (Mayotte)", "France (New Caledonia)", "France (Reunion)",
                       "Amsterdam & St Paul Isl. (France)", "New Caledonia",
                       "Clipperton Isl. (France)", "Crozet Isl. (France)", "French Guiana", "Guadeloupe (France)",
                       "Kerguelen Isl. (France)", "Martinique (France)", "Mayotte (France)", "Mozambique Channel Isl. (France)",
                       "New Caledonia (France)", "Reunion (France)", "Saint Pierre & Miquelon (France)", "Tromelin Isl. (France)",
                       "Wallis & Futuna Isl. (France)", "Wallis and Futuna", "Corsica (France)",
                       "France (Atlantic Coast)", "France (Mediterranean)", "St Barthelemy (France)",
                       "Clipperton Isl.  (France)", "Glorieuse Islands (France)", "Guadeloupe  (France)",
                       "Réunion (France)", "St Martin (France)", "St Paul & Amsterdam Isl. (France)"))
      return("France")
    if (country %in% c("Haiti (Navassa Isl.)", "Navassa Isl. (Haiti)"))
        return("Haiti")
    if (country %in% c("Indonesia (Eastern)", "Indonesia (Western)", "Indonesia (Central)", "Indonesia (Indian Ocean)"))
        return("Indonesia")
    if (country %in% c("Japan (main islands)", "Japan (outer islands)", "Japan (Daito Islands)", "Japan (Ogasawara Islands)"))
        return("Japan")
    if (country %in% c("Korea (North)", "North Korea", "Korea, Dem. Rep."))
        return("Korea, Dem. Rep.")
    if (country %in% c("Korea (South)", "South Korea", "Korea South", "Rep. of Korea", "Korea, Rep."))
      return("Korea, Rep.")
    if (country %in% c("Malaysia (Peninsula East)", "Malaysia (Peninsula West)", "Malaysia (Sabah)", "Malaysia (Sarawak)"))
        return("Malaysia")
    if (country %in% c("Western Sahara (Morocco)", "Western Sahara", "Morocco (Central)", "Morocco (Mediterranean)", "Morocco (South)"))
        return("Morocco")
    if (country %in% c("Leeward Netherlands Antilles", "Windward Netherlands Antilles", "Aruba (Netherlands)", "Bonaire (Netherlands)", "Curacao (Netherlands)", "Saba and Sint Eustaius (Netherlands)", "Sint Maarten (Netherlands)"))
        return("Netherlands")
    if (country %in% c("Cook Isl. (New Zealand)", "Kermadec Isl. (New Zealand)", "New Zealand (Niue)", "New Zealand (Cook Isl.)",
                       "New Zealand (Tokelau)", "Niue (New Zealand)", "Tokelau (New Zealand)", "Cook Islands"))
      return("New Zealand")
    if (country %in% c("Bouvet Isl. (Norway)", "Norway (Svalbard Isl.)", "Jan Mayen Isl. (Norway)", "Svalbard Isl. (Norway)"))
      return("Norway")
    if (country %in% c("Azores Isl. (Portugal)", "Madeira Isl. (Portugal)", "Madeira",
                       "Portugal (mainland)"))
      return("Portugal")
    if (country %in% c("Russia (Baltic Sea, St. Petersburg)", "Russia (Barents Sea)", "Russia (Pacific)",
                       "Russia (Baltic Sea, Kaliningrad)", "Russia (Black Sea)", "Russia (Siberia)", "Russian Fed", "Russia", "Russia (Baltic Sea)", "Russia (Far East)", "Russia (Kara Sea)", "Russia (Laptev to Chukchi Sea)"))
      return("Russian Federation")
    if (country %in% c("Saudi Arabia (Red Sea)", "Saudi Arabia (Persian Gulf)"))
        return("Saudi Arabia")
    if (country %in% c("Prince Edward Isl. (South Africa)", "South Africa (Atlantic and Cape)", "South Africa (Indian Ocean Coast)"))
        return("South Africa")
    if (country %in% c("Canary Isl. (Spain)", "Balearic Island (Spain)", "Spain (mainland, Med and Gulf of Cadiz)", "Spain (Northwest)"))
      return("Spain")
    if (country %in% c("Turkey (Mediterranean Sea)", "Turkey (Black Sea)", "Turkey (Marmara Sea)"))
      return("Turkey")
    if (country %in% c("Anguilla (UK)", "Anguilla", "Bermuda (UK)", "Bermuda",
                       "British Virgin Isl. (UK)",
                       "British Virgin Islands", "Cayman Isl. (UK)", "Cayman Islands",
                       "Chagos Archipel., Brit. Ind. Oc. Terr. (UK)", "Falkland Isl. (UK)",
                       "Montserrat (UK)", "Montserrat",
                       "Saint Helena (UK)", "Tristan da Cunha Isl. (UK)", "Ascension Isl. (UK)", "Bermuda (UK)",
                       "British Virgin Isl. (UK)", "Cayman Isl. (UK)", "Chagos Archipel., Brit. Ind. Oc. Terr. (UK)",
                       "Channel Isl. (UK)", "Falkland Isl. (UK)", "Pitcairn (UK)", "South Georgia & Sandwich Isl. (UK)",
                       "Turks & Caicos Isl. (UK)", "United Kingdom (UK)", "South Orkney Islands (UK)",
                       "Chagos Archipelago (UK)"))
      return("United Kingdom")
    if (country %in% c("Hawaii", "Alaska", "Alaska (USA)", "Guam (USA)", "Jarvis Isl. (USA)", "Johnston Atoll (USA)", "Northern Marianas (USA)",
                       "Puerto Rico", "Puerto Rico (USA)", "US Virgin Isl.", "Guam (USA)", "Hawaii Main Islands (USA)",
                       "Hawaii Northwest Islands (USA)", "Howland & Baker Isl. (USA)", "Jarvis Isl. (USA)",
                       "Johnston Atoll (USA)", "Palmyra Atoll & Kingman Reef (USA)", "United States, East Coast",
                       "United States, Gulf of Mexico", "United States, West Coast", "Wake Isl. (USA)", "USA",
                       "USA (East Coast)", "USA (Gulf of Mexico)", "USA (West Coast)", "USA (Alaska, Arctic)",
                       "USA (Alaska, Subarctic)"))
        return("United States")
    ## if (country %in% c("Gaza Strip"))
    ##     return("West Bank and Gaza")
    if (country %in% c("Canada (Arctic)", "Canada (East Coast)", "Canada (Pacific)"))
        return("Canada")
    if (country %in% c("Mexico (Atlantic)", "Mexico (Pacific)"))
        return("Mexico")
    if (country %in% c("Oman (Musandam)"))
        return("Oman")
    if (country %in% c("United Arab Emirates (Fujairah)"))
        return("United Arab Emirates")
    if (country %in% c("Thailand (Andaman Sea)", "Thailand (Gulf of Thailand)"))
        return("Thailand")
    if (country %in% c("Honduras (Caribbean)", "Honduras (Pacific)"))
        return("Honduras")
    if (country %in% c("Guatemala (Caribbean)", "Guatemala (Pacific)"))
        return("Guatemala")
    if (country %in% c("Colombia (Caribbean)", "Colombia (Pacific)"))
        return("Colombia")
    if (country %in% c("Nicaragua (Caribbean)", "Nicaragua (Pacific)"))
        return("Nicaragua")
    if (country %in% c("Panama (Caribbean)", "Panama (Pacific)"))
        return("Panama")
    if (country %in% c("Costa Rica (Caribbean)", "Costa Rica (Pacific)"))
        return("Costa Rica")
    if (country %in% c("Sweden (Baltic)", "Sweden (West Coast)"))
        return("Sweden")
    if (country %in% c("Italy (mainland)", "Sardinia (Italy)", "Sicily (Italy)"))
        return("Italy")
    if (country %in% c("Germany (Baltic Sea)", "Germany (North Sea)"))
        return("Germany")
    if (country %in% c("Greece (without Crete)", "Crete (Greece)"))
        return("Greece")
    if (country %in% c("Cyprus (North)", "Cyprus (South)"))
        return("Cyprus")
    if (country %in% c("West Bank and Gaza", "Israel (Mediterranean)", "Israel (Red Sea)"))
        return("Israel")
    if (country %in% c("Kiribati (Gilbert Islands)", "Kiribati (Line Islands)", "Kiribati (Phoenix Islands)"))
        return("Kiribati")

    if (length(grep("\\(", country)) == 1)
        stop(paste0("Cannot find sovereign for ", country))

    return(as.character(country))
}

canonical2continent <- function(country) {
  if (country %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo", "Congo, Democratic Republic of", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe", "Canary Isl. (Spain)", "Congo, R. of", "Cote d'Ivoire", "France (Mayotte)", "France (Reunion)", "France (Mozambique Channel Isl.)"))
    return("Africa")

  if (country %in% c("Afghanistan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Burma (Myanmar)", "Cambodia", "China", "East Timor", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Korea, North", "Korea, South", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Nepal", "Oman", "Pakistan", "Philippines", "Qatar", "Russian Federation", "Saudi Arabia", "Singapore", "Sri Lanka", "Syria", "Tajikistan", "Thailand", "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen", "Hong Kong", "Malaysia (Peninsula East)", "Korea (South)", "Turkey (Black Sea)", "Saudi Arabia (Persian Gulf)", "Myanmar", "Taiwan", "Russia", "Andaman & Nicobar Isl. (India)", "Korea (North)"))
    return("Asia")

  if (country %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom", "Vatican City", "Norway (Svalbard Isl.)", "Channel Isl. (UK)"))
    return("Europe")

  if (country %in% c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Canada", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "El Salvador", "Grenada", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago", "United States", "Alaska (USA)", "Greenland", "Haiti (Navassa Isl.)", "Puerto Rico (USA)", "France (Martinique)", "Turks & Caicos Isl. (UK)", "US Virgin Isl.", "France (Guadeloupe)", "Antigua & Barbuda", "British Virgin Isl. (UK)", "Anguilla (UK)", "Saint Vincent & the Grenadines", "Bermuda (UK)", "Cayman Isl. (UK)", "Montserrat (UK)"))
    return("North America")

  if (country %in% c("Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia", "Nauru", "New Zealand", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu", "American Samoa", "Australia (Heard & McDonald Isl.)", "Hawaii", "Australia (Macquarie Isl.)", "Kermadec Isl. (New Zealand)", "Australia (Lord Howe Isl.)", "Marshall Isl.", "France (New Caledonia)", "Solomon Isl.", "Palmyra Atoll & Kingman Reef (USA)", "Brunei Darussalam", "Guam (USA)", "Howland & Baker Isl. (USA)", "France (French Polynesia)", "Australia (Christmas Isl.)", "Johnston Atoll (USA)", "Northern Marianas (USA)", "Cook Isl. (New Zealand)", "Timor Leste", "Australia (Norfolk Isl.)", "France (Wallis & Futuna Isl.)"))
    return("Oceania")

  if (country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela", "Falkland Isl. (UK)", "France (French Guiana)", "Netherlands Antilles (Windward)", "Trinidad & Tobago", "Chile (Desventuradas Isl.)", "Brazil (Trindade & Martin Vaz Isl.)"))
    return("South America")

  if (country %in% c("Chagos Archipel., Brit. Ind. Oc. Terr. (UK)", "Azores Isl. (Portugal)", "Chile (J. Fernandez, Felix and Ambrosio Isl.)", "Ecuador (Galapagos Isl.)", "Amsterdam & St Paul Isl. (France)", "Madeira Isl. (Portugal)", "Jarvis Isl. (USA)", "Australia (Cocos (Keeling) Isl.)", "Tristan da Cunha Isl. (UK)", "Chile (Easter Isl.)", "South Georgia & Sandwich Isl. (UK)", "Denmark (Faeroe Isl.)", "Jan Mayen Isl. (Norway)", "France (Kerguelen Isl.)"))
    return("Open Ocean")

  return(NA)
}

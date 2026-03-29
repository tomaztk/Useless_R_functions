
##########################################
# 
# Irregular verbs for English and German
# no wacky things; actually useful function
#
# Series:
# Little Useless-useful R functions #96
# Created: Marhc 29, 2026
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com

# ToDo:
# get additional words
###########################################

# nepravilni glagoli s slovenskimi prevodi

en_verbs <- data.frame(
  v1 = c("be","beat","become","begin","bite","blow","break","bring","build","buy",
         "catch","choose","come","cost","cut","do","draw","drink","drive","eat",
         "fall","feel","fight","find","fly","forget","freeze","get","give","go",
         "grow","hang","have","hear","hide","hit","hold","hurt","keep","know",
         "lay","lead","leave","lend","let","lie","lose","make","mean","meet",
         "pay","put","read","ride","ring","rise","run","say","see","sell",
         "send","set","shake","shine","show","shut","sing","sink","sit","sleep",
         "speak","spend","stand","steal","stick","sting","strike","swim","swing",
         "take","teach","tear","tell","think","throw","understand","wake","wear","win","write"),
  v2 = c("was/were","beat","became","began","bit","blew","broke","brought","built","bought",
         "caught","chose","came","cost","cut","did","drew","drank","drove","ate",
         "fell","felt","fought","found","flew","forgot","froze","got","gave","went",
         "grew","hung","had","heard","hid","hit","held","hurt","kept","knew",
         "laid","led","left","lent","let","lay","lost","made","meant","met",
         "paid","put","read","rode","rang","rose","ran","said","saw","sold",
         "sent","set","shook","shone","showed","shut","sang","sank","sat","slept",
         "spoke","spent","stood","stole","stuck","stung","struck","swam","swung",
         "took","taught","tore","told","thought","threw","understood","woke","wore","won","wrote"),
  v3 = c("been","beaten","become","begun","bitten","blown","broken","brought","built","bought",
         "caught","chosen","come","cost","cut","done","drawn","drunk","driven","eaten",
         "fallen","felt","fought","found","flown","forgotten","frozen","gotten","given","gone",
         "grown","hung","had","heard","hidden","hit","held","hurt","kept","known",
         "laid","led","left","lent","let","lain","lost","made","meant","met",
         "paid","put","read","ridden","rung","risen","run","said","seen","sold",
         "sent","set","shaken","shone","shown","shut","sung","sunk","sat","slept",
         "spoken","spent","stood","stolen","stuck","stung","struck","swum","swung",
         "taken","taught","torn","told","thought","thrown","understood","woken","worn","won","written"),
  sl = c("biti","udariti/premagati","postati","zaceti","gristi","pihati","zlomiti","prinesti","zgraditi","kupiti",
         "ujeti","izbrati","priti","stati (o ceni)","rezati","narediti/delati","risati","piti","voziti","jesti",
         "pasti","cutiti","boriti se","najti","leteti","pozabiti","zmrzniti","dobiti/postati","dati","iti",
         "rasti","obesiti/viseti","imeti","slisati","skriti","udariti","drzati","poskodovati/boleti","obdrzati","vedeti/poznati",
         "poloziti","voditi","zapustiti/oditi","posoditi","pustiti/dovoliti","lezati","izgubiti","narediti","pomeniti","srecati",
         "placati","poloziti/dati","brati","jahati/voziti se","zvoniti","dvigniti se","teci","reci","videti","prodati",
         "poslati","nastaviti/postaviti","stresati","sijati","pokazati","zapreti","peti","potoniti","sedeti","spati",
         "govoriti","porabiti/preziveti","stati","ukrasti","prilepiti","piciti","udariti","plavati","gugati/nihati",
         "vzeti","poucevati","trgati","povedati","misliti","vreci/metati","razumeti","zbuditi se","nositi (oblacila)","zmagati","pisati"),
  stringsAsFactors = FALSE
)

de_verbs <- data.frame(
  v1 = c("backen","befehlen","beginnen","beißen","biegen","bieten","binden","bitten","blasen","bleiben",
         "braten","brechen","brennen","bringen","denken","dürfen","empfehlen","essen","fahren","fallen",
         "fangen","finden","fliegen","fließen","fressen","frieren","geben","gefallen","gehen","gelten",
         "genießen","geschehen","gewinnen","gießen","graben","greifen","haben","halten","hängen","heißen",
         "helfen","kennen","klingen","kommen","können","laden","lassen","laufen","leiden","leihen",
         "lesen","liegen","lügen","messen","mögen","müssen","nehmen","nennen","pfeifen","raten",
         "reißen","reiten","rennen","riechen","rufen","schaffen","schlafen","schlagen","schließen","schmelzen",
         "schneiden","schreiben","schreien","schweigen","schwimmen","sehen","sein","singen","sinken","sitzen",
         "sollen","sprechen","springen","stehen","stehlen","steigen","sterben","stoßen","streiten","tragen",
         "treffen","treiben","trinken","tun","vergessen","verlieren","wachsen","waschen","werfen","wissen","wollen","ziehen"),
  v2 = c("buk","befahl","begann","biss","bog","bot","band","bat","blies","blieb",
         "briet","brach","brannte","brachte","dachte","durfte","empfahl","aß","fuhr","fiel",
         "fing","fand","flog","floss","fraß","fror","gab","gefiel","ging","galt",
         "genoss","geschah","gewann","goss","grub","griff","hatte","hielt","hing","hieß",
         "half","kannte","klang","kam","konnte","lud","ließ","lief","litt","lieh",
         "las","lag","log","maß","mochte","musste","nahm","nannte","pfiff","riet",
         "riss","ritt","rannte","roch","rief","schuf","schlief","schlug","schloss","schmolz",
         "schnitt","schrieb","schrie","schwieg","schwamm","sah","war","sang","sank","saß",
         "sollte","sprach","sprang","stand","stahl","stieg","starb","stieß","stritt","trug",
         "traf","trieb","trank","tat","vergaß","verlor","wuchs","wusch","warf","wusste","wollte","zog"),
  v3 = c("gebacken","befohlen","begonnen","gebissen","gebogen","geboten","gebunden","gebeten","geblasen","geblieben",
         "gebraten","gebrochen","gebrannt","gebracht","gedacht","gedurft","empfohlen","gegessen","gefahren","gefallen",
         "gefangen","gefunden","geflogen","geflossen","gefressen","gefroren","gegeben","gefallen","gegangen","gegolten",
         "genossen","geschehen","gewonnen","gegossen","gegraben","gegriffen","gehabt","gehalten","gehangen","geheißen",
         "geholfen","gekannt","geklungen","gekommen","gekonnt","geladen","gelassen","gelaufen","gelitten","geliehen",
         "gelesen","gelegen","gelogen","gemessen","gemocht","gemusst","genommen","genannt","gepfiffen","geraten",
         "gerissen","geritten","gerannt","gerochen","gerufen","geschaffen","geschlafen","geschlagen","geschlossen","geschmolzen",
         "geschnitten","geschrieben","geschrien","geschwiegen","geschwommen","gesehen","gewesen","gesungen","gesunken","gesessen",
         "gesollt","gesprochen","gesprungen","gestanden","gestohlen","gestiegen","gestorben","gestoßen","gestritten","getragen",
         "getroffen","getrieben","getrunken","getan","vergessen","verloren","gewachsen","gewaschen","geworfen","gewusst","gewollt","gezogen"),
  sl = c("peci","ukazati","zaceti","gristi","upogibati","ponuditi","vezati","prositi","pihati","ostati",
         "peci/cvreti","zlomiti","goreti/zgati","prinesti","misliti","smeti","priporociti","jesti","voziti/potovati","pasti",
         "ujeti/loviti","najti","leteti","teci (voda)","jesti (zivali)","zmrzniti","dati","ugajati/dopasti","iti/hoditi","veljati",
         "uzivati","zgoditi se","zmagati/dobiti","naliti/zalivati","kopati","zgrabiti/prijeti","imeti","drzati/ustaviti","viseti","se imenovati",
         "pomagati","poznati","zveneti","priti","moci/znati","naloziti/povabiti","pustiti/dovoliti","teci/hoditi","trpeti","posoditi/si izposoditi",
         "brati","lezati","lagati","meriti","imeti rad","morati","vzeti","imenovati","zvizgati","svetovati/uganiti",
         "trgati/puliti","jahati","teci","vonjati/disati","klicati/kricati","ustvariti","spati","udariti/premagati","zapreti","taliti se",
         "rezati","pisati","kricati","molcati","plavati","videti","biti","peti","potoniti/upadati","sedeti",
         "morati/biti dolzan","govoriti","skakati","stati","ukrasti","vzpenjati se/narascati","umreti","potisniti/trciti","prepirati se","nositi/prenasati",
         "srecati/zadeti","goniti/ukvarjati se","piti","delati/poceti","pozabiti","izgubiti","rasti","prati/umiti","vreci/metati","vedeti","hoteti/zeleti","vleci/preseliti se"),
  stringsAsFactors = FALSE
)

# working on vocabulary; adding random words (en, de) with slovenian translation for practising

vocab <- data.frame(
  deu = c(
 
    "ja","nein","bitte","danke","hallo","tschüss","Entschuldigung","Hilfe","gut","schlecht",
    "groß","klein","neu","alt","schnell","langsam","schön","hässlich","teuer","billig",

    "ich","du","er","sie","wir","ihr","Mann","Frau","Kind","Freund",
    "Familie","Mutter","Vater","Bruder","Schwester","Oma","Opa","Baby","Arzt","Lehrer",
 
    "Haus","Wohnung","Zimmer","Küche","Badezimmer","Schlafzimmer","Tür","Fenster","Treppe","Garten",
    "Tisch","Stuhl","Bett","Sofa","Lampe","Schrank","Spiegel","Boden","Wand","Dach",

    "Essen","Trinken","Wasser","Brot","Käse","Fleisch","Fisch","Ei","Milch","Butter",
    "Apfel","Banane","Tomate","Kartoffel","Reis","Nudeln","Suppe","Kuchen","Kaffee","Tee",

    "rot","blau","grün","gelb","schwarz","weiß","grau","braun","orange","rosa",
 
    "eins","zwei","drei","vier","fünf","sechs","sieben","acht","neun","zehn",
    "zwanzig","dreißig","hundert","tausend",

    "heute","morgen","gestern","jetzt","später","früh","spät","Minute","Stunde","Tag",
    "Woche","Monat","Jahr","Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag",

    "Sonne","Mond","Stern","Himmel","Wolke","Regen","Schnee","Wind","Baum","Blume",
    "Berg","See","Meer","Fluss","Wald","Tier","Hund","Katze","Vogel","Fisch",
 
    "Auto","Zug","Bus","Flugzeug","Schiff","Straße","Bahnhof","Flughafen","Hotel","Karte",
    "links","rechts","geradeaus","weit","nah","oben","unten","hier","dort","wohin",

    "Kopf","Haar","Auge","Ohr","Nase","Mund","Zahn","Hand","Finger","Arm",
    "Bein","Fuß","Herz","Rücken","Bauch","Schulter","Hals","Knie","Haut","Blut",
 
    "Arbeit","Büro","Schule","Universität","Buch","Stift","Computer","Telefon","Geld","Preis",
    "kaufen","verkaufen","schreiben","lesen","sprechen","lernen","arbeiten","wohnen","kommen","gehen"
  ),

  slo = c(
    "ja","ne","prosim","hvala","zdravo","adijo","oprostite","pomoč","dobro","slabo",
    "velik","majhen","nov","star","hiter","počasen","lep","grd","drag","poceni",
    "jaz","ti","on","ona","mi","vi","mož","žena","otrok","prijatelj",
    "družina","mama","oče","brat","sestra","babica","dedek","dojenček","zdravnik","učitelj",
    "hiša","stanovanje","soba","kuhinja","kopalnica","spalnica","vrata","okno","stopnice","vrt",
    "miza","stol","postelja","kavč","svetilka","omara","ogledalo","tla","stena","streha",
    "hrana","pijaca","voda","kruh","sir","meso","riba","jajce","mleko","maslo",
    "jabolko","banana","paradižnik","krompir","riž","testenine","juha","torta","kava","čaj",
    "rdeča","modra","zelena","rumena","črna","bela","siva","rjava","oranžna","rozna",
    "ena","dve","tri","štiri","pet","šest","sedem","osem","devet","deset",
    "dvajset","trideset","sto","tisoč",
    "danes","jutri","včeraj","zdaj","kasneje","zgodaj","pozno","minuta","ura","dan",
    "teden","mesec","leto","ponedeljek","torek","sreda","četrtek","petek","sobota","nedelja",
    "sonce","luna","zvezda","nebo","oblak","dež","sneg","veter","drevo","cvet",
    "gora","jezero","morje","reka","gozd","žival","pes","maček","ptica","riba",
    "avto","vlak","avtobus","letalo","ladja","cesta","zelezniška postaja","letališče","hotel","zemljevid",
    "levo","desno","naravnost","daleč","blizu","zgoraj","spodaj","tukaj","tam","kam",
    "glava","lasje","oko","uho","nos","usta","zob","roka","prst","roka",
    "noga","stopalo","srce","hrbet","trebuh","rama","vrat","koleno","koza","kri",
    "delo","pisarna","sola","univerza","knjiga","svinčnik","računalnik","telefon","denar","cena",
    "kupiti","prodati","pisati","brati","govoriti","učiti","delati","bivati","priti","iti"
  ),
  stringsAsFactors = FALSE
)

# helper funciton

normalise <- function(s) tolower(trimws(s))

check_answer <- function(user, correct) {
  alts <- trimws(unlist(strsplit(correct, "/")))
  normalise(user) %in% normalise(alts)
}

pick_n <- function(df, n = 10) df[sample(nrow(df), min(n, nrow(df))), ]

cat_line <- function(...) cat(..., "\n", sep = "")

choose_option <- function(prompt, options) {
  cat_line(prompt)
  for (i in seq_along(options)) cat_line("  ", i, ") ", options[i])
  repeat {
    ans <- suppressWarnings(as.integer(readline("  Tvoja izbira: ")))
    if (!is.na(ans) && ans >= 1 && ans <= length(options)) return(ans)
    cat_line("  Vnesi številko med 1 in ", length(options), ".")
  }
}

# glagoli

q_verbs <- function() {
  lang_choice <- choose_option("Izberi jezik:", c("Angleščina (English)", "Nemščina (Deutsch)"))
  lang <- if (lang_choice == 1) "en" else "de"
  df   <- if (lang == "en") en_verbs else de_verbs
  
  if (lang == "en") {
    labels <- c("Nedolocnik", "Preteklik", "Deleznik preteklosti")
  } else {
    labels <- c("Nedolocnik", "Präteritum (jaz)", "Partizip II")
  }
  
  modes <- list(c(1,2),c(1,3),c(2,1),c(2,3),c(3,1),c(3,2))
  mode_labels <- sapply(modes, function(m) paste0(labels[m[1]], "  ->  ", labels[m[2]]))
  mode_choice <- choose_option("\nDano  ->  Vprasaj za:", mode_labels)
  given_col <- modes[[mode_choice]][1]
  ask_col   <- modes[[mode_choice]][2]
  given_label <- labels[given_col]
  ask_label   <- labels[ask_col]
  
  verbs <- pick_n(df, 10)
  
  cat_line("\n----------------------------------------")
  cat_line("  Odgovarjaj in pritisni Enter.")
  cat_line("  Sprejete so vse alternative (npr. was/were).")
  cat_line("----------------------------------------\n")
  
  score <- 0
  results <- vector("list", 10)
  
  for (i in 1:10) {
    verb    <- verbs[i, ]
    given   <- verb[[paste0("v", given_col)]]
    correct <- verb[[paste0("v", ask_col)]]
    meaning <- verb[["sl"]]
    
    cat_line(sprintf("[%d/10]  %s: %s  (%s)", i, given_label, given, meaning))
    user <- readline(sprintf("        %s: ", ask_label))
    ok   <- check_answer(user, correct)
    
    if (ok) { score <- score + 1; cat_line("        -> Pravilno!\n")
    } else  { cat_line(sprintf("        -> Narobe. Pravilen odgovor: %s\n", correct)) }
    
    results[[i]] <- list(given=given, correct=correct, user=user,
                         meaning=meaning, ok=ok,
                         given_label=given_label, ask_label=ask_label)
  }
  
  show_results(score, results, mode = "verbs")
}

# vocabulary

q_vocab <- function() {
  dir_choice <- choose_option("Izberi smer:", c("Nemščina -> Slovenščina", "Slovenščina -> Nemščina"))
  if (dir_choice == 1) {
    given_col <- "deu"; ask_col <- "slo"
    given_label <- "Nemščina"; ask_label <- "Slovenščina"
  } else {
    given_col <- "slo"; ask_col <- "deu"
    given_label <- "Slovenščina"; ask_label <- "Nemščina"
  }
  
  # Optional category filter
  n_total <- nrow(vocab)
  words <- pick_n(vocab, 10)
  
  cat_line("\n----------------------------------------")
  cat_line("  Vpiši prevod in pritisni Enter.")
  cat_line("  če je vec možnosti, ločenih s '/', je dovolj ena.")
  cat_line("----------------------------------------\n")
  
  score <- 0
  results <- vector("list", 10)
  
  for (i in 1:10) {
    word    <- words[i, ]
    given   <- word[[given_col]]
    correct <- word[[ask_col]]
    
    cat_line(sprintf("[%d/10]  %s: %s", i, given_label, given))
    user <- readline(sprintf("        %s: ", ask_label))
    ok   <- check_answer(user, correct)
    
    if (ok) { score <- score + 1; cat_line("        -> Pravilno!\n")
    } else  { cat_line(sprintf("        -> Narobe. Pravilen odgovor: %s\n", correct)) }
    
    results[[i]] <- list(given=given, correct=correct, user=user,
                         meaning=NULL, ok=ok,
                         given_label=given_label, ask_label=ask_label)
  }
  
  show_results(score, results, mode = "vocab")
}

# results

show_results <- function(score, results, mode = "verbs") {
  cat_line(sprintf("   REZULTAT:  %d / 10", score))
  cat_line("PREGLED ODGOVOROV:\n")
  
  for (r in results) {
    icon <- if (r$ok) "[OK]" else "[X] "
    if (mode == "verbs" && !is.null(r$meaning)) {
      cat_line(sprintf("  %s  %s: %-22s  %s: %-22s  (%s)", icon, r$given_label, r$given, r$ask_label, r$correct, r$meaning))
    } else {
      cat_line(sprintf("  %s  %s: %-22s  %s: %s",icon, r$given_label, r$given, r$ask_label, r$correct))
    }
    if (!r$ok) {
      user_str <- if (nchar(trimws(r$user)) == 0) "(prazno)" else r$user
      cat_line(sprintf("       Tvoj odgovor: %s", user_str))
    }
  }
}

# === main menu

run_game <- function() {
  cat_line("\n= ===========")
  cat_line("   JEZIKOVNA VADNICA ")
  cat_line("\n= ===========")

  
  mode <- choose_option("Želim vaditi?", c(
    "Nepravilni glagoli (EN ali DE)",
    "Vadba besednjaka  SLO <-> DEU  (splošne besede - v moškem spolu, nedoločeni obliki ednine)"
  ))
  
  cat_line("")
  if (mode == 1) q_verbs() else q_vocab()
  
  cat_line("\n----------------------------------------")
  again <- tolower(trimws(readline("Poskusi znova? (d/n): ")))
  if (again %in% c("d","da","y","yes")) run_game()
}


###############
# run the game
#############
run_game()

##########################################
# 
# Markov babbler
#
# Series:
# Little Useless-useful R functions #66
# Created: April 09, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


library(dplyr)
library(stringr)
library(tidyr)
library(igraph)
library(ggraph)


markov_babbler <- function(text, order = 2, n = 50, by_word = TRUE) {
  tokens <- if (by_word) str_split(text, "\\s+")[[1]] else unlist(str_split(text, ""))
  tokens <- tokens[tokens != ""]
  
  #add the removal of full stops,....
  # token <- 
  
  df <- data.frame(
    from = sapply(seq_len(length(tokens) - order), function(i) paste(tokens[i:(i + order - 1)], collapse = " ")),
    to = tokens[(order + 1):length(tokens)],
    stringsAsFactors = FALSE
  )
  
  probs <- df %>%
    group_by(from, to) %>%
    summarise(freq = n(), .groups = "drop") %>%
    group_by(from) %>%
    mutate(prob = freq / sum(freq))
  
  current <- sample(unique(probs$from), 1)
  output <- unlist(str_split(current, " "))
  
  for (i in seq_len(n)) {
    next_word <- probs %>% filter(from == current)
    if (nrow(next_word) == 0) break
    next_token <- sample(next_word$to, 1, prob = next_word$prob)
    output <- c(output, next_token)
    current <- paste(tail(output, order), collapse = " ")
  }
  
  g <- graph_from_data_frame(probs %>% filter(freq > 1), directed = TRUE)
  plot <- ggraph(g, layout = "fr") +
    geom_edge_link(aes(edge_alpha = prob, edge_width = prob), color = "firebrick") +
    geom_node_label(aes(label = name), size = 4, repel = TRUE) +
    theme_void() +
    labs(title = "Markov Chain: Token Transitions")
  
  list(text = paste(output, collapse = if (by_word) " " else ""), plot = plot)
}



text <- "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus. Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum. Aenean imperdiet. Etiam ultricies nisi vel augue. 
Curabitur ullamcorper ultricies nisi. Nam eget dui. 
Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus. Donec vitae sapien ut libero venenatis faucibus. Nullam quis ante. Etiam sit amet orci eget eros faucibus tincidunt. Duis leo. Sed fringilla mauris sit amet nibh. Donec sodales sagittis magna. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc, quis gravida magna mi a libero. Fusce vulputate eleifend sapien. 
Vestibulum purus quam, scelerisque ut, mollis sed, nonummy id, metus. Nullam accumsan lorem in dui. Cras ultricies mi eu turpis hendrerit fringilla. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; 
In ac dui quis mi consectetuer lacinia. 
Nam pretium turpis et arcu. Duis arcu tortor, suscipit eget, imperdiet nec, imperdiet iaculis, ipsum. Sed aliquam ultrices mauris. Integer ante arcu, accumsan a, consectetuer eget, posuere ut, mauris. Praesent adipiscing. Phasellus ullamcorper ipsum rutrum nunc. Nunc nonummy metus. Vestibulum volutpat pretium libero. Cras id dui. Aenean ut eros et nisl sagittis vestibulum. Nullam nulla eros, ultricies sit amet, nonummy id, imperdiet feugiat, pede. Sed lectus. Donec mollis hendrerit risus. Phasellus nec sem in justo pellentesque facilisis. Etiam imperdiet imperdiet orci. Nunc nec neque. Phasellus leo dolor, 
tempus non, auctor et, hendrerit quis, nisi. Curabitur ligula sapien, tincidunt non, euismod vitae, posuere imperdiet, leo. Maecenas malesuada. Praesent congue erat at massa. Sed cursus turpis vitae tortor. Donec posuere vulputate arcu. Phasellus accumsan cursus velit. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Sed aliquam, nisi quis porttitor congue, elit erat euismod orci, ac placerat dolor lectus quis orci. Phasellus consectetuer vestibulum elit. Aenean tellus metus, bibendum sed, posuere ac, mattis non, nunc. Vestibulum fringilla pede sit amet augue. In turpis. Pellentesque posuere. Praesent turpis. Aenean posuere, tortor sed cursus feugiat, nunc augue blandit nunc, eu sollicitudin urna dolor sagittis lacus. Donec elit libero, sodales nec, volutpat a, suscipit non, turpis. Nullam sagittis. Suspendisse pulvinar, 
augue ac venenatis condimentum, sem libero volutpat nibh, nec pellentesque velit pede quis nunc. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Fusce id purus. Ut varius tincidunt libero. Phasellus dolor. Maecenas vestibulum mollis diam. Pellentesque ut neque. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. 
In dui magna, posuere eget, vestibulum et, tempor auctor, justo. In ac felis quis tortor malesuada pretium. Pellentesque auctor neque nec urna. Proin sapien ipsum, porta a, auctor quis, euismod ut, mi. Aenean viverra rhoncus pede. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Ut non enim eleifend felis pretium feugiat. Vivamus quis mi. Phasellus a est. Phasellus magna. In hac habitasse platea dictumst. Curabitur at lacus ac velit ornare lobortis. Curabitur a felis in nunc fringilla tristique. Morbi mattis ullamcorper velit. Phasellus gravida semper nisi. Nullam vel sem. Pellentesque libero tortor, tincidunt et, tincidunt eget, semper nec, quam. Sed hendrerit. Morbi ac felis. Nunc egestas, augue at pellentesque laoreet, felis eros vehicula leo, at malesuada velit leo quis pede. Donec interdum, metus et hendrerit aliquet,
dolor diam sagittis ligula, eget egestas libero turpis vel mi. Nunc nulla. Fusce risus nisl, viverra et, tempor et, pretium in, sapien. Donec venenatis vulputate lorem. Morbi nec metus. Phasellus blandit leo ut odio. Maecenas ullamcorper, dui et placerat feugiat, eros pede varius nisi, condimentum viverra felis nunc et lorem. Sed magna purus, fermentum eu, tincidunt eu, varius ut, felis. In auctor lobortis lacus. Quisque libero metus, condimentum nec, tempor a, commodo mollis, magna. 
Vestibulum ullamcorper mauris at ligula. Fusce fermentum. Nullam cursus lacinia erat. Praesent blandit laoreet nibh. Fusce "

result <- markov_babbler(text, n=30)
cat(result$text)  
print(result$plot) 





text <- '"Potem ko so v soboto za večino držav in ozemelj po svetu začele veljati desetodstotne dodatne carine na uvoz blaga v ZDA, so ob polnoči po krajevnem času v Washingtonu za izbrano skupino zunanjetrgovinskih partneric, ki po prepričanju ameriškega predsednika Donalda Trumpa in njegove administracije v zunanji trgovini najbolj izkoriščajo ZDA, začele veljati še dodatne carinske stopnje. Te se gibljejo med 11 in 50 odstotki. Za Evropsko unijo je stopnja dodatnih carin po novem 20-odstotna.
Neverjetna 104-odstotna carinska stopnja za kitajske izdelke

Najvišje carine so ZDA uvedle za Kitajsko – po 20-odstotnih, uvedenih marca, bi se te danes morale zvišati še za 34 odstotkov, a ker se je Kitajska odzvala s 34-odstotnimi carinami na ameriško blago, jih je Trump dvignil še za 50 odstotkov. Skupno tako od danes znašajo kar 104 odstotke.

Ukrep je močno pretresel in osupnil svet. Čeprav je Trump carine označil za vzajemne, se vrstijo opozorila, da nimajo ničesar z vzajemnostjo. Carinske stopnje so daleč od povprečnih carinskih stopenj na ameriški uvoz in predstavljajo preprosto delež primanjkljaja v blagovni menjavi ZDA s posamezno partnerico v skupnem uvozu iz te partnerice.

Kljub šoku in razburjenju je večina ameriških zunanjetrgovinskih partneric, to velja tudi za Evropsko unijo, izrazila željo po pogajanjih in izogibanju nadaljnjemu stopnjevanju trgovinske vojne.

Izjema je Kitajska, carine v vrednosti 25 odstotkov na nekatere ameriške avtomobile je napovedala tudi Kanada.
Kitajska obljublja odločno ukrepanje

V Pekingu so napovedali, da bo Kitajska sprejela "odločne ukrepe" za zaščito lastnih interesov. Kitajsko finančno ministrstvo je nato sporočilo, da povračilne carine na uvoz ameriškega blaga s četrtkom zvišujejo s 34 na 84 odstotkov.

Predstavnik kitajskega zunanjega ministrstva Lin Džjan je poudaril, da ima kitajsko ljudstvo neodtujljivo pravico do razvoja. "Še naprej bomo sprejemali odločne ukrepe za zaščito svojih zakonitih pravic in interesov," je dejal.

Kitajsko ministrstvo za trgovino je medtem sporočilo, da ima država "trdno voljo" za boj v trgovinski vojni z Washingtonom. "Kitajska bo s trdno voljo in številnimi sredstvi odločno sprejela protiukrepe in se borila do konca, če bodo ZDA vztrajale pri nadaljnjem stopnjevanju gospodarskih in trgovinskih omejevalnih ukrepov," je povzela sporočilo ministrstva.

V drugem največjem gospodarstvu na svetu sicer menijo, da lahko trgovinske in gospodarske spore z ZDA rešijo z enakopravnim dialogom. "Kitajska in ZDA lahko razlike na gospodarskem in trgovinskem področju rešijo z enakopravnim dialogom in vzajemno koristnim sodelovanjem," je zapisano v beli knjigi, ki so jo pripravili v Pekingu.

Peking je pred tem v torek napovedal, da bo šel v trgovinski vojni z ZDA "do konca". Kot so poudarili, "pritiski, grožnje in izsiljevanje niso pravi način za spopadanje s Kitajsko".

Iz Washingtona sicer prihajajo mešani signali. Trump kot pogoj za pogajanja postavlja, da se blagovna menjava s partnericami tako ali drugače izravna ali preide v presežek.

Kot je poročal Andrej Stopar, dopisnik RTV Slovenija iz Washingtona, je v torek Trump dejal: "Veliko držav prihaja, ker se želijo dogovoriti. Če bi jim pred dvema letoma, tremi ali petimi leti rekel, da bodo skušale skleniti takšne dogovore, bi se nam smejale. Zdaj pa prosijo za sprejem. Čudovito se bo izteklo. Pobirali bomo po dve milijardi dolarjev na dan. To je veliko denarja."

Ameriški finančni minister Scott Bessent pa druge države opozarja, naj ne uvajajo protiukrepov, ker bo sledilo stopnjevanje. "50, 60, morda 70 držav nas je nagovorilo. April in maj, morda tudi junij bodo zelo naporni. Japonska je zelo pomembna vojaška, obenem pa tudi gospodarska zaveznica. Z ZDA ima veliko skupne zgodovine. Ker so pristopili zelo hitro, mislim, da bi morala imeti Japonska prednost," je dejal.

Prav ta pogajalski interes je v torek vplival na gibanje borznih indeksov v ZDA. Potem ko so se po novinarski konferenci Trumpa in izraelskega premierja Benjamina Netanjahuja nekoliko umirili, so v torek znova padali. Seth Sutel, novinar tiskovne agencije AP, je dejal: "Največji zalogaj je seveda Kitajska, ki ostaja težava. Čeprav v trgovinske pogovore z ZDA vstopata Japonska in Južna Koreja, Kitajska ne. Opolnoči je začel veljati debel sveženj carin. Ta konflikt še ni razrešen. Dokler ne bo, lahko vsi zgolj ugibamo, kaj se bo dogajalo z borzo."

Medijska hiša Bloomberg ocenjuje, da so v nekaj dneh na svetovni ravni zaradi ameriških carin podjetja, ki kotirajo na borzah, izgubila skupaj 10 trilijonov dolarjev, je dodal Stopar.
Carine na carine na carine

ZDA so doslej že uvedle tudi več t. i. sektorskih carin. Med drugim so tako sredi marca začele veljati 25-odstotne carine na uvoz jekla in aluminija, prejšnji teden pa dodatne 25-odstotne carine za avtomobile. Za to blago nove Trumpove carine ne veljajo. Prav tako za zdaj ne veljajo za farmacevtske izdelke, baker, čipe, les, zlato, energijo in nekatere rudnine, ki niso na voljo v ZDA.
Rusija opozarja na nespoštovanje trgovinskih pravil

Obsežne carine ameriškega predsednika kažejo na njegovo nespoštovanje temeljev mednarodne trgovine, ocenjujejo v Moskvi, kjer obenem izražajo zaskrbljenost zaradi svetovne trgovinske vojne.

Trumpove carine kršijo temeljna pravila Svetovne trgovinske organizacije (WTO) in dokazujejo prepričanje Washingtona, "da ni več zavezan normam mednarodnega trgovinskega prava", je v pogovoru z novinarji dejala tiskovna predstavnica ruskega zunanjega ministrstva Marija Zaharova.

Rusija, ki je od začetka vojne v Ukrajini poglobila gospodarske in politične odnose s Kitajsko, je od vrnitve Trumpa v Belo hišo previdna glede kritik na račun politik ameriškega predsednika. Vendar so nekateri pristojni v Moskvi v zadnjih dneh izrazili zaskrbljenost zaradi padanja cen nafte, ki so ključnega pomena za ruske javne finance.

"Vsak šok svetovnem gospodarstvu, ki grozi z upočasnitvijo gospodarske rasti in splošnim zmanjšanjem potrošnje, negativno vpliva na številne globalne procese," je dejala Zaharova.

Tudi guvernerka ruske centralne banke Elvira Nabiullina je na zaslišanju pred člani ruskega parlamenta trgovinsko vojno označila za veliko tveganje in dejala, da v svetovni trgovini trenutno prihaja do tektonskih sprememb. Po njenih besedah je težko oceniti, kam bodo te spremembe vodile svetovno gospodarstvo in kako bo to vplivalo na Rusijo.
Veljajo še višje dodatne carine za uvoz blaga v ZDA
Azijske borze po uveljavitvi dodatnih ameriških carin v rdečem

Uveljavitev še višjih carin na uvoz blaga v ZDA je osrednje indekse na azijskih borzah znova obarvala večinoma rdeče. Na vodilni azijski borzi v Tokiu je indeks Nikkei, ki ga izračunavajo na osnovi vrednosti 225 najpomembnejših delnic, nazadoval za 3,56 odstotka, na 31.836,49 točke.

Znižal se je tudi indeks borze v Sydneyju All Ordinaries, in sicer za 1,85 odstotka. V Seulu je osrednji borzni indeks Kospi izgubil 1,77 odstotka, tečaji delnic v Tajvanu pa so v povprečju padli za pet odstotkov. V Singapurju je indeks STI 2,35 odstotka nižje kot v torek.

Trgovanje na kitajskih borzah je medtem neenotno. V Hongkongu je šel indeks Hang Seng doslej navzdol za 0,68 odstotka, v Šanghaju pa je indeks Shanghai Composite 0,72 odstotka nad izhodiščem.
Padli so tudi evropski indeksi

Tudi indeksi na evropskih borzah so se po torkovem odboju, ki je sledil večdnevnim občutnim padcem, znova spustili pod gladino. Indeks najpomembnejših podjetij v območju evra Eurostoxx 50 je trenutno na 2,40 odstotka nižji ravni kot ob koncu trgovanja v torek.

Londonski indeks FTSE 100 je 2,15 odstotka pod izhodiščem, pariški CAC 40 pa 2,69 odstotka pod njim. Frankfurtski DAX je ob začetku trgovanja padel 2,34 odstotka pod gladino. Milanski indeks FTSE MIB izgublja 2,09 odstotka, dunajski ATX 1,72 odstotka. Züriški SMI medtem pada po 1,67-odstotni stopnji.

Tečaj evra se zvišuje. Na borzi v Frankfurtu je treba trenutno za en evro odšteti 1,1067 dolarja, kar je odstotek več kot v torek.
Cene nafte na štiriletnem dnu

Cene surove nafte so padle na najnižjo raven v več kot štirih letih. Trge skrbi, da se bo povpraševanje po črnem zlatu zmanjšalo zaradi stopnjevanja svetovne trgovinske vojne, v ospredju katere sta najmočnejši svetovni gospodarstvi ZDA in Kitajska, in da bodo na trgu presežki nafte, je na spletni strani poročal Reuters.

Za 159-litrski sod zahodnoteksaške nafte z dobavnim rokom maja je bilo treba popoldne po singapurskem času odšteti 57,12 dolarja, kar je 2,46 dolarja manj kot v torek. Severnomorska nafta brent za dobavo v juniju pa se je pocenila za 2,38 dolarja, na 60,44 dolarja.

Cene za obe vrsti nafte sta bili s tem na najnižji stopnji po februarju 2021.

Poleg tega se je skupina razširjenih proizvajalk nafte OPEC+ pretekli teden odločila, da bo maja povečala črpanje nafte za 411.000 sodov dnevno, kar bo po mnenju analitikov verjetno potisnilo trg v presežek."'

result <- markov_babbler(text, n=20)
cat(result$text)    
print(result$plot)


#sample of Grimm story (in Slovenian Language)

text <- "Brata Grimm: RDEČA KAPICA
Nekoč je živela majhna deklica. Bila je tako zelo ljubka, da jo je vsak že na prvi pogled vzljubil. Izmed
vseh ljudi na tem svetu pa jo je imela najraje njena babica. Ni je bilo stvari, ki je ne bi naredila zanjo.
Nekega dne ji je podarila žametno rdečo kapico. Ta je bila deklici tako zelo všeč, da jo je nosila,
kamorkoli je šla. Zato so jo vsi klicali Rdeča kapica.
Nekega dne ji je mama rekla: “Pridi, Rdeča kapica. Tukaj je kos potice in steklenica vina, nesi ju svoji
babici. Zelo je bolna in slabotna. Okrepčilo ji bo dobro delo. Pa le hitro se odpravi, preden postane
prevroče. Hodi previdno in tiho. Predvsem pa ne zahajaj s poti, da ne boš padla in razbila steklenice,
ker potem babica ne bo dobila ničesar. Ko vstopiš v sobo, ne pozabi reči: “Dobro jutro,” in ne preglej
vsakega kotička preden to storiš.”
“Pazljiva bom,” ji je obljubila Rdeča kapica.
Babica je živela globoko v gozdu, za dobro uro hoda stran. Ko je Rdeča kapica vstopila v gozd, ji je
naproti stopil volk. Ker Rdeča kapica še nikdar ni srečala volka in ni vedela, kako zlobna in prebrisana
je ta zver, se ga sploh ni bala.
“Pozdravljena, Rdeča kapica,” je rekel volk.
“Hvala enako, prijazni volk.”
“Kam pa kam tako zarana, Rdeča kapica?”
“K babici.”
“Kaj pa nosiš v košarici?”
“Potico in vino, včeraj je mama pekla. Uboga bolna babica bo
dobila nekaj dobrega za pod zob. Da se okrepča.”
“Kje pa živi tvoja babica?”
“Dobro uro hoda tja globoko v gozd. Njena hiša stoji pod tremi hrasti, in leske rastejo vse naokrog. Prav
gotovo si jo že videl,” je odgovorila Rdeča kapica.
Volk si je mislil: “Oh, kako okusna bo tale mala deklica, ko jo bom snedel. Precej boljša kot stara ženička.
Ampak, če bom dovolj prebrisan, bom lahko pohrustal obe.” Tako je volk del poti spremljal Rdečo
kapico, preden se je spet oglasil: “Poglej, Rdeča kapica, kako lepe rože rastejo povsod tu naokrog. In
samo poslušaj, kako lepo pojejo vse te ptičice. Ti pa tako resno hodiš, kot bi bila na poti v šolo. Vse
naokrog pa je tako čarobno.” Medtem pa jo je volk po najkrajši možni poti ucvrl naravnost do babičine
hiše in potrkal na vrata.
“Kdo je?”
“Rdeča kapica. Prinesla sem ti potice in vina. Odpri mi vrata.”
“Kar odpri si,” je zaklicala babica. “Preslabotna sem in ne morem vstati.”
Volk je pritisnil na kljuko in vrata so se na stežaj odprla. Brez besed je vstopil in planil naravnost na
babičino posteljo. V enem samem grižljaju jo je požrl. Potem si je nadel babičina oblačila in poveznil
nočno kapo na glavo. Zagrnil je zavese in se udobno ulegel v babičino posteljo.
Rdeča kapica je ves ta čas tekala po gozdu in nabirala cvetlice. Ko jih je nabrala toliko, da jih je že komaj
držala, se je spomnila na babico in se končno odpravila do nje.
Bila je presenečena, ko je našla vrata babičine hiše na stežaj odprta. Ko je vstopila, jo je prevzel čuden
občutek. Potiho si je rekla: “Oh. Danes se pa tukaj tako tesnobno počutim. Do sedaj mi je bilo pri babici
vedno prijetno.” Glasno je zaklicala: “Dobro jutro.” Vendar odgovora ni dobila. Počasi je stopila do
babičine postelje in odgrnila zavese. Zagledala je babico, ki je imela kapo poveznjeno skoraj čez cel
obraz. Zelo čudna se ji je zdela.
“Joj, babica, kako velika ušesa imaš!”
“Da te bolje slišim, otrok moj,” je dobila v odgovor.
“Joj babica, kako velike oči imaš!”
“Da te bolje vidim, draga moja.”
“Toda babica, kako velike roke imaš!”
“Da te lažje objamem.”
“Oh. Babica. Kako grozno velika usta imaš!”
“Da te lažje požrem!”
Še preden je volk do konca odgovoril, je že planil iz postelje
in Rdečo kapico celo pogoltnil.
Ko je volk pomiril svojo lakoto, je legel nazaj v posteljo in v trenutku zaspal. Ravno takrat je šel mimo
hiše lovec in si mislil: “Kako glasno babica danes smrči. Grem in jo vprašam, če morda kaj potrebuje.”
Vstopil je v hišo in ko je pristopil k postelji, je zagledal, da tam namesto babice leži volk. “Aha, tukaj
sem te našel!” si je tiho rekel. “Dolgo sem te iskal.” Vanj je nameril svojo puško. Ravno, ko je hotel
ustreliti, je pomislil, da je volk morda požrl babico in bi jo še lahko rešil. Zato je raje v roke vzel škarje
in volku začel rezati trebuh. Že takoj po prvem rezu, je v volkovem trebuhu opazil nekaj rdečega. Hitro
je naredil še dva reza in iz kosmatinčevega trebuha je ven skočila Rdeča kapica. Glasno je zajokala: “Oh,
kako me je bilo strah. V volkovem trebuhu je tako zelo temno.” Takoj za tem je ven prilezla še babica,
ki je komaj še dihala. Rdeča kapica je nato hitro nabrala nekaj velikih kamnov, s katerimi so napolnili
volkov trebuh. Ko se je volk zbudil, je bil zelo žejen. Šel je k vodnjaku, da popije malo vode, vendar so
bili kamni v njegovem trebuhu pretežki, zato je padel v vodnjak in poginil.
Babica, Rdeča kapica in lovec so se tega zelo razveselili. Skupaj so pojedli potico, babica in lovec sta
spila vino, ki ga je prinesla Rdeča kapica. Babica se je tako dobro okrepčala. Rdeča kapica pa si je mislila:
“Dokler bom živa, ne bom nikoli več skrenila s poti in tekala po gozdu, če mi bo mama tako rekla.”"


result <- markov_babbler(text, n=20)
print(result$plot)


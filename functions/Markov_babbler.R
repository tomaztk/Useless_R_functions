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
  token <- c('I', 'I am', 'to', 'all', 'Oh')
  
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


#sample of Grimm story (in Slovenian Language)

text <- "Nekoč je živela majhna deklica. Bila je tako zelo ljubka, da jo je vsak že na prvi pogled vzljubil. Izmed
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

#sample of Grimm story (in English Language)

text = "Once upon a time thee was a sweet little girl. Everyone who saw her liked her, but most of all her 
grandmother, who did not know what to give the child next. Once she gave her a little cap made of red velvet.
Because it suited her so well, and she wanted to wear it all the time, she came to be known as Little Red 
Riding Hood. One day her mother said to her:  Come Little Red Riding Hood. Here is a piece of cake and a 
bottle of wine. Take them to your grandmother. She is sick and weak, and they will do her well. Mind your 
manners and give her my greetings. Behave yourself on the way, and do not leave the path, or you might fall 
down and break the glass, and then there will be nothing for your sick grandmother.
Little Red Riding Hood promised to obey her mother. The grandmother lived out in the woods, a half hour from 
the village. When Little Red Riding Hood entered the woods a wolf came up to her. She did not know what a wicked 
animal he was, and was not afraid of him. Good day to you, Little Red Riding Hood. - Thank you, wolf. - 
Where are you going so early, Little Red Riding Hood? - To grandmother. - And what are you carrying under your
apron? - Grandmother is sick and weak, and I am taking her some cake and wine. We baked yesterday, and they
should give her strength. - Little Red Riding Hood, just where does your grandmother live? - Her house is a 
good quarter hour from here in the woods, under the three large oak trees. There  a hedge of hazel bushes there. You must know the place,  said Little Red Riding Hood. The wolf thought to himself:  Now there is a tasty bite for me. Just how are you going to catch her?  Then he said:  Listen, Little Red Riding Hood, haven t you seen the beautiful flowers that are blossoming in the woods? Why don t you go and take a look? And I don t believe you can hear how beautifully the birds are singing. You are walking along as though you were on your way to school in the village. It is very beautiful in the woods. 
Little Red Riding Hood opened her eyes and saw the sunlight breaking through the trees and how the ground was 
covered with beautiful flowers. She thought:  If a take a bouquet to grandmother, she will be very pleased. 
Anyway, it is still early, and I ll be home on time.  And she ran off into the woods looking for flowers. 
Each time she picked one she thought that she could see an even more beautiful one a little way off, and 
she ran after it, going further and further into the woods. But the wolf ran straight to the grandmother  
house and knocked on the door.  Who  there?  -  Little Red Riding Hood. I m bringing you some cake and wine.
Open the door for me.  -  Just press the latch,  called out the grandmother.  I m too weak to get up.  
The wolf pressed the latch, and the door opened. He stepped inside, went straight to the grandmother  
bed, and ate her up. Then he took her clothes, put them on, and put her cap on his head. He got into her 
bed and pulled the curtains shut.
Little Red Riding Hood had run after flowers, and did not continue on her way to grandmother until she 
had gathered all that she could carry. When she arrived, she found, to her surprise, that the door was 
open. She walked into the parlor, and everything looked so strange that she thought:  Oh, my God, why am 
I so afraid? I usually like it at grandmother .  Then she went to the bed and pulled back the curtains. 
Grandmother was lying there with her cap pulled down over her face and looking very strange.  Oh, 
grandmother, what big ears you have!  -  All the better to hear you with.  -  Oh, grandmother, what big 
eyes you have!  -  All the better to see you with.  -  Oh, grandmother, what big hands you have!  - 
   All the better to grab you with!  -  Oh, grandmother, what a horribly big mouth you have!  -  All 
the better to eat you with!  And with that he jumped out of bed, jumped on top of poor Little Red Riding 
Hood, and ate her up.
As soon as the wolf had finished this tasty bite, he climbed back into bed, fell asleep, and began to 
snore very loudly. A huntsman was just passing by. He thought it strange that the old woman was snoring 
so loudly, so he decided to take a look. He stepped inside, and in the bed there lay the wolf that he 
had been hunting for such a long time.  He has eaten the grandmother, but perhaps she still can be saved. 
I won t shoot him,  thought the huntsman. So he took a pair of scissors and cut open his belly. He had cut 
only a few strokes when he saw the red cap shining through. He cut a little more, and the girl jumped out 
and cried:  Oh, I was so frightened! It was so dark inside the wolf  body!  And then the grandmother came
out alive as well. Then Little Red Riding Hood fetched some large heavy stones. They filled the wolf  body 
with them, and when he woke up and tried to run away, the stones were so heavy that he fell down dead.
The three of them were happy. The huntsman took the wolf  pelt. The grandmother ate the cake and drank the 
wine that Little Red Riding Hood had brought. And Little Red Riding Hood thought to herself:  As long as I 
live, I will never leave the path and run off into the woods by myself if mother tells me not to. 

They also tell how Little Red Riding Hood was taking some baked things to her grandmother another time, 
when another wolf spoke to her and wanted her to leave the path. But Little Red Riding Hood took care and 
went straight to grandmother . She told her that she had seen the wolf, and that he had wished her a good 
day, but had stared at her in a wicked manner.  If we hadn t been on a public road, he would have eaten me 
up,  she said.  Come,  said the grandmother.  Let  lock the door, so he can t get in.  Soon afterward the 
wolf knocked on the door and called out:  Open up, grandmother. It  Little Red Riding Hood, and I m bringing 
you some baked things.  They remained silent, and did not open the door. The wicked one walked around the 
house several times, and finally jumped onto the roof. He wanted to wait until Little Red Riding Hood went 
home that evening, then follow her and eat her up in the darkness. But the grandmother saw what he was up 
to. There was a large stone trough in front of the house.  Fetch a bucket, Little Red Riding Hood,  she said.
 Yesterday I cooked some sausage. Carry the water that I boiled them with to the trough.  Little Red Riding
Hood carried water until the large, large trough was clear full. The smell of sausage arose into the wolf 
nose. He sniffed and looked down, stretching his neck so long that he could no longer hold himself, and he 
began to slide. He slid off the roof, fell into the trough, and drowned. And Little Red Riding Hood returned
home happily and safely. 
"

result <- markov_babbler(text, n=20)
print(result$plot)

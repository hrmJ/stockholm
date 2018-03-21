# Vot jeshe raz correspondence analysis, no na etot raz utshityvaetsa nalitshije / otsutsvije "moj"

#bolshaja <- readRDS("bolshaja_tablica.rds")

#Esli u vas polutshilos s komandoj github_install("hrmJ/stockholm[without_python]")
library(stockholm)

# Prisojedinjaem paru kategorij

bolshaja$participants[bolshaja$participants=="die reiche an armen vertikale relationen"]  <- "sverhu vniz"
bolshaja$participants[bolshaja$participants=="moj niania"] <- "niania"
bolshaja$participants[bolshaja$participants=="blizkije rodstvenniki starsjije k mladsjim"] <- "rodsvennik"


# To, shto iskljutshajem iz nashej tablitsy 
# Vnimanije: proverte etot spisok, tam mogut byt lyshnyje (ili mozhet byt 
# naoborot tshego-to ne hvatajet)

ubrannyje  <- c("otec ty", "otec vosklicanije", "drug",  "mama vosklicanije",
                "?",  "otec", "ty", "otcy moi", "chelovek", "otcy", "nomen", "rodnenkij moj", 
                "grazhdane", "vosklicajije", "tovarish", "vy moj chelovek", 
                "drug moj", "agress", "tovarishi", "druzja")

# Vot, kak my ih iskjlutshaem -
# ispolzujem dlja etogo funktsiju subset:

ne_takaja_bolshaja <- subset(bolshaja, !participants %in% ubrannyje)

# Sozdadim novuju kolonku pod nazvaniem "konstrukcija" (eta stroka vam ne budet
# ponjatnoj, no eto nevazhno. Glavnoje jejo prosto zapustit. I vazhno to, shto
# v rezultate polutshim novuju kolonku s informatsijej o tom, prisutsvujet li v
# dannom slutshaje mestoimenije "moj" ili net) Kstati, obratite vnimanije na
# to, shto tut ne ukazano gde eto mestoimenije nahoditsa, a tolko sam fakt ego
# nalitshija
ne_takaja_bolshaja$konstrukcija <- as.factor(apply(ne_takaja_bolshaja,1,function(r)ifelse(grepl("moj",r[5]),paste0(r[2], " + moj"),r[2])))

# Vot nam pora sozdat tablitsu s (absoljutnymi) tsiframi

tablica_s_absoljutnymi_tsiframi <- table(ne_takaja_bolshaja$participants, ne_takaja_bolshaja$konstrukcija)

# I, nakonjets-to, correspondence analysis
library(ca)

# ...teper u nas imejutsa vse neobhodimyje funktsii

correspondence_analys_s_konstrukcijami <- ca(tablica_s_absoljutnymi_tsiframi)

# I vot kartinka:
plot(correspondence_analys_s_konstrukcijami)

#... Tut ja prilozhu eshe drugije kartinki
# dlja nih trebuetsa otsherednaja biblioteka
# no ja, k sozhaleniju, ne uveren, polutshitsa
# vam jejo ustanovit. Pobrobuite zapustit sledujushuju stroku:
if(!"factoextra" %in% rownames(installed.packages())) install.packages("factoextra")

#... ustanovlenije etoj biblioteki dlitsa otshen dolgo...
#... Esli vse polutshilos, teper mozhno:
library("factoextra")
#... i potom, shtoby narisovat novuju kartinku:
fviz_ca_col(correspondence_analys_s_konstrukcijami)
#... esli hotshetsa smotret tolko na utshastnikov situatsii:
fviz_ca_row(correspondence_analys_s_konstrukcijami)


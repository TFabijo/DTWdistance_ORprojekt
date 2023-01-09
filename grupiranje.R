library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)


#Najprej sva iplementirala algoritem za izračun DTW razdalje.

# algoritem kot vhodna podatka vzame 2 časovni vrsti in vrne seznam z
# matriko cen, pot do optimalne rešitve in dtw razdaljo

dtw1 <- function(a, b) {
  
  m <- length(a)
  n <- length(b)
  razdalje <- matrix(0, nrow = m, ncol = n)
  for (i in 1:m) {
    for (j in 1:n) {
      razdalje[i,j] <- abs(a[i] - b[j])
    }
  }
  
  cene <- matrix(0, nrow = m+1, ncol = n+1)
  for (i in 2:(m+1)) {
    cene[i,1] <- Inf
  }
  for (j in 2:(n+1)) {
    cene[1,j] <- Inf
  }
  
  poti <- matrix(0, nrow = m, ncol = n)
  for (i in 2:(m+1)) {
    for (j in 2:(n+1)) {
      povezave <- c(
        cene[i-1,j-1], 
        cene[i-1,j], 
        cene[i,j-1]
      )
      izbran <- min(povezave)
      indeks <- match(izbran, povezave)
      cene[i,j] <- razdalje[i-1,j-1] + izbran
      poti[i-1,j-1] <- indeks #1, 2, 3
    }
  }
  
  k <- m
  l <- n
  pot <- list(c(k,l))
  while (k > 1 | l > 1) {
    korak <- poti[k,l]
    if (korak == 1) {
      k <- k - 1
      l <- l - 1
    } else if (korak == 2) {
      k <- k - 1
    } else if (korak == 3) {
      l <- l - 1
    } 
    
    pot <- append(list(c(k,l)), pot)
  }
  return(list(cene, pot, cene[m+1,n+1]))
}

#preberemo podatke o vredosti indeksa cen podjedji, ki kotrirajo v posamezni
#državi
indeks = read_excel("index.xlsx")

#izbremo stolpce ki nas zanimajo (država,čas,vrednost)
indeks = indeks[,c(1,6,7)]

#določimo vektor vseh držav, ki nastopajo v podatkih
opazovane = indeks$LOCATION[duplicated(indeks$LOCATION)==FALSE]

# Za grupiranje podatkov potrebujemo distančno matriko, ki nam poda
#(spodnje diagonalno matriko) dtw razdalji med vsemi opazovanimi časovnimi 
#vrstami (v našem primeru indeksov)

#OPOMBA: funkcija je pripravljena konkretno za naše podatke
#sprejme opazovane podatke in vektor opazovanih države vrne pa distančno matriko

distančna_matrika_1 = function(podatki,opazovane) {
  matrika = matrix(, length(opazovane), ncol =length(opazovane),dimnames = list(opazovane, opazovane))
  for (d1 in 1:length(opazovane)){
    for (d2 in (d1):length(opazovane)){
      ts1 = podatki %>% filter(LOCATION == opazovane[d1])
      ts1 = ts1$Value
      ts2 = podatki %>% filter(LOCATION == opazovane[d2])
      ts2 = ts2$Value
      
      rezultat = dtw1(as.numeric(ts1),as.numeric(ts2))
      matrika[d2,d1] =  rezultat[[3]]
      
    }
    
  }
  
  matrika
}

# izračunamo distančno matriko z gornjo funkcijo
mt = distančna_matrika_1(indeks,opazovane)

#NA vredosti nadomestimo z 0
mt[is.na(mt)] = 0

# če hočemo uporabiti funkcijo hclust maramo distančno mariko pretvoriti v
# posebno obliko. To naredimo z privzeto funkcijo as.dist()
Mt = as.dist(mt)
Mt

#za grupiranje podatkov vzamemo privzeto funkcijo v R-u hclaust
# za razdaljo med skupinami uporabimo povprečno razdaljo
rezultat  = Mt %>% hclust(method = "average")

# Narišemo hirarhični diagram
plot(rezultat, 
     ylab = "dtw distenc",
     main = "države",
     sub = "", hang = -1, cex=0.7)

tiff(file="hirarhicni_diagram.png")
plot(rezultat, 
     ylab = "dtw distenc",
     main = "države",
     sub = "", hang = -1, cex=0.7)
dev.off()

# za določitev optimalnega števila skupin si pomagamo z kolensko metodo
# metoda  duluje na tem da račuanmo razdaljo med skupinama ki smo ju združili
# koleno je tisto število pri katri zabeležimo največji padec razdalje, ko dodamo
#novo skupino. Pri dodajanju novih skupin se grupiranje bistveno ne popravi

#izračunamo razdlje med skupinami
v_skupine = tibble(
  k = length(hirar$height):1,
  visine = hirar$height) %>%
  arrange(k)
v_skupine

# tabela za določitev kolena
kolena = v_skupine %>%
  mutate(
    d_visine = 1 - lag(visine) / visine,
    dd_visine = lead(d_visine) - d_visine
  )
#najvecja razlika je pri 4 skupinah
kolena %>%filter(dd_visine>= max(kolena$dd_visine, na.rm = TRUE))

# še grafično
kolena %>%
  ggplot() +
  aes(x = k, y = visine) +
  xlab("Število skupin")+
  geom_line() +
  geom_point() +
  geom_point(
    data = kolena %>%
      filter(dd_visine>= max(kolena$dd_visine, na.rm = TRUE)),
    color = "red"
  ) +
  theme_classic()

#ugotovimo da je optimalno število skupin enako 4


# sedaj ko vemo optimalno lahko določimo skupine

kos = rezultat %>% cutree(k = 4)

# izračunamo tabelo, ki nam poda v katero skupino kam spada posamezna država
skupine_kos = data.frame(skupina = kos, 
                         LOCATION = names(kos)) %>% arrange(kos)
rownames(skupine_kos) = NULL
skupine_kos

# sedaj lahko skpine označimo še na našem hirarhičnem diagramu
plot(rezultat, 
     ylab = "dtw distenc",
     main = "države",
     sub = "", hang = -1, cex=0.7)
rect.hclust(rezultat, k = 4, border = "red")

#zemljevid
library(rnaturalearth) 

sk = skupine_kos
colnames(sk)[2] = "iso_a3"
sk$skupina = as.character(sk$skupina)

zemljevid = ne_countries(
  scale = "medium",
  continent = "Europe",
  returnclass = "sf")

evro_zemljevid =zemljevid %>% select(iso_a2, iso_a3, name, continent, subregion, starts_with("mapcolor")) %>%
  left_join(sk, by="iso_a3")

map = evro_zemljevid %>%
  ggplot() +
  aes(fill = skupina) +
  xlim(-25, 40) +
  ylim(35, 70) +
  geom_sf() +
  theme_void() +
  theme(legend.position = "right")+
  labs(title = "Razvrstitev opazovanih držav glede na pripadajočo skupino")

map


#################
# naše rezultate sedaj preverimo še z privzetimi funcijami, ki jih ponuja R

library(dtwclust)

# podatke moramo dati v obliko da je vsaka vrstica posmezna ćasovna vrsta
t = indeks %>% pivot_wider(names_from = "TIME",values_from="Value") ## samo za imena
time_seris = indeks %>% pivot_wider(names_from = "TIME",values_from="Value")

#### poravimo da imena držav niso prvi stolpec
time_seris= time_seris[,-1]
rownames(time_seris) = t$LOCATION

### podatke moramo dati v posebno obliko, ki jo lahko razume funcija tsclust()
# to naredimo z privzeto funcijo tslist()
po = tslist(time_seris)

# naredimo grupiranje za 3 skupine
hirar = tsclust(po, type = "hierarchical", k = 4L, distance = "dtw")

#izrišemo hirarhični diagram
plot(hirar, 
     ylab = "dtw distenc",
     main = "države",
     sub = "", hang = -1, cex=0.7)


## ker se namo rezultai ujemajo pom nadalje graf risal z privzetimi funkcijami
# ker so stvari enostavnejše in dobro narejene

###### iskanje mediane

mediana = function(distančna_mat){
  distančna_mat = distančna_mat + t(distančna_mat)
  matrik = data.frame(distančna_mat)
  matrik = matrik %>% mutate(vsota = rowSums(matrik))
  medi = min(matrik$vsota)
  matrik = matrik %>% filter(vsota == medi)
  return(matrik)
}

#naš dtw
s = mediana(mat)
#vgrajeni dtw
ss = mediana(mt)

#graf skupin
plot(hirar, type="sc")

###izris osem skupin
hirarhical = tsclust(po, type = "hierarchical", k = 8L, distance = "dtw")
plot(hirarhical, type="sc")

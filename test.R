library(readxl)
library(dplyr)
library(tidyr)
###################
a <- c(0, 0, 1, 1, 0, 0, -1, 0, 0, 0, 0) # vzorec
b <- c(0, 0, 0, 0, 1, 1, 0, 0, 0, -1, -0.5, 0, 0)
e= dtw1(a,b)
f = dtw(a,b)

dtw_p <- function(a, b) {
  
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
  
  # prilagodi, glede na to kaj potrebuješ
  return(list(cene, pot, cene[m+1,n+1]))
}

lol = dtw_p(ts1,ts2)
#################


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
  
  # prilagodi, glede na to kaj potrebuješ
  return(list(cene, pot, cene[m+1,n+1]))
}

indeks = read_excel("index.xlsx")

indeks = indeks[,c(1,6,7)]

indeks$LOCATION[duplicated(indeks$LOCATION)==FALSE]

opazovane = indeks$LOCATION[duplicated(indeks$LOCATION)==FALSE]

####################
ts1 = indeks %>% filter(LOCATION == opazovane[1])
ts1 = ts1$Value
ts2 = indeks %>% filter(LOCATION == opazovane[2])
ts2 = ts2$Value

poizvedba = dtw1(ts1,ts2)
pop = dtw(ts1,ts2)
poppp= dtw_basic(ts1,ts2)

##########################

library(dtw)

distančna_matrika = function(podatki,opazovane) {
  matrika = matrix(, length(opazovane), ncol =length(opazovane),dimnames = list(opazovane, opazovane))
  for (d1 in 1:length(opazovane)){
    for (d2 in (d1):length(opazovane)){
      ts1 = podatki %>% filter(LOCATION == opazovane[d1])
      ts1 = ts1$Value
      ts2 = podatki %>% filter(LOCATION == opazovane[d2])
      ts2 = ts2$Value
      
      rezultat = dtw(as.numeric(ts1),as.numeric(ts2))
      matrika[d2,d1] =  rezultat[[9]]
      
    }
    
  }
  
  matrika
}

mat = distančna_matrika(indeks,opazovane)
mat[is.na(mat)] = 0
M = as.dist(mat)
M
rezultat  = M %>% hclust(method = "average")
plot(rezultat, 
     ylab = "dtw distenc",
     main = "države",
     sub = "", hang = -1, cex=0.7)

ds = function(podatki,opazovane) {
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

mt = ds(indeks,opazovane)
mtt[is.na(mat)] = 0
Mt = as.dist(mat)
Mt
rezultat  = Mt %>% hclust(method = "average")
plot(rezultat, 
     ylab = "dtw distenc",
     main = "države",
     sub = "", hang = -1, cex=0.7)

kos = rezultat %>% cutree(k = 3)

skupine_kos = data.frame(skupina = kos, 
                         LOCATION = names(kos)) %>% arrange(kos)
rownames(skupine_kos) = NULL

skupine_kos

tabela = inner_join(indeks,skupine_kos,by="LOCATION")
library(dplyr)
library(tidyverse)

digram1 = ggplot(tabela, aes(x=TIME,y=Value,color=skupina)) + geom_line()+
  xlab("Leto") + ylab("indeks")
digram1

v=seq(1,3300,10) -1
v = v[2:length(v)]

zab = tabela %>% filter(TIME != "2022-11")
digram2 = ggplot(zab[v,], aes(x=TIME,y=Value,group=LOCATION,color=skupina))+
  xlab("Leto") + ylab("indeks")+geom_point()+ geom_line()+
  scale_color_manual(name = "skupina", labels = c("1", "2","3"),values=c('darkorchid','dodgerblue1',"red"))
digram2

########## preizkus z dano funkcijami
library(dtwclust)
t = indeks %>% pivot_wider(names_from = "TIME",values_from="Value")

time_seris = indeks %>% pivot_wider(names_from = "TIME",values_from="Value")

time_seris= time_seris[,-1]
rownames(time_seris) = t$LOCATION

po = tslist(time_seris)
hirar = tsclust(po, type = "hierarchical", k = 4L, distance = "dtw")

plot(hirar, 
     ylab = "dtw distenc",
     main = "države",
     sub = "", hang = -1, cex=0.7)

plot(hirar, type="sc")



v_skupine = tibble(
  k = length(hirar$height):1,
  visine = hirar$height
) %>%
  arrange(k)
v_skupine

v_skupine %>%
  ggplot() +
  aes(
    x = k, y = visine
  ) +
  geom_line() +
  geom_point() +
  theme_classic()


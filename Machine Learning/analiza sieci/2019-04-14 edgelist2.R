simply_network <- read.csv('X:/siec/EdgeList2.csv')

#tworzenie edglist w stylu igraph style
EdgeList2 <- simply_network
#tworzymy wykres dla kolejnych analiz, decydujemy czy graf bedzie skierowany czy nie
Prosty_Graph=graph.data.frame(EdgeList2, directed=TRUE)
plot(Prosty_Graph)


#rysowanie grafu , tworzenie grafu Fruchterman-Reingold
layout <- layout.fruchterman.reingold(Prosty_Graph)
#ustawianie kolorów
V(Prosty_Graph)$color <- "grey"
E(Prosty_Graph)$color <- "grey"
plot(Prosty_Graph)

#rysowanie grafu
set.seed(200)
plot(Prosty_Graph)

#dodatkowe - ustawianie waznych wierzcho³kow na inny kolor (np liczba krawedzi powzyej 2)
V(Prosty_Graph)$color <- "grey"
V(Prosty_Graph)[degree(Prosty_Graph, mode="in")>2]$color <- "yellow"
plot(Prosty_Graph)

#kolor krawedzi
E(Prosty_Graph)$color <- "grey"

#obliczanie miar sieci - gestosc
#density
graph.density(Prosty_Graph)
#jezeli w sieci sa petle to do obliczania mozna je wylaczyc
graph.density(Prosty_Graph, loop=FALSE)
edge_density(Prosty_Graph, loops = F)
#obliczanie srednich sciezek
#average path length
mean_distance(Prosty_Graph)
#degree distribution
degree_distribution(Prosty_Graph)
#zamienia dane tak, aby mozna je bylo eksportowac
Prosty_DegreeDis <- degree.distribution(Prosty_Graph)
Prosty_DegreeDis2 <- as.data.frame(Prosty_DegreeDis)
#wykres stopni
hist(Prosty_DegreeDis)

#Klastry
#clustering coefficeint
transitivity(Prosty_Graph) # <- mierzy prawdopodobienstwo ze sasiednie wierzcholki wierzcholka sa polaczone. jest to czasami nazywane wspolczynnikiem grupowania.
#closeness
closeness(Prosty_Graph, mode="all") # <-  mierzy, ile krokow jest wymaganych aby uzyskac dostep do kazdego innego wierzcholka z danego wierzcholka



y <- data.frame(EdgeList2$Nadawca, EdgeList2$Odbiorca)
net <- graph.data.frame(y,directed = T)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)
#histogram liczby polaczen
hist(V(net)$degree,
     col='green',
     main= 'Hisogram liczby polaczen',
     ylab='Frequency',
     xlab='Degree')

#wykres
set.seed(200) #opcjonalnie
plot(net)

set.seed(200)

plot(net,
     vertex.color='green',
     vertex.size=5,
     edge.arrow.size= 0.2,
     vertex.label.cex = 0.8)

#############


## POPRAWIANIE WYGL¥DU
set.seed(200)
plot(net,
     vertex.color=rainbow(52),
     vertex.size=V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)  # graphot  # kamada.kawai - zamiast friuchterman.reingold

set.seed(200)
plot(net,
     vertex.color=rainbow(52),
     vertex.size=V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.graphopt)

set.seed(200)
plot(net,
     vertex.color=rainbow(52),
     vertex.size=V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.kamada.kawai)


set.seed(200)
plot(net,
     vertex.color=rainbow(52),
     vertex.size=sqrt(V(net)$degree)*3,
     edge.arrow.size = 0.1,
     layout=layout.kamada.kawai)

# drzewko
plot(net,
     vertex.color="grey",
     adge.arrow.size=0.1,
     layout=layout.reingold.tilford(net, root="CC"))

# drzewko
plot(net,
     vertex.color="grey",
     adge.arrow.size=0.1,
     layout=layout.reingold.tilford(net, root="CA"))

## BETWEENNESS

betweenness(net)

plot(net,
     vertex.color="green",
     vertex.size=sqrt(betweenness(net)),
     edge.arrow.size = 0.1,
     layout=layout.kamada.kawai)


plot(net,
     vertex.color="green",
     vertex.size=sqrt(betweenness(net)/2+5),
     edge.arrow.size = 0.1,
     layout=layout.kamada.kawai)


### OPCJA NITER
set.seed(200)
plot(net,
     vertex.color="grey",
     vertex.size=sqrt(betweenness(net)),
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold(net, niter=1000))


### USUNIÊCIE PÊTLI ORAZ WIELOKTROTNYCH PO£¥CZEÑ

net2 <- simplify(net, remove.multiple = TRUE, remove.loops = TRUE)

plot(net2,
     vertex.color="grey",
     vertex.size=sqrt(betweenness(net)),
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold(net2, niter=1000))

plot(net2,
     vertex.color=rainbow(52),
     vertex.size=sqrt(V(net2)$degree)*4,
     edge.arrow.size = 0.1,
     layout=layout.kamada.kawai(net2, niter=1000))

# histogram liczby po³¹czeñ
hist(V(net2)$degree,
     col='green',
     main= 'Histogram liczby po³¹czeñ',
     ylab='Frequency',
     xlab='Degree')

set.seed(200)
plot(net2,
     vertex.color=rainbow(52),
     vertex.size=V(net2)$degree,
     edge.arrow.size = 0.1,
     layout=layout.kamada.kawai)


## HUBS(OUT) AND AUTHORITIES(IN)

hs <- hub_score(net)$vector
as <- authority.score(net)$vector

set.seed(200)
plot(net,
     vertex.size=hs*30,
     main='Hubs',
     vertex.color=rainbow(52),
     edge.arrow.size=0.1,
     layout=layout.kamada.kawai)

set.seed(200)
plot(net,
     vertex.size=as*30,
     main='Authorities',
     vertex.color=rainbow(52),
     edge.arrow.size=0.1,
     layout=layout.kamada.kawai)

hs2 <- hub_score(net2)$vector
as2 <- authority.score(net2)$vector

set.seed(200)
plot(net2,
     vertex.size=hs2*30,
     main='Hubs',
     vertex.color=rainbow(52),
     edge.arrow.size=0.1,
     layout=layout.kamada.kawai)

set.seed(200)
plot(net2,
     vertex.size=as2*30,
     main='Authorities',
     vertex.color=rainbow(52),
     edge.arrow.size=0.1,
     layout=layout.kamada.kawai)


## KLASTRY

set.seed(200)

net <- graph.data.frame(y, directed = F)

cnet <- cluster_edge_betweenness(net)

plot(cnet,
     net,
     vertex.size = 10,
     vertex.label.cex = 0.8)





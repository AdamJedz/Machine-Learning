library(igraph)

simply_network <- read.csv('x:/siec/Prosta_Edgelist.csv')

#Tworzenie Edgelsit w stylu iGraph Style

Prosta_EdgeList <- simply_network

# tworzymym wykres dla kolejnych analiza, decydujemy czy graf bêdzie skierowany czy nie

Prosty_Graph=graph.data.frame(Prosta_EdgeList, directed = FALSE)
plot(Prosty_Graph)


# tworzenie grafu Fruchterman-Reingold
layout <- layout.fruchterman.reingold(Prosty_Graph)


# ustawienie kolorów
V(Prosty_Graph)$color <- "grey"
E(Prosty_Graph)$color <- "grey"

# rysowanie grafu
set.seed(200)
plot(Prosty_Graph)


# ustawianie wa¿nych wierzcho³ków na inny kolor (liczba krawêdzi powy¿ej 2)
V(Prosty_Graph)$color <- "grey"
V(Prosty_Graph)[degree(Prosty_Graph, mode = "in")>2]$color <- "yellow"

plot(Prosty_Graph)

### OBLICZANIE MIAR SIECI ###

## Density (wynik 0.3809524)
graph.density(Prosty_Graph)

# je¿eli w sieci s¹ pêtle to do obliczania mo¿na je wy³¹czyæ (wynik 0.3809524)
graph.density(Prosty_Graph, loop=FALSE)
# lub
edge_density(Prosty_Graph, loops=F)

## Average Path Length (wynik 1.809524)
mean_distance(Prosty_Graph)


## Degreee Distrubution

# zamienia dane tak, aby mo¿na by³o je eksportowaæ
Prosty_DegreeDis <- degree_distribution(Prosty_Graph)

Prosty_DegreeDis2 <- as.data.frame(Prosty_DegreeDis)

# wykres stopni
hist(Prosty_DegreeDis)

## Clustering Coefficeint
# mierzy prawdopodobieñstwo, ¿e s¹siednie wierzcho³ki wierzcho³ka sa po³¹czone 
# jest to czasami nazywane wspó³czynnikiem grupowania (wynik 0.375)
transitivity(Prosty_Graph) 

##Closeness
# mierzy ile kroków jest wymaganych, aby uzyskaæ dostêp do ka¿dego innego wierzcho³ka z danego wierzcho³ka
closeness(Prosty_Graph, mode="all")

## WGRANIE DANYCH

simply_network <- read.csv('x:/siec/networkdata.csv')

#data <- read.csv(file.choose(), header=T)

y <- data.frame(data$first, data$second) 



## TWORZENIE SIECI

net <- graph.data.frame(y, directed = T)

V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

## HISTOGRAM LICZBY PO£ACZEÑ

hist(V(net)$degree,
     col='green',
     main = 'Histogram liczby po³¹czeñ',
     ylab = 'Frequency',
     xlab = 'Degree')

## WYKRES

set.seed(200) # opcjonalnie

plot(net)

set.seed(200)

plot(net,
     vertex.color='green',
     vertex.size=5,
     edge.arrow.size= 0.2,
     vertex.label.cex = 0.8)


















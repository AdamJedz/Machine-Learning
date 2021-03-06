library(igraph)

simply_network <- read.csv('x:/siec/Prosta_Edgelist.csv')

#Tworzenie Edgelsit w stylu iGraph Style

Prosta_EdgeList <- simply_network

# tworzymym wykres dla kolejnych analiza, decydujemy czy graf b�dzie skierowany czy nie

Prosty_Graph=graph.data.frame(Prosta_EdgeList, directed = FALSE)
plot(Prosty_Graph)


# tworzenie grafu Fruchterman-Reingold
layout <- layout.fruchterman.reingold(Prosty_Graph)


# ustawienie kolor�w
V(Prosty_Graph)$color <- "grey"
E(Prosty_Graph)$color <- "grey"

# rysowanie grafu
set.seed(200)
plot(Prosty_Graph)


# ustawianie wa�nych wierzcho�k�w na inny kolor (liczba kraw�dzi powy�ej 2)
V(Prosty_Graph)$color <- "grey"
V(Prosty_Graph)[degree(Prosty_Graph, mode = "in")>2]$color <- "yellow"

plot(Prosty_Graph)

### OBLICZANIE MIAR SIECI ###

## Density (wynik 0.3809524)
graph.density(Prosty_Graph)

# je�eli w sieci s� p�tle to do obliczania mo�na je wy��czy� (wynik 0.3809524)
graph.density(Prosty_Graph, loop=FALSE)
# lub
edge_density(Prosty_Graph, loops=F)

## Average Path Length (wynik 1.809524)
mean_distance(Prosty_Graph)


## Degreee Distrubution

# zamienia dane tak, aby mo�na by�o je eksportowa�
Prosty_DegreeDis <- degree_distribution(Prosty_Graph)

Prosty_DegreeDis2 <- as.data.frame(Prosty_DegreeDis)

# wykres stopni
hist(Prosty_DegreeDis)

## Clustering Coefficeint
# mierzy prawdopodobie�stwo, �e s�siednie wierzcho�ki wierzcho�ka sa po��czone 
# jest to czasami nazywane wsp�czynnikiem grupowania (wynik 0.375)
transitivity(Prosty_Graph) 

##Closeness
# mierzy ile krok�w jest wymaganych, aby uzyska� dost�p do ka�dego innego wierzcho�ka z danego wierzcho�ka
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

## HISTOGRAM LICZBY PO�ACZE�

hist(V(net)$degree,
     col='green',
     main = 'Histogram liczby po��cze�',
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


















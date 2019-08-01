library(igraph)

##13.04.2019

#nieskierowany
g<-graph.formula(1-2,1-3,2-3,2-4,3-5,4-5,4-6,4-7,5-6,6-7)
V(g)
E(g)
plot(g)
vcount(g)
ecount(g)

#skierowany
dg <- graph.formula(1-+2,1-+3,2++3)
plot(dg)

#wprowadzenie grafu
#strza³ki
f <- graph(c(1,2,2,3,3,4,4,5,5,1))
plot(f)
#bez strza³ek
f <- graph(c(1,2,2,3,3,4,4,1),
           directed = F)
plot(f)

f[]
#dodanie nazw
V(dg)$name <- c("Maciek", "Tomek", "Marcin")
plot(dg)
get.edgelist(dg)
#doanie zmiennej
V(dg)$gender <- c("M","M","M")

#Opcje wizualzacji
#dodanie kolorów wêz³ów
V(g)$color <- "red"
plot(g)
#Opcje grafu
igraph.options(vertex.size=6, vertex.label=NA, edge.arrow.size=1)
plot(g, layout=layout.circle)

plot(g,
     vertex.color="green",vertex.size=6,vertex.label=NA, edge.color="red",
     edge.arrow.size=1,
     layout=layout.circle)
#layout
#circle, lattice, fruchterman.reingold, kamada.kawai, reingold.tilford


plot(g,
     vertex.color="green",vertex.size=6,vertex.label=NA, edge.color="red",
     edge.arrow.size=1,
     layout=layout.lattice)

plot(g,
     vertex.color="green",vertex.size=6,vertex.label=NA, edge.color="red",
     edge.arrow.size=1,
     layout=layout.fruchterman.reingold)

plot(g,
     vertex.color="green",vertex.size=6,vertex.label=NA, edge.color="red",
     edge.arrow.size=1,
     layout=layout.kamada.kawai)

plot(g,
     vertex.color="green",vertex.size=6,vertex.label=NA, edge.color="red",
     edge.arrow.size=1,
     layout=layout.reingold.tilford)

#layout dalej
#circular, radial, layered
plot(g, layout=layout.circle)

plot(g, layout=layout.reingold.tilford(g, circular=T))
plot(g, layout=layout.kamada.kawai)
plot(g, layout=layout.reingold.tilford)

#analiza krawêdzi,
###liczba polaczen, dla grafow skierowanych mozna dodac mode= all, in, out
degree(g)
degree(dg,mode='in')
hist(degree(g), col="blue", xlab="Degree", ylab="Frequency")
mean(degree(g))
average.path.length(g)
#density
edge_density(g,loops=F)
ecount(g)/(vcount(g)*(vcount(g)-1)/2) #ile jest do ilu mozliwych

#closeness i betweenness

closeness(g, mode="all", weights = Na)
eb <- edge.betweenness(g)
E(g)[order(eb,decreasing = T)[1:3]]
plot(g)
betweenness(g,directed = F, weights=NA)
#klastry
kc <- fastgreedy.community(g)
length(kc) #ilosc klastrow
sizes(kc)
membership(kc) #do ktorego jaki nalezy
plot(kc,g)
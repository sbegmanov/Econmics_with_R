library(netrankr)
library(tidygraph)
library(intergraph)
library(ggnetwork)
require(ggplot2)
library(tidyverse)
library(tidygraph)

from <- c("Asif","Deng","Gita","Paul","Sure", "Sure")
to <- c("Gita","Gita","Anne","Anne","Paul", "Asif")

edge1 <- tibble(from, to)
str(edge1)

Talk <- tbl_graph(edges = edge1, directed = FALSE)
print(Talk)
class(Talk)

# activation to play
Talk %>%
  activate(edges) %>%
  as_tibble()
class(Talk)

Talk %>%
  activate(nodes) %>%
  as_tibble()
class(Talk)

library(intergraph)
# convert Talk object to a network object
Talk_n <- asNetwork(Talk)
print(Talk_n)
class(Talk_n)

library(ggnetwork)
# convert Talk_n to a data frame
Talk_g <- ggnetwork(Talk_n)
class(Talk_g)

# Plotting
Talk_GG <- ggplot(Talk_g, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "lightgrey") +
  geom_nodes(alpha = 0.6, size = 5) +
  geom_nodetext(aes(label = vertex.names), col = "blue") +
  theme_blank()
print(Talk_GG)
class(Talk_GG)

# different geoms
ggnetplot <- function(Net = Bali) {
  Net <- ggnetwork(Net, layout = "kamadakawai")
  ggplot(Net, aes(x,y, xend = xend, yend = yend)) +
    geom_edges(col = "tomato") +
    # node text repelled from node
    geom_nodetext_repel(aes(label = vertex.names), col = "black", size = 3) +
    theme_blank()
}

ggnetplot(Talk_n)

#### medici family example
data("florentine_m")
class(florentine_m)

flor <- as_tbl_graph(florentine_m)
class(flor)

flor_g <- asNetwork(flor)
class(flor_g)

# ggnetplot uses ggnetwork() and ggplot()
ggnetplot(flor_g)

flor2 <- flor %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree())

## extract wealth and degree by family and arrange data by degree
flor3 <- as_tibble(flor2) %>%
  arrange(-degree)
class(flor3)
print(flor3)

# use ggrepel to get uncluttered labels
library(ggrepel)

ggplot(flor3, aes(x = wealth, y = degree, label = name)) +
  geom_point() + 
  # position labels appropriately
  geom_text_repel()

#### Bali terrorist network
# install.packages("devtools") devtools::install_github("DougLuke/UserNetR")

library(UserNetR)
data("Bali")
class(Bali)

Bali_t <- as_tbl_graph(Bali)
print(Bali_t)

ggnetplot(Bali)
# by role of member, CT(command team), OA(operational assistant), BM(Bomb Maker)
# SB(suicide bomber), TL (team lime)

Bali_g <- ggnetwork(Bali)
ggplot(Bali_g, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(col = "tomato", alpha = 0.6) +
  geom_nodetext_repel(aes(label = role), size = 3) +
  theme_blank()

### Simulating Network Formation
# Erdos Renyi model of random networks where two nodes connect or 
#  do not with a given probability, at random
#Barabasi and Albert model where networks form over time and each new node
# prefers to attach itself to well connected node_similarity_with()

# n - number of nodes, p - probability of an edge connecting any two vertices

rg <- play_erdos_renyi(n = 30, p = 0.2, directed = FALSE)
class(rg)

rg2 <- rg %>%
  activate(nodes) %>%
  mutate(Deg = centrality_degree())

rg3 <- rg2 %>%
  activate(nodes) %>%
  as_tibble

# degree distribution from Erdos Renyi simulation
ggplot(rg3, aes(x = Deg)) +
  geom_bar()

rg_g <- asNetwork(rg2)
rg_g <- ggnetwork(rg_g, layout = "kamadakawai")
ggplot(rg_g, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(col = "tomato") +
  geom_nodes(aes(size = Deg), alpha = 0.4, size = 1, col = "black") +
  theme_blank()

# Barabasi and Albert model based on the concept of preferential attachment
ba <- play_barabasi_albert(n = 30, power = 1, directed = FALSE)
class(ba)

ba2 <- ba %>%
  activate(nodes) %>%
  mutate(Deg = centrality_degree())

ba3 <- ba2 %>%
  activate(nodes) %>%
  as_tibble()

ggrg3 <- ggplot(rg3, aes(x = Deg)) +
  geom_bar()

ggba3 <- ggplot(ba3, aes(x = Deg)) +
  geom_bar()

library(gridExtra)

grid.arrange(ggrg3, ggba3, ncol = 2)

ba_g <- asNetwork(ba2)
ba_g <- ggnetwork(ba_g, layout = "kamadakawai")


ggrg <- ggplot(rg_g, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(col = "tomato") +
  geom_nodes(aes(size = Deg), alpha = 0.4, size = 1, col = "black") +
  theme_blank()

ggba <- ggplot(ba_g, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(col = "tomato") +
  geom_nodes(aes(size = Deg), alpha = 0.4, size = between, col = "black") +
  geom_nodetext(aes(label = vertex.names), col = "black", size = 5) +
  theme_blank()

grid.arrange(ggrg, ggba, ncol = 2)


#### Electrical Automotive Goods Production Network
library(tidygraph)
library(ITNr)
data("ELEnet16")
class(ELEnet16)

ELE <- as_tbl_graph(ELEnet16)
class(ELE)
summary(ELE)

# data wrangling
library(ggnetwork)
ELE_n <- asNetwork(ELE)
ELE_g <- ggnetwork(ELE_n)
str(ELE_g$regionNAME)

ELE_g <- ELE_g %>%
  mutate(region_name = factor(regionNAME))

library(tidyverse)

# electrical automative good 2016 network
ELE_g <- ELE_g %>%
  mutate(region_name = fct_recode(region_name,
                                  "Sub-SahAfr" = "Sub-Saharan Africa (all income levels)",
                                  "mEast&NAf" = "Middle East & North Africa (all income levels)",
                                  "LatAm&Car" = "Latin America & Caribbean (all income levels)",
                                  "Eur&CAsia" = "Europe & Central Asia (all income levels)",
                                  "EAsia&Pac" = "East Asia & Pacific (all income levels)" ))
ggplot(ELE_g, aes(x, y, xend = xend, yend = yend, col = region_name)) +
  geom_edges(color = "grey70") +
  geom_nodes(alpha = 0.6, size = 5) +
  theme_blank() +
  theme(legend.position = "right") +
  scale_colour_brewer(palette = "Dark2")

# weighted outdegree measure, edge weights are proportion of global trade
ELE2 <- ELE %>%
  activate(nodes) %>%
  mutate(outdeg = centrality_degree(weights = weight, mode = "out")) %>%
  as_tibble()

ELE3 <- ELE2 %>%
  select("name", "regionNAME", "outdeg") %>%
  arrange(-outdeg)

library(xtable)
xtable(ELE3[1:10, ], caption = "Top 10 in weighted outdegree")

ITN3 <- ITNcentrality(ELEnet16) %>%
  as_tibble() %>%
  arrange(-Weighted.Out.Degree)
ITN3[1:10,]

library(ggplot2)

# weighted outdegree distribution, positively skewed
ggplot(ELE2, aes(x = outdeg)) +
  geom_histogram(bins = 30)

# weighted outdegree distribution by region
ELE2 <- ELE2 %>%
  mutate(region_name = factor(regionNAME))

library(tidyverse)
ELE2 <- ELE2 %>%
  mutate(region_name = fct_recode(region_name,
                                  "Sub-SahAf" = "Sub-Saharan Africa (all income levels)",
                                  "mEast&NAf" = "Middle East & North Africa (all income levels)",
                                  "LatAm&Car" = "Latin America & Caribbean (all income levels)",
                                  "Eur&CAsia" = "Europe & Central Asia (all income levels)",
                                  "EAsia&Pac" = "East Asia & Pacific (all income levels)" ))

library(ggplot2)

ggplot(ELE2, aes(x = region_name, y = outdeg)) +
  geom_boxplot() +
  coord_flip()


########################### Tasks #####################################
from <- c("Kim", "Tim", "Kim", "Jim")
to <- c("Jim", "Jim", "Jane", "Jane")

edge2 <- tibble(from, to)
Talk2 <- tbl_graph(edges = edge2, directed = FALSE)
Talk_n2 <- asNetwork(Talk2)
Talk_g2 <- ggnetwork(Talk_n2)

ggnetplot <- function(Net = Bali) {
  Net <- ggnetwork(Net, layout = "kamadakawai")
  ggplot(Net, aes(x, y, xend = xend, yend = yend)) +
    geom_edges(col = "tomato") +
    # node text repelled from node
    geom_nodetext_repel(aes(label = vertex.names), col = "black", size = 3) +
    theme_blank()
}

ggnetplot(Talk_n2)


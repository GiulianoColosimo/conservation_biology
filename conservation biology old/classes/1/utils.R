library(dplyr)
library(ggplot2)
library(reshape2)

library(readxl)
library(diagram)
library(grid)
library(gridExtra)
library(plotrix)
library(tidyverse)
library(dendextend)
library(vegan)
library(gt)
library(gtExtras)
library(gtsummary)
library(knitr)
library(kableExtra)

deforestation <- read.csv("./classes/1/data_treecover_loss_by_region__ha.csv", header = T)

png("./classes/1/deforestation.png", width = 12, height = 8, units = "in", res = 300)
ggplot(deforestation, aes(fill=iso, y=umd_tree_cover_loss__ha, x=umd_tree_cover_loss__year)) + 
  geom_bar(position="fill", stat="identity") +
  guides(fill="none") +
  xlab("Year") +
  ylab("Tree cover loss (ha)") +
  scale_y_continuous(labels = scales::percent_format())
dev.off()


ffcons <- read.csv("./classes/1/data_global-fossil-fuel-consumption.csv", header = T)
names(ffcons) <- c("Entity", "Code", "Year", "Gas", "Oil", "Coal")
ffcons_long <- melt(ffcons,                               
                    id.vars = c("Entity", "Code", "Year"))

png("./classes/1/ffconsumption.png", width = 12, height = 8, units = "in", res = 300)
ggplot(ffcons_long, aes(x=Year, y=value, color=variable)) +
  geom_point(shape=1) +
  geom_line() +
  xlab("Year") +
  ylab("TWh") +
  theme(legend.title = element_blank())
dev.off()  



glplan <- read.csv("./classes/1/data_global-living-planet-index.csv", header = T)
glplan_wrld <- glplan[glplan$Entity == "World", ]
glplan_wrld <- melt(glplan_wrld,                               
                    id.vars = c("Entity", "Code", "Year"))

png("./classes/1/glplan_wrld.png", width = 12, height = 8, units = "in", res = 300)
ggplot(glplan_wrld, aes(x=Year, y=value, color=variable)) +
  geom_point(shape=1) +
  geom_line() +
  xlab("Year") +
  ylab("Abundance (%)") +
  theme(legend.title = element_blank())
dev.off()  







pos <- coordinates(c(1, 1, 1, 1, 1, 1))
pos[1,1] <- 0.1
pos[2,1] <- 0.31
pos[3,1] <- 0.51
pos[4,1] <- 0.71
pos[5,1] <- 0.91
pos[6,1] <- 0.5

my_label <- c("Genes",
              "Species",
              "Habitats",
              "Landscapes",
              "Ecosystems",
              "Biodiversity")

my_text_size <- 1.5
my_edge_length <- .08

png("./classes/2/biodiversityPlot.png", width = 12, height = 8, units = "in", res = 300)
par(mar = rep(1, 4))
openplotmat()
straightarrow(from = pos[1, ], to = pos[6, ], lcol = "blue", lwd = 4, arr.col = "red", lty = 2)
straightarrow(from = pos[2, ], to = pos[6, ], lcol = "blue", lwd = 4, arr.col = "red", lty = 2)
straightarrow(from = pos[3, ], to = pos[6, ], lcol = "blue", lwd = 4, arr.col = "red", lty = 2)
straightarrow(from = pos[4, ], to = pos[6, ], lcol = "blue", lwd = 4, arr.col = "red", lty = 2)
straightarrow(from = pos[5, ], to = pos[6, ], lcol = "blue", lwd = 4, arr.col = "red", lty = 2)
bentarrow(from = pos[1, ], to = pos[2, ], lcol = "blue", lwd = 4, arr.col = "red", lty = 2)
bentarrow(from = pos[2, ], to = pos[3, ], lcol = "blue", lwd = 4, arr.col = "red", lty = 2)
bentarrow(from = pos[3, ], to = pos[4, ], lcol = "blue", lwd = 4, arr.col = "red", lty = 2)
bentarrow(from = pos[4, ], to = pos[5, ], lcol = "blue", lwd = 4, arr.col = "red", lty = 2)

for(i in 1:length(my_label)){
  if (i == 1) {
    textrect(mid = pos[i,],
             radx = my_edge_length,
             rady = my_edge_length, 
             lab = my_label[i], 
             cex = my_text_size, 
             box.col = "red", col = "white")
  } else if (i == 2){
    textrect(mid = pos[i,],
             radx = my_edge_length,
             rady = my_edge_length,
             lab = my_label[i],
             cex = my_text_size,
             box.col = "red", col = "white")
  } else if (i == 3){
    textrect(mid = pos[i,],
             radx = my_edge_length,
             rady = my_edge_length,
             lab = my_label[i],
             cex = my_text_size,
             box.col = "red", col = "white")
  } else if (i == 4){
    textrect(mid = pos[i,],
             radx = my_edge_length,
             rady = my_edge_length,
             lab = my_label[i],
             cex = my_text_size,
             box.col = "red", col = "white")
  } else if (i == 5){
    textrect(mid = pos[i,],
             radx = my_edge_length,
             rady = my_edge_length,
             lab = my_label[i],
             cex = my_text_size,
             box.col = "red", col = "white")
  } else if (i == 6){
    textrect(mid = pos[i,],
             radx = my_edge_length,
             rady = my_edge_length,
             lab = my_label[i],
             cex = my_text_size,
             box.col = "green")
  } 
}
dev.off()





redlstcats <- data.frame(var1 = c(8, 0, 0, 0, 0, 0, 0, 0, 0),
                         var2 = c(0, 7, 0, 0, 0, 0, 0, 0, 0),
                         var3 = c(0, 0, 6, 0, 0, 0, 0, 0, 0),
                         var4 = c(0, 0, 0, 5, 0, 0, 0, 0, 0),
                         var5 = c(0, 0, 0, 0, 4, 0, 0, 0, 0),
                         var6 = c(0, 0, 0, 0, 0, 3, 0, 0, 0),
                         var7 = c(0, 0, 0, 0, 0, 0, 2, 0, 0),
                         var8 = c(0, 0, 0, 0, 0, 0, 0, 1, 0),
                         var9 = c(0, 0, 0, 0, 0, 0, 0, 0, 0))

rownames(redlstcats) <-  c("Not Evaluated (NE)",
                           "Data Deficient (DD)",
                           "Least Concern (LC)",
                           "Near Threatened (NT)",
                           "Vulnerable (VU)",
                           "Endangered (EN)",
                           "Critically Endangered (CR)",
                           "Extinct in the Wild (EW)",
                           "Extinct (EX)")

redlstcats %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() -> dend

png("./classes/2/iucnredlist.png", width = 12, height = 8, units = "in", res = 300)
par(mar=c(4.3,1,1,15))  # Increase bottom margin to have the complete label
redlistcls <- c("beige", "gray", "green4", "greenyellow", "yellow", "orange", "red", "maroon4", "black")
dend %>% 
  #set("labels_colors", redlistcls) %>% 
  plot(horiz=TRUE, axes=FALSE) 
colored_bars(redlistcls, dend, horiz = T, rowLabels = "Red List \nCategories")
dev.off()


data(BCI)
plantDivExcercise <- BCI[1:10, 
                         c(144, 186, 208, 216, 223)]
species_names <- rep(NA, ncol(plantDivExcercise))

for (i in 1:ncol(plantDivExcercise)){
  species_names[i] <- paste("Sp_", i, sep = "")
}

names(plantDivExcercise) <- species_names
Site <-  1:10
plantDivExcercise <- cbind(Site, plantDivExcercise)

png("./classes/2/planttable.png", width = 12, height = 8, units = "in", res = 300)
kbl(plantDivExcercise)
dev.off()

kbl(plantDivExcercise[1:4, ])
kbl(plantDivExcercise[6:9, ], row.names = F)





# Exponential growth
expgrowth <- NULL
for(i in 1:15){
  expgrowth <- c(expgrowth, 2^i)
}

png("./classes/3/exponentialgrowth.png", width = 12, height = 8, units = "in", res = 300)
plot(expgrowth, 
     xlab = "Time",
     ylab = "Pop. size (N)",
     lwd=4,
     cex.lab=1.5,
     type = "l", xaxt='n',
     yaxt='n'#, 
     #main = "Exponential growth curve"
     )
dev.off()


# Logaritmic growth

logPopGrowth <- function(k, n, r){
  
  loggrowth <- NULL
  
  while (n < k) {
    print(n)
    n <- n*r*((k-n)/k)
    loggrowth <- c(loggrowth, n)
    
    if(length(loggrowth) > 1){
      if (loggrowth[length(loggrowth)-1] == n) {
        break
      }
    }
    
  }
  
  return(loggrowth)
}

x <- logPopGrowth(10000, 2, 1.1)

png("./classes/3/logisticgrowth.png", width = 12, height = 8, units = "in", res = 300)
plot(x[1:150], xlab = "Time", ylab = "Pop. size (N)", type = "l",
     xaxt='n', yaxt='n', main = "Logistic growth curve", lwd = 4,
     cex.lab=1.5)
abline(h = max(x) + 1, col = "red", lty = 2, lwd = 4)
text(35, max(x) - 40, "Carrying capacity (K)", col = "red")
dev.off()





pos <- coordinates(c(2, 1, 3, 3, 3, 1, 1))
pos[1,1] <- 0.1
pos[1,2] <- 0.93
pos[2,1] <- 0.3
pos[2,2] <- 0.93
pos[3,1] <- 0.55
pos[3,2] <- 0.85
pos[4,1] <- 0.2
pos[5,1] <- 0.4
pos[6,1] <- 0.8
pos[6,2] <- 0.74
pos[7,1] <- 0.2
pos[7,2] <- 0.45
pos[9,1] <- 0.7
pos[10,1] <- 0.2
pos[10,2] <- 0.25
pos[11,1] <- 0.4
pos[12,1] <- 0.7
pos[12,2] <- 0.25
pos[13,2] <- 0.2
pos[14,1] <- 0.35
pos[14,2] <- 0.05


my_label <- c("Population size",
              "Population structure",
              "Migration rates",
              "Genetic drift",
              "Inbreeding",
              "Hybridization",
              "Loss of genetic \ndiversity",
              "Gen.-by-env. \ninteraction",
              "Local adaptation",
              "Loss of adaptive \nvariation",
              "Inbreeding \ndepression",
              "Outbreeding \ndepression",
              "Demographic vital \nrates",
              "Population growth \nor viability")

my_text_size <- .8
my_edge_height <- .05
my_edge_width <- .08


par(mar = rep(2, 4))




png("./classes/3/popGenetics.png", width = 12, height = 8, units = "in", res = 300)
openplotmat()

straightarrow(from = pos[2, ],   to = pos[3, ],  lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
straightarrow(from = pos[1, ],   to = pos[4, ],  lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
straightarrow(from = pos[1, ],   to = pos[5, ],  lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
straightarrow(from = pos[3, ],   to = pos[4, ],  lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
bentarrow(    from = pos[3, ],   to = pos[5, ],  lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              path = "V", arr.pos = 0.1)
straightarrow(from = pos[3, ],   to = pos[9, ],  lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
straightarrow(from = pos[4, ],   to = pos[7, ],  lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.55)
straightarrow(from = pos[5, ],   to = pos[11, ], lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
bentarrow(    from = pos[6, ],   to = pos[12, ], lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              path = "V", arr.pos = 0.7, arr.side = 1)
straightarrow(from = pos[7, ],   to = pos[10, ], lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.55)
straightarrow(from = pos[8, ],   to = pos[9, ],  lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
bentarrow(    from = pos[8, ],   to = pos[11, ], lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              path = "V", arr.pos = 0.8, arr.side = 1)
straightarrow(from = pos[8, ],   to = pos[12, ], lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
straightarrow(from = pos[9, ],   to = pos[12, ], lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
straightarrow(from = pos[10, ],  to = pos[13, ], lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
straightarrow(from = pos[11, ],  to = pos[13, ], lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.55)
straightarrow(from = pos[12, ],  to = pos[13, ], lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)
bentarrow(    from = pos[13, ],  to = pos[14, ], lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              path = "V", arr.pos = 0.1)
bentarrow(    from = pos[14, ],  to = pos[1, ],  lcol = "black", lwd = 2, arr.col = "red", lty = 1,
              arr.pos = 0.5)

for(i in 1:length(my_label)){
  if (i == 1 | i == 2 | i == 4 | i == 5 | i == 6 | i == 7 | i == 13 | i == 14 ) {
    textrect(mid = pos[i,],
             radx = my_edge_width,
             rady = my_edge_height, 
             lab = my_label[i], 
             cex = my_text_size,
             shadow.size = 0.001,
             box.col = "blue", col = "white")
  } else {
    textrect(mid = pos[i,],
             radx = my_edge_width,
             rady = my_edge_height, 
             lab = my_label[i], 
             cex = my_text_size,
             shadow.size = 0.001,
             box.col = "red", col = "white")
  }
}
dev.off()





## Population genetics

-   Genetics (and genomic) have an important role in conservation biology and towards the protection of biodiversity

-   Darwin was the first to consider the importance of genetics and evolution in the persistence of natural populations. He postulated that because of the reduced population size of deer in British natural reserves they may experience loss of vigor [@Darwin1896]

-   The modern concern for genetics in conservation began in the 1970s when Frankel began to raise the alarm about the loss of primitive crop varieties and their replacement by genetically uniform cultivars [@Frankel1970; @Frankel1974]

------------------------------------------------------------------------
  
  -   As in other sciences, conservation genetics has benefited from the use of model organisms for experiments as well as modelistic and numerical approaches

-   How much gene flow is required to prevent the inbreeding effects of small population size?
  
  -   We can use species with short generation time and easy to control (*Drosophila*, peas, *Arabidopsis thaliana*) to make empirical experiments and answer the above question

-   Describing individuals from a molecular genetic standpoint can also help to better understand the basic biology of species and populations [@Allendorf2022]

------------------------------------------------------------------------
  
  -   Total population size can be estimated from the genotypes in populations that are difficult to sample with the classic methodologies [@Luikart2010]

-   Genetic analysis can help detecting cryptic effects of climate change on the distribution of species [@Allendorf2022]

-   @Gurgel2020 documented the reduction in genetic diversity after a heatwave despite the sea weed population had recovered

------------------------------------------------------------------------
  
  ![Interacting factors in conservation of natural populations. Difference between factors that could be adressed with traditional conservation genetics tools (neutral markers; in blue) and genomic approach (red). Adapted from @Allendorf2022](./popGenetics.png){width="80%"}

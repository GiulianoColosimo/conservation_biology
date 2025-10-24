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
library(plotly)
library(RColorBrewer)

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

png("./classes/4/logisticgrowth.png", width = 12, height = 8, units = "in", res = 300)
plot(x[1:150], xlab = "Time", ylab = "Pop. size (N)", type = "l",
     xaxt='n', yaxt='n', # main = "Logistic growth curve",
     lwd = 4,
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




png("./classes/5/popGenetics.png", width = 12, height = 8, units = "in", res = 300)
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


descSpec <- read.csv("./classes/3/number-of-described-species.csv", header = T)
descSpec <- descSpec %>% 
  filter(Year == 2022)
descSpec
names  <- descSpec[order(descSpec$Number.of.described.species), ]$Entity
specdata <- data.frame(count = sort(descSpec$Number.of.described.species),
                       name = factor(names,names),
                       y = seq(length(descSpec$Entity)) * 0.9)

plt <- ggplot(specdata) +
  geom_col(aes(count, name), fill = "blue", width = 0.6) +
  labs(x = "Count",
       y = "Name")
  

png("./classes/3/numspec.png", width = 12, height = 8, units = "in", res = 300)
plt
dev.off()

plt <- plt +
  scale_x_continuous(
    limits = c(0, 2200000),
    breaks = seq(0, 2200000, by = 500000), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "top"  # Labels are located on the top
  ) +
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)
  )

plt

plt <- plt + 
  geom_shadowtext(
    data = subset(specdata, count < 400000),
    aes(count, y = name, label = name),
    hjust = 0,
    nudge_x = 0.3,
    colour = BLUE,
    bg.colour = "white",
    bg.r = 0.2,
    family = "Econ Sans Cnd",
    size = 5
  ) + 
  geom_text(
    data = subset(specdata, count >= 400000),
    aes(0, y = name, label = name),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = "Econ Sans Cnd",
    size = 5
  )

plt


plt <- plt +
  labs(
    title = "Escape artists",
    subtitle = "Number of laboratory-acquired infections, 1970-2021"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 22
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 20
    )
  )
plt

# Make room for annotations
plt <- plt + 
  theme(
    plot.margin = margin(0.05, 0, 0.1, 0.01, "npc")
  )

# Print the ggplot2 plot
plt

# Add horizontal line on top
# It goes from x = 0 (left) to x = 1 (right) on the very top of the chart (y = 1)
# You can think of 'gp' and 'gpar' as 'graphical parameters'.
# There we indicate the line color and width
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#e5001c", lwd = 4)
)

grid.rect(
  x = 0,
  y = 1,
  width = 0.05,
  height = 0.025,
  just = c("left", "top"),
  gp = gpar(fill = "#e5001c", lwd = 0)
)

# We have two captions, so we use grid.text instead of 
# the caption provided by  ggplot2.
grid.text(
  "Sources: Laboratory-Acquired Infection Database; American Biological Safety Association", 
  x = 0.005, 
  y = 0.06, 
  just = c("left", "bottom"),
  gp = gpar(
    col = GREY,
    fontsize = 16,
    fontfamily = "Econ Sans Cnd"
  )
)
grid.text(
  "The Economist", 
  x = 0.005, 
  y = 0.005, 
  just = c("left", "bottom"),
  gp = gpar(
    col = GREY,
    fontsize = 16,
    fontfamily = "Milo TE W01"
  )
)


plot_ly(descSpec,
        x = ~Number.of.described.species,
        y = ~reorder(Entity, Number.of.described.species),
        name = "SF Zoo",
        type = "bar",
        orientation = "h") %>% 
  layout(xaxis = list(title = ''),
         yaxis = list(title = ''))



           


speclfsp <- data.frame(Taxon = c("All invertebrates",
                     "Marine invertebrates",
                     "Marine animals", 
                     "All fossil groups",
                     "Mammals",
                     "Cenozoic mammals",
                     "Diatoms",
                     "Dinoflagellates",
                     "Planktonic foraminifera",
                     "Echinoderms"),
           Source = c("[@Raup1978]",     
                      "[@Valentine1970]",
                      "[@Sepkoski1992]", 
                      "[@Simpson1952]",  
                      "[@Martin1993]",   
                      "[@Raup1978b]",    
                      "[@VanValen1973]", 
                      "[@VanValen1973]", 
                      "[@VanValen1973]", 
                      "[@Durham1970]") ,
           Average = c("11",
                       "5-10 ",
                       "5",
                       "0.5-5",
                       "5",
                       "1-2  ",
                       "8",
                       "13",
                       "7",
                       "6"
           ))

kbl(speclfsp) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)







p <- seq(0, 1, by = 0.1)
q <- 1-p
het <- 2*(p*q)

png("./classes/5/hweplot.png", width = 12, height = 8, units = "in", res = 300)
plot(p^2, type = "b", xaxt = "n", col = "red",
     xlab = "Allele Frequency",
     ylab = "Genotype Frequency", pch = 1)
axis(1, at = 1:length(p), labels = p)
points(q^2, type = "b", col = "blue", pch = 2)
points(het, type = "b", col = "forestgreen", pch = 3)
legend("top",
       legend = c("A1A1", "A1A2", "A2A2"), 
       col = c("red", "forestgreen", "blue"), 
       pch = c(1, 3, 2), horiz = T)
dev.off()






# This code has been modified from prof. Gratton
newSample <- t(sapply(1:10000, function(i){table(factor(sample(c("A","a"), size = 30, prob = c(p,q), replace = T),
                                                        levels = c("A","a")))}))

png("./classes/5/100size30.png", width = 12, height = 8, units = "in", res = 300)
hist(newSample[,"A"]/30, 
     xlim = c(0,1), xlab = "f(A)", 
     breaks = seq(0, 1, by= 0.1),
     col = "darkgoldenrod3", 
     border = NA, 
     main = "10,000 samples of size 30",
     cex.axis = 2, cex.main = 2, cex.lab = 2)
abline(v = 0.637, lty = 2, lwd = 2)
dev.off()




newSample <- t(sapply(1:10000, function(i){table(factor(sample(c("A","a"), size = 100, prob = c(p,q), replace = T),
                                                        levels = c("A","a")))}))


pop.ls <- list(geneA = c(rep("A",50), rep("a",50)), 
               geneB = c(rep("B",50), rep("b",50)),
               geneC = c(rep("C",50), rep("c",50)))

pop.df <- data.frame(Gene = c(rep("geneA", 100),
                              rep("geneB", 100),
                              rep("geneC", 100)),
                     Alleles = matrix(unlist(pop.ls),
                                      ncol = 1, byrow=F))

png("./classes/5/geneticDrift1.png", width = 12, height = 8, units = "in", res = 300)
barplot(
  table(pop.df$Allele,
        pop.df$Gene)/nrow(pop.df[pop.df$Gene == "geneA",]),
  col = c("#14B635", "#158A29",
          "#23A9F6", "#2970A1",
          "#F77023", "#AF4E18"),
  xlab="",
  ylab="Allele frequencies",
  axes = F
)
axis(side = 2,
     labels = seq(from = 0, to = 1, by = 0.1),
     at = seq(from = 0, to = 1, by = 0.1))
lines(x = c(0.06,1,2,3,3.6),
      y = c(0.5, 0.5, 0.5, 0.5, 0.5),
      lty = 2, col = "red")
par(xpd=TRUE)
legend(3.7, 1, c("A", "a", "B", "b", "C", "c"),
       pch = rep(19, 4), col = c("#14B635", "#158A29",
                                 "#23A9F6", "#2970A1",
                                 "#F77023", "#AF4E18"),
       horiz = F, bty = "n")
dev.off()

set.seed(482)
pop.ls.t1 <- lapply(pop.ls, sample, 100, replace = T)
pop.df.t1 <- data.frame(Gene = c(rep("geneA", 100),
                                 rep("geneB", 100), 
                                 rep("geneC", 100)),
                        Alleles = matrix(unlist(pop.ls.t1),
                                         ncol = 1, byrow=F))

png("./classes/5/geneticDrift2.png", width = 12, height = 8, units = "in", res = 300)
barplot(
  table(pop.df.t1$Allele,
        pop.df.t1$Gene)/nrow(pop.df.t1[pop.df.t1$Gene == "geneA",]),
  col = c("#14B635", "#158A29",
          "#23A9F6", "#2970A1",
          "#F77023", "#AF4E18" ),
  xlab="",
  ylab="Allele frequencies",
  axes = F
)
axis(side = 2, labels = seq(from = 0, to = 1, by = 0.1),
     at = seq(from = 0, to = 1, by = 0.1))
lines(x = c(0.06,1,2,3,3.6), y = c(0.5, 0.5, 0.5, 0.5, 0.5),
      lty = 2, col = "red")
par(xpd=TRUE)
legend(3.7, 1, c("A", "a", "B", "b", "C", "c"),
       pch = rep(19, 4), col = c("#14B635", "#158A29",
                                 "#23A9F6", "#2970A1",
                                 "#F77023", "#AF4E18" ),
       horiz = F, bty = "n")
dev.off()

















set.seed(23561)
geneA <- vector(mode = "list", length = 200)
geneB <- vector(mode = "list", length = 200)
geneC <- vector(mode = "list", length = 200)
geneA[[1]] <- pop.ls[[1]]
geneB[[1]] <- pop.ls[[2]]
geneC[[1]] <- pop.ls[[3]]

for (i in 2:200) {
  geneA[[i]] <- sample(geneA[[i-1]], 100, replace = T)
  geneB[[i]] <- sample(geneB[[i-1]], 100, replace = T)
  geneC[[i]] <- sample(geneC[[i-1]], 100, replace = T)
}

freqA <- NULL
freqa <- NULL
freqB <- NULL
freqb <- NULL 
freqC <- NULL
freqc <- NULL 
for (i in 1:200) {
  freqA[i] <- sum(geneA[[i]] == "A")/ length(geneA[[i]])
  freqa[i] <- sum(geneA[[i]] == "a")/ length(geneA[[i]])
  freqB[i] <- sum(geneB[[i]] == "B")/ length(geneB[[i]])
  freqb[i] <- sum(geneB[[i]] == "b")/ length(geneB[[i]])
  freqC[i] <- sum(geneC[[i]] == "C")/ length(geneC[[i]])
  freqc[i] <- sum(geneC[[i]] == "c")/ length(geneC[[i]])
}

png("./classes/5/geneticDrift3.png", width = 12, height = 8, units = "in", res = 300)
plot(1,0, type="n", xlim=c(1,200), ylim=c(0,1),
     main = "",
     xlab="Generations", ylab="Allele frequencies")
points(1:200, freqA, pch = 21, col = "black", bg = "#14B635")
points(1:200, freqa, pch = 21, col = "black", bg = "#158A29")
points(1:200, freqB, pch = 21, col = "black", bg = "#23A9F6")
points(1:200, freqb, pch = 21, col = "black", bg = "#2970A1")
points(1:200, freqC, pch = 21, col = "black", bg = "#F77023")
points(1:200, freqc, pch = 21, col = "black", bg = "#AF4E18")
par(xpd=TRUE)
legend(180, .75, c("A", "a", "B", "b", "C", "c"),
       pch = rep(19, 4), col = c("#14B635", "#158A29",
                                 "#23A9F6", "#2970A1",
                                 "#F77023", "#AF4E18" ),
       horiz = F, bty = "n")
dev.off()













# simulare il tempo medio di fissazione degli alleli
pop.ls <- list(geneA = c(rep("A",50), rep("a",50)), 
               geneB = c(rep("B",50), rep("b",50)),
               geneC = c(rep("C",50), rep("c",50)))


# pop.ls  =  lista contenente i geni della popolazione
# num.sim = numero di volte la simulazione deve girare
sim.gen2fix <- function (pop.ls, num.sims) {
  
  # numero totale di loci per ogni gene   
  n <- length(pop.ls[[1]])
  # numero di geni differenti nella popolazione
  n.genes <- length(pop.ls)
  # lista vuota per i risultati della simulazione
  generations <- vector(mode = "list", length = length(pop.ls))
  for (h in 1:n.genes){
    generations[[h]] <- rep(0, num.sims)
  }
  names(generations) <- names(pop.ls)
  
  # ciclo di simulazione 
  for (i in 1:num.sims) {
    temp.pop.ls <- pop.ls
    for (y in 1:n.genes) {
      while (max(table(temp.pop.ls[[y]])) < n) {
        temp.pop.ls[[y]] <- sample(temp.pop.ls[[y]],
                                   100, replace = T)
        generations[[y]][i] <- generations[[y]][i] + 1
      }
    }
  }
  
  # statistiche descrittive dei risultati
  df <- rbind(data.frame(lapply(generations, mean)),
              data.frame(lapply(generations, sd))) 
  rownames(df) <- c("mean", "sd")
  
  # ciclo per plottare i risultati
  # modificare se i geni da plottare sono > 4
  par(mfrow = c(2,2))
  for (z in 1:n.genes) {
    graph <- generations[[z]]
    hist(graph, 
         main = paste("Num. of generations to fix ",
                      names(generations[z])),
         xlab = "Generations")
    abline(v=mean(graph), lwd=2, lty=2, col="red")
  }
  
  #return(df)
}

png("./classes/5/fixationSimulation.png", width = 12, height = 8, units = "in", res = 300)
sim.gen2fix(pop.ls, 2000)
dev.off()



alleles <- c("A","a")

n_sizes <- c(5,10,20,50,100,500,1000)

df <- data.frame(N = rep(n_sizes, each=50),
                 Freq_A = NA,
                 Freq_a = NA)

for(row in 1:nrow(df)) {
  # selezione casuale di alleli 
  a <- sample(alleles, size=df$N[row], replace = T)
  # find the frequency
  f <- sum(a == "A") / length(a)
  # assign it back to the data.frame
  df$Freq_A[row] <- f
  df$Freq_a[row] <- 1-f
}

df_mean_freq_N <- df %>% 
  group_by(as.factor(N)) %>% 
  summarise(A = mean(Freq_A),
            a = mean(Freq_a))

df_sd_freq_N <- df %>% 
  group_by(as.factor(N)) %>% 
  summarise(A = sd(Freq_A),
            a = sd(Freq_a))

df_mean_freq_N_mtrx <- as.matrix(df_mean_freq_N[,2:3])
rownames(df_mean_freq_N_mtrx) <- c(5,10,20,50,100,500,1000)

df_sd_freq_N_mtrx <- as.matrix(df_sd_freq_N[,2:3])
rownames(df_sd_freq_N_mtrx) <- c(5,10,20,50,100,500,1000)

#A function to add arrows on the chart
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, 
         angle=90, code=3, length=length, ...)
}

png("./classes/6/sizeMatters.png", width = 12, height = 8, units = "in", res = 300)
df_mean_freq_N_mtrx.blt <-barplot(df_mean_freq_N_mtrx, 
                                  beside = T, 
                                  ylim = c(0,1), 
                                  col = rev(brewer.pal(7, "Blues")),
                                  xlab = "Alleles",
                                  ylab = "Frequency")
legend("top", legend = c(5,10,20,50,100,500,1000),
       fill = rev(brewer.pal(7, "Blues")), 
       title = "N", horiz = T)
lines(x = seq(0, 16, 1), y = rep(0.5, 17),
      lty = 2, col = "red", lwd = 3)
error.bar(df_mean_freq_N_mtrx.blt, df_mean_freq_N_mtrx, 
          df_sd_freq_N_mtrx)
dev.off()




png("./classes/6/coal1.png", width = 12, height = 8, units = "in", res = 300)
plot(x=1:10, y=rep(1,10), pch=21, cex=2.7, col = "black",
     bg = c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"),
     ylim = c(0, 11), asp = 1, frame.plot = F, ann = F, axes = F, lwd=2)
points(x=1:10, y=rep(2, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(3, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(4, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(5, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(6, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(7, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(8, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(9, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(10, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(11, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
text(x = -1.5, y = 1, labels = expression('Present (t'[i]*')'))
text(x = -1, y = 11, labels = expression('Past (t'[0]*')'))
text(x = 11.5, y = 11, labels = expression('G'[0]))
text(x = 11.5, y = 10, labels = expression('G'[1]))
text(x = 11.5, y = 9, labels = expression('G'[2]))
text(x = 11.5, y = 7, labels = ":")
text(x = 11.5, y = 6, labels = ":")
text(x = 11.5, y = 5, labels = ":")
text(x = 11.5, y = 2, labels = expression('G'[i-1]))
text(x = 11.5, y = 1, labels = expression('G'[i]))
dev.off()




mtDNAhap <- c("palegreen2", "yellow", "pink",      "darkgreen", "red",
              "steelblue2", "gray9",  "seagreen1", "plum3", "orange")

set.seed(1211)
mtDNAhap1 <- sample(mtDNAhap, size = 10, replace = T)
mtDNAhap2 <- sample(mtDNAhap1, size = 10, replace = T)
mtDNAhap3 <- sample(mtDNAhap2, size = 10, replace = T)
mtDNAhap4 <- sample(mtDNAhap3, size = 10, replace = T)
mtDNAhap5 <- sample(mtDNAhap4, size = 10, replace = T)
mtDNAhap6 <- sample(mtDNAhap5, size = 10, replace = T)
mtDNAhap7 <- sample(mtDNAhap6, size = 10, replace = T)
mtDNAhap8 <- sample(mtDNAhap7, size = 10, replace = T)
mtDNAhap9 <- sample(mtDNAhap8, size = 10, replace = T)
mtDNAhap10 <- sample(mtDNAhap9, size = 10, replace = T)


png("./classes/6/coal2.png", width = 12, height = 8, units = "in", res = 300)
plot(x=1:10, y=rep(1,10), pch=21, cex=2.7, col = "black",
     bg = c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"),
     ylim = c(0, 11), asp = 1, frame.plot = F, ann = F, axes = F, lwd=2)
points(x=1:10, y=rep(2, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(3, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(4, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(5, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(6, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(7, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(8, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(9, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(10, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(11, 10), pch=21, cex=2.7, col = "black", lwd=2,
       bg= mtDNAhap)
text(x = -1.5, y = 1, labels = expression('Present (t'[i]*')'))
text(x = -1, y = 11, labels = expression('Past (t'[0]*')'))
text(x = 11.5, y = 11, labels = expression('G'[0]))
text(x = 11.5, y = 10, labels = expression('G'[1]))
text(x = 11.5, y = 9, labels = expression('G'[2]))
text(x = 11.5, y = 7, labels = ":")
text(x = 11.5, y = 6, labels = ":")
text(x = 11.5, y = 5, labels = ":")
text(x = 11.5, y = 2, labels = expression('G'[i-1]))
text(x = 11.5, y = 1, labels = expression('G'[i]))
dev.off()


png("./classes/6/coal3.png", width = 12, height = 8, units = "in", res = 300)
plot(x=1:10, y=rep(1,10), pch=21, cex=2.7, col = "black",
     bg = c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"),
     ylim = c(0, 11), asp = 1, frame.plot = F, ann = F, axes = F, lwd=2)
points(x=1:10, y=rep(2, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(3, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(4, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(5, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(6, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(7, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(8, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(9, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(10, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap1, lwd=2)
points(x=1:10, y=rep(11, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap, lwd=2)
text(x = -1.5, y = 1, labels = expression('Present (t'[i]*')'))
text(x = -1, y = 11, labels = expression('Past (t'[0]*')'))
text(x = 11.5, y = 11, labels = expression('G'[0]))
text(x = 11.5, y = 10, labels = expression('G'[1]))
text(x = 11.5, y = 9, labels = expression('G'[2]))
text(x = 11.5, y = 7, labels = ":")
text(x = 11.5, y = 6, labels = ":")
text(x = 11.5, y = 5, labels = ":")
text(x = 11.5, y = 2, labels = expression('G'[i-1]))
text(x = 11.5, y = 1, labels = expression('G'[i]))
dev.off()



png("./classes/6/coal4.png", width = 12, height = 8, units = "in", res = 300)
plot(x=1:10, y=rep(1,10), pch=21, cex=2.7, col = "black",
     bg = c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"),
     ylim = c(0, 11), asp = 1, frame.plot = F, ann = F, axes = F, lwd=2)
points(x=1:10, y=rep(2, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(3, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(4, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(5, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(6, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(7, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(8, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(9, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap2, lwd=2)
points(x=1:10, y=rep(10, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap1, lwd=2)
points(x=1:10, y=rep(11, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap, lwd=2)
text(x = -1.5, y = 1, labels = expression('Present (t'[i]*')'))
text(x = -1, y = 11, labels = expression('Past (t'[0]*')'))
text(x = 11.5, y = 11, labels = expression('G'[0]))
text(x = 11.5, y = 10, labels = expression('G'[1]))
text(x = 11.5, y = 9, labels = expression('G'[2]))
text(x = 11.5, y = 7, labels = ":")
text(x = 11.5, y = 6, labels = ":")
text(x = 11.5, y = 5, labels = ":")
text(x = 11.5, y = 2, labels = expression('G'[i-1]))
text(x = 11.5, y = 1, labels = expression('G'[i]))
dev.off()




png("./classes/6/coal5.png", width = 12, height = 8, units = "in", res = 300)
plot(x=1:10, y=rep(1,10), pch=21, cex=2.7, col = "black",
     bg = c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"),
     ylim = c(0, 11), asp = 1, frame.plot = F, ann = F, axes = F, lwd=2)
points(x=1:10, y=rep(2, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(3, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(4, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(5, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(6, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(7, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(8, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap3, lwd=2)
points(x=1:10, y=rep(9, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap2, lwd=2)
points(x=1:10, y=rep(10, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap1, lwd=2)
points(x=1:10, y=rep(11, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap, lwd=2)
text(x = -1.5, y = 1, labels = expression('Present (t'[i]*')'))
text(x = -1, y = 11, labels = expression('Past (t'[0]*')'))
text(x = 11.5, y = 11, labels = expression('G'[0]))
text(x = 11.5, y = 10, labels = expression('G'[1]))
text(x = 11.5, y = 9, labels = expression('G'[2]))
text(x = 11.5, y = 7, labels = ":")
text(x = 11.5, y = 6, labels = ":")
text(x = 11.5, y = 5, labels = ":")
text(x = 11.5, y = 2, labels = expression('G'[i-1]))
text(x = 11.5, y = 1, labels = expression('G'[i]))
dev.off()

png("./classes/6/coal6.png", width = 12, height = 8, units = "in", res = 300)
plot(x=1:10, y=rep(1,10), pch=21, cex=2.7, col = "black",
     bg = mtDNAhap10,
     ylim = c(0, 11), asp = 1, frame.plot = F, ann = F, axes = F, lwd=2)
points(x=1:10, y=rep(2, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap9, lwd=2)
points(x=1:10, y=rep(3, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap8, lwd=2)
points(x=1:10, y=rep(4, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap7, lwd=2)
points(x=1:10, y=rep(5, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap6, lwd=2)
points(x=1:10, y=rep(6, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap5, lwd=2)
points(x=1:10, y=rep(7, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap4, lwd=2)
points(x=1:10, y=rep(8, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap3, lwd=2)
points(x=1:10, y=rep(9, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap2, lwd=2)
points(x=1:10, y=rep(10, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap1, lwd=2)
points(x=1:10, y=rep(11, 10), pch=21, cex=2.7, col = "black",
       bg= mtDNAhap, lwd=2)
text(x = -1.5, y = 1, labels = expression('Present (t'[i]*')'))
text(x = -1, y = 11, labels = expression('Past (t'[0]*')'))
text(x = 11.5, y = 11, labels = expression('G'[0]))
text(x = 11.5, y = 10, labels = expression('G'[1]))
text(x = 11.5, y = 9, labels = expression('G'[2]))
text(x = 11.5, y = 7, labels = ":")
text(x = 11.5, y = 6, labels = ":")
text(x = 11.5, y = 5, labels = ":")
text(x = 11.5, y = 2, labels = expression('G'[i-1]))
text(x = 11.5, y = 1, labels = expression('G'[i]))
dev.off()





png("./classes/6/coal7.png", width = 12, height = 8, units = "in", res = 300)
plot(x=1:10, y=rep(1,10), pch=21, cex=2.7, col = "black",
     bg = c("green", "green", "white", "green", "green", "white", "green", "white", "green", "white"),
     ylim = c(0, 11), asp = 1, frame.plot = F, ann = F, axes = F, lwd=2)
points(x=1:10, y=rep(2, 10), pch=21, cex=2.7, col = "black",
       bg= c("green", "white", "green", "white", "green", "white", "green", "white", "green", "white"), lwd=2)
points(x=1:10, y=rep(3, 10), pch=21, cex=2.7, col = "black",
       bg= c("green", "white", "green", "white", "green", "white", "white", "green", "green", "white"), lwd=2)
points(x=1:10, y=rep(4, 10), pch=21, cex=2.7, col = "black",
       bg= c("green", "white", "green", "white", "white", "green", "white", "green", "white", "white"), lwd=2)
points(x=1:10, y=rep(5, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "green", "white", "white", "green", "white", "green", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(6, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "green", "white", "white", "green", "white", "green", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(7, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "green", "white", "white", "green", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(8, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "green", "white", "green", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(9, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "green", "green", "white", "white", "white", "white", "white", "white"), lwd=2)
points(x=1:10, y=rep(10, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "white", "green", "white", "white", "white", "white", "white", "white"), lwd=2)
text(x = -1.5, y = 1, labels = expression('Present (t'[i]*')'))
text(x = -1, y = 10, labels = expression('Past (t'[0]*')'))
text(x = 4, y = 11, labels = "MRCA")
lines(x = c(1,1,1,1,2,2,3,3,3,4), y = c(1,2,3,4,5,6,7,8,9,10))
lines(x = c(2,3,3,3,2,2,3,3,3,4), y = c(1,2,3,4,5,6,7,8,9,10))
lines(x = c(4,5,5,6,5,5,6,5,4,4), y = c(1,2,3,4,5,6,7,8,9,10))
lines(x = c(5,5,5,6,5,5,6,5,4,4), y = c(1,2,3,4,5,6,7,8,9,10))
lines(x = c(7,7,8,8,7,7,6,5,4,4), y = c(1,2,3,4,5,6,7,8,9,10))
lines(x = c(9,9,9,8,7,7,6,5,4,4), y = c(1,2,3,4,5,6,7,8,9,10))
points(x=1:10, y=rep(1,10), pch=21, cex=2.7, 
       col = "black",
       bg= c("green", "green", "white", "green", "green", "white", "green", "white", "green", "white"))
points(x=1:10, y=rep(2, 10), pch=21, cex=2.7, 
       col = c("black", "black", "black", "black", "red", "black", "black", "black", "black", "black"),
       bg= c("green", "white", "green", "white", "green", "white", "green", "white", "green", "white"))
points(x=1:10, y=rep(3, 10), pch=21, cex=2.7, col = "black",
       bg= c("green", "white", "green", "white", "green", "white", "white", "green", "green", "white"))
points(x=1:10, y=rep(4, 10), pch=21, cex=2.7, 
       col = c("black", "black", "black", "black", "black", "black", "black", "red", "black", "black"),
       bg= c("green", "white", "green", "white", "white", "green", "white", "green", "white", "white"))
points(x=1:10, y=rep(5, 10), pch=21, cex=2.7, 
       col = c("black", "red", "black", "black", "black", "black", "black", "black", "black", "black"),
       bg= c("white", "green", "white", "white", "green", "white", "green", "white", "white", "white"))
points(x=1:10, y=rep(6, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "green", "white", "white", "green", "white", "green", "white", "white", "white"))
points(x=1:10, y=rep(7, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "green", "white", "white", "green", "white", "white", "white", "white"))
points(x=1:10, y=rep(8, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "green", "white", "green", "white", "white", "white", "white", "white"))
points(x=1:10, y=rep(9, 10), pch=21, cex=2.7, col = "black",
       bg= c("white", "white", "green", "green", "white", "white", "white", "white", "white", "white"))
points(x=1:10, y=rep(10, 10), pch=21, cex=2.7, 
       col = c("black", "black", "black", "red", "black", "black", "black", "black", "black", "black"),
       bg= c("white", "white", "white", "green", "white", "white", "white", "white", "white", "white"))
dev.off()



simulate.pop<-function(N.vec=rep(5,30), const.RS=TRUE,  mutation= TRUE, mut.rate=  0.1, for.class= TRUE, initial.state="all.black",plot.freqs=FALSE,mult.pop=FALSE,pops=FALSE){
  #  c(rep(10,5),rep(3,2),rep(10,5),rep(3,2),rep(10,5))  #
  stopifnot(initial.state %in% c("all.black","all.diff","two.alleles","single.mut") )
  
  if(plot.freqs){layout(c(1,2)); par(mar=c(1,2,0,1))}
  if(for.class){
    line.lwd<-1
    line.col<-"black"
    mut.line.lwd<-1
    mut.line.col<-"black"
    
  }else{
    line.lwd<-0.5
    line.col<-"grey"
    mut.line.lwd<-1
    mut.line.col<-"grey"
  }
  
  num.gens<- length(N.vec)-1	
  
  if(!mult.pop){
    ind.pop.par<-matrix(1,nrow=max(N.vec),ncol=num.gens+1)
    ind.pop<-matrix(1,nrow=max(N.vec),ncol=num.gens+1)
  }else{
    ind.pop.par<-pops[["ind.pop.par"]]
    ind.pop<-pops[["ind.pop"]]
  }
  
  num.gens<- length(N.vec)-1
  offset<-0.1
  plot(c(1,num.gens),c(0.5,max(N.vec))+c(-offset,offset),type="n",axes=FALSE,xlab="",ylab="")
  mtext(side=1,line=0,"Generations")
  text(1,0.5,"Past")
  text(num.gens-1,0.5,"Present")
  
  track.cols<- list()
  N <-N.vec[1]
  if(initial.state=="all.black") my.cols<-rep("black",2*N)  #sample(rainbow(2*N))
  if(initial.state=="all.diff") my.cols<-sample(rainbow(2*N))
  if(initial.state=="two.alleles")  my.cols<-  rep(c("blue","red"),N)
  if(initial.state=="single.mut")  my.cols<-  c("red",rep("blue",2*N-1))
  stopifnot((2*N)==length(my.cols))
  
  track.cols[[1]]<-my.cols
  points(rep(1,N),1:N+offset, pch=19,cex=1.3,col=my.cols[(1:N)*2])
  points(rep(1,N),1:N-offset, pch=19,cex=1.3,col=my.cols[(1:N)*2-1])
  
  for(i in 1:num.gens){
    
    N.new<-N.vec[i+1]
    N.old<-N.vec[i]
    points(rep(i,N.old),1:N.old+offset, pch=19,cex=1.3,col=my.cols[(1:N.old)*2])
    points(rep(i,N.old),1:N.old-offset, pch=19,cex=1.3,col=my.cols[(1:N.old)*2-1])
    
    new.cols<-rep("black",2*N.new)
    
    if(const.RS){ 
      repro.success<-rep(1/N.old,N.old)
    }else{
      repro.success<-sample(c(rep(0.5/(N.old),N.old-2),c(0.25,0.25)),replace=FALSE)
    }
    
    for(ind in 1:N.new){
      
      this.pop.par <- ind.pop.par[ind,i+1]
      available.pars <- (1:N.old)[which(ind.pop[1:N.old,i] == this.pop.par)]
      par<-sample(available.pars,2,replace=FALSE,prob=repro.success[which(ind.pop[1:N.old,i] == this.pop.par)])
      
      which.allele.1<-sample(c(-1,1),1)
      if(i != num.gens){ lines(c(i,i+1), c(par[1]+which.allele.1*offset,ind-offset),col=line.col,lwd=line.lwd)}
      new.cols[2*ind-1]<- my.cols[2*par[1] +ifelse(which.allele.1==1,0,-1)]
      
      which.allele.2<-sample(c(-1,1),1)
      if(i != num.gens){ lines(c(i,i+1), c(par[2]+which.allele.2*offset,ind+offset),col=line.col,lwd=line.lwd)}
      new.cols[2*ind]<- my.cols[2*par[2] +ifelse(which.allele.2==1,0,-1)]
      
      if(mutation){
        if(runif(1)<mut.rate){ 
          new.cols[2*ind-1]<- sample(rainbow(4*N),1)
          if(i != num.gens){ lines(c(i,i+1), c(par[1]+which.allele.1*offset,ind-offset),col=mut.line.col,lwd=mut.line.lwd)}
          
        }
        if(runif(1)<mut.rate){ 
          new.cols[2*ind]<- sample(rainbow(4*N),1)
          if(i != num.gens){ lines(c(i,i+1), c(par[2]+which.allele.2*offset,ind+offset),col=mut.line.col,lwd=mut.line.lwd)}
        } 
        
      }
    }	
    ##redraw points to cover lines		 
    points(rep(i,N.old),1:N.old+offset, pch=19,cex=1.3,col=my.cols[(1:N.old)*2])
    points(rep(i,N.old),1:N.old-offset, pch=19,cex=1.3,col=my.cols[(1:N.old)*2-1])
    
    my.cols<-new.cols
    track.cols[[i+1]]<-my.cols
    if(!const.RS) sapply(which(repro.success>1/N.old), function(ind){ draw.circle(x=i,y=ind,radius=0.2,nv=100,border=NULL,col=NA,lty=1,lwd=1)})
  }
  #	recover()
  if(plot.freqs){
    plot(c(1,num.gens),c(0,1),type="n",axes=FALSE,xlab="",ylab="")
    all.my.cols<-unique(unlist(track.cols))
    
    if(!mult.pop){ 
      my.col.freqs<-sapply(track.cols,function(my.gen){sapply(all.my.cols,function(my.col){sum(my.gen==my.col)})})
      
      sapply(all.my.cols,function(col.name){lines(my.col.freqs[col.name,]/(2*N.vec),col=col.name,lwd=2)});
    }else{
      
      for(pop in 1:max(ind.pop)){
        my.col.freqs<-sapply(1:num.gens, function(gen){
          #			recover()
          my.gen<-track.cols[[gen]]
          if(all(ind.pop.par[ind.pop[,gen]==pop,gen]==0)) return(rep(NA,length(all.my.cols)))  #if pop doesn't exist in this gen.
          
          these.inds<-which(ind.pop[,gen]==pop)
          my.gen<-c(my.gen[these.inds*2],my.gen[these.inds*2-1])
          sapply(all.my.cols,function(my.col){
            sum(my.gen==my.col)
          })})
        rownames(my.col.freqs)<-		all.my.cols
        sapply(all.my.cols[-length(all.my.cols)],function(col.name){lines(my.col.freqs[col.name,]/(2*5),col=col.name,lwd=2,lty=pop)});	
      }
    }
    
    axis(2)
  }
}

png("./classes/6/driftandH1.png", width = 12, height = 8, units = "in", res = 300)
simulate.pop(N.vec= rep(5,15), const.RS=TRUE,  mutation= FALSE, for.class= TRUE, initial.state="two.alleles")
dev.off()

png("./classes/6/driftandH2.png", width = 12, height = 8, units = "in", res = 300)
simulate.pop(N.vec=rep(5,15), const.RS=TRUE,  mutation= FALSE, for.class= TRUE, initial.state="all.diff")
dev.off()


png("./classes/6/driftandmutation1.png", width = 12, height = 8, units = "in", res = 300)
simulate.pop(N.vec=rep(5,30), const.RS=TRUE,  mutation=TRUE, mut.rate=  0.2, for.class= TRUE, initial.state="all.black")
dev.off()



simulate.pop(N.vec=rep(5,30), const.RS=TRUE,  mutation= TRUE, mut.rate=  0.1, for.class= TRUE, initial.state="all.black")
simulate.pop(N.vec=rep(5,30), const.RS=TRUE,  mutation= TRUE, mut.rate=  0.1, for.class= TRUE, initial.state="all.black")
simulate.pop(N.vec=rep(5,30), const.RS=TRUE,  mutation= TRUE, mut.rate=  0.1, for.class= TRUE, initial.state="all.black")

simulate.pop(N.vec=rep(5,30), const.RS=TRUE,  mutation=TRUE, mut.rate=  0.2, for.class= TRUE, initial.state="all.black")
simulate.pop(N.vec=rep(5,30), const.RS=TRUE,  mutation= TRUE, mut.rate=  0.2, for.class= TRUE, initial.state="all.black")
simulate.pop(N.vec=rep(5,30), const.RS=TRUE,  mutation= TRUE, mut.rate=  0.2, for.class= TRUE, initial.state="all.black")

simulate.pop(N.vec=rep(5,30), const.RS=TRUE,  mutation= TRUE, for.class= TRUE,plot.freqs=TRUE,initial.state="all.black")
simulate.pop(N.vec=rep(5,30), const.RS=TRUE,  mutation= TRUE, for.class= TRUE,plot.freqs=TRUE,initial.state="all.black")
simulate.pop(N.vec=rep(5,30), const.RS=TRUE,  mutation= TRUE, for.class= TRUE,plot.freqs=TRUE,initial.state="all.black")





wf <- function(N, ngens, p0=1/3, mu=0) {
  N <- 2*N  # diploid adjustment
  # initialize an empty matrix
  gns <- matrix(NA, nrow=ngens, ncol=N)
  # initialize the first generation, with two alleles, one at freq
  # p0
  alleles <- 2
  gns[1, ] <- sample(1:2, N, replace=TRUE, prob=c(p0, 1-p0))
  for (i in 2:ngens) {
    gns[i, ] <- gns[i-1, sample(1:N, N, replace=TRUE)]
    if (mu > 0) {
      # add mutations to this generation
      muts <- rbinom(N, 1, prob=mu)
      new_alleles <- sum(muts)
      alleles <- alleles + new_alleles
      if (new_alleles) {
        # there are mutations, add to population.
        gns[i, ] <- ifelse(muts, sample(alleles), gns[i, ])
      }
    }
  }
  gns
}




het <- function(x) {
  tbl <- table(x)
  1 - sum((tbl/sum(tbl))^2)
}

my.sims<-replicate(100,wf(N=100, ngens=150))

h <- apply(my.sims, 1, het)
plot(h, type='l', xlab='generation', ylab='heterozygosity')

p0<-0.3
N<-500
ngens<-150
my.sims<-replicate(40,wf(N=N, ngens=ngens,p0=p0))

png("./classes/6/lossofhet.png", width = 12, height = 8, units = "in", res = 300)
layout(t(1:2))
plot(type="n",y=c(0,1),x=c(0,ngens),xlab="Time, generations",ylab="Frequency, p", cex.lab=1.4,cex.axis=1.2)
apply(my.sims,3,function(sim){
  lines(c(p0,apply(sim==1,1,mean)),,col=adjustcolor("black",0.3))
})
lines(c(p0,apply(my.sims[,,1]==1,1,mean)),col="red",lwd=2)
lines(rowMeans(apply(my.sims,3,function(sim){c(p0,apply(sim==1,1,mean))})),col="blue",lwd=2)
abline(h=p0,col="blue",lwd=2,lty=3)
legend(x="topright",legend=c("1 sim.","Mean sim.","Expectation"),col=c("red","blue","blue"),lty=c(1,1,2),bg="white")
plot(type="n",y=c(0,0.5),x=c(0,ngens),xlab="Time, generations",ylab="Heterozygosity", cex.lab=1.4,cex.axis=1.2)
apply(my.sims,3,function(sim){
  lines(c(2*p0*(1-p0),apply(sim,1,het)),col=adjustcolor("black",0.3))
})
lines(c(2*p0*(1-p0),apply(my.sims[,,1],1,het)),col="red",lwd=2)
lines(rowMeans(apply(my.sims,3,function(sim){apply(sim,1,het)})),col="blue",lwd=2)
lines(0:ngens,2*p0*(1-p0)*(1-1/(2*N))^(0:ngens),col="blue",lty=3,lwd=2)
dev.off()



#https://academic.oup.com/jmammal/article/92/4/751/887640
#Genetic Diversity and Fitness in Black-Footed Ferrets Before and During a Bottleneck 
#S. M. Wisely S. W. Buskirk M. A. Fleming D. B. McDonald E. A. Ostrander 

#another set of numbers https://academic.oup.com/jmammal/article/92/4/751/887640
#In 1985 the last wild population (N = 40 adults) experienced simultaneous epizootics of canine distemper and sylvatic plague (Yersinia pestis). 
#. Eighteen individuals were captured for breeding
#
black_footed<-read.csv("./classes/6/black-footed-ferrets_He.csv")

black_footed[,1]<-c(1891,1972,1985,1986) ##displace postbottleneck pop 1 year
#black_footed<-rbind(black_footed,cbind(c(0.067,0.067),c(1999,2004))

png("./classes/6/blackfootedferret.png", width = 12, height = 8, units = "in", res = 300)
plot(black_footed,type="b",xlab= "Year", ylab="Heterozygosity (HE)",ylim=c(0,.3),cex.lab=1.4,cex=1.5,pch=19,range(black_footed$date)+c(-12,5),axes=FALSE)
N<-c("N>10k","N=62","N=40","N=7")
axis(1)
axis(2)
text(black_footed$date-6,black_footed$He-0.008,paste(" (",N,")",sep="")) #black_footed$date)
dev.off()








track_lineages<-function(N.vec, n.iter, num.tracked, col.allele,return.tracked=FALSE){
  offset<-0.2
  num.gens<-length(N.vec)
  for(iter in 1:n.iter){
    N.max<-max(N.vec)
    N<-N.vec[num.gens]
    N.prev<-N.vec[num.gens-1]
    plot(c(1,num.gens),c(1,N.max),type="n",axes=FALSE,xlab="",ylab="")
    mtext(side=1,line=1,"Generations")
    
    track.this.allele<-vector("list", 2*N)
    track.this.allele.time<-list()
    track.this.allele[sample(1:(2*N),num.tracked)]<-1:num.tracked
    
    track.this.allele.next.gen<-vector("list", 2*N.prev)
    
    for(i in num.gens:2){
      if(return.tracked) track.this.allele.time[[i]]<-track.this.allele
      N<-N.vec[i]
      N.prev<-N.vec[i-1]
      track.this.allele.next.gen<-vector("list", 2*N.prev)
      for(ind in 1:N){
        
        par<-sample(1:N.prev,2,replace=FALSE)
        which.allele<-sample(c(-1,1),1)
        lines(c(i,i-1), c(ind-offset,par[1]+which.allele*offset),col="light grey",lwd=0.5)
        if(!is.null(track.this.allele[[2*ind-1]])){
          this.one<-2*par[1] +ifelse(which.allele==1,0,-1); 
          track.this.allele.next.gen[[this.one]]  <- c(track.this.allele.next.gen[[this.one]],track.this.allele[[2*ind-1]])
        }
        
        which.allele<-sample(c(-1,1),1)
        lines(c(i,i-1), c(ind+offset,par[2]+which.allele*offset),col="light grey",lwd=0.5)
        if(!is.null(track.this.allele[[2*ind]])){ 
          this.one<-2*par[2] +ifelse(which.allele==1,0,-1); 
          track.this.allele.next.gen[[ this.one]]  <- c(track.this.allele.next.gen[[this.one]],track.this.allele[[2*ind]])
        }
        #		recover()
      }
      for(this.allele in 1:num.tracked){ 
        daughter<-which(sapply(track.this.allele,function(allele){any(allele==this.allele)}))
        parent<-which(sapply(track.this.allele.next.gen,function(allele){any(allele==this.allele)}))
        lines(c(i,i-1), c(ceiling(daughter/2)+offset* ifelse(daughter %% 2,-1,1) ,ceiling(parent/2) + offset*ifelse(parent %% 2,-1,1) ),col=col.allele[this.allele],lwd=2)
      }
      
      points(rep(i,N),1:N+offset, pch=19,cex=1)
      points(rep(i,N),1:N-offset, pch=19,cex=1)
      track.this.allele<-track.this.allele.next.gen
    }
    
    
  }
  if(return.tracked) track.this.allele.time
}


N<-10

###Track pairs
png("./classes/6/pairwisecoalescent.png", width = 12, height = 8, units = "in", res = 300)
track_lineages(N.vec=rep(10,20), n.iter=20, num.tracked=2, col.allele=c("red","blue"))
dev.off()







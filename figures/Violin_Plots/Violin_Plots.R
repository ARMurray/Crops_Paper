library(ggpubr)
library(grid)
library(here)

crops <- read.csv(here("data/Crops_Final_Figs/Crops_Final_Figs/Figs5and7/FilteredCropData.csv"))

#FilterCrops

soybeans <- crops%>%
  filter(Commodity == "SOYBEANS")

corn <-  crops%>%
  filter(Commodity == "CORN")

cotton <-  crops%>%
  filter(Commodity == "COTTON")

peanuts <-  crops%>%
  filter(Commodity == "PEANUTS")

sweetPotatoes <- crops%>%
  filter(Commodity == "SWEET POTATOES")

#Create for Each Crop and Climate Category

####    T - MAX     ####

#TMAX Corn

p <- ggviolin(corn, x = "GSTempCategories", y = "YieldPercentChange",
              fill = "GSTempCategories", shape = "GSTempCategories", palette =c("#00AFBB","#E7B800","#FC4E07"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Corn",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

CornTMAXPlot <- p2 +
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test",label.x = 2, label.y = 130)+
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = "Near Normal", label.y = 100)+
  theme(plot.title = element_text(vjust= -5))

#TMAXCOTTON#
p <- ggviolin(cotton, x = "GSTempCategories", y = "YieldPercentChange",
              fill = "GSTempCategories", shape = "GSTempCategories", palette =c("#00AFBB","#E7B800","#FC4E07"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Cotton",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
#p2

cottonTMAXPlot <- p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 130)+
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                    ref.group = "Near Normal", label.y = 100)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#cottonTMAXPlot

#TMAXPeanuts#
p <- ggviolin(peanuts, x = "GSTempCategories", y = "YieldPercentChange",
              fill = "GSTempCategories", shape = "GSTempCategories", palette =c("#00AFBB","#E7B800","#FC4E07"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Peanuts",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

peanutsTMAXPlot <- p2 +
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 130)+
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = "Near Normal", label.y = 100)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#peanutsTMAXPlot

#TMAXSOYBEANS#
p <- ggviolin(soybeans, x = "GSTempCategories", y = "YieldPercentChange",
              fill = "GSTempCategories", shape = "GSTempCategories", palette =c("#00AFBB","#E7B800","#FC4E07"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Soybeans",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

soybeansTMAXPlot <- p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 130)+
  stat_compare_means(label = "p.signif",
                     method = "wilcox.test", ref.group = "Near Normal", label.y = 100)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#soybeansTMAXPlot
p <- ggviolin(sweetPotatoes, x = "GSTempCategories", y = "YieldPercentChange",
              fill = "GSTempCategories", shape = "GSTempCategories", palette =c("#00AFBB","#E7B800","#FC4E07"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Sweet Potatoes",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

sweetpotatoesTMAXPlot <- p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 130)+
  stat_compare_means(label = "p.signif",
                     method = "wilcox.test", ref.group = "Near Normal", label.y = 100)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#sweetpotatoesTMAXPlot



####    ----    PRECIP PLOTS    ----    ####

#pptCorn
p <- ggviolin(corn, x = "GSPPTCategories", y = "YieldPercentChange",
              fill = "GSPPTCategories", shape = "GSPPTCategories", palette =c("#FC4E07","#E7B800","#00AFBB"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Corn",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


CornpptPlot <-  p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test",label.x=2, label.y = 150)+
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "Near Normal", label.y = 125)+
  theme(plot.title = element_text(vjust= -5))

#CornpptPlot

#pptCOTTON#
p <- ggviolin(cotton, x = "GSPPTCategories", y = "YieldPercentChange",
              fill = "GSPPTCategories", shape = "GSPPTCategories", palette =c("#FC4E07","#E7B800","#00AFBB"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Cotton",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

cottonpptPlot <-p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 150)+
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "Near Normal", label.y = 125)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#cottonpptPlot

#pptPeanuts#
p <- ggviolin(peanuts, x = "GSPPTCategories", y = "YieldPercentChange",
              fill = "GSPPTCategories", shape = "GSPPTCategories", palette =c("#FC4E07","#E7B800","#00AFBB"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Peanuts",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

peanutspptPlot <- p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 130)+
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "Near Normal", label.y = 100)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#peanutspptPlot

#pptSOYBEANS#
p <- ggviolin(soybeans, x = "GSPPTCategories", y = "YieldPercentChange",
              fill = "GSPPTCategories", shape = "GSPPTCategories", palette =c("#FC4E07","#E7B800","#00AFBB"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Soybeans",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

soybeanspptPlot <- p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 135)+
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "Near Normal", label.y = 115)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#soybeanspptPlot

#pptSweetPotatoes#
p <- ggviolin(sweetPotatoes, x = "GSPPTCategories", y = "YieldPercentChange",
              fill = "GSPPTCategories", shape = "GSPPTCategories", palette =c("#FC4E07","#E7B800","#00AFBB"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Sweet Potatoes",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

sweetpotatoespptPlot <- p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 130)+
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "Near Normal", label.y = 100)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#sweetpotatoespptPlot



####    ----    T-MIN PLOTS    ----    ####

#tminCorn
p <- ggviolin(corn, x = "GSMinTempCategories", y = "YieldPercentChange",
              fill = "GSMinTempCategories", shape = "GSMinTempCategories", palette =c("#FC4E07","#E7B800","#00AFBB"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Corn",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


CorntminPlot <-  p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 150)+
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "Near Normal", label.y = 125)+
  theme(plot.title = element_text(vjust= -5))

#CorntminPlot

#tminCOTTON
p <- ggviolin(cotton, x = "GSMinTempCategories", y = "YieldPercentChange",
              fill = "GSMinTempCategories", shape = "GSMinTempCategories", palette =c("#FC4E07","#E7B800","#00AFBB"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Cotton",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

cottontminPlot <-p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 150)+
  stat_compare_means(label = "p.signif",
                     method = "wilcox.test", ref.group = "Near Normal", label.y = 125)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#cottontminPlot

#tminPeanuts
p <- ggviolin(peanuts, x = "GSMinTempCategories", y = "YieldPercentChange",
              fill = "GSMinTempCategories", shape = "GSMinTempCategories", palette =c("#FC4E07","#E7B800","#00AFBB"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Peanuts",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

peanutstminPlot <- p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 130)+
  stat_compare_means(label = "p.signif",
                     method = "wilcox.test", ref.group = "Near Normal", label.y = 100)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#peanutstminPlot

# tmin SOYBEANS
p <- ggviolin(soybeans, x = "GSMinTempCategories", y = "YieldPercentChange",
              fill = "GSMinTempCategories", shape = "GSMinTempCategories", palette =c("#FC4E07","#E7B800","#00AFBB"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Soybeans",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

soybeanstminPlot <- p2+
  facet_wrap(. ~ State)+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 135)+
  stat_compare_means(label = "p.signif",
                     method = "wilcox.test", ref.group = "Near Normal", label.y = 115)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#soybeanstminPlot

#tmin Sweet Potatoes
p <- ggviolin(sweetPotatoes, x = "GSMinTempCategories", y = "YieldPercentChange",
              fill = "GSMinTempCategories", shape = "GSMinTempCategories", palette =c("#FC4E07","#E7B800","#00AFBB"), add = "boxplot", add.params = list(fill = "white") , order = c("Below Normal", "Near Normal", "Above Normal")) + scale_x_discrete(
                labels = c('Below\nNormal', 'Near\nNormal', 'Above\nNormal'))

p2 <- ggpar(p,
            title = "Sweet Potatoes",
            subtitle = "",
            caption = "",
            xlab ="",
            ylab = "Yield Percent Difference",
            legend.title = "     Growing Season \n Temperature") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

sweetpotatoestminPlot <- p2+
  stat_compare_means(aes(label = ifelse(..p..<.0005, "p < .0005",paste0("p = ", sprintf("%.4f",round(..p..,4))))),
                     method = "kruskal.test", label.y = 130)+
  stat_compare_means(label = "p.signif",
                     method = "wilcox.test", ref.group = "Near Normal", label.y = 100)+
  theme(plot.margin = margin(c(-1.5,0,0,0), unit = "cm"),
        plot.title = element_text(vjust= -5))

#sweetpotatoestminPlot



#Joined Plots for Figure

lay <- rbind(c(1,6,11),
             c(2,7,12),
             c(3,8,13),
             c(4,9,14),
             c(5,10,15))

# One Big Composite
#tiff(here("figures/Violin_Plots/Composite_Violin.tiff"), width = 50, height = 60, units = 'cm', res = 400)
#grid.arrange(CornpptPlot, cottonpptPlot, peanutspptPlot, soybeanspptPlot,sweetpotatoespptPlot,
#             CornTMAXPlot, cottonTMAXPlot, peanutsTMAXPlot, soybeansTMAXPlot,sweetpotatoesTMAXPlot,
#             CorntminPlot, cottontminPlot, peanutstminPlot, soybeanstminPlot,sweetpotatoestminPlot,
#             layout_matrix = lay)
#dev.off()


lay2 <- rbind(c(1,1,1),
             c(2,2,2),
             c(3,3,3),
             c(4,4,4),
             c(NA,5,NA))

# PPT Only
tiff(here("figures/Violin_Plots/PPT_Violin.tiff"), width = 18, height = 40, units = 'cm', res = 400)
grid.arrange(CornpptPlot, cottonpptPlot, peanutspptPlot, soybeanspptPlot,sweetpotatoespptPlot,
             layout_matrix = lay2)
dev.off()

# TMAX Only
tiff(here("figures/Violin_Plots/TMAX_Violin.tiff"), width = 18, height = 40, units = 'cm', res = 400)
grid.arrange(CornTMAXPlot, cottonTMAXPlot, peanutsTMAXPlot, soybeansTMAXPlot,sweetpotatoesTMAXPlot,
             layout_matrix = lay2)
dev.off()

# TMIN Only
tiff(here("figures/Violin_Plots/TMIN_Violin.tiff"), width = 18, height = 40, units = 'cm', res = 400)
grid.arrange(CorntminPlot, cottontminPlot, peanutstminPlot, soybeanstminPlot,sweetpotatoestminPlot,
             layout_matrix = lay2)
dev.off()            









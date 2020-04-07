
x1 <- 1
x2 <- 2
y1 <- 1
y2 <- 3

outdf <- data.frame()

for(n in 1:8){
  bl <- c(x1,y1)
  tl <- c(x1,y2)
  tr <- c(x2,y2)
  br <- c(x2,y1)
  xy <- list(rbind(bl,tl,tr,br,bl))
  poly <- st_as_sf(st_sfc(st_polygon(xy)))
  geom <- poly$x
  out <- data.frame("Object"=n, "geometry" = geom)
  outdf <- rbind(outdf,out)
  y1 <- y1+2
  y2 <- y2+2
}

sfp <- st_as_sf(outdf)
sfp$class <- c("NO DATA","-0.71 : -0.5","< -0.5","< -.05","< 0.05","< 0.25","< 0.50","0.50 : 0.67")

# Color Classification
cols <- c("NO DATA" = "#c4c4c0",
          "-0.71 : -0.5" = "#730000",
          "< -0.5" = "#a80000",
          "< -.05" = "#d95d5d",
          "< 0.05" = "#FFFFFF",
          "< 0.25" = "#94f2ab",
          "< 0.50" = "#43c463",
          "0.50 : 0.67" = "#0b992e",
          "NO DATA" = "#c4c4c0")

legend <- ggplot(sfp)+
  geom_sf(aes(fill=class))+
  scale_fill_manual(values = cols)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_blank())+
  annotate("text", x = 6, y = seq(2,16,2), label = sfp$class)+
  xlim(xmin = 0, xmax = 10)

# Save
tiff(here("figures/figure06_07_LEGEND.tiff"), width = 4, height = 6, units = 'cm', res = 1000)
legend
dev.off()

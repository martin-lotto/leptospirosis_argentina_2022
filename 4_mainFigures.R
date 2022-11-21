## ############################################################################
##
## Towards a Leptospirosis Early Warning System in North-Eastern Argentina
## Lotto Batista, M, Rees E., et al., 2022
##
## SCRIPT 04: Create main figures and tables
##
## ############################################################################
##
## Script author: 
## - Martín Lotto Batista, ORCID: 0000-0002-9437-5270
## - Eleanor Rees, ORCID: 0000-0002-4993-2795
##
## Contact: martin.lotto@bsc.es
## 
## License: 
##
## ############################################################################
pacman::p_load("ggthemes", "tidyverse", "zoo", 
               "sf", "ggspatial", "pROC")

# Prepare data
df.lepto <- read.csv("data_use/prov_cases.csv")
pop <- read.csv("data_use/pop.csv")

data.lepto <- df.lepto %>% 
  left_join(pop)

df.er <- data.lepto %>% filter(prov == "Entre Ríos")
df.sf <- data.lepto %>% filter(prov == "Santa Fe")

er.enso.fit <- readRDS("model_out/er_enso_fit.rds")
er.clim.fit <- readRDS("model_out/er_clim_fit.rds")
sf.enso.fit <- readRDS("model_out/sf_enso_fit.rds")
sf.clim.fit <- readRDS("model_out/sf_clim_fit.rds")
er.preds <- readRDS("model_out/er_preds.rds")
sf.preds <- readRDS("model_out/sf_preds.rds")
#####

# Figure 2a: map ####
# Shapefiles
# South America's contour
sa <- readRDS("data_use/south_america_full.rds")
bord <- sa %>% mutate(a=0) %>% group_by(a) %>% summarise(.groups="drop")
# Provinces
study.area <- readRDS("data_use/stafe_enrios.rds") %>%  
  group_by(NAME_1) %>% 
  summarise(.groups="drop")
# Province names
study.nmes <- cbind(study.area, 
                    st_coordinates(st_centroid(study.area$geometry)))
# Paraná river
map.riv <- readRDS("data_use/shp_riv.rds") %>% 
  filter(!grepl("Uru|Guale|Pla|Pal", fna)) %>% 
  st_crop(c(xmin=-60.80564,ymin=-34.22840,xmax=-57,ymax=-27.7))

# South America with study area
p1 <- ggplot() +
  # South America map
  geom_sf(data=sa, fill="#f7f4f9", color="white", lwd=0) +
  # South America border
  geom_sf(data=bord, fill=NA, lwd=0, color="#d4b9da") +
  # Provinces
  geom_sf(data=study.area, fill="#e7e1ef", color="white", lwd=0, alpha=0.5) +
  # Rectangle around provinces
  geom_rect(aes(xmin=-64, xmax=-57, ymin=-36, ymax=-27), 
            fill=NA, col="black", lwd=0.2) +
  # Aesthetics
  theme_void()
  

# Study area
stations <- data.frame(station=c("Sauce Viejo","Ceres","Reconquista","Rafaela", 
                                 "Rosario","Concordia","Gualeguaychú","Paraná"),
                       long=c(-60.807824,-61.946310,-59.659124,-61.490189,
                              -60.677072,-58.014574,-58.516761,-60.487925),
                       lat=c(-31.709946,-29.883555,-29.163602,-31.254377,
                             -32.953630,-31.385735,-33.008895,-31.777752))

cities <- data.frame(city=c("Santa Fe","Rosario","Paraná"),
                     long=c(-60.703686,-60.677072,-60.487925),
                     lat=c(-31.617098,-32.953630,-31.777752))

par.r <- data.frame(riv=c("Paraná River"),
                    long=c(-59.65),
                    lat=c(-29.88))

p2 <- ggplot() +
  # Provinces
  geom_sf(data=study.area, fill="#f7f4f9", color="#d4b9da") +
  # Paraná River
  geom_sf(data=map.riv, col="#38afcd", fill="#38afcd") +
  # Meteorological stations
  geom_point(data=stations, aes(x=long, y=lat),
             size=2, col="#980043", fill="#980043") +
  # Province names
  geom_text(data=study.nmes, aes(x=X, y=Y, label=NAME_1), size=6,
            color="#d4b9da", fontface="bold", check_overlap=FALSE) +
  # River name
  ggrepel::geom_text_repel(data=par.r, aes(x=long, y=lat, label=riv), 
                           nudge_x=1.8, nudge_y=0.5, size=5, fontface="bold") +
  # Aesthetics
  theme_void() +
  theme(legend.position="none",
        plot.margin=unit(c(0,0,0.5,0), "cm")) +
  coord_sf(xlim=c(-65,-57.8),
           ylim=c(-34.7, -27.99393)) +
  # North arrow
  ggspatial::annotation_north_arrow(location="tl",
                                    which_north="true", 
                                    style=north_arrow_orienteering(), 
                                    height=unit(0.75, "cm"), 
                                    width=unit(0.75, "cm")) +
  # Scale bar
  ggspatial::annotation_scale(location="br", pad_x=unit(0, "cm"), line_width=0.5)

# Mosaic maps
fig2a <- cowplot::ggdraw(p2) +
  cowplot::draw_plot(p1, 
                     x=0.01, y=0.01,
                     width=0.3,
                     height=0.3)

# Figure 2b: time series ####
fig2b <- data.lepto %>% 
  dplyr::select(1:6) %>% 
  mutate(inc=(cases/pop)*100000) %>% 
  ggplot() +
  geom_bar(aes(date, inc, fill=prov), stat="identity", position="dodge") +
  labs(fill="", x="",y="Cases per 100,000 inhabitants") +
  ggthemes::scale_fill_tableau("Color Blind") +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  facet_wrap(~prov, nrow=2, scales="free")+
  theme_bw() +
  theme(axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        legend.position="none",
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        strip.background=element_rect(fill="white", color="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text=element_text(size=14, face="bold"),
        plot.margin=unit(c(1,1,0,0), "cm"))

# Combine figure 1a with figure 1b
fig2 <- cowplot::plot_grid(fig2a, NULL, fig2b, rel_widths=c(0.5,0.1,1), 
                           nrow=1, labels=c("a.", "b."))

ggsave(plot=fig2, filename="figures/figure2.pdf", width=18, height=6)

# Figure 3: observed versus fitted ####
er <- er.enso.fit$fits[[2]] %>% 
  mutate(mod="Random effects only") %>% 
  bind_rows(er.enso.fit$fits[[6]] %>% 
              mutate(mod="ENSO"),
            er.clim.fit$fits[[18]] %>% 
              mutate(mod="Local climate")) %>%
  `rownames<-`(NULL) %>% 
  dplyr::select(mean, `0.025quant`, `0.975quant`, mod) %>%
  magrittr::set_colnames(c("mean", "lower", "upper", "mod")) %>% 
  bind_cols(date=rep(df.er$date, 3)) %>%
  mutate(mod=factor(mod, levels=c("Random effects only","ENSO","Local climate"))) %>% 
  ggplot() +
  geom_line(data=df.er, aes(date, cases), col="grey80", size=0.7) +
  scale_color_manual(values=c("Observed"="grey80")) +
  geom_line(aes(date, mean, col=mod), linetype="dashed", size=0.7) +
  geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=mod), alpha=0.2) +
  scale_x_date("", date_breaks="2 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
  ggthemes::scale_fill_tableau("Color Blind") +
  facet_wrap(~mod) +
  labs(y="", col="", fill="") +
  theme_bw() +
  theme(axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        legend.position="none",
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        strip.background=element_rect(fill="white", color="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text=element_text(size=14, face="bold"),
        plot.margin=unit(c(1,1,0,0), "cm"))

sf <- sf.enso.fit$fits[[2]] %>% 
  mutate(mod="Random effects only") %>% 
  bind_rows(sf.enso.fit$fits[[6]] %>% 
              mutate(mod="ENSO"),
            sf.clim.fit$fits[[18]] %>% 
              mutate(mod="Local climate")) %>%
  `rownames<-`(NULL) %>% 
  dplyr::select(mean, `0.025quant`, `0.975quant`, mod) %>%
  magrittr::set_colnames(c("mean", "lower", "upper", "mod")) %>% 
  bind_cols(date=rep(df.sf$date, 3)) %>%
  mutate(mod=factor(mod, levels=c("Random effects only","ENSO","Local climate"))) %>% 
  ggplot() +
  geom_line(data=df.er, aes(date, cases), col="grey80", size=0.7) +
  scale_color_manual(values=c("Observed"="grey80")) +
  geom_line(aes(date, mean, col=mod), linetype="dashed", size=0.7) +
  geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=mod), alpha=0.2) +
  scale_x_date("", date_breaks="2 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
  ggthemes::scale_fill_tableau("Color Blind") +
  facet_wrap(~mod) +
  labs(y="", col="", fill="") +
  theme_bw() +
  theme(axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        legend.position="none",
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        strip.background=element_rect(fill="white", color="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text=element_text(size=14, face="bold"),
        plot.margin=unit(c(1,1,0,0), "cm"))

fig3 <- cowplot::plot_grid(er, sf, nrow=2, labels=c("a.", "b."))

ggsave(plot=fig3, filename="figures/figure3.pdf", width=18, height=10)

# Figure 4: ROC curves ####
trig.er <- bind_rows(er.preds$trigger) %>% 
  slice(c(1,3,5)) %>% 
  mutate(model=c("Monthly random effects", "ENSO", "Local climate"))
trig.sf <- bind_rows(sf.preds$trigger) %>% 
  slice(c(1,3,4)) %>% 
  mutate(model=c("Monthly random effects", "ENSO", "Local climate"))

roc.er <- data.frame(model=rep(c("Monthly random effects", "ENSO", "Local climate"), 
                               times=c(94,74,73)),
                     sens=c(er.preds$roc[[1]]$sensitivities,
                            er.preds$roc[[3]]$sensitivities,
                            er.preds$roc[[5]]$sensitivities),
                     spec=1-c(er.preds$roc[[1]]$specificities,
                              er.preds$roc[[3]]$specificities,
                              er.preds$roc[[5]]$specificities)) %>% 
  arrange(sens)

er <- roc.er %>% 
    ggplot(aes(spec, sens, group=model, col=model)) +
    geom_line() +
    ggthemes::scale_color_tableau("Color Blind") +
  geom_segment(aes(x=0, xend=1, y=0, yend=1), color="darkgrey", linetype="dashed") +
  geom_point(data=trig.er, aes(x=1-specificity, y=sensitivity, col=model),
             shape=1, size=5) +
  geom_text(data=trig.er, aes(x=(1-specificity)+c(0.05,0.05,-0.05), 
                              y=sensitivity+c(-0.05,-0.05,0.05), 
                              label=round(threshold, 2),
                              col=model), 
            size=4,
            fontface="bold",
            inherit.aes=FALSE,
            show.legend=FALSE) +
  labs(x="FAR", y="HR", col="") +
  theme_bw() +
  theme(axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        legend.position="none",
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        strip.background=element_rect(fill="white", color="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text=element_text(size=14, face="bold"))

roc.sf <- data.frame(model=rep(c("Monthly random effects", "ENSO", "Local climate"), 
                               times=c(105,106,103)),
                     sens=c(sf.preds$roc[[1]]$sensitivities,
                            sf.preds$roc[[3]]$sensitivities,
                            sf.preds$roc[[4]]$sensitivities),
                     spec=1-c(sf.preds$roc[[1]]$specificities,
                              sf.preds$roc[[3]]$specificities,
                              sf.preds$roc[[4]]$specificities)) %>% 
  arrange(sens)

sf <- roc.sf %>% 
  ggplot(aes(spec, sens, group=model, col=model)) +
  geom_line() +
  ggthemes::scale_color_tableau("Color Blind") +
  geom_segment(aes(x=0, xend=1, y=0, yend=1), color="darkgrey", linetype="dashed") +
  geom_point(data=trig.sf, aes(x=1-specificity, y=sensitivity, col=model), shape=1, size=5) +
    geom_text(data=trig.sf, aes(x=(1-specificity)+c(0.05,0.04,-0.04), 
                                y=sensitivity+c(-0.05,-0.045,0.05), 
                                label=round(threshold, 2),
                                col=model), 
              size=4,
              fontface="bold",
              inherit.aes=FALSE,
              show.legend=FALSE) +
  labs(x="FAR", y="HR", col="") +
    
  theme_bw() +
  theme(axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        legend.position="none",
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        strip.background=element_rect(fill="white", color="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text=element_text(size=14, face="bold"))

leg <- cowplot::get_legend(sf+theme(legend.position="bottom", 
                                    legend.direction="horizontal",
                                    legend.text=element_text(size=14, face="bold"),
                                    legend.key.size=unit(1, "cm")))

up <- cowplot::plot_grid(er, sf, nrow=1, labels=c("a.", "b."))
fig4 <- cowplot::plot_grid(up, leg, nrow=2, rel_heights=c(1,0.1))
ggsave(plot=fig4, filename="figures/figure4.pdf", width=12, height=5)

# Figure 5: Outbreak in 2010 ####
# Compute outbreak probability for the period of interest
bind_rows(er.preds$outb.prob[[3]] %>% mutate(mod="ENSO model"),
          er.preds$outb.prob[[5]] %>% mutate(mod="Local model")) %>% 
  filter(date %in% c(as.Date("2010-03-01", format="%Y-%m-%d"))) %>% 
  dplyr::select(mod, prob.out)

# ENSO average outbreak prob in March 2010: 84%
# Local climate outbreak prob in March 2010: 89%

# Extract data of interest
dt <- bind_rows(
  data.frame(
    median=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.975),
    threshold=er.preds$outb.prob[[3]]$thresh,
    mod="ENSO"),
  data.frame(
    median=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.975),
    threshold=er.preds$outb.prob[[5]]$thresh,
    mod="Local climate")) %>%
  bind_cols(date=rep(df.er$date, 2)) %>%
  mutate(mod=factor(mod, levels=c("ENSO","Local climate")))

plot.out <- function(filt, text, max.rect=NA){
  dt %>% 
    filter(mod==filt,
           date %in% c(as.Date("2009-01-01", format="%Y-%m-%d"):as.Date("2013-06-15", format="%Y-%m-%d"))) %>% 
    ggplot() +
    geom_line(data=df.er %>% 
                filter(date %in% c(as.Date("2009-01-01", format="%Y-%m-%d"):as.Date("2013-06-15", format="%Y-%m-%d"))), 
              aes(date, cases), col="grey80", size=0.7) +
    scale_color_manual(values=c("Observed"="grey80")) +
    geom_line(aes(date, threshold), linetype="dashed", size=0.7, col="black", alpha=0.5) +
    geom_line(aes(date, median, col=mod), linetype="dashed", size=0.7) +
    geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=mod), alpha=0.2) +
    scale_x_date("", date_breaks="1 year", date_labels="%Y") +
    geom_rect(data=data.frame(xmin=as.Date("2010-02-15", format="%Y-%m-%d"),
                              xmax=as.Date("2010-04-15", format="%Y-%m-%d"),
                              ymin=-0.5,
                              ymax=max.rect),
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill=NA, col="grey20", size=1) +
    annotate(geom="text",
             x=as.Date("2010-07-01", format="%Y-%m-%d"),
             y=max.rect-5,
             label=paste0("Outbreak Prob:", text),
             hjust=0,
             size=8) +
    ylim(-0.5, 68) +
    ggthemes::scale_colour_tableau("Color Blind") +
    ggthemes::scale_fill_tableau("Color Blind") +
    labs(y="Number of cases", col="", fill="") +
    theme_bw() +
    theme(axis.text=element_text(size=16, face="bold"),
          axis.text.x=element_text(angle=45, hjust=1),
          axis.title=element_text(size=16, face="bold"),
          legend.position="none",
          panel.border=element_blank(),
          axis.line=element_line(color="black"),
          strip.background=element_rect(fill="white", color="white"),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
} 

pt1 <- plot.out("ENSO", "84%", 53)
pt2 <- plot.out("Local climate", "89%", 68)

fig5 <- cowplot::plot_grid(pt1, pt2)
ggsave(plot=fig5, filename="figures/figure5.pdf", width=18, height=7)

# Table 1: goodness of fit statistics ####
er.enso.fit$gof %>% slice(1,2,6) %>% dplyr::select(form, dic, rsq.re, rsq.null)
er.clim.fit$gof %>% slice(18) %>% dplyr::select(form, dic, rsq.re, rsq.null)
sf.enso.fit$gof %>% slice(1,2,6) %>% dplyr::select(form, dic, rsq.re, rsq.null)
sf.clim.fit$gof %>% slice(18) %>% dplyr::select(form, dic, rsq.re, rsq.null)

# Table 2: AUC and confidence interval ####
er.preds$sens.table %>% 
  filter(model%in%c(1,3,5), condition=="Hit") %>% 
  dplyr::select(model, auc, lower, upper)
sf.preds$sens.table %>% 
  filter(model%in%c(1,3,4), condition=="Hit") %>% 
  dplyr::select(model, auc, lower, upper)

## ############################################################################
## END
## ############################################################################
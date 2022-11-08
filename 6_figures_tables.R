################################################################################
## 6- EXTRACT OUTPUTS FROM MODELS FOR TABLES AND FIGURES 
## (Lotto-Rees et al., 2022)
##
## Author: Martin Lotto Batista
## Year: 2022
################################################################################
pacman::p_load("ggthemes", "tidyverse", "here", "zoo", "sf", "ggspatial", "pROC")

################################################################################
## DATA
################################################################################
df.lepto <- readRDS(here("data_use", "prov_cases.rds"))
pop <- readRDS(here("data_use", "pop.rds"))
prcp <- readRDS(here("data_use", "prcp.rds"))
riv <- readRDS(here("data_use", "river_height.rds"))
nino <- readRDS(here("data_use", "nino.rds"))

data.lepto <- df.lepto %>% 
  left_join(pop) %>% 
  left_join(prcp) %>% 
  left_join(riv) %>% 
  left_join(nino) %>% 
  arrange(prov, year, month) %>% 
  mutate(ID.year=rep(c(1:12), each=12, times=2),
         ID.month=rep(seq(1:144), 2))

df.er <- data.lepto %>% filter(prov == "Entre Ríos") %>% 
  mutate(across(.cols=c(7:35), ~(.x-mean(.x))/sd(.x)))
df.sf <- data.lepto %>% filter(prov == "Santa Fe") %>% 
  mutate(across(.cols=c(7:35), ~(.x-mean(.x))/sd(.x)))

er.enso.fit <- readRDS(here("model_out", "er_enso_fit.rds"))
er.clim.fit <- readRDS(here("model_out", "er_clim_fit.rds"))
sf.enso.fit <- readRDS(here("model_out", "sf_enso_fit.rds"))
sf.clim.fit <- readRDS(here("model_out", "sf_clim_fit.rds"))
er.preds <- readRDS(here("model_out", "er_preds.rds"))
sf.preds <- readRDS(here("model_out", "sf_preds.rds"))
#####

################################################################################
## MAIN TEXT
################################################################################
# Figure 2a: maps ####
# Shapefiles
sa <- readRDS(here("temp_files_draft", "south_america_full.rds"))
study.area <- readRDS(here("temp_files_draft", "stafe_enrios.rds")) %>%  
  group_by(NAME_1) %>% 
  summarise(.groups="drop")
study.nmes <- cbind(study.area, st_coordinates(st_centroid(study.area$geometry)))
map.riv <- readRDS(here("temp_files_draft", "shp_riv.rds")) %>% 
  filter(!grepl("Uru|Guale|Pla|Pal", fna)) %>% 
  st_crop(c(xmin=-60.80564,ymin=-34.22840,xmax=-57,ymax=-27.7))

# South America with study area
bord <- sa %>% mutate(a=0) %>% group_by(a) %>% summarise(.groups="drop")

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
  geom_text(data=study.nmes, aes(x=X, y=Y, label=NAME_1), size=3,
            color="#d4b9da", fontface="bold", check_overlap=FALSE) +
  # River name
  ggrepel::geom_text_repel(data=par.r, aes(x=long, y=lat, label=riv), 
                           nudge_x=1.2, nudge_y=0.5, size=3) +
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

ggsave(plot=fig2a, filename=here("figures", "figure2a.pdf"), width=7, height=6)

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
  theme(axis.text=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=12, face="bold"),
        legend.position="none",
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        strip.background=element_rect(fill="white", color="white"),
        strip.text=element_text(size=12, face="italic"),
        plot.margin=unit(c(1,1,0,0), "cm"))

ggsave(plot=fig2b, filename=here("figures", "figure2b.pdf"), width=18, height=6)

# Combine figure 1a with figure 1b
fig2 <- cowplot::plot_grid(fig2a, NULL, fig2b, rel_widths=c(0.5,0.1,1), 
                           nrow=1, labels=c("a.", "b."))

ggsave(plot=fig2, filename=here("figures", "figure2.pdf"), width=18, height=6)

# Figure 3: observed versus fitted (lines) ####
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
  theme(axis.text.x=element_text(angle=45, hjust=1, size=11),
        axis.text.y=element_text(size=12),
        strip.background =element_rect(fill="white"),
        strip.text=element_text(face="bold", size=12),
        legend.position="none")

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
  theme(axis.text.x=element_text(angle=45, hjust=1, size=11),
        axis.text.y=element_text(size=12),
        strip.background =element_rect(fill="white"),
        strip.text=element_text(face="bold", size=12),
        legend.position="none")

leg <- cowplot::get_legend(er+geom_line(data=df.er, aes(date, cases), col="grey80", size=0.7) +
                              scale_color_manual(values=c("Observed"="grey80")) +
                              theme(legend.position="bottom",
                                    legend.text=element_text(size=12, face="italic")))

fig3 <- cowplot::plot_grid(er, sf, nrow=2, labels=c("a", "b"))

ggsave(plot=fig3, filename=here("figures", "figure3.pdf"), width=18, height=10)

# Figure 3: observed versus fitted (points added) ####
#er <- 
  er.enso.fit$fits[[2]] %>% 
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
  geom_line(data=df.er, aes(date, cases), col="grey80", alpha=0.4, size=0.7) +
  geom_point(data=df.er, aes(date, cases), col="grey80", size=1.5) +
  scale_color_manual(values=c("Observed"="grey80")) +
  geom_line(aes(date, mean, col=mod), linetype="dashed", size=0.7) +
  geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=mod), alpha=0.2) +
  scale_x_date("", date_breaks="2 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
  ggthemes::scale_fill_tableau("Color Blind") +
  facet_wrap(~mod) +
  labs(y="", col="", fill="") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=11),
        axis.text.y=element_text(size=12),
        strip.background =element_rect(fill="white"),
        strip.text=element_text(face="bold", size=12),
        legend.position="none")

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
  geom_line(data=df.er, aes(date, cases), col="grey80", alpha=0.4, size=0.7) +
  geom_point(data=df.er, aes(date, cases), col="grey80", size=1.5) +
  scale_color_manual(values=c("Observed"="grey80")) +
  geom_line(aes(date, mean, col=mod), linetype="dashed", size=0.7) +
  geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=mod), alpha=0.2) +
  scale_x_date("", date_breaks="2 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
  ggthemes::scale_fill_tableau("Color Blind") +
  facet_wrap(~mod) +
  labs(y="", col="", fill="") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=11),
        axis.text.y=element_text(size=12),
        strip.background =element_rect(fill="white"),
        strip.text=element_text(face="bold", size=12),
        legend.position="none")

leg <- cowplot::get_legend(er+geom_line(data=df.er, aes(date, cases), col="grey80", size=0.7) +
                             scale_color_manual(values=c("Observed"="grey80")) +
                             theme(legend.position="bottom",
                                   legend.text=element_text(size=12, face="italic")))

fig3 <- cowplot::plot_grid(er, sf, nrow=2, labels=c("a", "b"))

ggsave(plot=fig3, filename=here("figures", "figure3.pdf"), width=18, height=10)

# Figure 4: ROC curves ####
trig.er <- bind_rows(er.preds$trigger) %>% 
  slice(c(1,3,5)) %>% 
  mutate(sensitivity=sensitivity+c(0,0.02,0),
         mod=c("Random effects only", "ENSO", "Local climate"))
trig.sf <- bind_rows(sf.preds$trigger) %>% 
  slice(c(1,3,4)) %>% 
  mutate(mod=c("Random effects only", "ENSO", "Local climate"))

er <- ggroc(list(`Random effects only`=er.preds$roc[[1]], 
                 ENSO=er.preds$roc[[3]], 
                 `Local climate`=er.preds$roc[[5]])) +
  ggthemes::scale_color_tableau("Color Blind") +
  geom_segment(aes(x=1, xend=0, y=0, yend=1), color="darkgrey", linetype="dashed") +
  geom_point(data=trig.er, aes(x=specificity, y=sensitivity-c(0,0.02,0), col=mod), shape=1, size=5) +
  geom_text(data=trig.er, aes(x=specificity+0.05, y=sensitivity+0.02, label=round(threshold, 2)), inherit.aes=FALSE) +
  labs(x="1-Specificity", y="Sensitivity", col="") +
  theme_bw() +
  theme(legend.position="none")

sf <- ggroc(list(`Random effects only`=sf.preds$roc[[1]], 
                 ENSO=sf.preds$roc[[3]], 
                 `Local climate`=sf.preds$roc[[4]])) +
  ggthemes::scale_color_tableau("Color Blind") +
  geom_segment(aes(x=1, xend=0, y=0, yend=1), color="darkgrey", linetype="dashed") +
  geom_point(data=trig.sf, aes(x=specificity, y=sensitivity, col=mod), shape=1, size=5) +
  geom_text(data=trig.sf, aes(x=specificity+0.05, y=sensitivity+0.02, label=round(threshold, 2)), inherit.aes=FALSE) +
  labs(x="1-Specificity", y="", col="") +
  theme_bw() +
  theme(legend.position="none")

leg <- cowplot::get_legend(sf+theme(legend.position="bottom", legend.direction="horizontal"))

up <- cowplot::plot_grid(er, sf, nrow=1, labels=c("a.", "b."))
fig4 <- cowplot::plot_grid(up, leg, nrow=2, rel_heights=c(1,0.1))
ggsave(plot=fig4, filename=here("figures", "figure4.pdf"), width=8, height=4)

# Figure 5: Outbreak in 2010 ####
# Compute outbreak probability for the period of interest
bind_rows(er.preds$outb.prob[[3]] %>% mutate(mod="ENSO model"),
          er.preds$outb.prob[[5]] %>% mutate(mod="Local model")) %>% 
  filter(date %in% c(as.Date("2010-03-01", format="%Y-%m-%d"))) %>% 
  select(mod, prob.out)

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
              fill=NA, col="grey50", size=0.5) +
    annotate(geom="text",
             x=as.Date("2010-07-01", format="%Y-%m-%d"),
             y=max.rect-5,
             label=paste0("Outbreak Prob:", text),
             hjust=0,
             size=8) +
    ylim(-0.5, 68) +
    ggthemes::scale_colour_tableau("Color Blind") +
    ggthemes::scale_fill_tableau("Color Blind") +
    labs(y="", col="", fill="") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, size=16, face="bold"),
          axis.text.y=element_text(size=16, face="bold"),
          strip.background =element_rect(fill="white"),
          strip.text=element_text(face="bold", size=12),
          legend.position="none")
} 

pt1 <- plot.out("ENSO", "84%", 53)
pt2 <- plot.out("Local climate", "89%", 68)

fig5 <- cowplot::plot_grid(pt1, pt2, labels=c("a.", "b."))
fig5
ggsave(plot=fig5, filename=here("figures", "figure5.pdf"), width=18, height=10)

# Table 1: goodness of fit statistics ####
er.enso.fit$gof %>% slice(1,2,6) %>% dplyr::select(form, dic, cpo, rsq.re, rsq.null)
er.clim.fit$gof %>% slice(18) %>% dplyr::select(form, dic, cpo, rsq.re, rsq.null)
sf.enso.fit$gof %>% slice(1,2,6) %>% dplyr::select(form, dic, cpo, rsq.re, rsq.null)
sf.clim.fit$gof %>% slice(18) %>% dplyr::select(form, dic, cpo, rsq.re, rsq.null)

# Table 2: AUC and confidence interval ####
er.preds$sens.table %>% filter(model%in%c(1,3,5), condition=="Hit") %>% dplyr::select(model, auc, lower, upper)
sf.preds$sens.table %>% filter(model%in%c(1,3,4), condition=="Hit") %>% dplyr::select(model, auc, lower, upper)

################################################################################
## SUPPLEMENTARY TEXT
################################################################################
# Figure S1: times series of predictors ####
p1 <- data.lepto %>%
  dplyr::select(date, prov, par.0) %>% 
  mutate(prov=str_replace_all(prov, c("Entre Ríos"="Paraná river (ER)", "Santa Fe"="Paraná river (SF)"))) %>% 
  ggplot() +
  geom_line(aes(date, par.0, col=prov), size=1) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
#  scale_color_manual(values=c("#67001f", "#c994c7")) +
  labs(x="", y="River height (m/month)", col="") +
  theme_bw() +
  theme(legend.position=c(0.9,0.9),
        axis.title.y=element_text(size=10),
        axis.line=element_line(size=0.7),
        panel.border=element_blank())

p2 <- data.lepto %>%
  dplyr::select(date, prov, prcp.0) %>% 
  ggplot() +
  geom_line(aes(date, prcp.0, col=prov), size=1) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
#  scale_color_manual(values=c("#67001f", "#c994c7")) +
  labs(x="", y="Precipitation (mm/month)", col="") +
  theme_minimal() +
  theme(legend.position=c(0.9,0.9),
        axis.title.y=element_text(size=10),
        axis.line=element_line(size=0.7),
        panel.border=element_blank())

# Segment plot for Niño
# p3 <- nino %>%
#   mutate(date=as.Date(lubridate::make_date(year, month)),
#          nino34.0=scale(nino34.0)[,1],
#          mycolor=ifelse(nino34.0>0, "type1", "type2")) %>% 
#   filter(year<2021) %>% 
#   ggplot() +
#   geom_segment(aes(x=date, y=0, xend=date, yend=nino34.0, color=mycolor)) +
#   scale_color_manual(values=c("#67001f", "#c994c7")) +
#   geom_hline(yintercept=0, alpha=0.2, linetype="dashed") +
#   scale_x_date(date_breaks="1 year", date_labels="%Y") +
#   labs(x="", y="Niño 3.4 Index", col="")+
#   theme_minimal() +
#   theme(legend.position="none",
#         axis.title.y=element_text(size=10),
#         axis.line=element_line(size=0.7),
#         panel.border=element_blank())

# Shaded plot for Niño
# p3 <- nino %>% 
#   mutate(date=as.Date(lubridate::make_date(year, month)),
#          nino34.0=scale(nino34.0)[,1],
#          nino=ifelse(nino34.0>=0, nino34.0,0),
#          nina=ifelse(nino34.0<0, nino34.0,0),
#          event=ifelse(nino34.0>=0.5, "niño", "niña")) %>% 
#   select(date, nino34.0, nino, nina, event) %>% 
#   ggplot(aes(x=date, y=nino34.0)) +
#   geom_hline(yintercept=c(0.5, -0.5), linetype="dashed", col="grey") +
#   geom_ribbon(aes(ymax=nino, ymin=0), fill="red", alpha=0.7) +
#   geom_ribbon(aes(ymax=nina, ymin=0), fill="blue", alpha=0.7) +
#   geom_line(size=.3) +
#   scale_x_date(date_breaks="1 year", date_labels="%Y") +
#   labs(x="", y="Niño 3.4 Index", col="")+
#   theme_minimal() +
#   theme(legend.position="none",
#         axis.title.y=element_text(size=10),
#         axis.line=element_line(size=0.7),
#         panel.border=element_blank())

# Attempt including a third category with neutral
# rle <- rle(dt$event)
# starts <- {ends <- cumsum(rle$lengths)} - rle$lengths + 1
# groups <- mapply(seq, from=starts, to=ends+1, SIMPLIFY=FALSE)
# dt2 <- dt[unlist(groups),]
# dt2$event <- rep(rle$lengths, lengths(groups))
# dt2$groups <- rep(seq_along(groups), lengths(groups))
# dt2 <- head(dt2, -1)
# 
# dt2 %>% 
#   ggplot(aes(date, nino34.0, group=groups)) +
#   geom_line() +
#   geom_ribbon(aes(ymin=0, ymax=nino34.0, fill=event), color=NA, alpha=.4)

# ENSO with neutral band
dt <- nino %>% 
  mutate(date=as.Date(lubridate::make_date(year, month)),
         nino34.0=scale(nino34.0)[,1],
         nino=case_when(nino34.0<=0.5 ~ 0,
                        nino34.0>=0 ~ nino34.0,
                        TRUE ~ 0),
         nina=ifelse(nino34.0< -0.5, nino34.0, 0),
         event=ifelse(nino34.0>=0.5, "niño", "niña")) %>% 
  select(date, nino34.0, nino, nina, event)

new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}

# Testing creating a plot with above or below 0.5/ -0.5 darker than the rest
#p3 <- 
  dt %>%
  ggplot(aes(x=date)) +
    geom_hline(yintercept=0, col="grey", linetype="dashed") +
    geom_hline(yintercept=c(0.5, -0.5), linetype="dashed", col="grey") +
    ggh4x::stat_difference(aes(ymin = 0, ymax = nino34.0), alpha=0.2) +
  scale_fill_manual(values=c("red", "blue")) +
  new_scale_fill() +
  ggh4x::stat_difference(aes(ymin = 0.5, ymax = nino34.0)) +
  scale_fill_manual(values=c(alpha("red", 0.7), "NA")) +
    new_scale_fill() +
    ggh4x::stat_difference(aes(ymin = -0.5, ymax = nino34.0)) +
  scale_fill_manual(values=c("NA", alpha("blue", 0.7))) +
  geom_line(aes(y=nino34.0)) +
  theme_bw() +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  labs(x="", y="Niño 3.4 Index", col="")+
  theme_minimal() +
  theme(legend.position="none",
        axis.title.y=element_text(size=10),
        axis.line=element_line(size=0.7),
        panel.border=element_blank(),
        axis.text.x=element_text(angle=45, hjust=1))

s1 <- cowplot::plot_grid(p1, p2, p3, nrow=3)
ggsave(plot=s1, filename=here("figures", "sup_figure1.pdf"), width=10, height=12)

# Figure S2: correlation matrix and variance inflation factor ####
# Functions for calculating the variance inflation factor
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}
corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  #correlation part
#  cat("Correlations of the variables\n\n")
  tmp_cor <- cor(dataz,use="complete.obs")
#  print(tmp_cor)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1,dataz)
  lm_mod  <- lm(form,dataz)
   
  # cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}

# Entre Rios
vars <- expand.grid(names(df.er[c(7,15,23)]),
                    names(df.er[c(7,15,23)])) %>%
  mutate(across(.cols=c(Var1, Var2), ~as.character(.x)))

cors1 <- c()
cors2 <- c()
for(i in 1:9){
  cors1[i] <- energy::dcor(df.er[,vars$Var1[i]], df.er[,vars$Var2[i]])
  cors2[i] <- cor(df.er[,vars$Var1[i]], df.er[,vars$Var2[i]])
  print(i)
}

cors.er <- vars %>%
  mutate(dcor=round(cors1,2),
         pcor=round(cors2,2))

vif.er <- corvif(df.er[,c(7,15,23)])

# Santa Fe
vars <- expand.grid(names(df.sf[c(7,15,23)]),
                    names(df.sf[c(7,15,23)])) %>%
  mutate(across(.cols=c(Var1, Var2), ~as.character(.x)))

cors1 <- c()
cors2 <- c()
for(i in 1:9){
  cors1[i] <- energy::dcor(df.sf[,vars$Var1[i]], df.sf[,vars$Var2[i]])
  cors2[i] <- cor(df.sf[,vars$Var1[i]], df.sf[,vars$Var2[i]])
}

cors.sf <- vars %>%
  mutate(dcor=round(cors1,2),
         pcor=round(cors2,2))

vif.sf <- corvif(df.sf[,c(7,29,23)])

# Create plots
p1 <- cors.er %>%
  mutate(Var1=str_replace_all(Var1, c("prcp"="Precipitation", "par.0"="Paraná River", 
                                      "nino34.0"="Niño")),
         Var2=str_replace_all(Var2, c("prcp"="Precipitation", "par.0"="Paraná River", 
                                      "nino34.0"="Niño"))) %>% 
  ggplot(aes(Var1, Var2)) +
  geom_tile(aes(fill=dcor)) +
  geom_text(aes(label=dcor), size=7) +
  scale_fill_gradient(low="white", high="red") +
  labs(x="", y="", fill="", title="Entre Ríos") +
  theme_bw() +
  theme(legend.position="none",
        axis.text.x=element_text(angle=35, hjust=1),
        title=element_text(size=12, face="bold"),
        axis.text=element_text(size=12, face="bold"))

p2 <- cors.sf %>%
  mutate(Var1=str_replace_all(Var1, c("prcp"="Precipitation", "par.0"="Paraná River", 
                                      "nino34.0"="Niño")),
         Var2=str_replace_all(Var2, c("prcp"="Precipitation", "par.0"="Paraná River", 
                                      "nino34.0"="Niño"))) %>% 
  ggplot(aes(Var1, Var2)) +
  geom_tile(aes(fill=dcor)) +
  geom_text(aes(label=dcor), size=7) +
  scale_fill_gradient(low="white", high="red") +
  labs(x="", y="", fill="", title="Santa Fe") +
  theme_bw() +
  theme(legend.position="none",
        axis.text.x=element_text(angle=35, hjust=1),
        title=element_text(size=12, face="bold"),
        axis.text=element_text(size=12, face="bold"))

p3 <- vif.er %>% 
  bind_rows(vif.sf) %>% 
  mutate(prov=rep(c("Santa Fe", "Entre Ríos"), each=3)) %>% 
  rownames_to_column("var") %>% 
  mutate(var=c("Precipitation", "Paraná River", "ENSO", 
               "Precipitation", "Paraná River", "ENSO")) %>% 
  ggplot(aes(prov, var)) +
  geom_tile(aes(fill=GVIF)) +
  geom_text(aes(label=round(GVIF,2)), size=7) +
  scale_fill_gradient(low="white", high="red") +
  labs(x="", y="", fill="", title="GVIF") +
  theme_bw() +
  theme(legend.position="none",
        axis.text.x=element_text(angle=35, hjust=1),
        title=element_text(size=12, face="bold"),
        axis.text=element_text(size=12, face="bold"))

s2 <- cowplot::plot_grid(p1, p2, p3, nrow=1, labels=c("a.", "b.", "c."))
ggsave(plot=s2, filename=here("figures", "sup_figure2.pdf"), width=18, height=6)

# Figure S3: random effects ####
month.labs <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

plot.res <- function(data, labs, ylab, pr, pos, dir){
  data %>% 
    mutate(mod=factor(mod, levels=c("Random effects only", "ENSO", "Local climate"))) %>% 
    rename(lower=`0.025quant`,
           upper=`0.975quant`) %>% 
    ggplot(aes(ID, mean, col=mod)) +
    geom_hline(yintercept=0, alpha=0.2, linetype="dashed") +
    geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.5)) +
    ggthemes::scale_color_tableau("Color Blind") +
    labs(y=ylab, x="", col="") +
    scale_x_continuous(breaks=c(1:length(labs)), labels=labs) +
    theme_bw() +
    theme(legend.position="none",
          strip.background=element_rect(fill="white"))
}

er1 <- bind_rows(er.enso.fit$random[[2]]$month %>% mutate(mod="Random effects only"), 
                 er.enso.fit$random[[6]]$month %>% mutate(mod="ENSO"),
                 er.clim.fit$random[[18]]$month %>% mutate(mod="Local climate")) 

er2 <- bind_rows(er.enso.fit$random[[2]]$ID.year %>% mutate(mod="Random effects only"), 
                 er.enso.fit$random[[6]]$ID.year %>% mutate(mod="ENSO"),
                 er.clim.fit$random[[18]]$ID.year %>% mutate(mod="Local climate")) 

sf1 <- bind_rows(sf.enso.fit$random[[2]]$month %>% mutate(mod="Random effects only"), 
                 sf.enso.fit$random[[6]]$month %>% mutate(mod="ENSO"),
                 sf.clim.fit$random[[18]]$month %>% mutate(mod="Local climate")) 

sf2 <- bind_rows(sf.enso.fit$random[[2]]$ID.year %>% mutate(mod="Random effects only"), 
                 sf.enso.fit$random[[6]]$ID.year %>% mutate(mod="ENSO"),
                 sf.clim.fit$random[[18]]$ID.year %>% mutate(mod="Local climate")) 

p1 <- plot.res(er1, labs=month.labs, ylab=expression("Coefficient (" ~delta[t]~ ")"))
p2 <- plot.res(er2, labs=c(2009:2020), ylab=expression("Coefficient (" ~gamma[t]~ ")"))
p3 <- plot.res(sf1, labs=month.labs, ylab=expression("Coefficient (" ~delta[t]~ ")"))
p4 <- plot.res(sf2, labs=c(2009:2020), ylab=expression("Coefficient (" ~gamma[t]~ ")"))
leg <- cowplot::get_legend(p4+theme(legend.position="bottom"))

up <- cowplot::plot_grid(p1, p2, nrow=1, labels=c("a.", "b."))
down <- cowplot::plot_grid(p3, p4, nrow=1, labels=c("c.", "d."))

s3 <- cowplot::plot_grid(up, down, leg, nrow=3, rel_heights=c(1,1,0.1))
ggsave(plot=s3, filename=here("figures", "sup_figure3.pdf"), width=10, height=5)

# Figure S4: effect sizes ####
er <- bind_rows(er.enso.fit$params[[6]],
                er.clim.fit$params[[18]]) %>%
  filter(!var=="(Intercept)") %>% 
  rename(lower=`0.025quant`,
         upper=`0.975quant`) %>%
  mutate(var=str_replace_all(var, c("par.1"="Paraná River",
                                    "nino34.3"="ENSO",
                                    "prcp.1"="Precipitation")),
         var=factor(var, levels=c("ENSO", "Paraná River", "Precipitation")),
         across(.cols=c(2:6), ~exp(.x))) %>% 
  ggplot(aes(x=var, y=mean)) +
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3)) +
  coord_flip() +
  geom_hline(yintercept=1, alpha=0.2, linetype="dashed") +
  geom_vline(xintercept=1.5, linetype="dashed") +
  geom_text(aes(label=round(mean, 2), hjust=0, vjust=-1.5)) +
  labs(x="",
       y=expression("Exp (" ~beta[t]~ ")")) +
  theme_bw() +
  theme(axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold"))

sf <- bind_rows(sf.enso.fit$params[[6]],
                sf.clim.fit$params[[18]]) %>%
  filter(!var=="(Intercept)") %>% 
  rename(lower=`0.025quant`,
         upper=`0.975quant`) %>%
  mutate(var=str_replace_all(var, c("par.1"="Paraná River",
                                    "nino34.3"="ENSO",
                                    "prcp.1"="Precipitation")),
         var=factor(var, levels=c("ENSO", "Paraná River", "Precipitation")),
         across(.cols=c(2:6), ~exp(.x))) %>% 
  ggplot(aes(x=var, y=mean)) +
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3)) +
  coord_flip() +
  geom_hline(yintercept=1, alpha=0.2, linetype="dashed") +
  geom_vline(xintercept=1.5, linetype="dashed") +
  geom_text(aes(label=round(mean, 2), hjust=0, vjust=-1.5)) +
  labs(x="",
       y=expression("Exp (" ~beta[t]~ ")")) +
  theme_bw() +
  theme(axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold"))

s4 <- cowplot::plot_grid(er, sf, labels=c("a.", "b."))
ggsave(plot=s4, filename=here("figures", "sup_figure4.pdf"), width=8, height=3)

# Figure S5: observed versus predicted ####
# Create dfs for predicted values
er <- bind_rows(
  data.frame(
    median=matrixStats::rowQuantiles(er.preds$post.samples[[1]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(er.preds$post.samples[[1]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(er.preds$post.samples[[1]], cols=c(1:1000), probs=0.975),
    mod="Random effects only"),
  data.frame(
    median=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.975),
    mod="ENSO"),
  data.frame(
    median=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.975),
    mod="Local climate")) %>%
  bind_cols(date=rep(df.er$date, 3)) %>%
  mutate(mod=factor(mod, levels=c("Random effects only","ENSO","Local climate"))) %>% 
  ggplot() +
  geom_line(data=df.er, aes(date, cases), col="grey80", size=0.7) +
  scale_color_manual(values=c("Observed"="grey80")) +
  geom_line(aes(date, median, col=mod), linetype="dashed", size=0.7) +
  geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=mod), alpha=0.2) +
  scale_x_date("", date_breaks="2 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
  ggthemes::scale_fill_tableau("Color Blind") +
  facet_wrap(~mod) +
  labs(y="", col="", fill="") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=11),
        axis.text.y=element_text(size=12),
        strip.background =element_rect(fill="white"),
        strip.text=element_text(face="bold", size=12),
        legend.position="none")

sf <- bind_rows(
  data.frame(
    median=matrixStats::rowQuantiles(sf.preds$post.samples[[1]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(sf.preds$post.samples[[1]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(sf.preds$post.samples[[1]], cols=c(1:1000), probs=0.975),
    mod="Random effects only"),
  data.frame(
    median=matrixStats::rowQuantiles(sf.preds$post.samples[[3]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(sf.preds$post.samples[[3]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(sf.preds$post.samples[[3]], cols=c(1:1000), probs=0.975),
    mod="ENSO"),
  data.frame(
    median=matrixStats::rowQuantiles(sf.preds$post.samples[[4]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(sf.preds$post.samples[[4]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(sf.preds$post.samples[[4]], cols=c(1:1000), probs=0.975),
    mod="Local climate")) %>%
  bind_cols(date=rep(df.sf$date, 3)) %>%
  mutate(mod=factor(mod, levels=c("Random effects only","ENSO","Local climate"))) %>% 
  ggplot() +
  geom_line(data=df.sf, aes(date, cases), col="grey80", size=0.7) +
  scale_color_manual(values=c("Observed"="grey80")) +
  geom_line(aes(date, median, col=mod), linetype="dashed", size=0.7) +
  geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=mod), alpha=0.2) +
  scale_x_date("", date_breaks="2 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
  ggthemes::scale_fill_tableau("Color Blind") +
  facet_wrap(~mod) +
  labs(y="", col="", fill="") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=11),
        axis.text.y=element_text(size=12),
        strip.background =element_rect(fill="white"),
        strip.text=element_text(face="bold", size=12),
        legend.position="none")

s5 <- cowplot::plot_grid(er, sf, nrow=2, labels=c("a.", "b."))
ggsave(plot=s5, filename=here("figures", "sup_figure5.pdf"), width=18, height=10)


# Table S2: predictive measures ####
# AUC
er.preds$sens.table %>% filter(model%in%c(1,3,5), condition=="Hit") %>% dplyr::select(model, auc)
sf.preds$sens.table %>% filter(model%in%c(1,3,4), condition=="Hit") %>% dplyr::select(model, auc)

# Hit and false alarm rates
er.preds$verif.stats %>% filter(mod%in%c(1,3,5)) %>% dplyr::select(mod, hit.rate, far.rate)
sf.preds$verif.stats %>% filter(mod%in%c(1,3,4)) %>% dplyr::select(mod, hit.rate, far.rate)

# CRPS
unlist(er.preds$crps)[c(1,3,5)]
unlist(sf.preds$crps)[c(1,3,4)]

# CRPSS
(1-unlist(er.preds$crps)/er.preds$crps[[1]])[c(1,3,5)]
(1-unlist(sf.preds$crps)/sf.preds$crps[[1]])[c(1,3,4)]

################################################################################
## END
################################################################################
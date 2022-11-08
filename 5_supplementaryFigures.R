## ############################################################################
##
## Towards a Leptospirosis Early Warning System in North-Eastern Argentina
## Lotto Batista, M, Rees E., et al., 2022
##
## SCRIPT 05: Create supplementary figures and tables
##
## ############################################################################
##
## Script author: 
## - Martín Lotto Batista, ORCID: 0000-0002-9437-5270
##
## Contact: martin.lotto@bsc.es
## 
## License: 
##
## ############################################################################
pacman::p_load("ggthemes", "tidyverse", "here", 
               "zoo", "sf", "ggspatial", "pROC")

################################################################################
## DATA
################################################################################
df.lepto <- readRDS("data_use/prov_cases.rds")
pop <- readRDS("data_use/pop.rds")
prcp <- readRDS("data_use/prcp.rds")
riv <- readRDS("data_use/river_height.rds")
nino <- readRDS("data_use/nino.rds")

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

er.enso.fit <- readRDS("model_out/er_enso_fit.rds")
er.clim.fit <- readRDS("model_out/er_clim_fit.rds")
sf.enso.fit <- readRDS("model_out/sf_enso_fit.rds")
sf.clim.fit <- readRDS("model_out/sf_clim_fit.rds")
er.preds <- readRDS("model_out/er_preds.rds")
sf.preds <- readRDS("model_out/sf_preds.rds")
#####

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
p3 <- dt %>%
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

## ############################################################################
## END
## ############################################################################


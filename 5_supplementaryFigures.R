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
## - Eleanor Rees, ORCID: 0000-0002-4993-2795
##
## Contact: martin.lotto@bsc.es
## 
## License: 
##
## ############################################################################
pacman::p_load("ggthemes", "tidyverse", "here", 
               "zoo", "sf", "ggspatial", "pROC")

# Prepare data
df.lepto <- read.csv("data_use/prov_cases.csv")
pop <- read.csv("data_use/pop.csv")
prcp <- read.csv("data_use/prcp.csv")
riv <- read.csv("data_use/river_height.csv")
nino <- read.csv("data_use/nino.csv")

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

# Figure S1: times series of predictors ####
p1 <- data.lepto %>%
  dplyr::select(date, prov, par.0) %>% 
  mutate(prov=str_replace_all(prov, c("Entre Ríos"="Paraná river (ER)", "Santa Fe"="Paraná river (SF)"))) %>% 
  ggplot() +
  geom_line(aes(date, par.0, col=prov), size=1) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(breaks=c(1:6), labels=c(1:6)) +
  ggthemes::scale_colour_tableau("Color Blind") +
  labs(x="", y="River height (m/month)", col="") +
  theme_bw() +
  theme(legend.position=c(0.9,0.9),
        legend.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

p2 <- data.lepto %>%
  dplyr::select(date, prov, prcp.0) %>% 
  ggplot() +
  geom_line(aes(date, prcp.0, col=prov), size=1) +
  geom_hline(yintercept=0, col="grey", linetype="dashed") +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
  labs(x="", y="Precipitation (mm/month)", col="") +
  theme_bw() +
  theme(legend.position=c(0.9,0.9),
        legend.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

p3 <- nino %>%
  mutate(date=as.Date(lubridate::make_date(year, month)),
         nino34.0=scale(nino34.0)[,1],
         nino=ifelse(nino34.0>=0, nino34.0,0),
         nina=ifelse(nino34.0<0, nino34.0,0),
         event=ifelse(nino34.0>=0.5, "niño", "niña")) %>%
  select(date, nino34.0, nino, nina, event) %>%
  ggplot(aes(x=date, y=nino34.0)) +
  geom_hline(yintercept=c(0.5, -0.5), linetype="dashed", col="grey") +
  geom_ribbon(aes(ymax=nino, ymin=0), fill="red", alpha=0.7) +
  geom_ribbon(aes(ymax=nina, ymin=0), fill="blue", alpha=0.7) +
  geom_line(size=.3) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  labs(x="", y="Niño 3.4 Index", col="")+
  theme_bw() +
  theme(legend.position="none",
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

s1 <- cowplot::plot_grid(p1, p2, p3, nrow=3)
ggsave(plot=s1, filename=here("figures", "sup_figure1.pdf"), width=10, height=12)
ggsave(plot=s1, filename=here("figures", "sup_figure1.jpeg"), width=10, height=12)

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

# Variance Inflation Factor
corvif <- function(dt) {
  dt <- as.data.frame(dt)
  # Correlation part
  tmp_cor <- cor(dt,use="complete.obs")

  # VIF part
  form <- formula(paste("fooy ~ ", paste(strsplit(names(dt), " "),
                                         collapse="+")))
  dt <- data.frame(fooy=1,dt)
  lm_mod <- lm(form,dt)

  return(myvif(lm_mod))
}

vars <- expand.grid(c("prcp", "par.0", "nino34.0"),
                    c("prcp", "par.0", "nino34.0"))

cors.er <- c()
cors.sf <- c()
for(i in 1:9){
  cors.er[i] <- cor(df.er[,vars[i,1]], df.er[,vars[i,2]])
  cors.sf[i] <- cor(df.sf[,vars[i,1]], df.sf[,vars[i,2]])
}

cors <- vars %>%
  mutate(`Entre Ríos`=round(cors.er,2),
         `Santa Fe`=round(cors.sf,2))

vif <- data.frame(var=c("Precipitation", "Paraná River", "Niño 3.4 Index"),
                  er=corvif(df.er[,c("prcp", "par.0", "nino34.0")])[,1],
                  sf=corvif(df.sf[,c("prcp", "par.0", "nino34.0")])[,1])

# Create plots
p1 <- cors %>%
  mutate(across(c(Var1,Var2), ~str_replace_all(.x, 
                                               c("prcp"="Precipitation", 
                                                 "par.0"="Paraná River",
                                                 "nino34.0"="Niño 3.4 Index")))) %>%
  pivot_longer(cols=3:4, names_to="prov", values_to="cors") %>% 
  ggplot(aes(Var1, Var2)) +
  geom_tile(aes(fill=cors)) +
  geom_text(aes(label=cors), size=7) +
  scale_fill_gradient(low="white", high="#D55E00") +
  facet_wrap(~prov) +
  labs(x="", y="", fill="Correlation") +
  guides(fill=guide_colorbar(title.position="right")) +
  theme_bw() +
  theme(legend.title=element_text(angle=-270, hjust=0.5, face="bold"),
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.background=element_rect(fill="white", color="white"),
        strip.text=element_text(size=14, face="bold"))

p2 <- vif %>% 
  pivot_longer(cols=2:3, names_to="prov", values_to="vif") %>% 
  mutate(prov=str_replace_all(prov,
                              c("er"="Entre Ríos",
                                "sf"="Santa Fe"))) %>% 
  ggplot(aes(prov, var)) +
  geom_tile(aes(fill=vif)) +
  geom_text(aes(label=round(vif,2)), size=7) +
  scale_fill_gradient(low="white", high="#D55E00") +
  guides(fill=guide_colorbar(title.position="right")) +
  labs(x="", y="", fill="VIF") +
  theme_bw() +
  theme(legend.title=element_text(angle=-270, hjust=0.5, face="bold"),
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

s2 <- cowplot::plot_grid(p1, p2, nrow=1,
                         labels=c("a.", "b."), rel_widths=c(1,0.5))

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
          legend.text=element_text(size=14, face="bold"),
          axis.text=element_text(size=14, face="bold"),
          axis.text.x=element_text(angle=45, hjust=1),
          axis.title=element_text(size=16, face="bold"),
          panel.border=element_blank(),
          axis.line=element_line(color="black"),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
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
ggsave(plot=s3, filename=here("figures", "sup_figure3.pdf"), width=16, height=8)

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
  theme(legend.position="none",
        legend.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

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
  theme(legend.position="none",
        legend.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

s4 <- cowplot::plot_grid(er, sf, labels=c("a.", "b."))
ggsave(plot=s4, filename=here("figures", "sup_figure4.pdf"), width=12, height=5)

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

s5 <- cowplot::plot_grid(er, sf, nrow=2, labels=c("a.", "b."))
ggsave(plot=s5, filename=here("figures", "sup_figure5.pdf"), width=18, height=10)

# Table S1: incidence per 100,000 inhabitants ####
data.lepto %>%
  group_by(year, prov) %>% 
  summarise(pop=unique(pop),
            cases=sum(cases),
            .groups="drop") %>%
  mutate(rate=round((cases/pop)*100000, 2)) %>% 
  dplyr::select(year, prov, rate) %>% 
  pivot_wider(names_from=prov, values_from=rate)
  
# Tables S2 and S3: confusion matrix and measures of predictive ability ####
data.er <- list(res=er.preds$outb.prob[[1]] %>% 
                  mutate(out.pred=ifelse(prob.out>=er.preds$trigger[[1]]$threshold, 1, 0)),
                enso=er.preds$outb.prob[[3]] %>% 
                  mutate(out.pred=ifelse(prob.out>=er.preds$trigger[[3]]$threshold, 1, 0)),
                local=er.preds$outb.prob[[5]] %>% 
                  mutate(out.pred=ifelse(prob.out>=er.preds$trigger[[5]]$threshold, 1, 0)))

conf.er <- NULL
for(mod in c("res", "enso", "local")){
  conf.er[[mod]] <- caret::confusionMatrix(data=factor(data.er[[mod]]$out.pred),
                                           reference=factor(data.er[[mod]]$outbreak),
                                           positive="1")
}

data.sf <- list(res=sf.preds$outb.prob[[1]] %>% 
                  mutate(out.pred=ifelse(prob.out>=sf.preds$trigger[[1]]$threshold, 1, 0)),
                enso=sf.preds$outb.prob[[3]] %>% 
                  mutate(out.pred=ifelse(prob.out>=sf.preds$trigger[[3]]$threshold, 1, 0)),
                local=sf.preds$outb.prob[[4]] %>% 
                  mutate(out.pred=ifelse(prob.out>=sf.preds$trigger[[4]]$threshold, 1, 0)))

conf.sf <- NULL
for(mod in c("res", "enso", "local")){
  conf.sf[[mod]] <- caret::confusionMatrix(data=factor(data.sf[[mod]]$out.pred),
                                           reference=factor(data.sf[[mod]]$outbreak),
                                           positive="1")
}

# AUC
list(res=er.preds$auc[[1]], enso=er.preds$auc[[3]], local=er.preds$auc[[5]])
list(res=sf.preds$auc[[1]], enso=sf.preds$auc[[3]], local=sf.preds$auc[[4]])

# CRPS
unlist(er.preds$crps)[c(1,3,5)]
unlist(sf.preds$crps)[c(1,3,4)]

# CRPSS
(1-unlist(er.preds$crps)/er.preds$crps[[1]])[c(1,3,5)]
(1-unlist(sf.preds$crps)/sf.preds$crps[[1]])[c(1,3,4)]

## ############################################################################
## END
## ############################################################################


setwd("/Volumes/RESEARCH_HD/007/007A/data_in_use/RR_data")
# Event.CAR <- read.csv("Event.CAR.05.21.18.csv", stringsAsFactors = FALSE)
# Event.CAR <- read.csv("Event.CAR.05.20.18.csv", stringsAsFactors = FALSE)
# Event.CAR <- read.csv("Event.CAR.06.06.18.csv", stringsAsFactors = FALSE)
# Event.CAR <- read.csv("Event.CAR.06.11.18.csv", stringsAsFactors = FALSE)
# Event.CAR <- read.csv("Event.CAR.06.20.18.csv", stringsAsFactors = FALSE)
# Event.CAR <- read.csv("Event.CAR.07.05.18.csv", stringsAsFactors = FALSE)
Event.CAR <- read.csv("Event.CAR.07.18.18.csv", stringsAsFactors = FALSE)

suppressMessages(lapply(c("lmtest","multiwayvcov","dplyr"), require, character.only = TRUE))

# First Run #
# Event.CAR$stock <- ifelse(Event.CAR$cash.or.stock == "Yes", 1, 0)
Event.CAR$M.similarity <- -1 * Event.CAR$AT.Mkt_Distance_STATE.log
Event.CAR$IMR_fx <- Event.CAR$IMR_fs
run <- lm( I(window3*100) ~ IMR_fx + I(log(size + 1)) + age + C.or.T +
             dpst.hist.3y + market.grow_dpst.3y + # I(Target.market.grow_dpst.3y-market.grow_dpst.3y) + # + Target.market.grow_dpst.3y
             I(log(expense.ratio*100)+1) + perf.social_STATE + perf.hist.3y +
             I(100*finance.capa) + I(100*slack/size) + log(StD+1) +
             I(log(Mkt_Scope_Sum_Geo + 1)) +
             Exp05ago +  I(log(out_of_mkt_experience+1)) + I(market_HHI_depst/10000) +
             ATsame.SIC + log(AT.Geo_Distance+1) +
             post.StrFoc + M.similarity + Asset_increase +
             balancing_fx
           # + balancing_fx * post.StrFoc
           # + balancing_fx * AT.Mkt_Distance_STATE.log
           # + balancing_fx * Asset_increase
          , data=Event.CAR)
(reg <- coeftest(run, cluster.vcov(run, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig)
compile.mgmt::reg.Vif(run$model[,-c(1,2)])

# ROA model #
run.roa.0 <- lm(I((eff.performance - performance)*100) ~ IMR_fx + I(log(size + 1)) + age + C.or.T +
                  dpst.hist.3y + market.grow_dpst.3y + # I(Target.market.grow_dpst.3y-market.grow_dpst.3y) + # + Target.market.grow_dpst.3y
                  I(log(expense.ratio*100)+1) + perf.social_STATE + perf.hist.3y +
                  I(100*finance.capa) + I(100*slack/size) + log(StD+1) +
                  I(log(Mkt_Scope_Sum_Geo + 1)) +
                  Exp05ago +  I(log(out_of_mkt_experience+1)) + I(market_HHI_depst/10000) +
                  ATsame.SIC + log(AT.Geo_Distance+1) +
                  post.StrFoc + M.similarity + Asset_increase, data=Event.CAR)
run.roa <- update(run, I((eff.performance - performance)*100) ~.)
reg.roa.0 <- coeftest(run.roa.0, cluster.vcov(run.roa, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
reg.roa   <- coeftest(run.roa,   cluster.vcov(run.roa, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
compile.mgmt::reg.Vif(run.roa$model)
var.names <- c("Intercept", "Inverse Mill's ratio", "Size", "Age", "Bank type (1=commercial)", "Deposit growth", "Market growth", "Expense ratio", 
               "Performance to social aspiration", "Performance to historical aspiration", "Financing capability", "Corporate slack", "Stock market volatility",
"Market scope", "Acquisition experience", "Out-of-market experience",  "Market concentration", 
"A-T Same Type", "A-T Geo-distance", 
"Strategic focus", "Market similarity", "Target size", "Positional balancing")
# roa.result <- reg.roa[,-c(1,5)]
# row.names(roa.result) <- var.names
# roa.result <- data.frame(round(roa.result[,1:4], 5), sig = roa.result[,5])
# knitr::kable(roa.result)
compare.roa <- compare.models(model1 = run.roa.0, model2 = run.roa, n=2)
names(compare.roa) <- c("Variables",paste0("Model ",0:1))
combine.roa  <- Combine.Result(tbl_1=reg.roa.0,tbl_2=reg.roa,n.tbl=2)
names(combine.roa) <- c("Variables",paste0("Model ", 0:1))
final.roa <- suppressWarnings(rbind(combine.roa, compare.roa))
final.roa[seq(1,nrow(combine.roa),2),1] <- var.names
knitr::kable(final.roa[1:(nrow(final.roa)-2),])

# Endog. Test # try multiple combinations # 
ivreg <- AER::ivreg(I(window3*100) ~ 
                      balancing_fx + IMR_fx + I(log(size + 1)) + age + C.or.T +
                      dpst.hist.3y + market.grow_dpst.3y + # I(Target.market.grow_dpst.3y-market.grow_dpst.3y) + # + Target.market.grow_dpst.3y
                      I(log(expense.ratio*100)+1) + perf.social_STATE + perf.hist.3y +
                      I(100*finance.capa) + I(100*slack/size) + log(StD+1) +
                      I(log(Mkt_Scope_Sum_Geo + 1)) +
                      Exp05ago +  I(log(out_of_mkt_experience+1)) +
                      ATsame.SIC + log(AT.Geo_Distance+1) +
                      post.StrFoc + AT.Mkt_Distance_STATE.log + Asset_increase,
                    ~ year + # [also worked: fed + market_HHI_asset; fed + stalp; fed + regagnt + market_HHI_asset]
                      IMR_fx + I(log(size + 1)) + age + C.or.T +
                      dpst.hist.3y + market.grow_dpst.3y + # I(Target.market.grow_dpst.3y-market.grow_dpst.3y) + # + Target.market.grow_dpst.3y
                      I(log(expense.ratio*100)+1) + perf.social_STATE + perf.hist.3y +
                      I(100*finance.capa) + I(100*slack/size) + log(StD+1) +
                      I(log(Mkt_Scope_Sum_Geo + 1)) +
                      Exp05ago +  I(log(out_of_mkt_experience+1)) + I(market_HHI_depst/10000) +
                      ATsame.SIC + log(AT.Geo_Distance+1) +
                      post.StrFoc + AT.Mkt_Distance_STATE.log + Asset_increase
                    , x=TRUE, data=Event.CAR)
# ivpack::robust.se(ivreg)
summary(ivreg, diagnostics = TRUE)
# # which one predicts balancing_fx?
# summary(lm(balancing_fx ~ I(year^3) + I(market_HHI_depst/10000) + # [also worked: fed + market_HHI_asset; fed + stalp; fed + regagnt + market_HHI_asset]
#              IMR_fx + I(log(size + 1)) + age + C.or.T + # I(window3*100) I(post.eff.performance - performance)
#              dpst.hist.3y + I(log(expense.ratio*100)+1) + perf.social_STATE + perf.hist.3y +
#              I(100*finance.capa) + I(100*slack/size) + 
#              I(log(Mkt_Scope_Sum_Geo + 1)) +
#              Exp05ago + I(log(out_of_mkt_experience+1)) + 
#              log(StD+1) + market.grow_dpst.3y +
#              ATsame.SIC + log(AT.Geo_Distance+1) +
#              I(loan_HHI_StrFoc/10000/Home_Focus_ofc) + I(AT.Mkt_Distance.log) + I((eff.size - size)/size)
#            , x=TRUE, data=Event.CAR))

# ITCV #########
ITCV <- function(t, df, x_name, y_name){
 df <- run$model
 numeric_col <- as.numeric(which(sapply(df, class) %in% c("numeric","integer","AsIs")))
 df_num <- df[,numeric_col]
 x_name <- "balancing_fx"
 y_name <- "I(window3 * 100)"
 t <- reg[26,4]
  n <- nrow(df_num)
  q <- ncol(df_num) - 2
  x <- df_num[,x_name]
  y <- df_num[,y_name]
  z <- df_num[,-c(which(names(df_num) %in% c(x_name, y_name)))]
  df_num1 <- df_num[,-(which(names(df_num) == y_name))]
  r1 <- summary(lm(df_num1[,c(which(names(df_num1) == x_name), which(!names(df_num1) %in% x_name))]))$r.squared
  r2 <- summary(lm(df_num[,-(which(names(df_num) == x_name))]))$r.squared
  r3 <- ggm::pcor(c(y_name, x_name, names(df_num)[!names(df_num) %in% c(x_name, y_name)]), var(df_num)) 
    d <- t^2 + (n-q-1)
  term1 <- sqrt((1-r1)*(1-r2))
  term2 <- (t^2+t*sqrt(d))/(-(n-q-1)) 
  term3 <- ( -d-t*sqrt(d))/(-(n-q-1))
  k <- term1 * (term2 + term3*r3)
  k*sqrt((1-r1)/(1-r2))
  k*sqrt((1-r2)/(1-r1))
}

# test Heckman exclusion restriction #####
setwd("/Volumes/RESEARCH_HD/007/007A/data_in_use/RR_data")
# event.all <- read.csv("IMR.csv", stringsAsFactors = FALSE)
event.all <- read.csv("IMR.07.05.csv", stringsAsFactors = FALSE)
s_fx <- glm( zero.one.event ~ n.event + market.share_STATE + market.grow_dpst.3y + expense.ratio + size + age + I(finance.capa*100) + perf.social_STATE + perf.hist.3y + dpst.hist.3y + I(log(loan_HHI_StrFoc/10000/Home_Focus_ofc)+1) + I(log(Mkt_Scope_Sum_Geo+1))
             + balancing_fs, family = binomial( link = "probit" ), data=event.all) # control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e8))
(s_fx.tbl <- suppressWarnings(coef(summary(s_fx))) %>% as.data.frame %>% add.p.z %>% `[`(1:5) %>% add.sig(Pr.col=5)) # %>% format.reg.table)
1 - (s_fx$deviance/s_fx$null.deviance)
cor.test(event.all$IMR_fx, event.all$balancing_fx) # should be low ...

cor.test(Event.CAR$IMR_fx, Event.CAR$balancing_fx) # should be low ...

cor.test(event.all$IMR_fs, event.all$balancing_fs) # should be low ...

# how many passed acorss Strategic Balance | % home focus ##########
length(unique(Event.CAR$Acquiror.CUSIP))
# Event.CAR$stay.same <- with(Event.CAR, ifelse((pre.eff.SDV_deephouse_STATE.log < fx_vertex & eff.SDV_deephouse_STATE.log < fx_vertex) | 
#                                                pre.eff.SDV_deephouse_STATE.log > fx_vertex & eff.SDV_deephouse_STATE.log > fx_vertex, 1, 0))
Event.CAR$stay.same <- with(Event.CAR, ifelse((pre.eff.SDV_dh_fs_STATE.log < fs_vertex & eff.SDV_dh_fs_STATE.log < fs_vertex) | 
                                               pre.eff.SDV_dh_fs_STATE.log > fs_vertex & eff.SDV_dh_fs_STATE.log > fs_vertex, 1, 0))
paste0(scales::percent(mean(Event.CAR$stay.same)), " of the banks do NOT move across Strategic Balance via an accquision.")
# Event.CAR$pre.acq.imbalance <- abs(Event.CAR$SDV_deephouse_STATE.log - Event.CAR$fx_vertex)
Event.CAR$pre.acq.imbalance <- abs(Event.CAR$SDV_dh_fs_STATE.log - Event.CAR$fs_vertex)
# cross.start <- ecdf(Event.CAR[which(Event.CAR$stay.same==0),]$pre.acq.imbalance)(1*sd(Event.CAR$SDV_deephouse_STATE.log))
cross.start <- ecdf(Event.CAR[which(Event.CAR$stay.same==0),]$pre.acq.imbalance)(1*sd(Event.CAR$SDV_dh_fs_STATE.log))
paste0("Among those who passed across strategic balance, only ", scales::percent(1-cross.start), " started from an over-conforming or over-deviating position (more than 1*S.D. of strategic deviation).")
paste0(scales::percent(1-ecdf(Event.CAR$Home_Focus_dep)(0.9)), " banks keep more than 90% deposit in their home states")
paste0("There are ", scales::percent(1-mean(Event.CAR$ATsame.State)) ," out-of-state acquisitons.")

# all_results_by_state_fix.firm <- read.csv("all_results_by_state_fix.firm.csv", stringsAsFactors = FALSE)
all_results_by_state_fs.firm <- read.csv("all_results_by_state_fs.firm.csv",  stringsAsFactors = FALSE)
# paste0("In the full sample of bank financials, comprised of a total of 906,205 bank-periods, there are ", scales::percent(mean(all_results_by_state_fix.firm$fx_U_exist)) ," states in which SDV has an inverted-U impact on ROA.")
paste0("In the full sample of bank financials, comprised of a total of 906,205 bank-periods, there are ", scales::percent(mean(all_results_by_state_fs.firm$fs_U_exist)) ," states in which SDV has an inverted-U impact on ROA.")
# paste0("In the final sample matched with acquisisition events, this proportion is ", scales::percent(mean(Event.CAR$fx_U_exist)) ,".")
paste0("In the final sample matched with acquisisition events, this proportion is ", scales::percent(mean(Event.CAR$fs_U_exist)) ,".")
Event.CAR$Mkt_Scope_N_State %>% table %>% prop.table()
Event.CAR$fx_U_exist %>% table %>% prop.table()
Event.CAR %>% dplyr::filter(fx_U_exist == 0) %>% summarize(n_distinct(stalp))
# see how ?% operate only in one states #
setwd("/Volumes/RESEARCH_HD/007/007A/data_in_use/RR_data")
bank_market_scope <- read.csv("bank_market_scope.csv", stringsAsFactors = FALSE)
bank_market_scope <- bank_market_scope %>% dplyr::select(-year)

bank_market_scope %>% group_by(year.lead) %>% summarise(sum(Mkt_Scope_N_State==1)/n()) %>% as.data.frame()
table(bank_market_scope$Mkt_Scope_N_State) %>% prop.table() %>% round(3)

# altnernative car windows ###########
car.01 <- lm( I(window1*100) ~ IMR_fx + I(log(size + 1)) + age + C.or.T +
                dpst.hist.3y + market.grow_dpst.3y + # I(Target.market.grow_dpst.3y-market.grow_dpst.3y) + # + Target.market.grow_dpst.3y
                I(log(expense.ratio*100)+1) + perf.social_STATE + perf.hist.3y +
                I(100*finance.capa) + I(100*slack/size) + log(StD+1) +
                I(log(Mkt_Scope_Sum_Geo + 1)) +
                Exp05ago +  I(log(out_of_mkt_experience+1)) + I(market_HHI_depst/10000) +
                ATsame.SIC + log(AT.Geo_Distance+1) +
                post.StrFoc + M.similarity + Asset_increase +
                balancing_fx
              # + balancing_fx * StrFoc
              # + balancing_fx * AT.Mkt_Distance_STATE.log
              # + balancing_fx * Asset_increase
           , data=Event.CAR)
car.03 <- update(car.01, I(window3*100)~.)
car.05 <- update(car.01, I(window5*100)~.)
car.07 <- update(car.01, I(window7*100)~.)
car.09 <- update(car.01, I(window9*100)~.)
car.11 <- update(car.01, I(window11*100)~.)
 c1 <- coeftest(car.01, cluster.vcov(car.01, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
 c3 <- coeftest(car.03, cluster.vcov(car.03, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
 c5 <- coeftest(car.05, cluster.vcov(car.05, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
 c7 <- coeftest(car.07, cluster.vcov(car.07, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
 c9 <- coeftest(car.09, cluster.vcov(car.09, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
 c11 <- coeftest(car.11, cluster.vcov(car.11, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
car.window <- Combine.Result(c1, c3, c5, c7, c9, c11, n.tbl = 6)
car.window[seq(1,nrow(car.window),2),1] <- var.names
names(car.window) <- c("Var.", "CAR [0, 0]", "CAR [-1, +1]", "CAR [-2, +2]", "CAR [-3, +3]", "CAR [-4, +4]", "CAR [-5, +5]")
fit.stats <- data.frame("R^2", round(do.call(cbind, map(list(car.01, car.03, car.05, car.07, car.09, car.11), function(x) {summary(x)$r.squared})),3))
names(fit.stats) <- names(car.window)
car.window <- rbind(car.window, fit.stats)
knitr::kable(car.window)

# lm results #
H.C <- lm( I(window3*100) ~ IMR_fx + I(log(size + 1)) + age + C.or.T +
             dpst.hist.3y + market.grow_dpst.3y + # I(Target.market.grow_dpst.3y-market.grow_dpst.3y) + # + Target.market.grow_dpst.3y
             I(log(expense.ratio*100)+1) + perf.social_STATE + perf.hist.3y +
             I(100*finance.capa) + I(100*slack/size) + log(StD+1) +
             I(log(Mkt_Scope_Sum_Geo + 1)) +
             Exp05ago +  I(log(out_of_mkt_experience+1)) + I(market_HHI_depst/10000) +
             ATsame.SIC + log(AT.Geo_Distance+1) +
             post.StrFoc + M.similarity + Asset_increase
           , data=Event.CAR)
reg.C  <- coeftest(H.C, cluster.vcov(H.C, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
H.00       <- update(H.C, .~. + balancing_fx)
reg.00  <- coeftest(H.00, cluster.vcov(H.00, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
H.01       <- update(H.00, .~. + balancing_fx * post.StrFoc)
reg.01  <- coeftest(H.01, cluster.vcov(H.01, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
H.02       <- update(H.00, .~. + balancing_fx * M.similarity)
reg.02  <- coeftest(H.02, cluster.vcov(H.02, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
H.03       <- update(H.00, .~. + balancing_fx * Asset_increase)
reg.03  <- coeftest(H.03, cluster.vcov(H.03, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
H.Full     <- update(H.00, .~. + balancing_fx * post.StrFoc + balancing_fx * M.similarity + balancing_fx * Asset_increase )
reg.Fl  <- coeftest(H.Full, cluster.vcov(H.Full, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
compare <- compare.models(model1 = H.C, model2 = H.00, model3 = H.01, model4 = H.02, model5 = H.03, model6 = H.Full, n=6, intn.effect.only=c(3,4,5,6))
names(compare) <- c("Variables",paste0("Model ",0:5))
combine <- Combine.Result(tbl_1=reg.C,tbl_2=reg.00,tbl_3=reg.01,tbl_4=reg.02,tbl_5=reg.03, tbl_6 =reg.Fl ,n.tbl=6)
names(combine) <- c("Variables",paste0("Model ", 0:5))
final <- suppressWarnings(rbind(combine, compare))
final[seq(1,nrow(combine),2),1] <- c(var.names, "PB x Strategic focus", "PB x T. mkt dissimilarity", "PB x Size increase")
final <- final %>% `[`(-(nrow(final)-1), 1:7)
knitr::kable(final)

# significance of slope (Aug 3rd)
m2 <- coeftest(H.01, cluster.vcov(H.01, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced)))
slope.sig_after.mod <- function(m, mod_name){
  # m <- m2
  # mod_name <- "post.StrFoc"
vcv <- cluster.vcov(H.01, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))[23,][mod_name]
mod_vec <- unlist(Event.CAR[mod_name])
mod_high <- mean(mod_vec) + sd(mod_vec)
 mod_low <- mean(mod_vec) - sd(mod_vec)
beta_high <- m[23,1] + m[24,1]*mod_high
 beta_low <- m[23,1] + m[24,1]*mod_low
se_high <- sqrt(m[23,2]^2 + (mod_high^2)*m[24,2]^2 + 2*mod_high*vcv)
 se_low <- sqrt(m[23,2]^2  + (mod_low^2)*m[24,2]^2  + 2*mod_low*vcv)
sig <- function(beta, se){pt(beta/se, df = 965-2, lower.tail = FALSE)*2}
 result <- purrr::pmap(list(beta=list(beta_high, beta_low), se=list(se_high, se_low)), sig)
 return(result)}

slope.sig_after.mod(m2, "post.StrFoc")

m3 <- coeftest(H.02, cluster.vcov(H.02, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced)))
slope.sig_after.mod(m3, "M.similarity")

m4 <- coeftest(H.03, cluster.vcov(H.03, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced)))
slope.sig_after.mod(m4, "Asset_increase")

#
waldtest(H.C,    vcov = cluster.vcov(H.C,    cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced)))
waldtest(H.00,   vcov = cluster.vcov(H.00,   cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced)))
waldtest(H.01,   vcov = cluster.vcov(H.01,   cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced)))
waldtest(H.02,   vcov = cluster.vcov(H.02,   cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced)))
waldtest(H.03,   vcov = cluster.vcov(H.03,   cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced)))
waldtest(H.Full, vcov = cluster.vcov(H.Full, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced)))

# original measure #
Event.CAR$reposition <- with(Event.CAR, eff.SDV_dh_fs_STATE.log - SDV_dh_fs_STATE.log)
H.o0 <- lm(I(window3*100) ~ IMR_ori + I(log(size + 1)) + age + C.or.T +
                   dpst.hist.3y + market.grow_dpst.3y + # I(Target.market.grow_dpst.3y-market.grow_dpst.3y) + # + Target.market.grow_dpst.3y
                   I(log(expense.ratio*100)+1) + perf.social_STATE + perf.hist.3y +
                   I(100*finance.capa) + I(100*slack/size) + log(StD+1) +
                   I(log(Mkt_Scope_Sum_Geo + 1)) +
                   Exp05ago +  I(log(out_of_mkt_experience+1)) + I(market_HHI_depst/10000) +
                   ATsame.SIC + log(AT.Geo_Distance+1) +
                   post.StrFoc + M.similarity + Asset_increase + SDV_dh_fs_STATE.log, data = Event.CAR) # * age
reg.o0  <- coeftest(H.o0, cluster.vcov(H.o0, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
H.o1       <- update(H.o0, I(window3*100) ~. + SDV_dh_fs_STATE.log * reposition) # * age # (reposition + I(reposition^2)) # (pre.eff.SDV_deephouse_STATE.log + I(pre.eff.SDV_deephouse_STATE.log^2))
reg.o1  <- coeftest(H.o1, cluster.vcov(H.o1, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
H.o2       <- update(H.o0, I(window3*100) ~. + SDV_dh_fs_STATE.log * reposition + SDV_dh_fs_STATE.log * I(reposition^2)) # * age # (reposition + I(reposition^2)) # (pre.eff.SDV_deephouse_STATE.log + I(pre.eff.SDV_deephouse_STATE.log^2))
reg.o2  <- coeftest(H.o2, cluster.vcov(H.o2, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
# H.o1       <- update(H.C, .~. + pre.eff.SDV_deephouse_STATE.log * I(post.eff.SDV_deephouse_STATE.log - pre.eff.SDV_deephouse_STATE.log) * I(loan_HHI_StrFoc/10000/Home_Focus_ofc))
# reg.o1  <- coeftest(H.o1, cluster.vcov(H.o1, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
# H.o1       <- update(H.C, .~. + pre.eff.SDV_deephouse_STATE.log * I(post.eff.SDV_deephouse_STATE.log - pre.eff.SDV_deephouse_STATE.log) * I(AT.Mkt_Distance.log))
# reg.o1  <- coeftest(H.o1, cluster.vcov(H.o1, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
# H.o1       <- update(H.C, .~. + pre.eff.SDV_deephouse_STATE.log * I(post.eff.SDV_deephouse_STATE.log - pre.eff.SDV_deephouse_STATE.log) * I((eff.size - size)/size))
# reg.o1  <- coeftest(H.o1, cluster.vcov(H.o1, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig %>% format.reg.table
compare.ori <- compare.models(model1 = H.o0, model2 = H.o1, n=2)
names(compare.ori) <- c("Variables",paste0("Model ",0:1))
combine.ori  <- Combine.Result(tbl_1=reg.o0,tbl_2=reg.o1,n.tbl=2)
names(combine.ori) <- c("Variables",paste0("Model ", 0:1))
final.ori <- suppressWarnings(rbind(combine.ori, compare.ori))
final.ori[seq(1,nrow(combine.ori),2),1] <- c(var.names[-length(var.names)], "Pre-acq. strategic deviation", "Reposition", "Pre-acq. deviation x Reposition")
knitr::kable(final.ori)

compare.ori <- compare.models(model1 = H.o0, model2 = H.o1, model3 = H.o2, n=3)
names(compare.ori) <- c("Variables",paste0("Model ", 0:2))
combine.ori  <- Combine.Result(tbl_1 = reg.o0, tbl_2 = reg.o1, tbl_3 = reg.o2, n.tbl=3)
names(combine.ori) <- c("Variables",paste0("Model ", 0:2))
final.ori <- suppressWarnings(rbind(combine.ori, compare.ori))
final.ori[seq(1,nrow(combine.ori),2),1] <- c(var.names[-length(var.names)], "Pre-acq. SDV", "Reposition", "Pre-acq. SDV x Reposition", "Reposition^2", "Post-acq. SDV x Reposition^2")
knitr::kable(final.ori)

# plot #
# p.o1 <- coeftest(H.o1, cluster.vcov(H.o1, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig
# 
# po1 <- plot.gg(df=p.o1, nameofdata=H.o1$model, by_color=FALSE, x_name = "reposition", y_name = "`I(window3 * 100)`",
#               main1.r=25, main2.r=NULL, mdrt.r=24, mdrt.square.r=FALSE, int1.r=26, int2.r=NULL,
#               find.median=FALSE, min_x=0.01, max_x=0.985, max.y=1,
#               mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95, n.sd=1,
#               main=NULL, xlab="Repositioning to Conformity (-) vs. Deviation (+)", ylab="CAR [-1, +1]", moderator.name="Pre-acq. SDV",
#               mdrt.05.name="Mean - 1*SD", mdrt.50.name=NULL, mdrt.95.name="Mean + 1*SD",
#               flip.low.high=FALSE,
#               y.hi.lim=2, y.low.lim=-2)
# 
# po1_ <- plot.gg(df=p.o1, nameofdata=H.o1$model, by_color=FALSE, x_name = "reposition", y_name = "`I(window3 * 100)`",
#                main1.r=25, main2.r=NULL, mdrt.r=24, mdrt.square.r=FALSE, int1.r=26, int2.r=NULL,
#                find.median=FALSE, min_x=0.01, max_x=0.985, max.y=1,
#                mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95, n.sd=2,
#                main=NULL, xlab="Repositioning to Conformity (-) vs. Deviation (+)", ylab=NULL, moderator.name="Pre-acq. SDV",
#                mdrt.05.name="Mean - 2*SD", mdrt.50.name=NULL, mdrt.95.name="Mean + 2*SD",
#                flip.low.high=FALSE,
#                y.hi.lim=2, y.low.lim=-2)

p.o2 <- coeftest(H.o2, cluster.vcov(H.o2, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig

po2.1 <- plot.gg(df=p.o2, nameofdata=H.o2$model, by_color=FALSE, x_name = "reposition", y_name = "`I(window3 * 100)`",
              main1.r=24, main2.r=25, mdrt.r=23, mdrt.square.r=FALSE, int1.r=26, int2.r=27,
              find.median=FALSE, min_x=0.0028, max_x=0.99, max.y=1,
              mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95, n.sd=1,
              main=NULL, xlab="Repositioning to Conformity (-) vs. Deviation (+)", ylab="CAR% [-1, +1]", moderator.name="Pre-acq. SDV",
              mdrt.05.name="Mean - 1*SD", mdrt.50.name=NULL, mdrt.95.name="Mean + 1*SD",
              flip.low.high=FALSE,
              y.hi.lim=3, y.low.lim=-3)

po2.2 <- plot.gg(df=p.o2, nameofdata=H.o2$model, by_color=FALSE, x_name = "reposition", y_name = "`I(window3 * 100)`",
               main1.r=24, main2.r=25, mdrt.r=23, mdrt.square.r=FALSE, int1.r=26, int2.r=27,
               find.median=FALSE, min_x=0.002, max_x=.986, max.y=1,
               mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95, n.sd=2,
               main=NULL, xlab="Repositioning to Conformity (-) vs. Deviation (+)", ylab=NULL, moderator.name="Pre-acq. SDV",
               mdrt.05.name="Mean - 2*SD", mdrt.50.name=NULL, mdrt.95.name="Mean + 2*SD",
               flip.low.high=FALSE,
               y.hi.lim=3, y.low.lim=-3)

gridExtra::grid.arrange(po2.1, po2.2, ncol = 2, grobs = lapply(list(po2.1, po2.2), "+", theme(plot.margin = unit(c(1,1,1,1), "cm"))))


hist1 <- ggplot(H.o1$model, aes(x = reposition)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(-4, 4, by = 0.25), 
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(H.o1$model$reposition), sd = sd(H.o1$model$reposition))) +
  labs(x = "Repositioning to Conformity(-) vs. Deviation(+)", y = "Density") +
  theme(text=element_text(family="Times New Roman", size=16)) 

hist2 <- ggplot(H.o1$model, aes(x = pre.eff.SDV_deephouse_STATE.log)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(-0.5, 7.5, by = 0.25), 
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(H.o1$model$pre.eff.SDV_deephouse_STATE.log), sd = sd(H.o1$model$pre.eff.SDV_deephouse_STATE.log))) +
  labs(x = "Pre-acq. Strategic Deviation", y = "Density") +
  theme(text=element_text(family="Times New Roman", size=16)) 

gridExtra::grid.arrange(po1, hist1, hist2, ncol = 3)

gridExtra::grid.arrange(po1, po1_, ncol = 2, grobs = lapply(list(po1, po1_), "+", theme(plot.margin = unit(c(1,1,1,1), "cm"))))

# Correlation Table #
x.names.in.reg <- c(   "I(log(size + 1))", "age", "C.or.T",
                       "dpst.hist.3y", "I(log(expense.ratio * 100) + 1)", "perf.social_STATE", "perf.hist.3y",
                       "I(100 * finance.capa)", "I(100 * slack/size)", 
                       "I(log(Mkt_Scope_Sum_Geo + 1))",
                       "Exp05ago", "I(log(out_of_mkt_experience + 1))", 
                       "log(StD + 1)", "I(market_HHI_depst/10000)", "market.grow_dpst.3y",
                       "ATsame.SIC", "log(AT.Geo_Distance + 1)", "post.StrFoc", "M.similarity", 'Asset_increase', "balancing_fx")
cor_matr <- cor.matrix(result.w.full.var = H.00, number.of.IVs = 21, y.name.in.doc = "CAR% [-1, +1]",   y.name.in.reg = "I(window3 * 100)", 
                       x.names = var.names[-(1:2)], x.names.in.reg = x.names.in.reg, digits=2)

knitr::kable(cor_matr)

# GEE #
library(geepack) 
library(compile.mgmt)
gee.C  <- geeglm(I(window3*100) ~ IMR_fx + I(log(size + 1)) + age + C.or.T +
                   dpst.hist.3y + market.grow_dpst.3y + # I(Target.market.grow_dpst.3y-market.grow_dpst.3y) + # + Target.market.grow_dpst.3y
                   I(log(expense.ratio*100)+1) + perf.social_STATE + perf.hist.3y +
                   I(100*finance.capa) + I(100*slack/size) + log(StD+1) +
                   I(log(Mkt_Scope_Sum_Geo + 1)) +
                   Exp05ago +  I(log(out_of_mkt_experience+1)) + I(market_HHI_depst/10000) +
                   ATsame.SIC + log(AT.Geo_Distance+1) +
                   post.StrFoc + M.similarity + Asset_increase
                  , data=Event.CAR, id=Acquiror.fed_rssd, waves = Quarter.Announced, corstr = "independence")
gee.00  <- update(gee.C, .~. + balancing_fx)
gee.01    <- update(gee.00, .~. + balancing_fx * post.StrFoc)
gee.02    <- update(gee.00, .~. + balancing_fx * M.similarity)
gee.03    <- update(gee.00, .~. + balancing_fx * Asset_increase)
gee.Full  <- update(gee.00, .~. + balancing_fx * post.StrFoc +  balancing_fx * M.similarity + balancing_fx * Asset_increase )
reg.gee.C  <- summary(gee.C) %>% coef %>% as.data.frame %>% add.sig(Pr.col=4) %>% add.n.r %>% format.reg.table(d=3)
reg.gee.00 <- summary(gee.00) %>% coef %>% as.data.frame %>% add.sig(Pr.col=4) %>% add.n.r %>% format.reg.table(d=3)
reg.gee.01 <- summary(gee.01) %>% coef %>% as.data.frame %>% add.sig(Pr.col=4) %>% add.n.r %>% format.reg.table(d=3)
reg.gee.02 <- summary(gee.02) %>% coef %>% as.data.frame %>% add.sig(Pr.col=4) %>% add.n.r %>% format.reg.table(d=3)
reg.gee.03 <- summary(gee.03) %>% coef %>% as.data.frame %>% add.sig(Pr.col=4) %>% add.n.r %>% format.reg.table(d=3)
reg.gee.Full <- summary(gee.Full) %>% coef %>% as.data.frame %>% add.sig(Pr.col=4) %>% add.n.r %>% format.reg.table(d=3)
combine.GEE <- Combine.Result(tbl_1=reg.gee.C,tbl_2=reg.gee.00,tbl_3=reg.gee.01,tbl_4=reg.gee.02,tbl_5=reg.gee.03,tbl_6=reg.gee.Full,n.tbl=6)
names(combine.GEE) <- c("Variables",paste0("Model ", 0:5))
compare.GEE <- c("QIC", formatC(unlist(map(list(gee.C,gee.00,gee.01,gee.02,gee.03,gee.Full),MuMIn::QIC)),digits=3,format="f"))
names(compare.GEE) <- c("Variables",paste0("Model ", 0:5))
final.GEE <- suppressWarnings(bind_rows(combine.GEE, compare.GEE))
final.GEE[seq(1,nrow(combine.GEE),2),1] <- c(var.names, "PB x Strategic focus", "PB x Mkt similarity", "PB x Target size")
knitr::kable(final.GEE)

# Practical Significance #

# plotting interactions #
p.01  <- coeftest(H.01, cluster.vcov(H.01, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig 
p.02  <- coeftest(H.02, cluster.vcov(H.02, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig 
p.03  <- coeftest(H.03, cluster.vcov(H.03, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))) %>% `[`() %>% as.data.frame() %>% `[`(1:3) %>% add.p.z %>% add.sig 

          p1 <- plot.gg(df=p.01, nameofdata=H.01$model, by_color=FALSE, x_name = "balancing_fx", y_name = "`I(window3 * 100)`",
                    main1.r=23, main2.r=NULL, mdrt.r=20, mdrt.square.r=FALSE, int1.r=24, int2.r=NULL,
                    find.median=FALSE, min_x=0.02, max_x=0.991, max.y=2,
                    mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95, n.sd=1,
                    main=NULL, xlab="Positional Balancing", ylab="CAR% [-1, +1]", moderator.name="Strategic Focus",
                    mdrt.05.name="Low", mdrt.50.name=NULL, mdrt.95.name="High",
                    flip.low.high=FALSE,
                    y.hi.lim=2.6, y.low.lim=-2.6)
            
          p2 <- plot.gg(df=p.02, nameofdata=H.02$model, by_color=FALSE, x_name = "balancing_fx", y_name = "`I(window3 * 100)`",
                    main1.r=23, main2.r=NULL, mdrt.r=21, mdrt.square.r=FALSE, int1.r=24, int2.r=NULL,
                    find.median=FALSE, min_x=0.02, max_x=0.991, max.y=1,
                    mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95, n.sd=1,
                    main=NULL, xlab="Positional Balancing", ylab=NULL, moderator.name="Market Similarity",
                    mdrt.05.name="Low", mdrt.50.name=NULL, mdrt.95.name="High",
                    flip.low.high=FALSE,
                    y.hi.lim=2.6, y.low.lim=-2.6)
            
          p3 <- plot.gg(df=p.03, nameofdata=H.03$model, by_color=FALSE, x_name = "balancing_fx", y_name = "`I(window3 * 100)`",
                    main1.r=23, main2.r=NULL, mdrt.r=22, mdrt.square.r=FALSE, int1.r=24, int2.r=NULL,
                    find.median=FALSE, min_x=0.02, max_x=0.991, max.y=1,
                    mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95, n.sd=1,
                    main=NULL, xlab="Positional Balancing", ylab=NULL, moderator.name="Target Size",
                    mdrt.05.name="Low", mdrt.50.name=NULL, mdrt.95.name="High",
                    flip.low.high=FALSE,
                    y.hi.lim=2.6, y.low.lim=-2.6)
          
          margin = theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 
          gridExtra::grid.arrange(p1, p2, p3, ncol = 3, grobs = lapply(list(p1, p2, p3), "+", margin))
          
# geographical distribuiton of inverse-U #####
all_results_by_state_fix.firm <- read.csv("all_results_by_state_fix.firm.csv", stringsAsFactors = FALSE)
 all_results_by_state_fs.firm <- read.csv("all_results_by_state_fs.firm.csv",  stringsAsFactors = FALSE)
 names(all_results_by_state_fs.firm) <- names(all_results_by_state_fix.firm)
all_results_by_state_fix.firm <- all_results_by_state_fs.firm
# all_results_by_state_fix.firm <- all_results_by_state_fix.firm[-8,]
 states.br <- readxl::read_excel(path = "/Volumes/RESEARCH_HD/007/007A/RR_FDIC_SDC/States and Regions.xlsx", sheet=2) %>% as.data.frame()
 states.br$state_whole <- tolower(states.br$state_whole)
 states.br$state_whole[9] <- "district of columbia"
all_results_by_state_fix.firm <- merge(all_results_by_state_fix.firm, states.br, by.x = "stalp", by.y = "state_abbr")
 state_centers <- read.csv("/Volumes/RESEARCH_HD/007/007A/RR_FDIC_SDC/State_Centers.csv", stringsAsFactors = FALSE)
 state_centers$State <- tolower(state_centers$State)
 state_centers <- state_centers %>% dplyr::filter(!State %in% c("alaska","hawaii"))
 state_centers <- state_centers %>% dplyr::filter(!State %in% all_results_by_state_fix.firm[all_results_by_state_fix.firm$fx_U_exist == 0,]$state_whole)
 state_centers <- merge(state_centers, all_results_by_state_fix.firm[,c("state_whole", "fx_vertex")], by.x = "State", by.y = "state_whole") %>% dplyr::select(-1) %>% round(3)

  paste0("there are ", sum(all_results_by_state_fix.firm$fx_U_exist), " (", scales::percent(sum(all_results_by_state_fix.firm$fx_U_exist)/nrow(all_results_by_state_fix.firm)),")", " states that has an inverse-U curve")
  
  suppressMessages(lapply(c("ggplot2","ggmap","ggthemes"), require, character.only = TRUE))
  
  state <- map_data("state")
  state <- merge(state, all_results_by_state_fix.firm, by.x="region", by.y = "state_whole")

  # centroids.df <- aggregate(cbind(long, lat) ~ fx_vertex, data=state, FUN=mean) %>% round(2)
  # names(centroids.df)[2:3] <- paste0("center_", names(centroids.df)[2:3])
  ggplot(state, aes(x = long, y = lat, fill = factor(fx_U_exist), group = group)) +
    geom_polygon(col = "white") +
    coord_map() +
    theme_map() +
    scale_fill_discrete(name="Strategic Balance Exists in the State",
                        breaks=c(1, 0),
                        labels=c("YES", "NO")) +
    # with(centroids.df, annotate(geom="text", x = center_long, y=center_lat, label = fx_vertex, size = 2.5)) +
    with(state_centers, annotate(geom="text", x = Longitude, y=Latitude, label = round(fx_vertex,2), size = 3.6)) +
    ggtitle("The Geographic Distribution of the Strategic Balance") +
    theme(text = element_text(size=16, family="Times New Roman")) 
  

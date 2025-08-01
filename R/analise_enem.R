library(data.table)
library(mirt)

# dados_enem <- data.table::fread('https://raw.githubusercontent.com/alexandrejaloto/DIF_avaliacao_educacional/refs/heads/master/dados/banco_enem.csv')
dados_enem <- data.table::fread('dados/banco_enem.csv')

tab.pars <- mirt::multipleGroup(dados_enem[,1:45], 1, group = dados_enem$group,
                            pars = 'values',
                            itemtype = '3PL',
                            invariance = c('free_mean',
                                           'free_var',
                                           colnames(dados_enem[,1:45])))

tab.pars[tab.pars$name == 'a1', 'prior.type'] <- 'lnorm'
tab.pars[tab.pars$name == 'a1', 'prior_1'] <- 1.7
tab.pars[tab.pars$name == 'a1', 'prior_2'] <- log(1.7)
tab.pars[tab.pars$name == 'a1', 'value'] <- 1.7
tab.pars[tab.pars$name == 'g', 'prior.type'] <- 'expbeta'
tab.pars[tab.pars$name == 'g', 'prior_1'] <- 5
tab.pars[tab.pars$name == 'g', 'prior_2'] <- 17
tab.pars[tab.pars$name == 'g', 'value'] <- .2

fit1 <- mirt::multipleGroup(dados_enem[,1:45],
                            1, group = dados_enem$group,
                            pars = tab.pars,
                            itemtype = '3PL',
                            invariance = c('free_mean',
                                           'free_var',
                                           colnames(dados_enem[,1:45])),
                            TOL = .01)

dif.rmsd <- mirt::RMSD_DIF(fit1, flag = .1)
dif.rmsd

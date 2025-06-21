library(mirt)
library(data.table)

dados <- fread('dados/banco_simulado.csv')

grupo <- rep (c('G1', 'G2'), c(1000, 1000))

pars <- multipleGroup(dados, 1, group = grupo,
                     pars = 'values',
                     itemtype = '3PL', TOL = .01,
                     invariance = c('free_means', 'free_var',
                                    colnames(dados)))


pars[pars$name == 'a1', 'prior.type'] <- 'lnorm'
pars[pars$name == 'a1', 'prior_1'] <- log(1.7)
pars[pars$name == 'a1', 'prior_2'] <- 0.5
pars[pars$name == 'a1', 'value'] <- 1.7
pars[pars$name == 'g', 'prior.type'] <- 'expbeta'
pars[pars$name == 'g', 'prior_1'] <- 5
pars[pars$name == 'g', 'prior_2'] <- 17
pars[pars$name == 'g', 'value'] <- .2

fit1 <- multipleGroup(dados, 1, group = grupo,
                      pars = pars,
                      itemtype = '3PL', TOL = .01,
                      invariance = c('free_means', 'free_var',
                                     colnames(dados)))

coef(fit1, IRTpars = TRUE, simplify = TRUE)

dif.rmsd <- mirt::RMSD_DIF(fit1)
dif.rmsd

dif.rmsd <- mirt::RMSD_DIF(fit1, flag = .1)
dif.rmsd

fit2 <- multipleGroup(dados, 1, group = grupo,
                      pars = pars,
                      itemtype = '3PL', TOL = .01,
                      invariance = c('free_means', 'free_var',
                                     colnames(dados)[-c(1, 2)]))

coef(fit2, IRTpars = TRUE, simplify = TRUE)

itemplot(fit2, 1)
itemplot(fit2, 2)

png (filename = "graficos/plot_item1.png", width = 1000,
     height = 1000,
     res = 300)
itemplot(fit2, 1)
dev.off()

png (filename = "graficos/plot_item2.png", width = 1000,
     height = 1000,
     res = 300)
itemplot(fit2, 2)
dev.off()


dif.rmsd2 <- mirt::RMSD_DIF(fit2, flag = .1)
dif.rmsd2

rm(list = ls())

source('R/fct_abre.banco2.R')

# import data
micro <- data.table::fread('D:/Microdados/2021/MICRODADOS_ENEM_2021.csv')
micro <- data.frame(micro)

micro <- subset(
  micro,
  # regular school
  TP_ENSINO == 1 &
    # 2 == Estou cursando e concluirei o Ensino Médio em 2021
    TP_ST_CONCLUSAO == 2 &
    TP_FAIXA_ETARIA >= 2 &
    TP_PRESENCA_CN == 1
)

items <- data.table::fread('D:/Microdados/2021/ITENS_PROVA_2021.csv', nrows = 10000)
items.ns <- subset(items, SG_AREA == 'CN')

# select only regular and computerized tests in natural sciences
# regular
micro.ns_reg <- subset(micro,
                       CO_PROVA_CN %in% c(
                         909,
                         910,
                         911,
                         912))
# computerized
micro.ns_dig <- subset(micro,
                       CO_PROVA_CN %in% c(
                         1011,
                         1012,
                         1013,
                         1014))


bib <- data.frame(Caderno = 1:4, Disciplina1 = 'CN', Bloco1 = 1:4)

itens <- data.frame(Bloco = items.ns$CO_PROVA,
                    Posicao = items.ns$CO_POSICAO,
                    Item = items.ns$CO_ITEM,
                    Gabarito = items.ns$TX_GABARITO,
                    Disciplina = 'CN')

itens.dig <- subset(itens,
                    Bloco %in% c(1011,
                                 1012,
                                 1013,
                                 1014))

itens.dig$Bloco <- itens.dig$Bloco - 1010

itens.reg <- subset(itens,
                    Bloco %in% c(909,
                                 910,
                                 911,
                                 912))

itens.reg$Bloco <- itens.reg$Bloco - 908

micro.ns_reg$CO_PROVA_CN <- micro.ns_reg$CO_PROVA_CN - 908
micro.ns_dig$CO_PROVA_CN <- micro.ns_dig$CO_PROVA_CN - 1010

data_reg <- data.frame(CAD = micro.ns_reg$CO_PROVA_CN, INEPsico::abre.resp(micro.ns_reg$TX_RESPOSTAS_CN))
data_dig <- data.frame(CAD = micro.ns_dig$CO_PROVA_CN, INEPsico::abre.resp(micro.ns_dig$TX_RESPOSTAS_CN))

data_reg <- abre.banco2(banco = data_reg, itens = itens.reg, bib = bib, disc = 'CN', disc.cad = 1)
data_dig <- abre.banco2(banco = data_dig, itens = itens.dig, bib = bib, disc = 'CN', disc.cad = 1)

data_reg <- data.frame(mirt::key2binary(data_reg$respostas[,-1], data_reg$gabarito))
data_dig <- data.frame(mirt::key2binary(data_dig$respostas[,-1], data_dig$gabarito))

data_reg$group <- 'REG'
data_dig$group <- 'DIG'

data_dig <- cbind(data_dig, micro.ns_dig$NU_INSCRICAO)

names(data_dig)[47] <- c('NU_INSCRICAO')

data_reg <- cbind(data_reg, micro.ns_reg$NU_INSCRICAO)

names(data_reg)[47] <- c('NU_INSCRICAO')

set.seed(1111)
reg_sample <- sample(nrow(data_reg), nrow(data_dig), replace = FALSE)
reg_sample <- data_reg[reg_sample,]

data <- rbind(reg_sample, data_dig)
names(data)

# data.table::fwrite(data, 'results/data.csv')
data <- data.table::fread('results/data.csv')

pars <- mirt::multipleGroup(data[,1:45], 1, group = data$group,
                            pars = 'values',
                            itemtype = '3PL',
                            invariance = c('free_mean',
                                           'free_var',
                                           colnames(data[,1:45])))

pars[pars$name == 'a1', 'prior.type'] <- 'lnorm'
pars[pars$name == 'a1', 'prior_1'] <- 1.7
pars[pars$name == 'a1', 'prior_2'] <- log(1.7)
pars[pars$name == 'a1', 'value'] <- 1.7
pars[pars$name == 'g', 'prior.type'] <- 'expbeta'
pars[pars$name == 'g', 'prior_1'] <- 5
pars[pars$name == 'g', 'prior_2'] <- 17
pars[pars$name == 'g', 'value'] <- .2

fit1 <- mirt::multipleGroup(data[,1:45],
                            1, group = data$group,
                            pars = pars,
                            itemtype = '3PL',
                            invariance = c('free_mean',
                                           'free_var',
                                           colnames(data[,1:45])),
                            TOL = .01)

dif.rmsd <- mirt::RMSD_DIF(fit1, flag = .12)
dif.rmsd <- mirt::RMSD_DIF(fit1)
dif.rmsd

dif.inepsico <- INEPsico::dif.mirt(fit.atual = mirt::extract.group(fit1, 'REG'),
                   fit.antigo = mirt::extract.group(fit1, 'DIG'))
dif.inepsico

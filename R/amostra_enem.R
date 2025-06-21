library(data.table)

rm(list = ls())

source('R/fct_abre.banco2.R')

# importar arquivo de microdados
micro <- data.table::fread('D:/Microdados/2021/MICRODADOS_ENEM_2021.csv')
micro <- data.frame(micro)

micro <- subset(
  micro,
  # ensino regular
  TP_ENSINO == 1 &
    # situação da conclusão: Estou cursando e concluirei o Ensino Médio em 2021
    TP_ST_CONCLUSAO == 2 &
    # pelo menos 17 anos no final de 2021
    TP_FAIXA_ETARIA >= 2 &
    # presente na prova de CN
    TP_PRESENCA_CN == 1
)

# dados dos itens
items <- data.table::fread('D:/Microdados/2021/ITENS_PROVA_2021.csv', nrows = 10000)

# itens de CN
items.ns <- subset(items, SG_AREA == 'CN')

# selecionar somente as provas de CN sem adaptações

# prova em papel (será chamada de regular)
micro.ns_reg <- subset(micro,
                       CO_PROVA_CN %in% c(
                         909,
                         910,
                         911,
                         912))
# prova digital
micro.ns_dig <- subset(micro,
                       CO_PROVA_CN %in% c(
                         1011,
                         1012,
                         1013,
                         1014))


# preparação para usar a função abre.banco2, que imita a função abre.banco do pacote INEPsico
# o pacote INEPsico está disponível em github.com/alexandrejaloto/INEPsico

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

# nomear os grupos
data_reg$group <- 'REG'
data_dig$group <- 'DIG'

data_dig <- cbind(data_dig, micro.ns_dig$NU_INSCRICAO)

names(data_dig)[47] <- c('NU_INSCRICAO')

data_reg <- cbind(data_reg, micro.ns_reg$NU_INSCRICAO)

names(data_reg)[47] <- c('NU_INSCRICAO')

# sortear a amostra do regular (papel)
set.seed(1111)
reg_sample <- sample(nrow(data_reg), nrow(data_dig), replace = FALSE)
reg_sample <- data_reg[reg_sample,]

data <- rbind(reg_sample, data_dig)
names(data)

data.table::fwrite(data, 'dados/banco_enem.csv')

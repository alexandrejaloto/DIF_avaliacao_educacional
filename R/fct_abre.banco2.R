abre.banco2 <- function(banco, itens, bib, disc, disc.cad = 2)
{
  estrutura = INEPsico::gera.caderno(itens, bib, disc.cad)
  tot.cad = max(estrutura[[disc]]$Caderno)
  disciplinas = as.character(unique(unlist(bib[, paste0("Disciplina",
                                                        1:disc.cad)])))
  banco$id <- 1:nrow(banco)
  aberto. = data.table::as.data.table(data.frame())
  for (i in 1:tot.cad) {
    resp = data.table::as.data.table(subset(banco, banco[, 1] == i))
    if (nrow(resp) == 0)
      next
    disc.n = list()
    cad. = list()
    for (j in 1:disc.cad) {
      cad.[[j]] = subset(estrutura[[disciplinas[[j]]]], Caderno == i)
      names(cad.)[j] = disciplinas[[j]]
      disc.n[[j]] = subset(bib, Caderno == i)[[paste0("Disciplina", j)]]
    }
    cad = data.frame()
    for (j in 1:disc.cad) cad = rbind(cad, cad.[[disc.n[[j]]]])
    names(resp) = c("CADERNO", cad$Item, 'id')
    aberto. = rbind(aberto., resp, fill = TRUE)
  }
  aberto = list()
  aberto$respostas = data.frame(aberto.)
  names(aberto$respostas) = names(aberto.)
  rm(aberto.)
  # cod.itens = itens %>% subset(Disciplina == disc) %>% dplyr::arrange(Bloco,
                                                                      # Posicao) %>% dplyr::distinct(Item) %>% dplyr::pull()

  cod.itens = subset(itens, Disciplina == disc)
  cod.itens = dplyr::arrange(cod.itens, Bloco, Posicao)
  cod.itens = dplyr::distinct(cod.itens, Item)
  cod.itens = dplyr::pull(cod.itens)

  aberto$respostas = aberto$respostas[order(aberto$respostas$id),]
  # aberto$respostas = aberto$respostas[,-ncol(aberto$respostas)]

  # aberto[["respostas"]][, c("CADERNO", cod.itens)]
  aberto$respostas = aberto$respostas[, c("CADERNO", cod.itens)]
  rownames(aberto$respostas) <- 1:nrow(aberto$respostas)
  aberto$gabarito = data.frame(Item = cod.itens)
  for (i in 1:length(aberto[["gabarito"]]$Item)) {
    cod = aberto[["gabarito"]]$Item[i]
    aberto[["gabarito"]]$Gabarito[i] = as.character(estrutura[[disc]]$Gabarito[which(estrutura[[disc]]$Item ==
                                                                                       cod)[1]])
  }
  return(aberto)
}

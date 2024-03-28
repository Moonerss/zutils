#' Downlaod KEGG pathway
#'
#' Download KEGG pathway and gene list
#'
#' @param species supported organism listed in 'https://www.genome.jp/kegg/catalog/org_list.html'
#' @param OrgDb OrgDb to convert gene ID
#'
#'
#' @return A \code{tibble} object contain pathway informations
#' @export

download_kegg_geneset <- function(species = 'hsa', OrgDb = 'org.Hs.eg.db') {
  KEGG_DATA <- clusterProfiler:::prepare_KEGG(species = species, KEGG_Type = "KEGG", keyType = "kegg")
  kegg_category <- clusterProfiler:::kegg_category_data()

  ## pathway to gene
  pathway_to_gene <- KEGG_DATA$PATHID2EXTID %>%
    tibble::enframe(name = 'id', value = 'gene') %>%
    tidyr::unnest(cols = c(.data$gene))
  ## ene to pathway
  gene_to_pathway <- KEGG_DATA$EXTID2PATHID %>%
    tibble::enframe(name = 'gene', value = 'id') %>%
    tidyr::unnest(cols = c(.data$id))
  ## merge into one
  merge_pathway_gene <- pathway_to_gene %>%
    dplyr::bind_rows(gene_to_pathway[,c(2,1)]) %>%
    dplyr::group_by(.data$id) %>%
    tidyr::nest() %>% dplyr::ungroup() %>%
    dplyr::mutate(data = purrr::map(.data$data, function(x) {
      x %>% dplyr::pull(.data$gene) %>% unique()
    })) %>%
    dplyr::rename(gene = data)

  species_id <- merge_pathway_gene$id %>% stringr::str_sub(end = 3L) %>% unique()
  ## merge result
  kegg_data <- kegg_category %>%
    dplyr::mutate(id = stringr::str_c(species_id, id)) %>%
    dplyr::left_join(merge_pathway_gene, by = 'id')
  ## convert id
  all_gene <- merge_pathway_gene$gene %>% unlist() %>% unique()
  all_gene_convert <- clusterProfiler::bitr(all_gene, fromType = 'ENTREZID', toType = 'SYMBOL', OrgDb = OrgDb)

  ## merge to the result
  kegg_data <- kegg_data %>%
    dplyr::mutate(Symbol = purrr::map(gene, function(x) {
      if (!is.null(x)) {
        all_gene_convert[match(x, all_gene_convert$ENTREZID), 'SYMBOL']
      }
    }))
  attr(kegg_data, 'Date') <- Sys.Date()
  return(kegg_data)
}



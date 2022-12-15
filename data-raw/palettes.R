## code to prepare `palettes` dataset goes here

colorblind_8 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

divergent_24 <- c("#332288", "#117733", "#44AA99", "#88CCEE",
                  "#DDCC77", "#CC6677", "#AA4499", "#882255",
                  "#ACE476", "#B96CFE", "#5392E3", "#8A9E15",
                  "#28C241", "#F66E3A", "#D9FB49", "#FF63AA",
                  "#8F4CB7", "#F4A242", "#AC8EF8", "#000000",
                  "#3A839C", "#12BCFE", "#18DFB7", "#68002D")

pal_alerte <- c("#5AB5BF", "#BF3111")

qual_col_pals <- RColorBrewer::brewer.pal.info[
  RColorBrewer::brewer.pal.info$category == 'qual',]
RandomCol_74 <- unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors,
                              rownames(qual_col_pals)))

usethis::use_data(colorblind_8,
                  divergent_24,
                  pal_alerte,
                  RandomCol_74,
                  overwrite = TRUE)


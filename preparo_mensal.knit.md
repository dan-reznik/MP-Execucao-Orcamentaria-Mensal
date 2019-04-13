---
title: "Preparo dos dados execução mensal"
author: Daniel Lima (MP-RJ), Dan S. Reznik (PUC-CCE)
date: Abril, 2019
output: 
  html_document:
    mathjax: null
    toc: true
    toc_depth: 3
    toc_float: true
    theme: united
    highlight: tango
---



## Carrega libraries


```r
library(tidyverse)
library(lubridate)
library(fs)
library(stringi) # for %s+%
library(furrr)
library(zip)
library(tictoc)
source("util.R")
```


```r
data_dir <- "data"
dir_ls(data_dir,regexp=".*zip")
#> data/Despesas MRJ.zip
```

## Lista conteúdo do zip


```r
fname_zip <- dir_slash_fname(data_dir,"Despesas MRJ.zip")
fnames_in_zip <- zip_list(fname_zip)
fnames_in_zip
#>                  filename compressed_size uncompressed_size
#> 1  MP_EMPENHOS_201401.TXT         2451474          23766764
#> 2  MP_EMPENHOS_201402.TXT         1556590          16132126
#> 3  MP_EMPENHOS_201403.TXT         2500675          22998390
#> 4  MP_EMPENHOS_201404.TXT         2789165          25203925
#> 5  MP_EMPENHOS_201405.TXT         3166358          27041619
#> 6  MP_EMPENHOS_201406.TXT         2980841          25716209
#> 7  MP_EMPENHOS_201407.TXT         3570329          29915376
#> 8  MP_EMPENHOS_201408.TXT         3221303          27240744
#> 9  MP_EMPENHOS_201409.TXT         3918330          32209536
#> 10 MP_EMPENHOS_201410.TXT         3766820          31151803
#> 11 MP_EMPENHOS_201411.TXT         3707314          29898701
#> 12 MP_EMPENHOS_201412.TXT         7645363          61131666
#> 13 MP_EMPENHOS_201501.TXT         3276326          29507823
#> 14 MP_EMPENHOS_201502.TXT         1250661          11951930
#> 15 MP_EMPENHOS_201503.TXT         2659701          23979891
#> 16 MP_EMPENHOS_201504.TXT         3047246          26649077
#> 17 MP_EMPENHOS_201505.TXT         2992394          25363598
#> 18 MP_EMPENHOS_201506.TXT         3875453          33304462
#> 19 MP_EMPENHOS_201507.TXT         3664056          31540204
#> 20 MP_EMPENHOS_201508.TXT         3565663          30421998
#> 21 MP_EMPENHOS_201509.TXT         3632647          31379083
#> 22 MP_EMPENHOS_201510.TXT         3571131          30327006
#> 23 MP_EMPENHOS_201511.TXT         4213536          33779908
#> 24 MP_EMPENHOS_201512.TXT         6707797          50241044
#> 25 MP_EMPENHOS_201601.TXT         3111796          26644553
#> 26 MP_EMPENHOS_201602.TXT         1361024          12376333
#> 27 MP_EMPENHOS_201603.TXT         3045152          25991162
#> 28 MP_EMPENHOS_201604.TXT         3193100          26634821
#> 29 MP_EMPENHOS_201605.TXT         3670143          34148201
#> 30 MP_EMPENHOS_201606.TXT         3828771          36517208
#> 31 MP_EMPENHOS_201607.TXT         3742385          36593571
#> 32 MP_EMPENHOS_201608.TXT         3651756          35552348
#> 33 MP_EMPENHOS_201609.TXT         3658237          35402400
#> 34 MP_EMPENHOS_201610.TXT         4108543          38693525
#> 35 MP_EMPENHOS_201611.TXT         4438187          39570644
#> 36 MP_EMPENHOS_201612.TXT         3483771          36159648
#> 37 MP_EMPENHOS_201701.TXT         1551538          16353261
#> 38 MP_EMPENHOS_201702.TXT          592546           6855341
#> 39 MP_EMPENHOS_201703.TXT         2170560          23010482
#> 40 MP_EMPENHOS_201704.TXT         2377178          24887491
#> 41 MP_EMPENHOS_201705.TXT         3008143          29626903
#> 42 MP_EMPENHOS_201706.TXT         2510526          25583958
#> 43 MP_EMPENHOS_201707.TXT         2924519          29614638
#> 44 MP_EMPENHOS_201708.TXT         2846901          28089635
#> 45 MP_EMPENHOS_201709.TXT         2617917          25503501
#> 46 MP_EMPENHOS_201710.TXT         3471917          33209738
#> 47 MP_EMPENHOS_201711.TXT         2287911          24553415
#> 48 MP_EMPENHOS_201712.TXT         4924664          51700864
#> 49 MP_EMPENHOS_201801.TXT         2443688          24885040
#> 50 MP_EMPENHOS_201802.TXT         1530552          14570090
#> 51 MP_EMPENHOS_201803.TXT         3022859          28080678
#> 52 MP_EMPENHOS_201804.TXT         3095241          33724423
#> 53 MP_EMPENHOS_201805.TXT         2763372          27844544
#> 54 MP_EMPENHOS_201806.TXT         2835054          27751324
#> 55 MP_EMPENHOS_201807.TXT         2730578          27331044
#> 56 MP_EMPENHOS_201808.TXT         2917811          29121799
#> 57 MP_EMPENHOS_201809.TXT         2861234          30322636
#>              timestamp permissions
#> 1  2019-04-06 13:18:30         600
#> 2  2019-04-06 13:18:30         600
#> 3  2019-04-06 13:18:30         600
#> 4  2019-04-06 13:18:30         600
#> 5  2019-04-06 13:18:32         600
#> 6  2019-04-06 13:18:32         600
#> 7  2019-04-06 13:18:32         600
#> 8  2019-04-06 13:18:32         600
#> 9  2019-04-06 13:18:32         600
#> 10 2019-04-06 13:18:32         600
#> 11 2019-04-06 13:18:32         600
#> 12 2019-04-06 13:18:34         600
#> 13 2019-04-06 13:18:34         600
#> 14 2019-04-06 13:18:34         600
#> 15 2019-04-06 13:18:34         600
#> 16 2019-04-06 13:18:34         600
#> 17 2019-04-06 13:18:34         600
#> 18 2019-04-06 13:18:34         600
#> 19 2019-04-06 13:18:34         600
#> 20 2019-04-06 13:18:34         600
#> 21 2019-04-06 13:18:34         600
#> 22 2019-04-06 13:18:36         600
#> 23 2019-04-06 13:18:36         600
#> 24 2019-04-06 13:18:36         600
#> 25 2019-04-06 13:18:36         600
#> 26 2019-04-06 13:18:36         600
#> 27 2019-04-06 13:18:36         600
#> 28 2019-04-06 13:18:36         600
#> 29 2019-04-06 13:18:36         600
#> 30 2019-04-06 13:18:38         600
#> 31 2019-04-06 13:18:38         600
#> 32 2019-04-06 13:18:38         600
#> 33 2019-04-06 13:18:40         600
#> 34 2019-04-06 13:18:40         600
#> 35 2019-04-06 13:18:42         600
#> 36 2019-04-06 13:18:50         600
#> 37 2019-04-06 13:18:50         600
#> 38 2019-04-06 13:18:50         600
#> 39 2019-04-06 13:18:50         600
#> 40 2019-04-06 13:18:52         600
#> 41 2019-04-06 13:18:52         600
#> 42 2019-04-06 13:18:28         600
#> 43 2019-04-06 13:18:28         600
#> 44 2019-04-06 13:18:28         600
#> 45 2019-04-06 13:18:28         600
#> 46 2019-04-06 13:18:28         600
#> 47 2019-04-06 13:18:28         600
#> 48 2019-04-06 13:18:28         600
#> 49 2019-04-06 13:18:28         600
#> 50 2019-04-06 13:18:28         600
#> 51 2019-04-06 13:18:30         600
#> 52 2019-04-06 13:18:30         600
#> 53 2019-04-06 13:18:30         600
#> 54 2019-04-06 13:18:30         600
#> 55 2019-04-06 13:18:30         600
#> 56 2019-04-06 13:18:30         600
#> 57 2019-04-06 13:18:30         600
```

## Vamos analizar o primeiro arquivo


```r
fname1 <- fnames_in_zip$filename[1]
zip::unzip(fname_zip,files=fname1,exdir=data_dir)
dir_ls(data_dir,regexp="*.TXT")
#> data/MP_EMPENHOS_201401.TXT
```

### Adiciona o diretório


```r
fname1_txt <- dir_slash_fname(data_dir,fname1) %>%
  path_norm
fname1_txt
#> data/MP_EMPENHOS_201401.TXT
```

### Estima o encoding do arquivo


```r
guess_encoding(fname1_txt)
#> # A tibble: 2 x 2
#>   encoding   confidence
#>   <chr>           <dbl>
#> 1 ISO-8859-1       0.7 
#> 2 ISO-8859-2       0.32
```

### Dados de localização típicos do Brasil


```r
locale_bra <- locale(encoding="ISO-8859-1",
                     decimal_mark = ",",
                     date_format="%d/%m/%Y")
```

### Olha para as primeiras 2 linhas


```r
first2 <- read_lines(fname1_txt,
                     locale=locale_bra,n_max=2)
first2
#> [1] "Poder;Grupo;Modalidade;Elemento;NomeElemento;SubElemento;NomeSubElemento;Orgao;NomeOrgao;UO;NomeUO;UG;NomeUG;Credor;NomeCredor;FonteRecursos;NomeFonteRecursos;Processo;Funcao;NomeFuncao;SubFuncao;NomeSubFuncao;Programa;NomePrograma;Acao;NomeAcao;Licitacao;Data;TipoAto;Valor;Exercicio;EmpenhoExercicio;Liquidacao;Pagamento;Historico;Banco;NomeBanco;Agencia;ContaCorrente;NomeContaCorrente;ASPS;MDE;ExercicioContrato;NumeroContrato;ObjetoContrato;"                                                                                                                                                                                                                                                                                                                                                                                   
#> [2] "Executivo;OUTRAS DESPESAS CORRENTES;APLICACOES DIRETAS;30;MATERIAL DE CONSUMO;27;MATERIAIS DE PROTECAO E SEGURANCA;4351;Companhia Municipal de Limpeza Urbana;4351;Companhia Municipal de Limpeza Urbana;4351;Companhia Municipal de Limpeza Urbana;7913842000183;DMX DO BRASIL COMERCIO LTDA ME;100;ORDINARIOS NAO VINCULADOS;15043092013;15;URBANISMO;122;ADMINISTRACAO GERAL;0385;GESTAO ADMINISTRATIVA - MEIO AMBIENTE E SUSTENTABILIDADE;4165;APOIO ADMINISTRATIVO - ADM. INDIRETA - MEIO AMBIENTE E SUSTENTABILIDADE;PREGÃO;02/01/2014;EMPENHO;5183,200000;2014;48/2014;;;AQUISIÇÃO DE CONE, POR SOLICITAÇÃO DA FGX        -ITENS -01 E 02        -PRAZO DE PAGAMENTO - 30 DIAS APÓS A ENTREGA DO MATERIAL        -DEMAIS CONDIÇÕES E ESPECIFICAÇÕES CONFORME EDITAL E SEUS ANEXOS;1;BANCO DO BRASIL S/A;2234;73067;CONTA MOVIMENTO;N;N;;;;"
```

Quantos separadores nestas duas primeiras linhas


```r
first2 %>%
  str_count(fixed(";"))%>%
  table
#> .
#> 45 
#>  2
```


Quantos separadores ";" por linha? Note que a maioria tem 45 mas algumas linhas tem 46 e 48 separadores


```r
read_lines(fname1_txt,locale=locale_bra)%>%
  str_count(fixed(";"))%>%
  table
#> .
#>    45    46    48 
#> 31678    14     3
```

Mostre quais linhas tem separadores anomalos


```r
df_probs <- tibble(sep_cnt=read_lines(fname1_txt,locale=locale_bra)%>%
  str_count(fixed(";")))%>%
  mutate(row=row_number())%>%
  filter(sep_cnt!=45)%>%
  select(row,sep_cnt) %>%
  arrange(sep_cnt) #%>%
df_probs
#> # A tibble: 17 x 2
#>      row sep_cnt
#>    <int>   <int>
#>  1 24268      46
#>  2 24269      46
#>  3 26000      46
#>  4 27535      46
#>  5 29736      46
#>  6 29737      46
#>  7 29738      46
#>  8 29755      46
#>  9 29764      46
#> 10 29936      46
#> 11 29937      46
#> 12 30383      46
#> 13 30384      46
#> 14 30385      46
#> 15 14579      48
#> 16 26792      48
#> 17 26793      48
```

### Reporta quais linhas tem problemas no arquivo "bad_lines_txt"


```r
bad_lines <- read_lines(fname1_txt,locale=locale_bra)%>%
  keep(~str_count(.x,fixed(";"))!=45)
c(first2[1],bad_lines)%>%write_lines("bad_lines.txt")
```

### Reporta campos lado a lado (transpostos) num arquivo "bad_lines_transp.txt" para análise


```r
{
  lgts<-c(first2,bad_lines)%>%str_split(fixed(";"))%>%map_int(length)
  cols<-str_c(c(letters,LETTERS),lgts)
  df_transp <- c(first2,bad_lines)%>%
    str_split(fixed(";"))%>%
    map2(lgts,~c(.x,rep(NA,49-.y))) %>%
    {l<-.;do.call(data.frame,l%>%set_names(cols%>%head(length(l))))}
  df_transp%>%write_delim('bad_lines_transp.txt',delim="|")
}
#> Warning in stri_c(..., sep = sep, collapse = collapse, ignore_null = TRUE):
#> longer object length is not a multiple of shorter object length
```

### Linhas com colunas adicionais: colunas serão ignoradas, são anotações manuais


```r
read_delim("bad_lines_transp.txt",
           delim=fixed("|")) %>%
  mutate_all(~str_sub(.,end=10))
#> Parsed with column specification:
#> cols(
#>   a46 = col_character(),
#>   b46 = col_character(),
#>   c49 = col_character(),
#>   d47 = col_character(),
#>   e47 = col_character(),
#>   f47 = col_character(),
#>   g49 = col_character(),
#>   h49 = col_character(),
#>   i47 = col_character(),
#>   j47 = col_character(),
#>   k47 = col_character(),
#>   l47 = col_character(),
#>   m47 = col_character(),
#>   n47 = col_character(),
#>   o47 = col_character(),
#>   p47 = col_character(),
#>   q47 = col_character(),
#>   r47 = col_character(),
#>   s47 = col_character()
#> )
#> # A tibble: 49 x 19
#>    a46    b46   c49   d47   e47   f47   g49   h49   i47   j47   k47   l47  
#>    <chr>  <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 Poder  Exec~ Exec~ Exec~ Exec~ Exec~ Exec~ Exec~ Exec~ Exec~ Exec~ Exec~
#>  2 Grupo  OUTR~ OUTR~ INVE~ INVE~ INVE~ OUTR~ OUTR~ OUTR~ OUTR~ OUTR~ OUTR~
#>  3 Modal~ APLI~ APLI~ APLI~ APLI~ APLI~ APLI~ APLI~ APLI~ APLI~ APLI~ APLI~
#>  4 Eleme~ 30    39    51    51    51    39    39    93    39    39    39   
#>  5 NomeE~ MATE~ OUTR~ OBRA~ OBRA~ OBRA~ OUTR~ OUTR~ INDE~ OUTR~ OUTR~ OUTR~
#>  6 SubEl~ 27    48    01    01    01    48    48    02    04    04    04   
#>  7 NomeS~ "MAT~ SERV~ EXEC~ EXEC~ EXEC~ SERV~ SERV~ OUTR~ SERV~ SERV~ SERV~
#>  8 Orgao  4351  1600  2400  2400  2400  1600  1600  1600  4352  4352  4352 
#>  9 NomeO~ "Com~ Secr~ Secr~ Secr~ Secr~ Secr~ Secr~ Secr~ "Com~ "Com~ "Com~
#> 10 UO     4351  1601  2401  2401  2401  1601  1601  1601  4352  4352  4352 
#> # ... with 39 more rows, and 7 more variables: m47 <chr>, n47 <chr>,
#> #   o47 <chr>, p47 <chr>, q47 <chr>, r47 <chr>, s47 <chr>
  #%>%View
```

### Verifica truncagem de todas as linhas para o numero correto de colunas (46)


```r
trunc_cols(fname1_txt,locale_bra,
           max_cols=46,n_max=10)%>%
  str_count(fixed(";"))%>%
  table
#> .
#> 45 
#> 10
```

### Lento: trunca as colunas de todos os arquivos no .zip (usando processamento paralelo do library(furrr)) e converte cada um para .rds (com alta compressao)


```r
fnames <- zip_list(fname_zip) %>% pull(filename)
repl_ext(fnames,"rds") %>%
  dir_slash_fname(data_dir,.) %>%
  walk(~if(file_exists(.))file_delete(.))
```


```r
tic()
plan(multiprocess)
fnames[10] %>%
  furrr::future_map_chr(~{
  #print(.x)
  unzip(fname_zip,files=.x,exdir=data_dir)
  trunc_to_rds(dir_slash_fname(data_dir,.x),locale_bra,45) # em util.R
  f_txt <- dir_slash_fname(data_dir,.x)
  f_utf8 <- get_fname_utf8(f_txt)
  file_delete(f_txt)
  file_delete(f_utf8)
  .x
  })
toc()
```

Vamos examinar o primeiro rds


```r
fname_rds1 <- dir_ls(data_dir,regexp="MP_EMPENHOS.*rds") %>%
  sort %>%
  first
df_rds1 <- fname_rds1 %>% read_rds
dim(df_rds1)
```


```r
glimpse(df_rds1)
```

Verifica cardinalidade das colunas nao numericas


```r
df_rds1 %>% get_card
```

### Combina todos os data frames num só usando "purrr::map_dfr"


```r
fnames_rds <- dir_ls(path=data_dir,regexp="\\d{6}\\.rds$")
df_all <- fnames_rds %>%
  map_dfr(~{
    #print(.x)
    read_rds(.x)%>%
      # already taken care of at read_delim
      mutate_at(vars(Elemento,SubFuncao,ContaCorrente),as.character)
  }) %>%
  mutate_if(is.character,as.factor)
fnames_rds %>% walk(file_delete)
```


```r
df_all %>% 
  arrange(Data) %>%
  write_rds(dir_slash_fname(data_dir,"MP_EMPENHOS_ALL.rds"),
            compress="bz")
```




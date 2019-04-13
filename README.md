Despesas Mensais
================

Nota: preparo dos dados [aqui](preparo_mensal.html)

``` r
library(tidyverse)
library(lubridate)
library(fs)
library(tictoc) # mede tempo de execução
```

``` r
source("util.R")
```

Le arquivo

``` r
fname_despesas <- "./data/MP_EMPENHOS_ALL.rds"
tic()
df_despesas <- read_rds(fname_despesas)
toc()
#> 17.996 sec elapsed
```

Quais dimensões

``` r
dim(df_despesas)
#> [1] 2252570      45
```

Qual tamanho

``` r
object.size(df_despesas)%>%format(units="Mb")
#> [1] "534.8 Mb"
```

``` r
df_despesas%>%glimpse
#> Observations: 2,252,570
#> Variables: 45
#> $ Poder             <fct> Executivo, Executivo, Executivo, Executivo, Ex…
#> $ Grupo             <fct> OUTRAS DESPESAS CORRENTES, OUTRAS DESPESAS COR…
#> $ Modalidade        <fct> "APLICACOES DIRETAS", "APLICACOES DIRETAS", "A…
#> $ Elemento          <fct> 39, 39, 52, 39, 39, 39, 39, 39, 39, 39, 39, 52…
#> $ NomeElemento      <fct> OUTROS SERVICOS DE TERCEIROS - PESSOA JURIDICA…
#> $ SubElemento       <fct> 77, 04, 01, 07, 13, 78, 77, 04, 04, 19, 78, 03…
#> $ NomeSubElemento   <fct> "MANUTENCAO E SUPORTE DE SOFTWARE", "SERVS.DE …
#> $ Orgao             <dbl> 3041, 3041, 3041, 3041, 3041, 3041, 3041, 3041…
#> $ NomeOrgao         <fct> Fundação Planetário da Cidade do Rio de Janeir…
#> $ UO                <dbl> 3041, 3041, 3041, 3041, 3041, 3041, 3041, 3041…
#> $ NomeUO            <fct> Fundação Planetário da Cidade do Rio de Janeir…
#> $ UG                <dbl> 3041, 3041, 3041, 3041, 3041, 3041, 3041, 3041…
#> $ NomeUG            <fct> Fundação Planetário da Cidade do Rio de Janeir…
#> $ Credor            <fct> 860640000171, 2270703000165, 31978612000187, 6…
#> $ NomeCredor        <fct> INGRESSO.COM SA, HIDROLIGHT EQUIPAMENTOS E SER…
#> $ FonteRecursos     <dbl> 200, 200, 408, 200, 200, 200, 200, 200, 200, 2…
#> $ NomeFonteRecursos <fct> "RECEITA PROPRIA DE AUTARQUIAS, FUNDACOES E EM…
#> $ Processo          <fct> 126000472010, 126003602012, 126003292013, 1260…
#> $ Funcao            <fct> 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13…
#> $ NomeFuncao        <fct> CULTURA, CULTURA, CULTURA, CULTURA, CULTURA, C…
#> $ SubFuncao         <fct> 126, 122, 126, 122, 122, 126, 126, 573, 122, 3…
#> $ NomeSubFuncao     <fct> TECNOLOGIA DA INFORMACAO, ADMINISTRACAO GERAL,…
#> $ Programa          <fct> 0397, 0387, 0397, 0387, 0387, 0397, 0397, 0310…
#> $ NomePrograma      <fct> "GESTAO DOS SERVICOS E SISTEMAS INFORMATIZADOS…
#> $ Acao              <dbl> 4757, 4167, 4757, 4167, 4167, 4757, 4757, 4506…
#> $ NomeAcao          <fct> "MANUTENCAO E DESENVOLVIMENTO DA INFORMATICA -…
#> $ Licitacao         <fct> DISPENSA, DISPENSA, DISPENSA, DISPENSA, PREGÃO…
#> $ Data              <date> 2014-01-01, 2014-01-01, 2014-01-01, 2014-01-0…
#> $ TipoAto           <fct> PAGAMENTO DE RPP, PAGAMENTO DE RPP, PAGAMENTO …
#> $ Valor             <dbl> 416.00, 665.00, 8000.00, 4014.18, 10000.32, 32…
#> $ Exercicio         <dbl> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013…
#> $ EmpenhoExercicio  <fct> 84/2013, 13/2013, 269/2013, 211/2013, 237/2013…
#> $ Liquidacao        <dbl> 3, 9, 1, 3, 3, 1, 1, 8, 1, 2, 8, 1, 56, 4, 37,…
#> $ Pagamento         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ Historico         <fct> "LOCACÃO DE SISTEMA DE AUTOMAÇÃO DE BILHETERIA…
#> $ Banco             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ NomeBanco         <fct> BANCO DO BRASIL S/A, BANCO DO BRASIL S/A, BANC…
#> $ Agencia           <dbl> 2234, 2234, 22349, 2234, 2234, 2234, 2234, 223…
#> $ ContaCorrente     <fct> 10146, 10146, 298332X, 10146, 10146, 10146, 10…
#> $ NomeContaCorrente <fct> BCO BRASIL C/C 1.014-6, BCO BRASIL C/C 1.014-6…
#> $ ASPS              <fct> N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N…
#> $ MDE               <fct> N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N…
#> $ ExercicioContrato <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, 2013, NA, …
#> $ NumeroContrato    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, 16, NA, NA…
#> $ ObjetoContrato    <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, "CONTRATAÇ…
```

Classes das colunas

``` r
df_despesas %>% map_chr(class)%>%table
#> .
#>    Date  factor numeric 
#>       1      31      13
```

Cardinalidade

``` r
df_despesas %>% get_card_fact
#> # A tibble: 31 x 2
#>    colname           unicos
#>    <chr>              <int>
#>  1 Historico          99057
#>  2 Processo           60886
#>  3 EmpenhoExercicio   20674
#>  4 ObjetoContrato     15396
#>  5 Credor             14315
#>  6 NomeCredor         14083
#>  7 NomeAcao             808
#>  8 ContaCorrente        588
#>  9 NomeContaCorrente    504
#> 10 NomeSubElemento      463
#> # … with 21 more rows
```

Quais colunas sao numéricas?

``` r
df_despesas %>% select_if(is.numeric) %>% colnames
#>  [1] "Orgao"             "UO"                "UG"               
#>  [4] "FonteRecursos"     "Acao"              "Valor"            
#>  [7] "Exercicio"         "Liquidacao"        "Pagamento"        
#> [10] "Banco"             "Agencia"           "ExercicioContrato"
#> [13] "NumeroContrato"
```

Sumariza-as

``` r
df_despesas %>% select_if(is.numeric) %>% summary
#>      Orgao            UO             UG       FonteRecursos  
#>  Min.   :1100   Min.   :1101   Min.   :1100   Min.   :100.0  
#>  1st Qu.:1606   1st Qu.:1606   1st Qu.:1606   1st Qu.:100.0  
#>  Median :1851   Median :1851   Median :1851   Median :107.0  
#>  Mean   :2161   Mean   :2161   Mean   :2161   Mean   :134.9  
#>  3rd Qu.:2400   3rd Qu.:2401   3rd Qu.:2400   3rd Qu.:181.0  
#>  Max.   :5100   Max.   :5101   Max.   :5100   Max.   :468.0  
#>                                                              
#>       Acao          Valor             Exercicio      Liquidacao    
#>  Min.   :1001   Min.   :0.000e+00   Min.   :2008   Min.   :   1.0  
#>  1st Qu.:2133   1st Qu.:9.880e+02   1st Qu.:2015   1st Qu.:   1.0  
#>  Median :2504   Median :5.077e+03   Median :2016   Median :   4.0  
#>  Mean   :2846   Mean   :1.953e+05   Mean   :2016   Mean   :  28.9  
#>  3rd Qu.:4011   3rd Qu.:2.414e+04   3rd Qu.:2017   3rd Qu.:  14.0  
#>  Max.   :6003   Max.   :1.206e+09   Max.   :2018   Max.   :1718.0  
#>                                                    NA's   :401498  
#>    Pagamento           Banco           Agencia      ExercicioContrato
#>  Min.   : 1        Min.   :  1.00   Min.   :  127   Min.   :2007     
#>  1st Qu.: 1        1st Qu.:  1.00   1st Qu.: 2234   1st Qu.:2014     
#>  Median : 1        Median :  1.00   Median : 2234   Median :2015     
#>  Mean   : 1        Mean   : 10.94   Mean   : 2836   Mean   :2016     
#>  3rd Qu.: 1        3rd Qu.:  1.00   3rd Qu.: 2234   3rd Qu.:2017     
#>  Max.   :10        Max.   :341.00   Max.   :22349   Max.   :2018     
#>  NA's   :1275498                                    NA's   :2029409  
#>  NumeroContrato   
#>  Min.   :     1   
#>  1st Qu.:     8   
#>  Median :    29   
#>  Mean   : 18264   
#>  3rd Qu.:   157   
#>  Max.   :998140   
#>  NA's   :2029409
```

Nota: algumas colunas numéricas tem NAs excessivos

``` r
df_despesas %>% pct_na %>% filter(pct_na>.1)
#> # A tibble: 6 x 5
#>   col               class     lines  cnt_na pct_na
#>   <chr>             <chr>     <int>   <int>  <dbl>
#> 1 ObjetoContrato    factor  2252570 2029409  0.9  
#> 2 Historico         factor  2252570  824303  0.37 
#> 3 ExercicioContrato numeric 2252570 2029409  0.9  
#> 4 NumeroContrato    numeric 2252570 2029409  0.9  
#> 5 Pagamento         numeric 2252570 1275498  0.570
#> 6 Liquidacao        numeric 2252570  401498  0.18
```

Despesas Mensais
================

Nota: preparo dos dados [aqui](preparo_mensal.html)

``` r
library(tidyverse)
library(fs)
```

Le arquivo

``` r
fname_despesas <- "./data/MP_EMPENHOS_ALL.rds"
df_despesas <- read_rds(fname_despesas)
```

``` r
dim(df_despesas)
```

``` r
object.size(df_despesas)%>%format(units="Mb")
```

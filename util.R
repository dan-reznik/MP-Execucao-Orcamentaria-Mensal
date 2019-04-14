dir_slash_fname <- function(d,f) d%s+%"/"%s+%f

get_fname_utf8 <- function(fname) fname %>%
  str_extract("^.*(?=\\.TXT)")%>%
  str_c("_utf8.TXT")
repl_ext <- function(fname,ext) fname %>%
  str_replace("(?<=\\.)[:alpha:]+$",ext)

get_card <- function(df) df %>%
  summarize_if(is.character,n_distinct) %>%
  {
    tibble(colname=colnames(.),unicos=unlist(.)) %>%
      arrange(desc(unicos))
  }

get_card_fact <- function(df) df %>%
  summarize_if(is.factor,~length(levels(.))) %>%
  {
    tibble(colname=colnames(.),unicos=unlist(.)) %>%
      arrange(desc(unicos))
  }

pct_na <- function(df) df %>%
{tibble(col=colnames(.),
        class=map_chr(.,class),
        lines=nrow(.),
        cnt_na=map_int(.,~sum(is.na(.))),
        pct_na=round(cnt_na/lines,2))} %>%
  arrange(class,desc(cnt_na))

cnt_seps <- function(s_vec,sep) {
  df <- tibble(line=s_vec,
               cnt=str_count(line,fixed(sep)))
  df %>% count(cnt,sort=T)
}

cnt_seps_df <- function(fname,locale,sep,norm_cnt,max_rows=10)
  tibble(sep_cnt=read_lines(fname,locale=locale) %>%
           str_count(fixed(sep))) %>%
  mutate(row=row_number()) %>%
  filter(sep_cnt!=norm_cnt) %>%
  select(row,sep_cnt) %>%
  arrange(sep_cnt) %>%
  group_by(sep_cnt) %>%
  summarize(n=n(),row_list=str_c(head(row,max_rows),collapse=",")) %>%
  mutate(row_list=if_else(n>max_rows,str_c(row_list,",..."),row_list))

trunc_cols <- function(fname,locale,max_cols,sep,n_max=-1L)
  read_lines(fname,locale=locale,n_max=n_max)%>%
  str_split(fixed(sep)) %>%
  map(str_squish)%>%
  map(head,max_cols) %>% # trunc
  map_chr(str_c,collapse=sep)

trunc_file <- function(fname,locale,max_cols,sep) {
  fname_utf8 <- get_fname_utf8(fname) 
  trunc_cols(fname,locale,max_cols,sep)%>%
    write_lines(fname_utf8)
  fname_utf8
}

trunc_to_rds <- function(fname_txt,locale,max_cols,sep) {
  # talvez nao seja necessario: tentar cols_only() em read_delim()
  fname_utf8 <- trunc_file(fname_txt,locale,max_cols,sep)
  locale$encoding <- "UTF-8"
  df_fname <- read_delim(fname_utf8
                         ,locale=locale
                         ,delim=fixed(sep)
                         ,trim_ws = T
                         ,quote="'"
                         #,col_types = cols(.default = "c")
                         ,col_types = cols(
                           # they are guesses as lgl because of
                           # multiple NAs in beginning
                           Liquidacao=col_double(),
                           Pagamento=col_double(),
                           Valor=col_double(),
                           Data=col_date(),
                           Credor=col_character(),
                           Processo=col_character(),
                           Elemento=col_character(),
                           SubFuncao=col_character(),
                           ContaCorrente=col_character())
  )
  fname_rds <- fname_txt%>%repl_ext("rds") 
  df_fname %>% write_rds(fname_rds,compress="bz")
}

unzip_trunc_rds <- function(fname,fname_zip,data_dir,locale,max_cols) {
  #print(.x)
  unzip(fname_zip,files=fname,exdir=data_dir)
  trunc_to_rds(dir_slash_fname(data_dir,fname),locale,max_cols)
  f_txt <- dir_slash_fname(data_dir,fname)
  f_utf8 <- get_fname_utf8(f_txt)
  file_delete(f_txt)
  file_delete(f_utf8)
  fname
} 
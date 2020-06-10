install.packages("rvest")
library(rvest)

# COMO FAZER A RASPAGEM DE DADOS DA B3 - DERIVATIVOS E AÇÕES


#DERIVATIVOS 
# Insira a url alvo - ajuste derivativos - Pregão

url <- "http://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-ajustes-do-pregao-ptBR.asp"

# Ler o código da HTML indicada

site <- read_html(url)
site

# Escolher qual elemento do endereço HTML que usar para fazer o ajuste

info_HTML_ajuste <- html_nodes(site,"table") # Essa função procurar as estruturas do código HTML
info_HTML_ajuste

# Converter a HTML para texto

HTML_ajuste <- html_text(info_HTML_ajuste) # Trasnforma aquele código em texto
HTML_ajuste

#Visualização do texto

head(HTML_ajuste,20)

#Como melhorar a visualização do texto e tabela
head(info_HTML_ajuste)

# A melhor forma de trasnformar aquele código em tabela é transformando-o em lista da seguinte forma:

lista_tabela <- site %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)

# vISUALIZAÇÃO

str(lista_tabela)

head(lista_tabela[[1]], 10)

View(lista_tabela[[1]])


#Atribuição

AJUSTE <- (lista_tabela[1])


# AÇÕES - ITAÚSA

# Insira a url alvo - Balanço

url1 <- "http://bvmf.bmfbovespa.com.br/pt-br/mercados/acoes/empresas/ExecutaAcaoConsultaInfoEmp.asp?CodCVM=7617&ViewDoc=1&AnoDoc=2019&VersaoDoc=1&NumSeqDoc=82855#a"


# Ler o código da HTML indicada

site1 <- read_html(url1)

# Escolher qual elemento do endereço HTML que usar para fazer o ajuste

info_balanco <- html_nodes(site1, "table")

# Converter a HTML para texto

html_texto <- html_text(info_balanco)


# VISUALIZAÇÃO

head(html_texto,20)

#COMO MELHORAR A VISUALIZAÇÃO
head(info_balanco)

# A melhor forma de trasnformar aquele código em tabela é transformando-o em lista da seguinte forma:

lista_tabela2 <- site1 %>% 
  html_nodes("table") %>% 
  .[3:5] %>%
  html_table(fill = TRUE)

# VISUZALIZAÇÃO

str(lista_tabela2)

head(lista_tabela2[[1]],10)
head(lista_tabela2[[2]],10)
head(lista_tabela2[[3]],10)

View(lista_tabela2[[1]])
View(lista_tabela2[[2]])
View(lista_tabela2[[3]])



BP <- (lista_tabela2[[1]])
DR <- (lista_tabela2[[2]])
DFC <-(lista_tabela2[[3]])










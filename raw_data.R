### Bibliotecas ####

library(tidyverse)

### Download de dados ####

## Proficiencia do Sistema de Avaliacao de Rendimento Escolar do Estado de Sao Paulo (SARESP) por escola ------

ano_saresp <- 'https://dados.educacao.sp.gov.br/sites/default/files/SARESP_escolas_2011.csv'

for (i in as.character(11:18)) {
  url_pagina <- str_replace(ano_saresp,"11",i)
  nome <- paste("SARESP",i,sep = "_")
  assign(nome, read_csv2(url_pagina))
}



## Histirico de Diretores, Vice Diretores e Professores Coordenadores por Unidade Escolar ------

## Coordenadores

dados_coordenadores <- 'https://dados.educacao.sp.gov.br/sites/default/files/PROFESSOR_COORDENADOR.csv'
coordernador <- read.csv2(dados_coordenador)

## Diretores

dados_diretores <- 'https://dados.educacao.sp.gov.br/sites/default/files/DIRETORES%20DE%20ESCOLA.csv'
diretores <- read.csv2(dados_diretores)

## Vice Diretores
dados_vice_diretor <- 'https://dados.educacao.sp.gov.br/sites/default/files/VICE_DIRETOR.csv'
vice_diretores <-  read.csv2(dados_vice_diretor)

## Ausencias por servidor ----

## Ausencia 1118

dados_ausencia_1118 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_AUSENCIAS_1118.csv'
ausencia_1118 <-  read.csv2(dados_ausencia_1118)

## Ausencia 1119

dados_ausencia_1119 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_AUSENCIAS_1119.csv'
ausencia_1119 <-  read.csv2(dados_ausencia_1119)


## Ausencia 0419

dados_ausencia_0419 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_AUSENCIAS_0419.csv'
ausencia_0419 <-  read.csv2(dados_ausencia_0419)

## Indice de Desenvolvimento da Educacao do Estado de Sao Paulo (IDESP) por Escola -----

## IDESP Escola - 2018

dados_IDESP_ESCOLA_2018 <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP%20por%20Escola%20-%202018.csv'
IDESP_ESCOLA_2018 <- read.csv2(dados_IDESP_ESCOLA_2018)

## IDESP AI - 2007--2018

DADOS_IDESP_AI <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP_Escolas_2007_2018_AI.CSV'
IDESP_AI <- read_csv(DADOS_IDESP_AI)

## IDESP AF - 2007--2018

DADOS_IDESP_AF <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP_Escolas_2007_2018_AF.CSV'
IDESP_AF <- read_csv(DADOS_IDESP_AF)


## IDESP EM - 2007--2018

DADOS_IDESP_EM <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP_Escolas_2007_2018_EM.CSV'
IDESP_EM <- read_csv(DADOS_IDESP_EM)


## Disciplinas sem docentes associados ----

DADOS_ATR_TOTAL<- 'https://dados.educacao.sp.gov.br/sites/default/files/total.csv'
ATR_TOTAL <- read_csv(DADOS_ATR_TOTAL)

DADOS_ATR<- 'https://dados.educacao.sp.gov.br/sites/default/files/atribuicao.csv'
ATR <- read_csv(DADOS_ATR_TOTAL)

## Servidores ativos por Unidade ----

DADOS_SA_1118 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_SERVIDORES_ATIVOS_1118.csv'

DADOS_SA_1119 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_SERVIDORES_ATIVOS_1119.csv'

DADOS_SA_0419 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_SERVIDORES_ATIVOS_0419.csv'


SA_1118 <- read_csv2(DADOS_SA_1118)

SA_1119 <- read_csv2(DADOS_SA_1119)

SA_0419 <- read_csv2(DADOS_SA_0419)

## Carga Horaria por docente----

DADOS_HD_1119 <-'https://dados.educacao.sp.gov.br/sites/default/files/BASE_CARGA_HOR_SALA_AULA_1119.csv'

DADOS_HD_0419 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_CARGA_HOR_SALA_AULA_0419.csv'

HD_1119 <- read_csv2(DADOS_HD_1119)

HD_0419 <- read_csv2(DADOS_HD_0419)

## Hist?rico de matr?culas por turma ----

DADOS_ALUNO_ESCOLA <- 'https://dados.educacao.sp.gov.br/sites/default/files/10_Escolas_Classes_Qtde_Alunos.csv'

ALUNO_ESCOLA <- read.csv2(DADOS_ALUNO_ESCOLA)

## Enderecos de escolas ----

DADOS_END20 <-'https://dados.educacao.sp.gov.br/sites/default/files/11_Escolas_Coordenadas.csv'

DADOS_END18 <- 'https://dados.educacao.sp.gov.br/sites/default/files/escolas_enderecos_0.csv'

END20 <- read_csv2(DADOS_END20)

END18 <- read_csv2(DADOS_END18)

## Instalacoes f?sicas por Unidade Escolar ----

DADOS_IF_ESCOLA <-'https://dados.educacao.sp.gov.br/sites/default/files/06_Escolas_Dependencias.csv'

IF_ESCOLA <- read_csv2(DADOS_IF_ESCOLA)

## Cluster Metodo de Melhoria de Resultados----

DADOS_MMR <- 'https://dados.educacao.sp.gov.br/sites/default/files/12_MMR_Clusters.csv'
MMR <- read_csv2(DADOS_MMR)
### Bibliotecas ####

library(tidyverse)

### Download de dados ####

## Formacao por Servidor ----

DADOS_FORMACAO1118 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_FORMACAO_1118.csv'

FORMACAO1118 <- read_csv2(DADOS_FORMACAO1118, locale = readr::locale(encoding = "ASCII"))

AUXILIAR <- FORMACAO1118%>%
  select(id_interno,CIE_ESCOLA,UA_EXERC)

    ### FORMACAO1118_PERC - Descreve o percentual de membro do QM por nível de formação

FORMACAO1118_PERC <- FORMACAO1118%>%
  group_by(CIE_ESCOLA)%>%
  filter(CIE_ESCOLA != 0, QUADRO_E == 'QM')%>%
  select(CIE_ESCOLA, NOME_UA_EXERC,QUADRO_E, NMCARGO_E,FORMACAO, id_interno)%>%
  mutate(TOTAL_QM = n())

FORMACAO1118_PERC <- FORMACAO1118_PERC%>%
  group_by(CIE_ESCOLA,FORMACAO)%>%
  mutate(perc_form = n()/TOTAL_QM)%>%
  select(CIE_ESCOLA, FORMACAO,perc_form)%>%
  distinct()

FORMACAO1118_PERC <- reshape2::dcast(FORMACAO1118_PERC,CIE_ESCOLA  ~ FORMACAO, value.var="perc_form") 

FORMACAO1118_PERC[is.na(FORMACAO1118_PERC)] <- 0

FORMACAO1118_PERC <- FORMACAO1118_PERC%>%
  rename(EM = "ENSINO MLdDIO",
         BA_TE ="BACHARELADO/TECNLRLOGO",
         BA_TE_DO ="BACHARELADO/TECNLRLOGO + DOUTORADO",
         BA_TE_ES ="BACHARELADO/TECNLRLOGO + ESPECIALIZAL`LDO",
         BA_TE_MA ="BACHARELADO/TECNLRLOGO + MESTRADO",
         BA_TE_MA_DO = "BACHARELADO/TECNLRLOGO + MESTRADO + DOUTORADO",
         ES = "ESPECIALIZAL`LDO",
         ES_MA = "ESPECIALIZAL`LDO+MESTRADO",
         LI = "LICENCIATURA",
         LI_BA_TE = "LICENCIATURA + BACHARELADO/TECNLRLOGO",
         LI_BA_TE_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO + DOUTORADO",
         LI_BA_TE_ES = "LICENCIATURA + BACHARELADO/TECNLRLOGO + ESPECIALIZAL`LDO",
         LI_BA_TE_ES_MA = "LICENCIATURA + BACHARELADO/TECNLRLOGO + ESPECIALIZAL`LDO + MESTRADO",
         LI_BA_TE_MA_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO + MESTRADO + DOUTORADO",
         LI_BA_TE_ES_MA_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO+ESPECIALIZAL`LDO + MESTRADO + DOUTORADO",
         LI_BA_TE_ES_DO = "LICENCIATURA + BACHARELADO/TECNLRLOGO+ESPECIALIZAL`LDO+DOUTORADO",
         LI_DO = "LICENCIATURA + DOUTORADO",
         LI_ES = "LICENCIATURA + ESPECIALIZAL`LDO",
         LI_ES_MA = "LICENCIATURA + ESPECIALIZAL`LDO + MESTRADO",
         LI_ES_MA_DO = "LICENCIATURA + ESPECIALIZAL`LDO + MESTRADO + DOUTORADO",
         LI_ES_DO = "LICENCIATURA + ESPECIALIZAL`LDO+DOUTORADO",
         LI_MA = "LICENCIATURA + MESTRADO",
         LI_MA_DO = "LICENCIATURA + MESTRADO+DOUTORADO",
         MA = "MESTRADO",
         LI_BA_TE_MA = "LICENCIATURA + BACHARELADO/TECNLRLOGO + MESTRADO")%>%
  mutate(PERC_DO = LI_MA_DO + LI_ES_DO + LI_ES_MA_DO + LI_DO + LI_BA_TE_ES_DO + LI_BA_TE_ES_MA_DO + LI_BA_TE_MA_DO + LI_BA_TE_DO + BA_TE_MA_DO + BA_TE_DO,
         PERC_MA = LI_BA_TE_MA +LI_MA + LI_ES_MA + LI_BA_TE_ES_MA + ES_MA + BA_TE_MA + MA,
         PERC_ES = LI_ES + LI_BA_TE_ES + ES,
         PERC_LI_BA = LI_BA_TE + LI +BA_TE)%>%
  select(CIE_ESCOLA,PERC_DO,PERC_MA,PERC_ES,PERC_LI_BA)

write_csv2(FORMACAO1118_PERC, "FORMACAO1118_PERC.csv")

  ### FORMACAO1118_DIR - Nível de formação do diretor da unidade de ensino

source("function_str.R")

FORMACAO1118_DIR <- FORMACAO1118%>%
  filter(CIE_ESCOLA != 0, CARGO_E == 6200)%>%
  mutate(DIR_POS = if_else(right(FORMACAO,8)=="MESTRADO"|right(FORMACAO,9)=="DOUTORADO",1,0))%>%
  select(CIE_ESCOLA, FORMACAO,DIR_POS)

write_csv2(FORMACAO1118_DIR, "FORMACAO1118_DIR.csv")
## Ausencias por servidor ----

## Ausencia 1118

dados_ausencia_1118 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_AUSENCIAS_1118.csv'

ausencia_1118 <-  read.csv2(dados_ausencia_1118)

    ### ausencia_1118_escola

ausencia_1118_escola <- ausencia_1118%>%
  filter(left(QUADRO_E,2)=="QM")%>%
  select(UA_E,QUADRO_E,CARGO_E,TT_DIAS_FALTA_INJUST,id_interno)%>%
  left_join(AUXILIAR, by ="id_interno")%>%
  group_by(CIE_ESCOLA)%>%
  mutate(TOTAL_FALTAS = sum(TT_DIAS_FALTA_INJUST, na.rm = T))%>%
  select(CIE_ESCOLA,TOTAL_FALTAS)%>%
  distinct()


## Servidores ativos por Unidade ----

DADOS_SA_1118 <- 'https://dados.educacao.sp.gov.br/sites/default/files/BASE_SERVIDORES_ATIVOS_1118.csv'

SA_1118 <- read_csv2(DADOS_SA_1118)

###SA_1118

SA_1118_ESCOLA<- SA_1118%>%
  filter(QUADRO_E == "QM")%>%
  mutate(DIAS_EXERC = (difftime("2018-12-31",as.Date(DATA_INICIO_EXERCICIO_E,format='%d/%m/%Y'),units = "weeks"))/52.25)%>%
  left_join(AUXILIAR, by ="id_interno")%>%
  group_by(CIE_ESCOLA)%>%
    mutate(MED_DIAS_EXERC = mean(DIAS_EXERC, na.rm = T),
           MED_IDADE = mean(IDADE, na.rm = T))%>%
    select(CIE_ESCOLA,MED_DIAS_EXERC,MED_IDADE)%>%
    distinct()
  
  
  

## Instalacoes fisicas por Unidade Escolar ----

DADOS_IF_ESCOLA <-'https://dados.educacao.sp.gov.br/sites/default/files/06_Escolas_Dependencias.csv'

IF_ESCOLA <- read_csv2(DADOS_IF_ESCOLA)

## Enderecos de escolas ----

DADOS_END18 <- 'https://dados.educacao.sp.gov.br/sites/default/files/escolas_enderecos_0.csv'

END18 <- read_csv(DADOS_END18)

## Histirico de matriculas por turma ----

DADOS_ALUNO_ESCOLA <- 'https://dados.educacao.sp.gov.br/sites/default/files/10_Escolas_Classes_Qtde_Alunos.csv'

ALUNO_ESCOLA <- read_csv2(DADOS_ALUNO_ESCOLA,locale = readr::locale(encoding = "ASCII"))

    ### ALUNO_ESCOLA - Número de alunos dos grupos de ensino (AI,AF,EM)

ALUNO_ESCOLA <- ALUNO_ESCOLA%>%
  filter(ANO == 2018)

ALUNO_ESCOLA_GERAL <- ALUNO_ESCOLA%>%
  group_by(COD_ESC)%>%
  mutate(MED_ALUNO_TURMA = mean(QTDE_ALUNOS,na.rm = T))%>%
  select(COD_ESC,MED_ALUNO_TURMA)%>%
  distinct()

ALUNO_ESCOLA_AI <- ALUNO_ESCOLA%>%
  group_by(COD_ESC)%>%
  filter(GRAU %in% c(14,30), SERIE %in% c(1,2,3,4,5) )%>%
  mutate(MED_ALUNO_TURMA_AI = mean(QTDE_ALUNOS,na.rm = T))%>%
  select(COD_ESC,MED_ALUNO_TURMA_AI)%>%
  distinct()

ALUNO_ESCOLA_AF <- ALUNO_ESCOLA%>%
  group_by(COD_ESC)%>%
  filter(GRAU == 14, SERIE %in% c(6,7,8,9)|GRAU == 1)%>%
  mutate(MED_ALUNO_TURMA_AF = mean(QTDE_ALUNOS,na.rm = T))%>%
  select(COD_ESC,MED_ALUNO_TURMA_AF)%>%
  distinct()

ALUNO_ESCOLA_EM <- ALUNO_ESCOLA%>%
  group_by(COD_ESC)%>%
  filter(GRAU == 2)%>%
  mutate(MED_ALUNO_TURMA_em = mean(QTDE_ALUNOS,na.rm = T))%>%
  select(COD_ESC,MED_ALUNO_TURMA_em)%>%
  distinct()

ALUNO_ESCOLA_FINAL <- ALUNO_ESCOLA_GERAL%>%
  left_join(ALUNO_ESCOLA_AI, by = "COD_ESC")%>%
  left_join(ALUNO_ESCOLA_AF, by = "COD_ESC")%>%
  left_join(ALUNO_ESCOLA_EM, by = "COD_ESC")

rm(list = c("ALUNO_ESCOLA_AI","ALUNO_ESCOLA_AF","ALUNO_ESCOLA_EM","ALUNO_ESCOLA_GERAL"))

write_csv2(ALUNO_ESCOLA_FINAL, "ALUNO_ESCOLA_FINAL.csv")
## Indice de Desenvolvimento da Educacao do Estado de Sao Paulo (IDESP) por Escola -----

## IDESP Escola - 2018

DADOS_IDESP_ESCOLA_2018 <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP%20por%20Escola%20-%202018.csv'

IDESP_ESCOLA_2018 <- read.csv2(DADOS_IDESP_ESCOLA_2018)


## IDESP AI - 2007--2018

DADOS_IDESP_AI <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP_Escolas_2007_2018_AI.CSV'

IDESP_AI <- read_csv(DADOS_IDESP_AI)

## IDESP AF - 2007--2018

DADOS_IDESP_AF <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP_Escolas_2007_2018_AF.CSV'

IDESP_AF <- read_csv(DADOS_IDESP_AF)


## IDESP EM - 2007--2018----

DADOS_IDESP_EM <- 'https://dados.educacao.sp.gov.br/sites/default/files/IDESP_Escolas_2007_2018_EM.CSV'

IDESP_EM <- read_csv(DADOS_IDESP_EM)

## Histirico de Diretores, Vice Diretores e Professores Coordenadores por Unidade Escolar ------

## Coordenadores

DADOS_COORD <- 'https://dados.educacao.sp.gov.br/sites/default/files/PROFESSOR_COORDENADOR.csv'

COORD <- read_csv2(DADOS_COORD)

## Diretores

DADOS_DIRET <- 'https://dados.educacao.sp.gov.br/sites/default/files/DIRETORES%20DE%20ESCOLA.csv'

DIRET <- read_csv2(DADOS_DIRET)

## Vice Diretores
DADOS_VICE_DIRET <- 'https://dados.educacao.sp.gov.br/sites/default/files/VICE_DIRETOR.csv'

VICE_DIRET <-  read_csv2(DADOS_VICE_DIRET)

## Proficiencia do Sistema de Avaliacao de Rendimento Escolar do Estado de Sao Paulo (SARESP) por escola ------

DADOS_SARESP2018 <- 'https://dados.educacao.sp.gov.br/sites/default/files/SARESP_escolas_2018.csv'

SARESP2018 <- read_csv2(DADOS_SARESP2018, locale = readr::locale(encoding = "latin1"))


# IndicadoresB3

O IndicadoresB3 permite aos usuários explorar demonstrações financeiras trimestrais e anuais das empresas listadas na B3 (Brasil, Bolsa, Balcão), com visualizações dinâmicas de indicadores financeiros críticos.

## Sobre Este Projeto

Utilizando dados abertos da Comissão de Valores Mobiliários (CVM), o IndicadoresB3 foi desenvolvido para demonstrar uma aplicação prática de técnicas de ciência de dados à análise financeira. Ele apresenta:

- **Habilidades Técnicas**: Programação em R, ciência de dados aplicada aos dados financeiros, visualização de dados
- **Conhecimento Financeiro**: Análise de demonstrações contábeis, indicadores financeiros, avaliação de desempenho empresarial
- **Conhecimento de Negócios**: Interpretação contextual de métricas financeiras por setor e segmento

### Processo de Desenvolvimento

1. **Coleta de Dados**: Importação automática de demonstrações financeiras do portal de dados abertos da CVM
2. **Processamento e Limpeza**: Transformação e normalização dos dados contábeis para análise
3. **Modelagem**: Cálculo de indicadores financeiros relevantes e métricas comparativas
4. **Desenvolvimento do Dashboard**: Criação de visualizações interativas usando Shiny, bs4Dash e Echarts4r
5. **Documentação**: Fornecimento de explicações metodológicas e guias de usuário

### Extensões Potenciais

- Análises preditivas de desempenho financeiro
- Comparação de indicadores com médias setoriais
- Alertas para variações significativas em métricas-chave
- Funcionalidades de exportação de relatórios customizados

## Estrutura de Arquivos

O projeto está organizado nos seguintes componentes:

## Scripts de Importação e Processamento

- importing.R: Realiza a importação automática dos dados abertos da CVM
- cleaning_fca.R: Processa e limpa os dados cadastrais das empresas
- cleaning_dfp.R: Processa os dados das demonstrações financeiras padronizadas anuais
- cleaning_itr.R: Processa os dados das informações trimestrais
- transforming_dre.R: Transforma os dados da DRE para análise
- transforming_indicadores.R: Calcula indicadores financeiros a partir dos dados processados
- visualizing.R: Teste de componentes de visualização de dados do dashboard, como gráficos e tabelas

## Aplicação

- app.R: Contém a interface e a lógica do dashboard Shiny


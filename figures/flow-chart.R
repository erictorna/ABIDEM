library(DiagrammeR)

load('data/01-diabi_population.RData')
load('build.data/K.RData')
source('R/global.R')
tab = table(maria$itb_cat)

label_node = function(.graph, text_, ...){
  .graph %>%
    add_node(label = text_, 
             type = "label",
             node_aes = node_aes(fillcolor = 'white', fontcolor = 'black', col = 'black', width = 3.5, height = 0.7, shape = 'rectangle', ...))
}
label_node2 = function(.graph, text_, ...){
  .graph %>%
    add_node(label = text_, 
             type = "label",
             node_aes = node_aes(fillcolor = 'white', fontcolor = 'black', col = 'black', width = 1.25, shape = 'rectangle', ...))
}
join_node = function(.graph, x, y){
  .graph %>%
    add_node(type = "join", label='', node_aes = node_aes(width = 0, x = x, y = y, penwidth = 0.1, color = 'black'))
}

add_arrow = function(.graph, from, to, ...){
  .graph %>%
    add_edge(from = from, to = to, edge_aes = edge_aes(color = 'black', ...))
}

get_text = function(text_, n_) sprintf("%s\n(n = %d)", text_, n_)
TEXT = names(FLOWCHART)
WIDTH = 2.7
WIDTH2 = 1.5
ABI_LABELS_H = 5
VLINE = -1.25
graph = create_graph() %>%
  label_node(sprintf("%d \npersons from 35 to 85 years old\nwith ABI measurement", FLOWCHART$`Aged 35-85`), 
             x = VLINE, y = 10) %>%
  label_node(sprintf("%d \ncohort members", FLOWCHART$`ABI < 3`), 
             x = VLINE, y = 8) %>%
  join_node(x = VLINE, y = 9) %>%
  label_node(sprintf("data from %d persons excluded\\l- %d had type I diabetes\\l- %d had symptomatic peripheral arterial disease\\l- %d had ankle brachial index ≥3\\l", 
                     FLOWCHART$`Aged 35-85` - nrow(maria),
                     FLOWCHART$`Aged 35-85` - FLOWCHART$`DM2 or without DM with ABI`,
                     FLOWCHART$`DM2 or without DM with ABI` - FLOWCHART$`Without PAD symptomatic`,
                     FLOWCHART$`Without PAD symptomatic` - FLOWCHART$`ABI < 3`),
             x = VLINE+2.5, y = 9, style = 'dashed') %>%
  add_arrow(1,2) %>%
  add_arrow(3, 4, arrowhead = 0, style = 'dashed')
                                                       

# graph %>%
#   render_graph()

library(DiagrammeRsvg)
export_graph(graph, file_name = 'www/flow-chart.svg', file_type = 'SVG')
export_graph(graph, file_name = 'www/flow-chart.pdf', file_type = 'PDF')

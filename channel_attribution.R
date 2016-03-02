rm(list=ls())

# libraries ---------------------------------------------------------------

if(!exists("package", mode="function")) source("util.R")

list.of.packages <- c('ggplot2', 'ChannelAttribution', 'reshape2', 'ggthemes', 'markovchain', 'htmlTable', 'plotly', 'RColorBrewer')

package(list.of.packages, update=FALSE, verbose=TRUE, install=FALSE, quiet=FALSE)

lapply(list.of.packages, library, character.only = TRUE)

rm(list=c('list.of.packages', 'package'))


# Facilitar la ruta al archivo con las rutas de conversión con el formato:
# EAM.Model Conversiones Valor.de.las.conversiones
# SEMMarca > SEMMarca        1.182              1.599,77 €
# Directo > Directo          844              2.525,61 €
# ...
# Los nombres de las variables no importa.

filename <- 'data/conversionpaths.csv'

mcf.df <- read.csv2(filename, sep = ',', header = T, skip = 6, nrows = length(readLines(filename)) - 8, stringsAsFactors = F, encoding = 'UTF-8')
rm(filename)

names(mcf.df) <- c('path', 'total_conversions','total_conversion_value')


# cleaning format ---------------------------------------------------------


mcf.df$total_conversions <- gsub("\\.", "",mcf.df$total_conversions)
mcf.df$total_conversions <- as.numeric(mcf.df$total_conversions)
mcf.df$total_conversion_value <- gsub("\\.", "",mcf.df$total_conversion_value)
mcf.df[3] <- apply(mcf.df, 1, function(x) as.numeric(gsub(",[0-9]{2}[[:space:]]\\€", "",as.character(x[3]))) )

mcf.df <- mcf.df[!(is.na(mcf.df$path) | mcf.df$path==""), ]


H <- heuristic_models(mcf.df,"path","total_conversions",var_value="total_conversion_value")
M <- markov_model(mcf.df, "path", "total_conversions", var_value="total_conversion_value", order = 1, out_more = F)
R <- merge(H,M, by='channel_name')
R1 <- R[,(colnames(R)%in%c("channel_name","first_touch_conversions","last_touch_conversions" ,"linear_touch_conversions","total_conversion"))] 

colnames(R1)=c('channel_name','first_touch','last_touch','linear_touch','eam_model') 

rm(list=c('H', 'M'))


# plot --------------------------------------------------------------------

R1 <- melt(R1,id="channel_name") 

R1.plot <- ggplot(R1, aes(channel_name, value, fill = variable)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("grey80", "grey60", "grey70","#E1313A")) +
  labs(y = "Conversions") +
  labs(x = "Marketing Channels") +
  scale_color_fivethirtyeight("cyl") +
  theme_ibex() + 
  theme(axis.text=element_text(size=9), axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(
    aes(
      y = value,
      ymax = value,
      label = round(value,0)
    ),
    position = position_dodge(width = 0.9),
    vjust = -.5,
    size = 2.5
  ) +
  labs(title='Total Conversions') +
  theme(plot.title = element_text(size=15,lineheight=.8,hjust = 0.0,  vjust=1,family="Arial"))


print(R1.plot)


R2 <- R[,(colnames(R) %in% c("channel_name","first_touch_value","last_touch_value","linear_touch_value","total_conversion_value"))] 

colnames(R2) <- c('channel_name','first_touch','last_touch','linear_touch','eam_model') 

# R2.value <- R2[,c('channel_name', 'eam_model')]  
# rm(R2.value)

R2 <- melt(R2,id="channel_name") 

R2.plot <-ggplot(R2, aes(channel_name, value, fill = variable)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("grey80", "grey60", "grey70","#E1313A")) +
  labs(y = "Conversion Value") +
  labs(x = "Marketing Channels") +
  scale_color_fivethirtyeight("cyl") +
  theme_ibex() + 
  theme(axis.text=element_text(size=9), axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(
    aes(
      y = value,
      ymax = value,
      label = round(value,0)
    ),
    position = position_dodge(width = 0.9),
    vjust = -.5,
    size = 2.5
  ) +
  labs(title='Conversion Value') +
  theme(plot.title = element_text(size=15,lineheight=.8,hjust = 0.0,  vjust=1,family="Arial"))

print(R2.plot)

rm(list=c('R1', 'R2'))

# channel transition matrix -----------------------------------------------

M.ext <- markov_model(mcf.df, "path", "total_conversions", var_value="total_conversion_value", order = 1, out_more = T)

transition.matrix <- dcast(M.ext$transition_matrix, channel_from ~ channel_to) 

transition.matrix[is.na(transition.matrix)] <- 0

heatmap.mt <- transition.matrix

transition.matrix <- as.matrix(transition.matrix[,2:dim(transition.matrix)[2]])

transition.matrix <- matrix( data = transition.matrix,
                             nrow = nrow(transition.matrix), ncol = ncol(transition.matrix),
                             dimnames = list( colnames(transition.matrix), colnames(transition.matrix) ) )

write.csv(as.data.frame(heatmap.mt), 'data/transition_matrix.csv', row.names = F)

transition.matrix <- new( "markovchain", transitionMatrix = transition.matrix )

png(filename = "channel_transition_matrix.png",
    width =7000,
    height = 7000)
plot(transition.matrix, edge.arrow.size=0.2)
dev.off()

trmat <- as.matrix(transition.matrix@transitionMatrix)

htmlTable(round(trmat,2), title = "Transitions", ctable = TRUE)



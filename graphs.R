library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(devtools)
library(gganimate)
library(ggTimeSeries)
library(animation)
library(grid)
library(zoo)
theme_set(theme_bw())


# Apple region revenue
p_region_rev <- ggplot(data = countries_rev2, aes(x = Y_Q, y = Rev, color = Region, group = Region, frame = Year)) + 
  geom_point(size = 4) + 
  geom_line() + theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  xlab('Quarters') + ylab('Revenue (USD in millions)') + ggtitle('Apple Worldwide Revenue 2010 - 2016')
  #scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1)) +
  scale_y_continuous(limits = c(0, 35000), breaks = round(seq(min(countries_rev2$Rev), max(countries_rev2$Rev), by = 2000),1))
ggplotly(p_region_rev)
#gg_animate(g)


b_region_rev <- ggplot(data = countries_rev2, aes(x = Y_Q, y = Rev)) + #geom_point(size = 4) +
  xlab('Quarters') + ylab('Revenue (USD in millions)') + ggtitle('Apple Worldwide Revenue 2010 - 2016') +
  geom_bar(aes(fill = countries_rev2$Region), stat = 'identity', position = 'dodge') + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplotly(b_region_rev)


ggplot_waterfall(
  dtData = countries_rev,
  'Y_Q', 'Americas') + xlab('Quarters') + ylab('Revenue (USD in millions)') + theme_bw() + ggtitle('Americas')


ggplot_waterfall(
  dtData = countries_rev,
  'Y_Q', 'Europe') + xlab('Quarters') + ylab('Revenue (USD in millions)') + theme_bw() + ggtitle('Europe')

ggplot_waterfall(
  dtData = countries_rev,
  'Y_Q', 'Asia') + xlab('Quarters') + ylab('Revenue (USD in millions)') + theme_bw() + ggtitle('Asia-Pacific')


p_prod_rev <- ggplot(data = prod_rev2, aes(x = Y_Q, y = Rev, color = Product, group = Product )) +
  geom_point(size = 4) + geom_line() +
  xlab('Quarters') + ylab('Revenue (USD in millions)') + ggtitle('Apple Product Revenue 2010 - 2016')
ggplotly(p_prod_rev)

b_prod_rev <- ggplot(data = prod_rev2, aes(x = Y_Q, y = Rev)) + #geom_point(size = 4) +
  geom_bar(aes(fill = prod_rev2$Product), stat = 'identity', position = 'dodge') + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
xlab('Quarters') + ylab('Revenue (USD in millions)') + ggtitle('Apple Product Revenue 2010 - 2016')
ggplotly(b_prod_rev)

p_prod_units <- ggplot(data = prod_units2, aes(x = Y_Q, y = Units, color = Product, group = Product )) +
  geom_point(size = 4) + geom_line() +
  xlab('') + ylab('Units Sold (thousands)') + ggtitle('Apple Product Units Sold 2010 - 2016')
ggplotly(p_prod_rev)

b_prod_units <- ggplot(data = prod_units2, aes(x = Y_Q, y = Units)) + #geom_point(size = 4) +
  geom_bar(aes(fill = prod_units2$Product), stat = 'identity', position = 'dodge') + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab('') + ylab('Units Sold (thousands)') + ggtitle('Apple Product Units Sold 2010 - 2016')
ggplotly(b_prod_units)

# p1 <- ggplot_waterfall(
#   dtData = countries_rev2, cXColumnName = interaction(countries_rev2$Quarter, countries_rev2$Year), 'Rev')
# p1 + 
#   xlab(NULL) + 
#   ylab(NULL)




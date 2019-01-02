library(ggplot2)
library(maps)
library(ggthemes)
library(gganimate)
library(leaflet)


# read data
df <- read.csv("C:/Users/xingk/Desktop/Booty Bay/Sales, YoY S and First Store by Lat and Long.csv",header = T)
colnames(df) <- c("lat","lon","store","yoy","sales")

# manipulate for visulization, anything >1 will be showing 1 here
df$yoy <- ifelse(df$yoy>=1,1,df$yoy)

# making the base map
world0 <- ggplot(df,aes(x = lon)) +
  borders("usa", colour = "gray85", fill = "gray80") +
  theme_map() 

# adding points to the map
map0 <- world0 +
  geom_point(aes(y = lat, 
                 size = sales,
                 color = yoy, alpha = 0.5)) +
  scale_size_continuous(range = c(1, 12), 
                        breaks = c(400, 800, 1200, 1600, 2000)) +
  scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  labs(size = 'Sales', color = 'YoY Comp')

# add percentiles to the dataframe
df$tilerang <- findInterval(df$yoy,quantile(df$yoy,probs=seq(0,1, by=0.05)),rightmost.closed = T)

## animate

# starting and ending points
df_start <- data.frame(tilerang = 0, yoy = -1, lon = 0, lat = 0, store = 0, sales = 0)
df_end <- data.frame(tilerang = 21, yoy = 1.01, lon = 0, lat = 0, store = 0, sales = 0)

df_map <- rbind(df,df_start,df_end)

# new plotting
world <- ggplot(df_map,aes(x = lon)) +
  borders("usa", colour = "gray85", fill = "gray80") +
  theme_map() 

map <- world +
  geom_point(aes(y = lat, 
                 size = sales,
                 color = yoy, alpha = 0.7)) +
  scale_size_continuous(range = c(1, 12), 
                        breaks = c(400, 800, 1200, 1600, 2000)) +
  scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  labs(size = 'Sales', color = 'YoY Comp') +
  transition_manual(tilerang) +
  enter_grow()+
  exit_shrink()

# animating
# seems the current 0.9.9.9999 version is not able to take options/setting for resolution
options(ddanimate.dev_args = list(width = 1280, height = 800))
gganimate::animate(map, fps = 2, options = T)


### trying leaflet
pal <- colorNumeric(palette = c("red","pink","green"), domain = df$yoy)

m <- leaflet(df) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(~lon, ~lat,
    radius = ~(sales/50),
    color = ~pal(yoy),
    stroke = FALSE,
    fillOpacity = 0.8
  )

m
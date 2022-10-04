library(tidyverse)
install.packages("palmerpenguins")

library(palmerpenguins)

?palmerpenguins

glimpse(penguins)

penguins_data<-penguins

# First example with ggplot
ggplot(data = penguins_data,
       
              mapping = aes(x=bill_length_mm,
                            y=bill_depth_mm)) + 
                                              
                                                geom_point()

# What is the difference between mapping and setting the aesthetics?

# Setting aesthetics mostly in the geometry
ggplot(data = penguins_data,
              mapping = aes(x=bill_length_mm,
                            y=bill_depth_mm,color='red')) + geom_point()

ggplot(data = penguins_data,
       mapping = aes(x=bill_length_mm,
                     y=bill_depth_mm)) + geom_point(size=5)

# Setting aesthetics
ggplot(data = penguins_data,
       mapping = aes(x=bill_length_mm,
                     y=bill_depth_mm)) + 
                          geom_point(color='red',shape=25)



# Mapping aesthetics
ggplot(data = penguins_data,
       mapping = aes(x=bill_length_mm,
                     y=bill_depth_mm,
                     color=species)) + geom_point()





# Each aesthetic has a scale 
ggplot(data = penguins_data,
       mapping = aes(x=bill_length_mm,
                     y=bill_depth_mm,
                     color=species)) + geom_point()+
                      scale_x_continuous(limits=c(0,70),breaks=seq(0,70,10))+
                      scale_y_continuous(limits=c(0,25),breaks=seq(0,25,5))+
                        labs(x='Bill Length',
                             y='Bill Depth',
                             color='Penguin Species')


#geom_bar raw count
ggplot(data=penguins_data,
        mapping=aes(x=species))+geom_bar()


# geom_bar percentage
ggplot(data = penguins_data, 
          mapping = aes(x=species)) +
geom_bar(aes(y=after_stat(100*count/sum(count))))


#geom_col raw count
penguins_data%>%
        group_by(species)%>%
          summarise(species_count=n())%>%
            ggplot(mapping=aes(x=species,
                      y=species_count)) + geom_col()

#geom_col percentage
penguins_data%>%
        group_by(species)%>%
          summarise(species_count=n())%>%
mutate(species_percentage=100*species_count/sum(species_count))%>%
          ggplot(mapping=aes(x=species,
                      y=species_percentage))+geom_col()



ggplot(data=penguins_data,
         mapping=aes(x=species,
                   fill=island))+geom_bar()+
                        facet_wrap(~year)

ggplot(data = penguins_data,
        aes(x = flipper_length_mm)) +
          geom_histogram(aes(fill = species), 
                 alpha = 0.5, position = "identity")


ggplot(data=penguins_data,
          mapping=aes(x=flipper_length_mm))+geom_bar()



#### Some notes on how to use ggplot

# works and is my preferred way // very verbose but clear
ggplot(data=penguins_data,mapping=aes(x=bill_length_mm,y=bill_depth_mm))+geom_point()

# works because of 'positional matching'
ggplot(penguins_data,aes(bill_length_mm,bill_depth_mm))+geom_point()

# works too - this is called 'geometry related mapping'
ggplot(penguins_data)+geom_point(aes(x=bill_length_mm,y=bill_depth_mm))

# doesn't work 
ggplot(aes(bill_length_mm,bill_depth_mm),penguins_data)+geom_point()

# however this works again // matching by complete name
ggplot(mapping=aes(bill_length_mm,bill_depth_mm),data=penguins_data)+geom_point()



### The benefit of plots as objects

p<-ggplot(data=penguins_data,
              mapping=aes(x=bill_length_mm,
                          y=bill_depth_mm))
p #run 'p' to see what is in p

p<-p+geom_point()
p

p<-p + geom_jitter()
p


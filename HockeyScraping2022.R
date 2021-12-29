
# Load Libraries ----------------------------------------------------------


library(rvest)
library(tidyverse)


# Web Scraping ------------------------------------------------------------

# Read the table in Hockey Reference
HockeyRef <- read_html("https://www.hockey-reference.com/leagues/NHL_2022_skaters.html")

Leaders2022 <- HockeyRef %>% html_table(fill = TRUE)

Leaders2022 <- as.data.frame(Leaders2022) # make the table a dataframe 
Leaders2022 <- Leaders2022[-1,] # delete first row

# Rename Columns
Leaders2022 <- Leaders2022 %>% 
  rename(ID = 'Var.1',
         PlayerName = 'Var.2',
         Age = 'Var.3',
         Team = 'Var.4',
         Position = 'Var.5',
         GP = 'Var.6',
         G = 'Scoring',
         A = 'Scoring.1',
         PTS = 'Scoring.2') 

# Select only the columns needed and filter out the rows that are actually headers
Leaders2022 <- Leaders2022 %>% 
  select(PlayerName:PTS) %>% 
  filter(PlayerName != "Player")

# Mutate to make columns correct data types
Leaders2022 <- Leaders2022 %>% 
  mutate(Age = as.double(Age),
         GP = as.double(GP),
         G = as.double(G),
         A = as.double(A),
         PTS = as.double(PTS)) %>% 
  arrange(desc(PTS))

# Remove excess lines for traded players
Leaders2022 <- Leaders2022 %>% 
  mutate(Duplicate = case_when(
    duplicated(PlayerName) == TRUE ~ 1,
    TRUE ~ 0
  )) %>% 
  filter(Duplicate != 1) %>% 
  select(!Duplicate)



# Data Cleaning & Create Tables used for Dashboard ------------------------


Jared <- Leaders2022 %>% 
  filter(PlayerName %in% c("Anders Lee", "John Carlson", "David Pastrnak", "Victor Olofsson",
                           "Max Comtois", "Tim Stützle", "Roope Hintz", "Phil Kessel",
                           "Sidney Crosby", "Mitch Marner", "Tyler Bertuzzi", "Adam Fox",
                           "Filip Forsberg", "Claude Giroux", "Cale Makar", "Brayden Point",
                           "Brent Burns", "Shea Theodore", "Zach Werenski", "Johnny Gaudreau",
                           "William Karlsson", "Patric Hornqvist", "Jonathan Toews", "Nicklas Backstrom",
                           "Vladimir Tarasenko", "Sam Reinhart", "Dominik Kubalík", "Kyle Connor",
                           "Jack Hughes", "Andrei Svechnikov", "Mark Giordano", "Ryan Nugent-Hopkins", 
                           "J.T. Miller", "Dustin Brown", "Cole Caufield", "Jared Spurgeon")) %>% 
  mutate(Owner = "Jared")

Dan <- Leaders2022 %>% 
  filter(PlayerName %in% c("Anthony Beauvillier", "Jonathan Huberdeau", "Alex DeBrincat", "Nikolaj Ehlers",
                           "Evgeny Kuznetsov", "Nick Suzuki", "Kevin Fiala", "Yegor Sharangovich",
                           "Taylor Hall", "Sebastian Aho", "Jaden Schwartz", "Pavel Buchnevich",
                           "Casey Mittelstadt", "Dylan Larkin", "Mika Zibanejad", "Mikael Granlund",
                           "William Nylander", "Joshua Norris", "Jason Robertson", "Jakob Chychrun",
                           "Timo Meier", "Mark Stone", "Patrik Laine", "Elias Lindholm",
                           "Martin Necas", "Andre Burakovsky", "Carter Verhaeghe", "Bo Horvat",
                           "Connor McDavid", "Brock Boeser", "Kasperi Kapanen", "Anze Kopitar",
                           "Joel Farabee", "Troy Terry", "Victor Hedman", "Mikko Rantanen")) %>% 
  mutate(Owner = "Dan")

# Devin
Devin <- Leaders2022 %>% 
  filter(PlayerName %in% c("Rickard Rakell", "Nick Schmaltz", "Patrice Bergeron", "Rasmus Dahlin",
                           "Sean Monahan", "Teuvo Teravainen", "Patrick Kane", "Nathan MacKinnon",
                           "Jakub Voracek", "Tyler Seguin", "Filip Hronek", "Leon Draisaitl",
                           "Aleksander Barkov", "Drew Doughty", "Kirill Kaprizov", "Jeff Petry", 
                           "Ryan Johansen", "Nico Hischier", "Josh Bailey", "Artemi Panarin",
                           "Brady Tkachuk", "Travis Konecny", "Kris Letang", "Jordan Eberle", 
                           "David Perron", "Steven Stamkos", "John Tavares", "Elias Pettersson",
                           "Max Pacioretty", "T.J. Oshie", "Blake Wheeler", "Ondrej Palat",
                           "Bryan Rust", "Pierre-Luc Dubois", "Conor Garland","Logan Couture")) %>% 
  mutate(Owner = "Devin")

# John
John <- Leaders2022 %>% 
  filter(PlayerName %in% c(
    "Trevor Zegras", "Clayton Keller", "Brad Marchand", "Jack Eichel",
    "Matthew Tkachuk", "Vincent Trocheck", "Tyler Johnson", "Gabriel Landeskog",
    "Oliver Bjorkstrand", "Joe Pavelski", "Robby Fabbri", "Zach Hyman",
    "Sam Bennett", "Viktor Arvidsson", "Mats Zuccarello", "Tyler Toffoli",
    "Roman Josi", "Dougie Hamilton", "Mathew Barzal", "Ryan Strome",
    "Drake Batherson", "Sean Couturier", "Jake Guentzel", "Alexander Wennberg",
    "Tomas Hertl", "Ryan O'Reilly", "Nikita Kucherov", "Auston Matthews",
    "Quinn Hughes", "Jonathan Marchessault", "Alex Ovechkin", "Mark Scheifele",
    "Craig Smith", "Alexis Lafreniere", "Jamie Benn", "Chandler Stephenson"
  )) %>% 
  mutate(Owner = "John")

# Bind the four sets into one

Fantasy_Players <- bind_rows(Jared, Devin, Dan, John)

# Order Dataframe by points
Fantasy_Players <- Fantasy_Players %>% 
  arrange(desc(PTS))


Teams <- Fantasy_Players %>% 
  group_by(Owner) %>% 
  summarise(PTS = sum(PTS),
            A = sum(A),
            G = sum(G),
            GP = sum(GP)) %>% 
  arrange(desc(PTS))


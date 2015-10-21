#############################
#### HEADER #################
#############################


	Title <- "Netflix in the World"
	# See data visualisation: http://chcariou.fr/post/130191786162
	# Code inspiration: https://workspace13.wordpress.com/2014/10/30/mirror-mirror-on-the-wall-which-is-your-favorite-german-car-brand-in-the-world-2/
	# Visual inspiration: https://www.behance.net/gallery/24276859/City-Layouts
	# By Christophe Cariou - October 2015


#############################
#### LIBRARIES ##############
#############################


	Lib <- c(
		"RCurl", # scrape data
		"stringr", # clean data
		"countrycode", # ISO3 code for country
		"sp", # transform points in Spatial points
		"rworldmap", # join world map data via ISO 3 + polygons & points
		"ggplot2", # for previous version
		"ggmap", # geocode data via google
		"dplyr", # fonction arrange()
		"rgdal", # for mercator projection + new center of the map
		"extrafont" # for font
	)


	NLib <- length(Lib)
	for (l in 1:NLib) {
		print(Lib[l])
		library(Lib[l], character.only = TRUE)
	}



#############################
#### FOOTER #################
#############################


	Who <- "By Christophe Cariou with R"
	Sys.setlocale("LC_TIME", "C")
	When <- format(Sys.Date(), "%B %Y")
	WW <- paste(Who, When, sep=" - ")
	Author <- paste(WW,".",sep="")

	Data <- "Data: Netflix."
	
	Lib_paste <- paste(sort(Lib), collapse=", ")
	Libraries <- paste("Libraries: ", Lib_paste, ".", sep="")

	Font_titre <- "Tex Gyre Schola"
	Font_texte <- "Open Sans"
	Font_paste <- paste(sort(c(Font_titre, Font_texte)), collapse=", ")
	Fonts <- paste("Fonts: ", Font_paste,".", sep="")

	Footer <- paste(Author, Data, Libraries, Fonts, sep=" ")
	


##############################################
#### EXTRACT LIST OF COUNTRIES ###############
##############################################


# Extract and prepare countries

	url <- "https://help.netflix.com/en/node/14164" # Netflix webpage with the list of countries
	page <- getURL(url)
	webpage <- str_split(page, "table")[[1]]
	data <- webpage[2]

	colonnes <- str_split(data,"vertical-align: top")[[1]]
	n <- length(colonnes)
	data1 <- colonnes[2:n]

	data2 <- "Country"

	for (i in 1: (n-1)) {
		d <- str_split(data1[i],"\n\t\t\t")[[1]]
		data2 <- c(data2,d)
	}
	
	a <- str_locate(data2,"<")[,1]-1
	b <- str_sub(data2,1,a)
	c <- str_replace(b, "[\"]","")
	d <- str_replace(c, ">","")
	Country <- subset(d, nchar(d)>1 & !is.na(d))
	Country <- str_replace_all(Country,"á","a") # problem with Panama: ok
	Country_id <- rep(1, length(Country))

	Countries <- data.frame(cbind(Country, Country_id))
	N_current <- dim(Countries)[1] 


# I add the next countries 

	Country <- c("Portugal","Italy","Spain","Hong Kong","Singapore","South Korea","Taiwan")
	Country_id <- rep(2, length(Country))
	Announced <- data.frame(cbind(Country, Country_id))
	
	Country <- c("China")
	Country_id <- rep(3, length(Country))
	InDiscussion <- data.frame(cbind(Country, Country_id))

	Next <- rbind(Announced, InDiscussion)


# Merge and code ISO3 countries

	Countries2 <- rbind(Countries, Next)
	Countries2$ISO3C <- countrycode(Countries2$Country, "country.name", "iso3c")
	#subset(Countries2,is.na(ISO3C)) # With Antigua
	#countrycode("ATG","iso3c", "country.name")

	# updated date: remove Next if Netflix is available
	Countries <- aggregate(as.numeric(Countries2[,2]),by=list(Country = Countries2[,1], ISO3C =Countries2[,3] ),FUN=min) 
	colnames(Countries)[3] <- "Country_id" 

	# Count next countries
	N_soon <- dim(subset(Countries, Country_id==2))[1] 
	N_discussion <- dim(subset(Countries, Country_id==3))[1] 



########################################################
########################################################
#### COUNTRIES => MERCATOR PROJECTION ##################
########################################################
########################################################


# Join data with World Map 

	Map00 <- joinCountryData2Map(Countries, joinCode = "ISO3", nameJoinColumn = "ISO3C",verbose=TRUE)

		# Three islands have failed : BES Islands, Guadeloupe and Martinique.
		# Absent <- c("Bonaire", "Sint Eustatius", "Saba", "Guadeloupe","Martinique")
		# The Map is too small to see such absents but:
		# To obtain point for these islands, use geocode(Absent, source="google") 

	Map01 <- subset(Map00, Map00@data$NAME!="Antarctica")


# Transform longlat to mercator projection with a new center for a better view

	Los_Gatos <- geocode("Los Gatos, California", source="google") # New center
	Mercator <- paste("+proj=merc +ellps=GRS80 +datum=WGS84 +init=epsg:4326 +lon_0=",Los_Gatos$lon, sep="") # New projection
	Map02 <- spTransform(Map01, CRS(Mercator)) # Transform


# Extract polygons for countries

	Map03 <- fortify(Map02)


# Merge with previous data & reorder

	Map04 <- merge(Map03, Map02@data, by.x="id", by.y="ADMIN", all.x=T)
	Map05 <- Map04 %>% arrange(id, order)


# Mercator projection for capitale of countries (fot texte or points) + Los Gatos

	LG00 <- SpatialPoints(cbind(Los_Gatos$lon,Los_Gatos$lat), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
	Los_Gatos <- spTransform(LG00, CRS(Mercator))

	Cap00 <- distinct(Map05, NAME, LON, LAT)
	Cap01 <- SpatialPoints(cbind(Cap00$LON, Cap00$LAT), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
	Cap02 <- spTransform(Cap01, CRS(Mercator))
	Cap03 <- cbind(Cap00,Cap02)


# Get only necessary data 

	Map06 <- subset(Map05, select=c("NAME","long","lat","order","piece","Country_id"))
	Capitale <- subset(Cap03, select=c("NAME","coords.x1","coords.x2", "Country_id"))
	colnames(Capitale)[2:3] <- c("long","lat")



##########################################################
##########################################################
#### COUNTRIES => ADJUST LEFT/RIGHT BORDERS ##############
##########################################################
##########################################################


	border <- 0.9*max(max(Map06$long), abs(min(Map06$long)))
	
	Min <- aggregate(as.numeric(Map06$long),by=list(Name=Map06$NAME, Piece=Map06$piece),FUN=min)
	Max <- aggregate(as.numeric(Map06$long),by=list(Name=Map06$NAME, Piece=Map06$piece),FUN=max)
	Lborder <- merge(Min,Max,by=c("Name","Piece"))
	colnames(Lborder)[3:4] <- c("left","right")

	Problems <- subset(Lborder, left<(-border) & right>border)
	Map07 <- subset(Map06, !(NAME %in% Problems$Name & piece %in% Problems$Piece))

	P0 <- subset(Map06, NAME %in% Problems$Name & piece %in% Problems$Piece)
	P0_l <- subset(P0, long<0)
	P0_r <- subset(P0, long>0)

	border_left <- min(P0_l$long)
	border_right <- max(P0_r$long)

	N_piece <- aggregate(as.numeric(Map06$piece),by=list(Name=Map06$NAME),FUN=max)

	# For every country and piece = every polygon
	# Change the id_piece + add limits (start and end) to draw a line at left and right

	P_n <- dim(Problems)[1]
	for (p in 1:P_n) {
		
		P1 <- subset(P0, NAME==Problems$Name[p] & piece==Problems$Piece[p])
		
		n1 <- dim(P1)[1]
		P1$order <- seq(1,n1,1)

		P1_l <- subset(P1, long<0)
		n1_l <- dim(P1_l)[1]
		
		P1_r <- subset(P1, long>0)
		n1_r <- dim(P1_r)[1]


		# Points of interest

			if (P1_l$order[1]!=1) { 
				id1 <- P1_l$order[1]
				id2 <- P1_l$order[n1_l]
			} else {
				id1 <- P1_r$order[1]-1
				id2 <- P1_r$order[n1_r]+1
			}


		# Left : take first and last of right and change long

			l1 <- P1[id1+1,]
			l1$long <- border_left
			#l1$order <- P1_l$order[n1_l]+1
			
			l2 <- P1[id2-1,]
			l2$long <- border_left
			#l2$order <- P1_l$order[n1_l]+2

			P2_l <- rbind(P1_l, l1, l2)


		# Right : take first and last of left and change long + change piece

			r1 <- P1[id2,]
			r1$long <- border_right
			#r1$order <- P1_r$order[n1_r]+1
			
			r2 <- P1[id1,]
			r2$long <- border_right
			#r2$order <- P1_r$order[n1_r]+2
		
			P2_r <- rbind(P1_r, r1, r2)
			P1_p <- N_piece[N_piece$Name==Problems$Name[p],]$x+1
			P2_r$piece <- P1_p


		P2 <- rbind(P2_l, P2_r)

		Map07 <- rbind(Map07, P2)

	}

Map08 <- Map07 %>% arrange(NAME, piece, order)


#######################
######################
#### CUT HEIGHT #####
######################
######################

	cut_height <- 0.75
	cut <- cut_height*max(Map08$lat)

	Top <- aggregate(as.numeric(Map08$lat),by=list(Name=Map08$NAME, Piece=Map08$piece),FUN=max)
	colnames(Top)[3] <- c("top")

	Problems <- subset(Top, top > cut)

	# Différence > ajuster ci-dessus : un pays avec plusieurs pieces ici donc pas possible comme tout à l'heure
	# 1) Les pays sans problème de côté pour commencer

	Map09 <- subset(Map08, !(NAME %in% Problems$Name))

	# 2) Les pays à problème, pays par pays

	Prob00 <- subset(Map08, NAME %in% Problems$Name)
	Prob01 <- unique(Problems$Name)
	Prob02 <- length(Prob01)
	T0 <- Map08[1,]

	for (j in 1:Prob02) {
		Prob03 <- subset(Prob00, NAME==Prob01[j])
		Prob04 <- subset(Problems, Name==Prob01[j])$Piece

		Prob05 <- subset(Prob03, piece %in% Prob04)
		T0 <- rbind(T0,Prob05)

		Prob06 <- subset(Prob03, !(piece %in% Prob04))
		Map09 <- rbind(Map09,Prob06)

		

	}

	T0 <- T0[-1,]

	# Et ensuite nous revenons au programme initial
	
	T_n <- dim(Problems)[1]
	
	for (t in 1:T_n) {
		
		T1 <- subset(T0, NAME==Problems$Name[t] & piece==Problems$Piece[t])
		n1 <- dim(T1)[1]
		T1$order <- seq(1,n1,1)
		T1_nocut <- subset(T1, lat < cut)
		n_nocut <- dim(T1_nocut)[1]
		
		if (n_nocut != 0) {

			T1_cut <- subset(T1, lat >= cut)
			n_cut <- dim(T1_cut)[1]

			if (T1_cut$order[1]!=1) { 
				id1 <- T1_cut$order[1]
				id2 <- T1_cut$order[n_cut]
			} else {
				id1 <- T1_nocut$order[1]-1
				id2 <- T1_nocut$order[n_nocut]+1
			}

	
			c1 <- 	T1[c(id1, id2),]
			c1$lat <- cut

			T2 <- rbind(T1_nocut, c1)

		
			Map09 <- rbind(Map09, T2)
		}
	}

Map10 <- Map09 %>% arrange(NAME, piece, order)




###########################################
##########################################
#### SOME COUNTRIES ARE NOTE VISIBLE #####
##########################################
##########################################

# Identify countries too small to be visible > points and not polygons
		
	Too_small <- sort(c("Singapore","Hong Kong", "Taiwan"))
	Map_xy <- subset(Capitale, NAME %in% Too_small)




##########################################
##########################################
#### CHANGE NAME+PIECE WITH ID_POLYGON #####
##########################################
##########################################


# Do this before in the program

	poly00 <- distinct(Map10, NAME, piece)[,c(1,5)]
	poly00$Id_polygon <- seq(1,dim(poly00)[1],1)

	Map11 <- merge(Map10,poly00, by=c("NAME","piece"))


##########################################
##########################################
#### PREPARE DATA WITH MY OWN SCALE #####
##########################################
##########################################


	rx <- range(Map11$long)
	ry <- range(Map11$lat)


	V_x0 <- 0
	V_w <- 2100-V_x0*2

	Ratio <- (ry[2]-ry[1])/(rx[2]-rx[1])
	# Base on asp=1

	V_y0 <- 2970/2-V_w*Ratio/2


	Map12 <- Map11
	Map12$long <- V_x0+(Map11$long-rx[1])/(rx[2]-rx[1])*V_w
	Map12$lat <- V_y0+(Map11$lat-ry[1])/(ry[2]-ry[1])*(V_w*Ratio)

	Map <- Map12


	Points <- Map_xy
	Points$long <-  V_x0+(Points$long-rx[1])/(rx[2]-rx[1])*V_w
	Points$lat <-  V_y0+(Points$lat-ry[1])/(ry[2]-ry[1])*(V_w*Ratio)


	Netflix <- Los_Gatos
	Netflix$long <-  V_x0+(Los_Gatos$coords.x1-rx[1])/(rx[2]-rx[1])*V_w
	Netflix$lat <-  V_y0+(Los_Gatos$coords.x2-ry[1])/(ry[2]-ry[1])*(V_w*Ratio)




#########################################
#########################################
#### DATA VISUALISATION #################
#########################################
#########################################


# Choose colors

	Col_bg <- "#2B2A2F"
	Col_lines <- "#2B2A2F"
	# Map : No netflix, Netflix, soon, in discussion > follow 0,1,2,3
	Col_countries <- c("#574940","#8B8079", "#AB977C","#574940") 

	Col_txt <- "#8B8079"


# Prepare window : I prefer plot() at ggplot() just because it's closer to my mean of build...

	dev.off()
	quartz(width=21/2.6,height=29.7/2.6, bg= Col_bg)
	par(mar=c(0,0,0,0), oma=c(0,0,0,0))
	plot(0,0,type="n",xlim=c(0,2100), ylim=c(0,2970),xaxs="i", yaxs="i", axes=FALSE, xlab=NA, ylab=NA, asp=1)
	# asp=1 to preserve the ratio of the map


# Visualisation

	noms <- "pays"
	morceaux <- "piece"

	K <- length(unique(Map$Id_polygon))

	for (k in 1:K) {
	
		xyz <- subset(Map,  Id_polygon==k)
		C_id <- unique(xyz$Country_id)+1

		if (is.na(unique(xyz$Country_id))) { polygon(as.numeric(xyz$long),as.numeric(xyz$lat), col= Col_countries[1], border= Col_lines, lwd=0.4, density=40, angle=45)
		} else  { polygon(as.numeric(xyz$long),as.numeric(xyz$lat), col= Col_countries[C_id], border= Col_lines, lwd=0.4)}
	
	
		
	}


	Points[is.na(Points)] <- 0

	points(Points$long, Points$lat, pch=21, cex=1, col=Col_countries[Points$Country_id+1], bg=Col_countries[Points$Country_id+1], lwd=0)
	points(Netflix$long, Netflix$lat, pch="+", cex=1, col="#B65B68", bg="#B65B68", lwd=2)



# Titre et footer

	text(1050,2850,toupper("Netflix in the World"), family="Open Sans Light", cex=1.5, col= Col_txt )
	text(1050,120,Footer, family="Open Sans", cex=0.5, col= Col_txt )
	# Faire appel à ma fonction text ici au cas où la longueur deviendrait trop importante.


# Légende

	fam <- "Open Sans Semibold"

	h0 <- 2970/2+V_w*Ratio/2+0.02*2970

	N <- "Netflix"
	Already <- paste(" is available in",N_current, "countries, ", sep=" ")
	Soon <- paste("soon in",N_soon,"additional countries ", sep=" ")
	Discussion <- paste("plus",N_discussion,"under discussion.", sep=" ")

	Ligne <- paste(N, Already,Soon,Discussion,sep="")
	x0 <- 2100/2-strwidth(Ligne, family=fam,cex=0.75)/2
	x1 <- x0+strwidth(N, family=fam,cex=0.75)
	x2 <- x1+strwidth(Already, family=fam,cex=0.75)
	x3 <- x2+strwidth(Soon, family=fam,cex=0.75)

	text(x0,h0,N, family=fam,cex=0.75, pos=4, offset=0, col="#B65B68")
	text(x1,h0,Already, family=fam,cex=0.75, pos=4, offset=0, col=Col_countries[2])
	text(x2,h0,Soon, family=fam,cex=0.75, pos=4, offset=0, col=Col_countries[3])
	text(x3,h0,Discussion, family=fam,cex=0.75, pos=4, offset=0, col=Col_countries[4] )




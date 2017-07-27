/**
* Name: courlisgis
* Author: Etienne Delay[1]   and Sebastien Rey-Coyrehourcq [2]
* Afiliation : [1] Univ. Clermont-Ferrand Auvergne (GEOLAB) - Univ. Cheikh Anta Diop (OHMi Téssékéré)
* 		   [2] Univ. Rouen
*
* Description: This prototype version of our model is an abstraction of the theoretical behaviours of Courlis cendré. This model is developed for the LiENs  challenge 2007
* Tags: ecology, Courlis, path
* 
* Data Source : 
* 			* DEM : GTOPO30: It is a digital elevation model for whole world created and distributed by USGS. https://earthexplorer.usgs.gov/
*			* NDVI : MODIS NDVI data from MOD13Q1 https://earthexplorer.usgs.gov/
*                      * world shape : From http://thematicmapping.org 
* 			* events were created from courlis GPS data from LiENs
* 
* 
* Acknowledgement :  
*			* Raster data were resampled with GRASS-GIS 7
*			* vector data were created with Qgis 2.18 
*			* Data analyses were maid with R (library : readxl, rgeos, geosphere, rgdal, raster, stringr, MODISTools, MODIS, tidyverse)
*
*/

model courlisgis

global{ //Declare the world as a torus or not torus environment
	//Shapefile of country
	file country_shapefile <- file("../includes/world_shape/europ_migration3034.shp");
	//Shapefile of event
	file event_shapefile <- file("../includes/world_shape/events_3034.shp");
	//Shape pts gps
	file gps_file <- file("../includes/gps/gps_pts_courlis.shp");
	// Raster
	file dem_file <- file("../includes/dem/resamp_dem.asc") ;
	
	
	/** Insert the global definitions, variables and actions here */
	//float step <- 60 #mn; // GPS point are point each 30 min 
	geometry shape <- envelope(dem_file); // defined the agent size world
	/*ALERT : Spatial unit stay meter... */
	float perception_distance <- 200#km parameter: true;
	bool follow_trajectory <- true parameter: true;
	
	init{
		create pts_ctrl from:gps_file with:[loggerID::string(read("Logger ID"))];
		create events from: event_shapefile with: [ev_type::string(read("event")),logg::string(read("name"))];
		ask events where(each.ev_type = "Arrive"){
			myPath <- pts_ctrl where(each.loggerID = self.logg);
		}
		
		matrix NDVI1 <- matrix(csv_file("../includes/dem/resamp_A2016097.250m_16_days.asc", " ", float));
		ask dem_cell {
			elevation <- grid_value;
			NDVI97 <- float(NDVI1[grid_x, grid_y]) * 0.0001; // rescaling NDVI
		}
		
		//Creation des courlis par les events
		let nb_courlis <- length(events where(each.ev_type="Arrive"));
		write nb_courlis ;
		let myDepart <-one_of(events where(each.ev_type="Depart"));
		ask events where(each.ev_type="Arrive"){ //init ile de re
    			 create courlis  number:1{
    			 	location <- myDepart.location;
    			 	theo_path <- myself.myPath;
    			 	closestGPS <- (self distance_to (theo_path closest_to self)) / 1000;
    			 	myName <- myself.logg;
    			 }
			//create courlis  number:10 with:[location::location];
		}	
	}
	
	reflex halting {
		if (cycle > 120){
			do pause;	
		}
     }
}

//Grid species representing a cellular automata
grid dem_cell file:dem_file {
	float elevation;
	float NDVI97;
	rgb pcolor <- blend(#green,#white, elevation) update: blend(#green,#white, elevation);
	aspect dem {
		draw shape color:pcolor border:#black empty:false;	
	}
}

species events{
	string ev_type;
	string logg ;
	list<pts_ctrl> myPath;
	aspect default {
		draw circle(20000) color: #red;
	}
}

species pts_ctrl {
	int how ;
	string loggerID;
	
	aspect base {
		 draw triangle(20000) color: #yellow;
	}
}



species courlis skills:[moving]{
	geometry target_travel <- point(one_of(events where(each.ev_type="Arrive")));
	geometry food_target;
	geometry perceived_area ;
	list<dem_cell> myVisibility;
	list<point> trajectory <- [];
	list<pts_ctrl> theo_path <- [];
	float closestGPS ;
	string myName;
	int energy <- rnd(10) min:-1 max: 10;
	
	//moving facet
	float speed <- rnd(80.0)#km min:0.0#km max:100.0#km; //Our step is one hour so if it fly at 
	int heading <-  self towards(target_travel) update: self towards(target_travel);
	
	reflex perception {
		/* Define the courlis percetion */
		//perceived_area <-(square(perception_distance)) intersection circle(perception_distance);
		perceived_area <- (cone((180+heading)-3#km,(180+heading)+3#km) intersection world.shape) intersection circle(perception_distance);
		 if (perceived_area != nil) {
		 	myVisibility <- dem_cell overlapping perceived_area where(each.elevation < 179);
		 }
		 
	}
	
	reflex mvt {
		speed <- rnd(100.0)#km;
		if(myVisibility != nil){
			if empty(myVisibility where(each.NDVI97 < 0.3 and each.NDVI97 > 0.2)){
				if( mean(myVisibility collect(each.NDVI97)) <= 0.0){
					food_target <-  (myVisibility farthest_to self).location;
				}else{
					food_target <- point(one_of(myVisibility));
				}
				food_target <- point(one_of(myVisibility));
			}else{
				food_target <- point(one_of(myVisibility where(each.NDVI97 < 0.3 and each.NDVI97 > 0.2)));
			}
			
		}
		if(self.location != target_travel.location){
			do goto target: food_target;
		}
		/*On pourra ajouter le retour mais pour le moment 
		 * les courlis ne font qu'aller a leur lieu d'accouplement
		 */
		
		//do move;
		 closestGPS <- (self distance_to (theo_path closest_to self)) / 1000;
	}
	
	reflex eat {
		let myCell <- one_of(dem_cell overlapping self);
		if(energy < 3){
			let energytack <- myCell.NDVI97;
			energy <- energy + energytack * 10;
			myCell.NDVI97 <- myCell.NDVI97 - energytack;
			
		}
	}
	
	// Ce reflex permet de tracer le chemin suivis par les agents
	reflex updateTrajectory {
	    if follow_trajectory{
	      trajectory <- trajectory + location;
	    }
   	}
   	
   	/*GESTION des aspect */
   	aspect body {
		 draw triangle(20000) rotate:90 + heading color: #red;
	}
	
	aspect perception {
		if (perceived_area != nil) {
			draw perceived_area color: #magenta;
		}
	}
	
	aspect trajectory{
	     if follow_trajectory{
	       draw line(trajectory) color:#pink;   
	     }
   	}
}


experiment courlisDev type: gui {
	/** Insert here the definition of the input and output of the model */
	output {
		display sp_display type: opengl{
			//species country refresh: false transparency: 0.5;
			//image country gis:"../includes/world_shape/europ_migration3034.shp" color: rgb('grey');
			species events refresh: false;
			//species dem_cell;
			species courlis aspect:body;
			species courlis aspect: perception transparency: 0.5;
			species courlis aspect: trajectory transparency: 0.5;
			species pts_ctrl aspect: base;
			
		}
		display graph_eat{
			chart "Distance empirique" size: {0.5,0.5}{
				data "evol. energy" value: mean(courlis collect (each.closestGPS));
			}
			chart "Distance agents (km)" type: series size: {0.5,0.5} position: {0.5,0.0}{
				datalist value:(courlis collect each.closestGPS) legend:(courlis collect each.myName); 	
			}
			/*chart "number of gregarious locusts" size: {0.5,0.5} position: {0.5,0.0}{
				data "gregarious locusts" value: length (locust where (each.gregarious = true));
				data "solitarious locusts" value: length (locust where (each.gregarious = false));
			}
			chart "mean P(gregarious) of locusts" size: {0.5,0.5} position: {0.0,0.5}{
				data "P(gregarious) of locusts" value: mean (locust collect(each.pGregarious));
			}*/
		}
	}
}

/**
* Name: courlisabstract
* Author: Etienne Delay[1]   and Sebastien Rey-Coyrehourcq [2]
* Afiliation : [1] Univ. Clermont-Ferrand Auvergne (GEOLAB) - Univ. Cheikh Anta Diop (OHMi Téssékéré)
* 		   [2] Univ. Rouen
*
* Description: This proto version of our model is an abstraction of the theorical behaviors of Courlis cendré. This model is develop for the LiENs  challenge 2007
* Tags: ecology, Courlis, path
*/

model courlisabstract

global{ //Declare the world as a torus or not torus environment

	/** Insert the global definitions, variables and actions here */
	float step <- 60 #mn; // GPS point are point each 30 min 
	geometry shape <- square(2000 #km); // defined the agent size world
	/*ALERT : Spatial unit stay meter... */
	float perception_distance <- 200#km parameter: true;
	 bool follow_trajectory <- true parameter: true;
	
	init{
		ask veg_cell[99,1]{ // final objectif
			create goal_agent number:1{
				location <- myself.location;
			}
		}
		
		ask veg_cell[33,66]{ //init ile de re
			create courlis  number:10 with:[location::location];
		}
	}
}

//Grid species representing a cellular automata
grid veg_cell width: 100 height: 100 neighbors: 8 {
	float NDVI <- gauss(0.5,0.5) update:gauss(0.5,0.5) min: 0.0 max:1.0 ;
	rgb pcolor <- blend(#green,#white, NDVI) update: blend(#green,#white, NDVI);
	list<veg_cell> neighbours  <- (self neighbors_at 1);
	
	
	
	aspect col_ndvi{
		draw shape color:pcolor border:#black empty:false;
	}
}

species goal_agent {
	aspect obj{
		draw circle(2) color:#yellow;
	}
}

species courlis skills:[moving]{
	veg_cell myCell <- one_of (veg_cell);
	geometry target_travel <- point(one_of(goal_agent));
	geometry food_target;
	geometry perceived_area ; 
	list<veg_cell> myVisibility;
	list<point> trajectory <- [];
	//moving facet
	float speed <- rnd(80.0) min:0.0 max:100.0; //Our step is one hour so if it fly at 
	int heading <-  self towards(target_travel) update: self towards(target_travel);
	
	reflex perception {
		/* Define the courlis percetion */
		//perceived_area <-(square(perception_distance)) intersection circle(perception_distance);
		perceived_area <- (cone((180+heading)-3#km,(180+heading)+3#km) intersection world.shape) intersection circle(perception_distance);
		 if (perceived_area != nil) {
		 	myVisibility <- veg_cell overlapping perceived_area;
		 }
	}
	
	reflex mvt {
		speed <- rnd(100.0);
		if(myVisibility != nil){
			let myTraget <- myVisibility with_max_of(each.NDVI);	
			food_target <- point(myTraget);
		}
		do goto target: food_target;
		//do move;
	}
	
	reflex updateTrajectory {
	    if follow_trajectory{
	      trajectory <- trajectory + location;
	    }
   	}


	
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



experiment courlisabstract type: gui {
	/** Insert here the definition of the input and output of the model */
	output {
		display sp_display type: opengl{
			species veg_cell aspect:col_ndvi;
			species courlis aspect:body;
			species courlis aspect: perception transparency: 0.5;
			species courlis aspect: trajectory transparency: 0.5;
			species goal_agent aspect:obj;
		}
	}
}

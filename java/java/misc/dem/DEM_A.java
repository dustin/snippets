// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
// $Id: DEM_A.java,v 1.1 2000/07/30 10:42:23 dustin Exp $

import java.io.*;
import java.util.*;

public class DEM_A extends Object {

	protected String name=null;
	protected String descr=null;
	protected int dem_level=-1;
	protected int pattern_code=-1;
	protected int planimetric=-1;
	protected int zone_code=-1;
	protected int units_code_g=-1;
	protected int units_code_e=-1;
	protected int n_sides=-1;
	protected int accuracy_code=0;

	protected int rows=0;
	protected int columns=0;

	protected double corners[][]=null;
	protected double min_elevation=0.0;
	protected double max_elevation=0.0;
	protected double angle=0.0;

	protected double spacial_resolution[]=null;

	protected double proj_params[]=null;

	public DEM_A(String block) throws Exception {
		super();

		grokABlock(block);
	}

	public String toString() {
		String ret="DEM data for ``" + name + "''\n";

		ret+="\tDescription:  " + descr + "\n";
		ret+="\tCorners:  (units: " + units_code_g + ")\n";
		for(int i=0; i<4; i++) {
			ret+="\t\t" + corners[i][0] + ", " + corners[i][1] + "\n";
		}

		ret+="\tElevation range:  " + min_elevation + " to "
			+ max_elevation + " (units:  " + units_code_e + ")\n";

		ret+="\tAccuracy code:  " + accuracy_code + "\n";
		ret+="\tNumber of sides:  " + n_sides + "\n";
		ret+="\tSpacial resolution:\n"
			+ "\t\t" + spacial_resolution[0] + "\n"
			+ "\t\t" + spacial_resolution[1] + "\n"
			+ "\t\t" + spacial_resolution[2] + "\n";

		ret+="\tRows and columns:  " + rows + ", " + columns + "\n";

		return(ret);
	}

	protected double grokEngDouble(String in) {
		double ret=0.0;

		int thed=in.indexOf('D');
		if(thed<0) {
			thed=in.indexOf('E');
		}
		ret=Double.valueOf(in.substring(0, thed)).doubleValue();
		int r=Integer.parseInt(in.substring(thed+2));
		if( in.charAt(thed+1) == '-') {
			r=-r;
		}
		ret*=( java.lang.Math.pow(10.0, (double)r));

		return(ret);
	}

	protected double arcSecToDMS(double as) {
		return(as * (1.0/3600.0));
	}

	protected void grokABlock(String type_a) throws Exception {
		// Get the name.
		name=type_a.substring(0, 40).trim();
		descr=type_a.substring(40, 80).trim();

		dem_level=Integer.parseInt(type_a.substring(144, 150).trim());
		pattern_code=Integer.parseInt(type_a.substring(150, 156).trim());
		planimetric=Integer.parseInt(type_a.substring(156, 162).trim());
		zone_code=Integer.parseInt(type_a.substring(162, 168).trim());

		// The next section is the projection parameters...ignored, but
		// let's grab them, anyway.
		StringTokenizer st=
			new StringTokenizer(type_a.substring(168, 528));
		proj_params=new double[15];
		for(int i=0; i<15; i++) {
			proj_params[i]=Double.valueOf(st.nextToken()).doubleValue();
		}

		// Units for ground coordinates
		// 1: feet
		// 2: meters
		// 3: arc-seconds
		units_code_g=Integer.parseInt(type_a.substring(528, 534).trim());

		// Units for elevation
		// 1: feet
		// 2: meters
		units_code_e=Integer.parseInt(type_a.substring(534, 540).trim());

		// Number of sides of the polygon
		n_sides=Integer.parseInt(type_a.substring(540, 546).trim());

		// the four corners
		st=new StringTokenizer(type_a.substring(546, 738).trim());
		corners=new double[4][2];
		for(int i=0; i<4; i++) {
			for(int j=0; j<2; j++) {
				corners[i][j]=arcSecToDMS(grokEngDouble(st.nextToken()));
			}
		}

		// Elevation
		min_elevation=grokEngDouble(type_a.substring(738, 763).trim());
		max_elevation=grokEngDouble(type_a.substring(763, 786).trim());

		// Counterclockwise angle from the primary axis of ground
		// planimetric referenced to the primary axis of the DEM local
		// reference system.
		angle=Double.valueOf(type_a.substring(786, 810).trim()).doubleValue();


		// Accuracy code
		accuracy_code=Integer.parseInt(type_a.substring(810, 816).trim());

		// OK, now the spacial resolution.
		spacial_resolution=new double[3];
		spacial_resolution[0]=grokEngDouble(type_a.substring(816, 828).trim());
		spacial_resolution[1]=grokEngDouble(type_a.substring(828, 840).trim());
		spacial_resolution[2]=grokEngDouble(type_a.substring(840, 852).trim());

		// Rows and columns
		rows=Integer.parseInt(type_a.substring(852, 858).trim());
		columns=Integer.parseInt(type_a.substring(858, 864).trim());

		// That's the end of the A block.
	}
}

// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TempSet.java,v 1.2 2000/10/15 10:04:47 dustin Exp $

package net.spy.temperature;

import java.lang.*;
import java.util.*;

/**
 * This object represents a sample.
 */

public class TempSet {
	protected Date ts = null;
	protected int sensor_id = -1;
	protected float sample = -1.0f;

	public TempSet(Date ts, int sensor_id, float sample) {
		this.ts=ts;
		this.sensor_id = sensor_id;
		this.sample = sample;
	}

	public String toSQL() {
		String ret = null;

		ret="insert into samples (ts, sensor_id, sample) values(\n"
			+ "\t'" + ts + "', " + sensor_id + ", " + sample + ")\n";
		return(ret);
	}
}

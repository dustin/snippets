// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
// $Id: kittycode.js,v 1.1 2000/09/19 20:47:38 dustin Exp $
// Any work derived from this code must retain the above copyright.
// Please send all modifications of this script to me.

// Someone just let me know if there's a hash in javascript...
function lookupCode(input) {

	var done=false;
	var retval=' > ' + input + ' < ';

	var keys = new Array('b','D','f','j','Ch','Dh','n','P','0','1','CN',
					'r','DN','2','EN','3','T','6','v','C3','7','W','D3',
					'E3','X','Y','Z','z','Cx','Dx','CG');
	var vals = new Array('3','4','2','1','3','7','0','9','7','6','1','7',
					'5','5','9','4','8','9','6','0','8','3','4','8','2',
					'1','0','5','2','6','1');

	for(i=0; i<keys.length && !done; i++) {
		if(keys[i] == input) {
			retval=vals[i];
			done=true;
		}
	}

	return retval;
}

function decodeChunk(input, chunk) {

	if(chunk.length==2) {
		input += lookupCode(chunk);
	} else if(chunk.length==3) {
		input += lookupCode(chunk.substring(0, 2));
		input += lookupCode(chunk.substring(2, 3));
	} else if(chunk.length>=4) {
		input += lookupCode(chunk.substring(0, 2));
		input += lookupCode(chunk.substring(2, 3));
		input += lookupCode(chunk.substring(3, 4));
		// We need to go again if the input was greater than six
		input=decodeChunk(input, chunk.substring(4));
	} else if(chunk.length==0) {
		return input;
	} else {
		return "INVALID CHUNK!";
	}


	return(input);
}

function kittyCode() {
	// Make sure there's something in there before we do this.
	if(document.form.kittyinput.value.length > 0) {
		var parts=document.form.kittyinput.value.split('.');
		// Valid data will have five parts (the first and last are empty)
		// Parts are as follows:
		// 0 = empty
		// 1 = id
		// 2 = type
		// 3 = code
		// 4 = empty
		if(parts.length == 5) {
			// We just care about the actual scanned code right now
			document.form.kittyinput.value=decodeChunk("", parts[3]);
		}
	}
}

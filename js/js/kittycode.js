// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
// $Id: kittycode.js,v 1.2 2000/09/20 03:42:24 dustin Exp $
// Any work derived from this code must retain the above copyright.
// Please send all modifications of this script to me.

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
			document.form.kittyinput.value=decodePart(parts[3]);
		}
	}
}

function decodePart(str) {
	var m = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-";
	var result = "";
	var packer = 0;
	var count = 0;

	var i = 0;
	for (i=0; i < str.length; i++) {
		// Get the offset to the current character in our map
		var x = m.indexOf(str.charAt(i));

		// For invalid characters, point them out really well!
		if(x<0) {
			result += " > " + str.charAt(i) + " < ";
			continue;
		}

		// only count valid characters...why not?
		count++;

		// Pack them bits.
		packer = packer << 6 | x

		// every four bytes, we have three valid characters.
		if (count == 4) {
			result += String.fromCharCode((packer >> 16) ^ 67);
			result += String.fromCharCode((packer >> 8 & 255) ^ 67);
			result += String.fromCharCode((packer & 255) ^ 67);
			count=0; packer=0;
		}

	}

	// Now, deal with the remainders
	if(count==2) {
		result += String.fromCharCode((( (packer << 12) >> 16) ^ 67));
	} else if(count == 3) {
		result += String.fromCharCode(( (packer << 6) >> 16) ^ 67);
		result += String.fromCharCode(( (packer << 6) >> 8 & 255) ^ 67);
	}
	return result;
}

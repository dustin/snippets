// Copyright (c) 2000  Dustin Sallings dustin@spy.net
// Any work derived from this code must retain the above copyright.
// Please send all modifications of this script to me.
//
// modified by Andrew Irwin  irwin@aesop.rutgers.edu

// The following fields are required:
//
// Input:
// kittycode: will be replaced with the decoded value
//
// Output
// CatID:   Will be replaced with the unique ID of the CueCat
// type:    Will be replaced with the type of barcode scanned
// code:    Will be replaced with the decoded value
// isbn:    Will be replaced with the ISBN in the case of an ISBN code
// cuecat:  Will be replaced with a decoded CueCat cue
// country: Will be replaced with the country in the case of an IB5
// price:   Will be replaced with the price in the case of an IB5 code
// url:     Used for sending the user to a specific location based on
//          decoded ISBN
// bookChoice:      Where to lookup book information
// ISBNAutoLookup:  Whether to automatically send go to the ISBN page
// upcAutoLookup:   Whether to automatically send go to the UPC page

function kittyCode() {
	// Make sure there's something in there before we do this.
	if(document.form.kittyinput.value.length > 0) {
		var parts=document.form.kittyinput.value.split('.');

		// Valid data will have five parts (the first and last are empty)
		// Parts are as follows:
		// 0 = empt	// 1 = id	// 2 = type	// 3 = code	// 4 = empty
		if(parts.length == 5) {
			document.form.CatID.value=decodePart(parts[1]);
                  var code=decodePart(parts[3]);
			document.form.code.value=code;
			var type= decodePart(parts[2]);
			document.form.type.value=type;
			document.form.kittyinput.value=code;

			// clear earlier results if any
			document.form.isbn.value="";
			document.form.price.value="";
			document.form.cuecat.value="";
			document.form.country.value="";
			// document.upcform.upc.value="";

            var a = /IBN/; var b = /IB5/; var c = /C39/; var a1=/UA5/;
            if (a.test(type)){
			document.form.isbn.value=code.substr(3,9);
            } else if (c.test(type)) {
                  document.form.isbn.value=code;
            } else if (b.test(type)) {
				document.form.isbn.value=code.substr(3,9);
				document.form.price.value=toPrice(code.substr(14));
				var country="";
				if (code.substr(13,1) ==5) {country="USA";
				} else if (code.substr(13,1) ==6) {country="CANADA";
				} else if (code.substr(13,1) ==9) {country="OTHER"; }
				document.form.country.value=country;
			} else if (a1.test(type)) {
				document.form.isbn.value = upcISBNmap(code.substr(0,6))
					+code.substr(12,5);
				document.form.price.value=toPrice(code.substr(7,4));
				var country="";
				if (code.substr(6,1) ==5) {
					country="USA";
				} else if (code.substr(6,1) ==6) {
					country="CANADA";
				} else if (code.substr(6,1) ==9) {
					country="OTHER";
				}
				document.form.country.value=country;
			}

			if( a.test(type) | b.test(type) | c.test(type) | a1.test(type)) {
				document.form.isbn.value = checkISBN(document.form.isbn.value);
				if (document.form.ISBNAutoLookup.checked)
					formHandler();
			}
			var d = /CC/;
			if (d.test(type)) { // we got a cuecat code - all CC! in RS cat.
				document.form.cuecat.value="CC "
				+ decodeCC(type[2]) + decodeCC(code);
				document.form.code.value="";
			}

			if (((type == "UPA") | (type=="UPE") )
				& (document.form.upcAutoLookup.checked)) {
				window.location.href=
					"http://www.upcdatabase.com/item.pl?upc="+code;
			}
		} // Real CueCat input
	} // Input
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

function decodeCC(str) {
	var m = "0123456789ABCDEF";
	var result = "";
	var i = 0;
	for (i=0; i < str.length; i++) {
		var x = str.charCodeAt(i)-32;
		if (x<10) x = "0"+x;
		result += x+" "; // convert x to string
	}
	return result;
}

function checkISBN(str) {
	var i;
	var sum = 0;
	var len = str.length;
	var result = "";
	if ((len>10) | (len<9) ) {
		return result = "INVALID";
	} else {
		len=9;
	}
	for (i=0; i<len; i++) {
		sum += (i+1)* (str.charCodeAt(i)- 48);
	}
	result = sum % 11;
	str = str.substr(0,9);
	if (result==10) {
		str+="X";
	} else {
		str+= result;
	}
	return str;
}

function toPrice(str) {
	var price=0.0;
	var j;
	for (j=0; j<str.length; j++) {
		price *= 10;
		price += str.charCodeAt(j)-48;
	}
	return price/100;
}

function amazonURL(str) {
	return "http://www.amazon.com/exec/obidos/ASIN/"+str+"/";
}

function nuURL(str) {
	return "http://isbn.nu/"+str+"/price";
}

function chaptersURL(str) {
	return "http://www.chapters.ca/books/details/default.asp?ISBN="+str;
}

function bnURL(str) {
	return "http://shop.barnesandnoble.com/bookSearch/isbnInquiry.asp?isbn="+str;
}


function formHandler(){
	var temp = document.form.bookChoice.options[document.form.bookChoice.selectedIndex].value;

	if (temp=="amazon") {
		document.form.url.value = amazonURL(document.form.isbn.value);
	} else if (temp=="isbn.nu") {
		document.form.url.value = nuURL(document.form.isbn.value);
	} else if (temp=="chapters") {
		document.form.url.value = chaptersURL(document.form.isbn.value);
	} else if (temp=="bn") {
		document.form.url.value = bnURL(document.form.isbn.value);
	}

	window.location.href = document.form.url.value;
}


function upcHandler(){
	if ((document.form.type.value == "UPA") |
		(document.form.type.value=="UPE") ) {
		window.location.href=
			"http://www.upcdatabase.com/item.pl?upc="+document.form.code.value;
	}
}


// sample cue cat scans from RS catalogue
// .C3nZC3nZC3nZC3jYDNrZC3nX.aabI.y2nIy244.	C 01 00 00 01 00 13 91
// .C3nZC3nZC3nZC3jYDNrZC3nX.aabI.y2nIy2HW.	C 01 00 00 01 00 11 19
// .C3nZC3nZC3nZC3jYDNrZC3nX.aabI.y2nIy2H3.	C 01 00 00 01 00 11 20
// .C3nZC3nZC3nZC3jYDNrZC3nX.aabI.y2nIy2fG.	C 01 00 00 01 00 02 03
// .C3nZC3nZC3nZC3jYDNrZC3nX.aabI.y2nIy28B.	C 01 00 00 01 00 12 56

// document.form.isbn.value=code.replace(/978/, "");

function upcISBNmap(str) {

	var upcisbn=[
		"014794","08041",
		"018926","0445",
		"027778","0449",
		"037145","0812",
		"042799","0785",
		"043144","0688",
		"044903","0312",
		"045863","0517",
		"046594","0064",
		"047132","0152",
		"051487","08167",
		"051488","0140",
		"060771","0002",
		"065373","0373",
		"070992","0523",
		"070993","0446",
		"070999","0345",
		"071001","0380",
		"071009","0440",
		"071125","088677",
		"071136","0451",
		"071149","0451",
		"071152","0515",
		"071162","0451",
		"071268","08217",
		"071831","0425",
		"071842","08439",
		"072742","0441",
		"076714","0671",
		"076783","0770",
		"076814","0449",
		"078021","0872",
		"079808","0394",
		"090129","0679",
		"099455","0061",
		"099769","0451"
	];

	// "076783","0770",  // McClelland-Bantam  or?? "0553",

	var j;
	var result="";

	for (j=0; j<upcisbn.length; j+=2) {
		if (upcisbn[j]==str) {
			result=upcisbn[j+1];
			break;
		}
	}

	if (result=="")
		result=str;
	return(result);
}

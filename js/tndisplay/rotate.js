// Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
var rollTimer=null;
var animationTimer=null;

var TIMEOUT=2000;

var imgInfo=new Object();
var pairs=new Array();

function makeImgInfo(name, width, left) {
	var storage=new Object();
	storage.width=width;
	storage.left=left;
	storage.el=document.getElementById(name);
	imgInfo[name]=storage;
}

function addPair(img1, img2) {
	var storage=new Object();
	storage.img1=img1;
	storage.img2=img2;
	storage.current=img1;
	pairs.push(storage);
}

function animateFlip(from, to, crossed, start, until) {
	var now=new Date().getTime();
	var duration=until-start;
	var togo=(until-now);
	if(togo > 0) {
		var distance=Math.abs((duration/2) - togo);

		var widthFactor=distance*2 / duration;

		if(togo < (duration / 2)) {
			if(!crossed) {
				crossed=true;
				imgInfo[to].el.style.display="";
				imgInfo[from].el.style.display="none";
			}

			var itemWidth=(widthFactor * imgInfo[to].width);
			var leftSIDE=imgInfo[to].left + ((imgInfo[to].width-itemWidth)/2);
			imgInfo[to].el.style.width=itemWidth + "px";
			imgInfo[to].el.style.left=leftSIDE+"px";
		} else {
			var itemWidth=(widthFactor * imgInfo[from].width);
			var leftSIDE=imgInfo[from].left
				+ ((imgInfo[from].width-itemWidth)/2);
			imgInfo[from].el.style.width=itemWidth + "px";
			imgInfo[from].el.style.left=leftSIDE+"px";
		}

		animationTimer=setTimeout("animateFlip('" + from
			+ "', '" + to + "', " + crossed
			+ ", " + start + ", " + until + ");", 25);
	} else {
		imgInfo[to].el.style.width=imgInfo[to].width + "px";
		imgInfo[to].el.style.left=imgInfo[to].left + "px";
	}
}

function swapImages() {
	// Randomly select an image to flip
	var n = Math.round(Math.random()*(pairs.length-1));
	var pair=pairs[n];

	var nextImage="";

	if(pair.current == pair.img1) {
		nextImage=pair.img2;
	} else {
		nextImage=pair.img1;
	}

	var d=new Date().getTime();
	animateFlip(pair.current, nextImage, false, d, d+1000);

	pair.current=nextImage;
	rollTimer=setTimeout("swapImages();", TIMEOUT);
}

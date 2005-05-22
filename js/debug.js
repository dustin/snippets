function debug(msg) {
	if (!debug.box) {
		debug.box = document.createElement("div");
		debug.box.setAttribute("style", "background-color: white; " +
			"font-family: monospace; " +
			"border: solid black 3px; " +
			"position: absolute;top:300px;" +
			"padding: 10px;");
		document.body.appendChild(debug.box);
		debug.box.innerHTML = "<h1 style='text-align:center'>Debug Output</h1>";
	}
	var p = document.createElement("p");
	p.appendChild(document.createTextNode(msg));
	debug.box.appendChild(p);
}

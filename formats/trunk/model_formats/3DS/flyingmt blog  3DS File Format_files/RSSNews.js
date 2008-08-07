function __RSSNews(category, limit) {
	this.category = category;
	this.limit = limit;
	this.URL = new Array();
	this.img = new Image();
	this.img.src = pluginURL + "/image/loading.gif";

	this.setURL = function() {
		for(var i = 0; i < eval(this.category).setURL.arguments.length; i++) {
			this.URL.push(eval(this.category).setURL.arguments[i]);
		}
	}

	this.getNext = function() {
		var getURL = this.URL.shift();
		this.URL.push(getURL);

		this.getRSS(getURL);
	}

	this.getRSS = function(getURL) {
		var REQ = new newXMLHttpRequest();

		REQ.onreadystatechange = function() {
			if(REQ.readyState == 4) {
				if(REQ.status == 200) {
					var category = REQ.responseXML.getElementsByTagName("channel")[0].getAttribute("category");
					var item = REQ.responseXML.getElementsByTagName("item");

					var cnt = (item.length > eval(category).limit) ? eval(category).limit : item.length;

					for(var i = 0, _contents = ""; i < cnt; i++) {
						try {
							var title = item[i].getElementsByTagName("title")[0].firstChild.nodeValue;
							if(title.trim() == "") {
								title = item[i].getElementsByTagName("title")[0].textContent;
							}
							title = title.trim().replace('"', '&quot;').replace(/<.*>/g, '');

							if(!title) { cnt++; continue; }
						} catch(e) { cnt++; continue; }
						try {
							var link = item[i].getElementsByTagName("link")[0].firstChild.nodeValue;
						} catch(e) { var link = ""; }
						try {
							var description = item[i].getElementsByTagName("description")[0].firstChild.nodeValue;
							if(description.trim() == "") {
								description = item[i].getElementsByTagName("description")[0].textContent;
							}
							description = description.trim().replace('"', '&quot;').replace(/<.*>/g, '');
						} catch(e) { var description = title; }

						_contents += '<li><a href="' + link + '" onclick="window.open(this.href); return false;" title="' + description + '">' + title + '</a></li>';
					}

					document.getElementById("RSSNewsList_" + category).innerHTML = _contents;
					document.getElementById("RSSNewsButton_" + category).src = pluginURL + "/image/reload.gif";
				} else {
					document.getElementById("RSSNewsButton_" + category).src = pluginURL + "/image/reload.gif";
					alert(REQ.statusText);
				}
			}
		};

		document.getElementById("RSSNewsButton_" + this.category).src = this.img.src;

		REQ.open("GET", pluginURL + "/RSSNews.php?url=" + getURL + "&category=" + this.category, true);
		REQ.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		REQ.send(null);
	}
}

function newXMLHttpRequest() {
	var xmlReq = false;

	if(window.XMLHttpRequest) {
		xmlReq = new XMLHttpRequest();
	} else if(window.ActiveXObject) {
		try {
			xmlReq = new ActiveXObject("Msxml2.XMLHTTP");
		} catch(e1) {
			try {
				xmlReq = new ActiveXObject("Microsoft.XMLHTTP");
			} catch(e2) {
			}
		}
	}

	return xmlReq;
}

function addEvent(obj, event, listener) {
	try	{
		obj.addEventListener(event, listener, false);
	} catch(e) {
		try {
			obj.attachEvent("on"+event, listener);
		} catch(e) { }
	}
}

String.prototype.trim = function () {
	return this.replace(/^\s*/g, "").replace(/\s*$/g, "");
}
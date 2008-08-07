function viewBookmarkForm(bookmarkurl, gubun){
	getInnerHtml(bookmarkurl, gubun);
	document.getElementById('bookmark_handle').style.display ="block";
	ttDesk_centerLayer("bookmark_handle","550","570");
	loadScript(pluginURL+"/eolin_drag.js");
	//YAHOO.example.DDApp.init();
}

function viewMemberForm(bookmarkurl){
	loadScript(pluginURL+"/eolin_drag.js");
	var oCreateDiv = document.getElementById('eolin_Bookmark');

	if (oCreateDiv != null) {
		oCreateDiv.innerHTML = getInnerHtml2(bookmarkurl);
		oCreateDiv.style.display="inline";
	} else {
		oCreateDiv = document.createElement("DIV");
	    oCreateDiv.id = 'eolin_Bookmark';
		oCreateDiv.style.left = '10';
		oCreateDiv.style.top = '10';    
	    oCreateDiv.style.zIndex = '99999999';
		document.body.appendChild(oCreateDiv);
		oCreateDiv.innerHTML = getInnerHtml2(bookmarkurl);
	}
		ttDesk_centerLayer("bookmark_handle","550","570");
		//YAHOO.example.DDApp.init();
}

function loadScript(src) {
	var s = document.getElementsByTagName('HEAD')[0].appendChild(document.createElement('script'));
	s.type = 'text/javascript';
	s.src = src;
}

function __checkAndGet(value, defaultValue) {
	if (typeof value != "undefined" && value != null && value != "") {
		return value;
	}
	return defaultValue;
}

function getInnerHtml(bookmarkurl, gubun) {	

	if (document.getSelection) {
			var description = document.getSelection();
	} else if (document.selection && document.selection.createRange) {
			var range = document.selection.createRange();  
			var description = range.text;
	} else { var description = ''; };	
	
	if(gubun == "bookmark") 		var src = "http://www.eolin.com/bookmark/post.php?frame=iframe&"+bookmarkurl;
	else 		var src = "http://www.eolin.com/bookmark/vote.php?frame=iframe&"+bookmarkurl;
	src = src.replace(/'/g, '%27');
	src = src.replace(/"/g, '%22');
	
	document.getElementById('eolin_iframe').src=src;
	return;	
}


function getInnerHtml2(bookmarkurl) {	

	var src = "http://www.eolin.com/member/blog/?"+bookmarkurl;
	src = src.replace(/'/g, '%27');
	src = src.replace(/"/g, '%22');
	
	var html = "";
	html += "<div id='bookmark_handle' style='position:absolute; top:10px; left:105px; height:570px; width:550px; z-index:9999999; background:#FFF url(http://file.eolin.com/image/plugin_pop.gif)  left top repeat-x; border:1px solid #666;'>"; 
	html += "<div id='mini_bookmark_handle' style='background:url(http://file.eolin.com/image/plugin_logo.gif) 3px 3px no-repeat; text-align:left; z-index:9999998; width:432px; height:48px; cursor:move; float:left'></div>";	
	html += "<div id='mini_close' style='margin:11px 10px 0 0; float:right'><a href='javascript:closePopup()' title='close'><img src='http://file.eolin.com/image/icon_close.gif' border='0'  alt='close' /></a></div>";	
	html += "<div style='clear:both;'><iframe src='" + src + "' width='550' height='510' scrolling='no' frameborder='no'></iframe></div>";
    html += "</div>";
	return html;	
}



function closePopup() {
	document.getElementById('bookmark_handle').style.display ="none";
}


function ttDesk_getPageSize(){
	
	var xScroll, yScroll;
	
	if (window.innerHeight && window.scrollMaxY) {	
		xScroll = document.body.scrollWidth;
		yScroll = window.innerHeight + window.scrollMaxY;
	} else if (document.body.scrollHeight > document.body.offsetHeight){ // all but Explorer Mac
		xScroll = document.body.scrollWidth;
		yScroll = document.body.scrollHeight;
	} else { // Explorer Mac...would also work in Explorer 6 Strict, Mozilla and Safari
		xScroll = document.body.offsetWidth;
		yScroll = document.body.offsetHeight;
	}
	
	var windowWidth, windowHeight;
	if (self.innerHeight) {	// all except Explorer
		windowWidth = self.innerWidth;
		windowHeight = self.innerHeight;
	} else if (document.documentElement && document.documentElement.clientHeight) { // Explorer 6 Strict Mode
		windowWidth = document.documentElement.clientWidth;
		windowHeight = document.documentElement.clientHeight;
	} else if (document.body) { // other Explorers
		windowWidth = document.body.clientWidth;
		windowHeight = document.body.clientHeight;
	}	
	
	// for small pages with total height less then height of the viewport
	if(yScroll < windowHeight){
		pageHeight = windowHeight;
	} else { 
		pageHeight = yScroll;
	}

	// for small pages with total width less then width of the viewport
	if(xScroll < windowWidth){	
		pageWidth = windowWidth;
	} else {
		pageWidth = xScroll;
	}

	arrayPageSize = new Array(pageWidth,pageHeight,windowWidth,windowHeight) 
	return arrayPageSize;
}

function ttDesk_getPageScroll(){

	var yScroll;

	if (self.pageYOffset) {
		yScroll = self.pageYOffset;
	} else if (document.documentElement && document.documentElement.scrollTop){	 // Explorer 6 Strict
		yScroll = document.documentElement.scrollTop;
	} else if (document.body) {// all other Explorers
		yScroll = document.body.scrollTop;
	}

	arrayPageScroll = new Array('',yScroll) 
	return arrayPageScroll;
}

function ttDesk_centerLayer(id,w,h){
	var top, left, w, h;
	var obj = document.getElementById(id);
	var arrayPageSize = ttDesk_getPageSize();	
	var arrayPageScroll = ttDesk_getPageScroll();	
	var pagewidth  = arrayPageSize[0];
	var pageheight  = arrayPageSize[1];
	var windowwidth  = arrayPageSize[2];		
	var windowheight  = arrayPageSize[3];	
	var curscroll = arrayPageScroll[1];
	toppx = curscroll + ((windowheight -h)/ 2);
	leftpx = (pagewidth -w) /2;
	obj.style.top = toppx+"px";
	obj.style.left = leftpx+"px";
	
}
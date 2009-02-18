/* xapi object for sever signals */
function srvapi(){}
xapi = new srvapi();

function ajax(json, url){
	var data = "json=" + urlEncode(JSON.stringify(json));
	var d = doXHR( url, {method: 'POST', sendContent: data, headers: {"Content-Type": "application/x-www-form-urlencoded"}});
	d.addCallbacks(cbResult, cbError);
}

function cbResult(XHR){
	log("Ajax Result: " + XHR.responseText + " status: " + XHR.status);
	json = evalJSONRequest(XHR);
	log("Ajax event: " + json.event);
	signal(xapi, json.event, json);
}

function cbError(result){
	log("Ajax Error: " + result.number);
	if(result.number == "403"){
	    alert("Session has expired! Login again please.");
	    logOut();
        }else if(result.number == "404"){
	    alert("This function is not available (possible You have to log in?)");
        }else if(result.number == "500"){
	    alert("Unexpected error occured while processing request");
        }else if(result.number == "501"){
	    alert("Function not implemented");
	}
}


/* Simple get content into id: get('testResult', '/hello.html'); */
function get(id, url){
	var d = doXHR( url, {method: 'GET'});
	d.addCallbacks(function(XHR){cbGetResult(id,XHR)}, cbGetError);
}

function cbGetResult(id, XHR){
	log("Get Result: " + XHR.responseText + " status: " + XHR.status);
	$(id).innerHTML = XHR.responseText;
}

function cbGetError(result){
	log("Get Error: " + result.number);
}

function rand(upper){
	return Math.floor(Math.random()*upper) + 50
}

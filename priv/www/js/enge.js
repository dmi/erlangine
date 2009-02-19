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


/*
   Simple get content into id.innerHTML:
      get('testResult', '/hello.html');

   hello.html may contain plain html, mixed with <script>...</script> elements,
   which will be sequentally evaluated.

   calls tuneInterface(DOM), if redefined (default - false)
   calls getCompleted() when no more callbacks expected, if redefined (default - false)
   
*/

var getCount = 0;
var getCompleted = false;
var tuneInterface = false;

function get(id, url){
	getCount++;
	var d = doXHR( url, {method: 'GET'});
	d.addCallbacks(function(XHR){cbGetResult(id,XHR)}, cbGetError);
}

function cbGetResult(id, XHR){
	//log("Get Result status: " + XHR.status);
        var parts = XHR.responseText.split('<script>');
	var html = parts[0];
	for(i = 1; i<parts.length; i++){
	    var script = parts[i].split('</script>',2);
	    html += script[1];
	    //log("Get script: " + script[0]);
	    eval(script[0]);
	}
	var elt = document.createElement("div");
	elt.innerHTML = html.replace(/\n\n/g,'\n');
	if(tuneInterface)tuneInterface(elt);
	swapDOM(id, elt);
	getCount--;
	if((getCount==0) && getCompleted) getCompleted();
}

function cbGetError(result){
	log("Get Error: " + result.number);
	getCount--;
	if((getCount==0) && getCompleted) getCompleted();
}

function rand(upper){
	return Math.floor(Math.random()*upper) + 50
}

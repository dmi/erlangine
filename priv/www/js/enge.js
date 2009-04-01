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
	    statusM("Session has expired! Login again please.");
	    logOut();
        }else if(result.number == "404"){
	    statusM("This function is not available (possible You have to log in?)");
        }else if(result.number == "500"){
	    statusM("Unexpected error occured while processing request");
        }else if(result.number == "501"){
	    statusM("Function not implemented");
	}
}


/*
   Simple insert content into id.innerHTML from url:

     insert('testResult', '/hello.html');

   hello.html may contain plain html, mixed with <script>...</script> elements,
   which will be sequentally evaluated after inserting html into page.

   calls tuneInterface(DOM) to dynamically tune loaded content after inserting, if redefined (default - false)
   calls insertCompleted() when no more callbacks expected, if redefined (default - false)

   in scripts, external functions must be defined as
     
     fname = function(){};
*/

var insertCount = 0;
var insertCompleted = false;
var tuneInterface = false;

function insert(id, url){
	insertCount++;
	var d = doXHR( url, {method: 'GET'});
	d.addCallbacks(function(XHR){cbInsertResult(id,XHR)}, cbInsertError);
}

function cbInsertResult(id, XHR){
	insertCount--;
	//log("Get Result status: " + XHR.status);
    var parts = XHR.responseText.split('<script>');
	var html = parts[0];
	var scripts = [];
	for(i = 1; i<parts.length; i++){
	    var script = parts[i].split('</script>',2);
	    html += script[1];
	    //log("Get script: " + script[0]);
	    scripts[i] = script[0];
	}
	var elt = document.getElementById(id);
	//log("Get html: " + html);
	elt.innerHTML = html.replace(/\n\n/g,'\n');
	if(tuneInterface)tuneInterface(elt);
	for(i in scripts)
        try{ eval(scripts[i]) }
        catch(E){ log("insert script error: " + E) };
	if((insertCount==0) && insertCompleted) insertCompleted();
}

function cbInsertError(result){
	insertCount--;
	log("Get Error: " + result.number);
	if((insertCount==0) && insertCompleted) insertCompleted();
}

function rand(upper){
	return Math.floor(Math.random()*upper) + 50
}

statusM = function(str){
    alert(str)
}

function quote(str){
    return str.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;')
}
function dequote(str){
    return str.replace(/&lt;/g,'<').replace(/&gt;/g,'>').replace(/&quot;/g,'"').replace(/&amp;/g,'&')
}

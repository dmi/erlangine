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
		statusM("This function is not available (possible You are not logged on?)");
	}else if(result.number == "500"){
		statusM("Unexpected error occured while processing request");
	}else if(result.number == "501"){
		statusM("Function not implemented");
	}
}


/*
   Insert content into id.innerHTML from url:

     insertChunks([{id: 'testResult', chunk: '/hello.html'}, ...]);

   hello.html may contain plain html, mixed with <script>...</script> elements,
   which will be sequentally evaluated after inserting html into page.

   If defuned tuneInterface(DOM) - it will be called to dynamically tune loaded content just after inserting
   If defined insertCompleted() - it will be called after all chunks are processed

   In scripts, external functions must be defined as
     
     fname = function(){};
*/

// chunks is [{id: ID, file: FILE}]
function insertChunks(chunks){
	ajax({module: "chunks", action: "load", data: chunks}, "/enge");
}

// should be connected to chunks:load-ok
function cbChunksOk(json){
    forEach(json.reply,
            function(x){
				try{ insertChunk(x.id, Base64.decode(x.chunk)) }
				catch(E){ log("insert chunk error: " + E) };
            });
    if(insertCompleted)insertCompleted();
}

MochiKit.Signal.connect(window, "onload", function(){ connect(xapi, 'chunks:load-ok', cbChunksOk); });

var insertCompleted = false;
var tuneInterface = false;

function insertChunk(id, chunk){
    // log("get chunk id: "+ id);
    var parts = chunk.split('<script>');
	var html = parts[0];
	var scripts = [];
	for(i = 1; i<parts.length; i++){
	    var script = parts[i].split('</script>',2);
	    html += script[1];
	    //log("Get script: " + script[0]);
	    scripts[i] = script[0];
	}
        if(id){
            var elt = document.getElementById(id);
            //log("Get html: " + html);
			if(elt){
				elt.innerHTML = html.replace(/\n\n/g,'\n');
				if(tuneInterface)tuneInterface(elt);
			}
        }
	for(i in scripts)
        try{ eval(scripts[i]) }
        catch(E){ log("insert script error: " + E) };
}

var insertCount = 0;

// rudimentary function for one-file insert
function insert(id, url){
	insertCount++;
	var d = doXHR( url, {method: 'GET'});
	d.addCallbacks(function(XHR){cbInsertResult(id,XHR)}, cbInsertError);
}

function cbInsertResult(id, XHR){
	insertCount--;
	//log("Get Result status: " + XHR.status);
    insertChunk(id, XHR.responseText);
}

function cbInsertError(result){
	insertCount--;
	log("Get Error: " + result.number);
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
// XXX possible to check sanity here
function dequote(str){
    return str.replace(/&lt;/g,'<').replace(/&gt;/g,'>').replace(/&quot;/g,'"').replace(/&amp;/g,'&')
}

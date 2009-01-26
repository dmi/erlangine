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

function rand(upper){
	return Math.floor(Math.random()*upper) + 50
}

function logIn(){
    var json = {
	    "module": "login",
	    "action": "login",
	    "data": {
		"uid": $('loginName').value,
		"password": $('loginPassword').value
	    }
    }
    ajax(json, "/enge");
};

function logOut(){
    var json = {
	    "module": "login",
	    "action": "logout"
    }
    ajax(json, "/enge");
};

function registerAccount(){
    var json = {
	    "module": "register",
	    "action": "account",
	    "data": {
		"name": $('regName').value,
		"uid": $('regLogin').value,
		"password": $('regPassword').value,
		"repeat": $('regPassword2').value,
		"email": $('regMail').value,
		"captchalink": $('regCaptchaLink').value,
		"captchacode": $('regCaptcha').value
	    }
    }
    ajax(json, "/enge");
}; 

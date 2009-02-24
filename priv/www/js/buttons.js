/*
   CSS button converter 
   converts all
       <a id=docSave class=button>Save</a>
   into gray buttons
       <a id=docSave class=gbutton>Save</a>
   into green buttons
   for parent DOM element 'par';

   uses MochiKit
   see buttons.css
*/

function buttonActivate(par){
	map(
		function(elt){
		    elt.innerHTML="<span>" + elt.innerHTML + "</span>";
		    elt.href="#";
		    connect(elt, 'onclick', function(){elt.blur()});
		},
		findChildElements(par, ['.gbutton']))
	map(
		function(elt){
		    elt.innerHTML="<span>" + elt.innerHTML + "</span>";
		    elt.href="#";
		    connect(elt, 'onclick', function(){elt.blur()});
		},
		findChildElements(par, ['.button']))
}

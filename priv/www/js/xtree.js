/* Tree view for MochiKit */

// Create an array of nodes, insert into elt, activate
// You got a fully ready structure
// jsona is [json]
// json is {id, name, anno, childs}
// childs is [json] | []
function xtreeCreateStruct(elt, jsona, UserCB){
    elt = $(elt);
    elt.innerHTML = "";
    appendChildNodes(elt, map(xtreeCreateNode, jsona));
    xtreeActivate(elt, UserCB);
}

// activate xtree structure with callbacks
function xtreeActivate(Id, UserCB){
    map(
	function(elt){log(elt);connect(elt, 'onclick', xtreeToggle)},
	findChildElements($(Id), ['.xtreePlus']));
    if(UserCB != undefined){
	map(
	    function(elt){log(elt);connect(elt, 'onclick', UserCB)},
	    findChildElements($(Id), ['.xtreeItem']));
    }
}

// Create one node with subnodes
// xtreeActivate must be applied explicitly
// json is {id, name, anno, childs}
// childs is [json] | []
function xtreeCreateNode(json){
    return DIV({'class': 'xtreeNode'},
	       IMG({'src': "images/minus.gif", 'class': 'xtreePlus'}),
	       SPAN({'id': json.id, 'class': 'xtreeItem', 'title': json.anno}, json.name),
	       DIV({'class': 'xtreeChilds'},
	       map(xtreeCreateNode, json.childs)));
}

function xtreeToggle(){
    var par = this.parentNode;
    var childs = getFirstElementByTagAndClassName("*", 'xtreeChilds', par);
    if(getNodeAttribute(this, 'src') == "images/plus.gif"){
	showElement(childs);
	setNodeAttribute(this, 'src', "images/minus.gif");
    }else{
	hideElement(childs);
	addElementClass(childs, 'xtreeCollapsed');
	setNodeAttribute(this, 'src', "images/plus.gif");
    }
}

/*
Example:
<script>
var xtreeTestJSON = [
{"id": "i1",
 "name": "item 1",
 "anno": "the item",
 "childs": [
    {"id": "i1-1",
     "name": "item 1-1",
     "anno": "the item",
     "childs": [
	{"id": "i1-1-1",
	 "name": "item 1-1-1",
	 "anno": "the item",
	 "childs": [
	 ]},
     ]},
 ]},
{"id": "i2",
 "name": "item 2",
 "anno": "the item",
 "childs": [
 ]},
];

xtreeCreateStruct('xtreeTest', xtreeTestJSON, function(){log('touched tree leaf: ' + this.id)});
</script>

<div id=xtreeTest></div>
*/

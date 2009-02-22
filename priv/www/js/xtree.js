/* Tree view for MochiKit */

// Create an array of nodes, insert into elt, activate
// You got a fully ready structure
// jsona is [json]
// json is {id, name, anno, childs}
// childs is [json] | []
function xtreeCreateStruct(elt, jsona){
    elt = $(elt);
    elt.innerHTML = "";
    appendChildNodes(elt, map(xtreeCreateNode, jsona));
    xtreeActivate(elt);
}

// activate xtree structure with callbacks
function xtreeActivate(Id){
    map(
	function(elt){log(elt);connect(elt, 'onclick', xtreeToggle)},
	findChildElements($(Id), ['.xtreePlus']));
}

// Create one node with subnodes
// xtreeActivate must be applied explicitly
// json is {id, name, anno, childs}
// childs is [json] | []
function xtreeCreateNode(json){
    return DIV({'id': json.id, 'class': 'xtreeNode'},
	       IMG({'src': "images/minus.gif", 'class': 'xtreePlus'}),
	       SPAN({'class': 'xtreeItem', 'title': json.anno}, json.name),
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

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

// Insert node into structure as child of id
// possible, node can be an array
// node must be activated explicitly, see Example below
function xtreeInsertNode(id, node){
    var childs = findChildElements($(id).parentNode, ['.xtreeChilds']);
    appendChildNodes(childs[0], node);
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

function cbTree(){
    log('touched tree leaf: ' + this.id)
}

xtreeCreateStruct('xtreeTest', xtreeTestJSON, cbTree);

var node = xtreeCreateNode({'id': "i2-1", 'name': "item 2-1", 'anno': "the item", 'childs': []});
xtreeActivate(node, cbTree);
xtreeInsertNode('i2', node);

</script>

<div id=xtreeTest></div>
*/

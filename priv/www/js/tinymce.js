function setupTinyMCE(elt, style) {
    try{ tinyMCE.remove(tinyMCE.get(elt)) }catch(err){ null };

    if(style == "full")
	tinyMCE.init({
	    mode : "exact",
	    elements: elt,
	    theme : "advanced",
	    plugins : "safari,pagebreak,style,layer,table,save,advhr,advimage,advlink,emotions,iespell,insertdatetime,preview,media,searchreplace,print,contextmenu,paste,directionality,fullscreen,noneditable,visualchars,nonbreaking,xhtmlxtras,template",
	    theme_advanced_buttons1 : "save,newdocument,|,bold,italic,underline,strikethrough,|,justifyleft,justifycenter,justifyright,justifyfull,|,styleselect,formatselect,fontselect,fontsizeselect",
	    theme_advanced_buttons2 : "cut,copy,paste,pastetext,pasteword,|,search,replace,|,bullist,numlist,|,outdent,indent,blockquote,|,undo,redo,|,link,unlink,anchor,image,cleanup,help,code,|,insertdate,inserttime,preview,|,forecolor,backcolor",
	    theme_advanced_buttons3 : "tablecontrols,|,hr,removeformat,visualaid,|,sub,sup,|,charmap,emotions,iespell,media,advhr,|,print,|,ltr,rtl,|,fullscreen",
	    theme_advanced_buttons4 : "insertlayer,moveforward,movebackward,absolute,|,styleprops,|,cite,abbr,acronym,del,ins,attribs,|,visualchars,nonbreaking,template,pagebreak",
	    theme_advanced_toolbar_location : "top",
	    theme_advanced_toolbar_align : "left",
	    theme_advanced_statusbar_location : "bottom",
	    theme_advanced_resizing : true,
	    content_css : "css/tinymce.css"
	});
    else
	tinyMCE.init({
	    mode : "exact",
	    elements: elt,
	    theme : "advanced",
	    plugins : "safari,pagebreak,style,layer,table,save,advhr,advimage,advlink,emotions,iespell,insertdatetime,preview,media,searchreplace,print,contextmenu,paste,directionality,fullscreen,noneditable,visualchars,nonbreaking,xhtmlxtras,template",
	    theme_advanced_buttons1 : "formatselect,|,bold,italic,underline,strikethrough,forecolor,backcolor,|,bullist,numlist,|,justifyleft,justifycenter,justifyright,justifyfull,outdent,indent,blockquote,|,link,unlink,|,paste,pastetext,pasteword,|,undo,redo",
	    theme_advanced_buttons2 : "",
	    theme_advanced_buttons3 : "",
	    theme_advanced_buttons4 : "",
	    theme_advanced_toolbar_location : "top",
	    theme_advanced_toolbar_align : "left",
	    theme_advanced_statusbar_location : "bottom",
	    theme_advanced_resizing : true,
	    init_instance_callback : "resizeMCE",
	    content_css : "css/tinymce.css"
	})
}

// XXX light hack. troubles are possible
function resizeMCE(){
	$('docEditorText_tbl').style.height = "300px";
	$('docEditorText_tbl').style.width = "1000px";
	$('docEditorText_ifr').style.height = "249px";
	$('docEditorText_ifr').style.width = "1000px";
}

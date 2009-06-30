/****** erlangine library start ******/
test = function(X){return X}

extract_field = function(doc, fld){
    return doc.fields.map(function(field){
	if(field.name == fld)
	    return field.values[0].value
	else return undefined
    }).filter(test)[0]
}

extract_values = function(doc, fld){
    return doc.fields.map(function(field){
	if(field.name == fld)
	    return field.values.map(function(V){return V.value})
	else return undefined
    }).filter(test)[0]
}

simplify = function(str){
    if(typeof(str) == 'string')
        return str.substr(0, 50)
                  .toLowerCase()
                  .replace(/&lt;/g,'<')
                  .replace(/&gt;/g,'>')
                  .replace(/<[^>]*>/g,' ')
                  .replace(/^[^<]*>/,'')
                  .replace(/<[^>]*$/,'')
                  .replace(/ +/g,' ')
    else return toJSON(str)
}
/******* erlangine library end *******/

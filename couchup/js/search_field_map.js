!(inc:js/erlangine.js)
function(doc){
var simplify = function(str){
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

    if(doc.type == 'entry')
        var title = extract_field(doc, 'Название');
        doc.fields.forEach(function(field){
            field.values.forEach(function(value){
                if(//(value.value.length < 50) &&
                   //(value.value.split(/\s+/).length < 4))
                   true)
                  emit([field.name.toLowerCase(), simplify(value.value)],
                       {'rev': doc._rev, 'date': doc.date, 'destination': doc.destination, 'author': doc.author,
                        'title': extract_field(doc, 'Название'), 'type': extract_values(doc, 'Тип')})
            })
        })
}

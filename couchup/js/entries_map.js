!(inc:js/erlangine.js)
function(doc){
    if(doc.type == 'entry')
        emit(doc.author, {'rev': doc._rev, 'date': doc.date, 'destination': doc.destination, 'author': doc.author,
                          'title': extract_field(doc, 'Название'), 'type': extract_values(doc, 'Тип')})
}

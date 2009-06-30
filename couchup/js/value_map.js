!(inc:js/erlangine.js)
function(doc){
    if(doc.type == 'entry')
        if(extract_field(doc, 'Тип') == 'Значение'){
            var name = extract_field(doc, 'Название');
            emit(name, {'name': name, 'type': extract_field(doc, 'Данные')})
        }
}

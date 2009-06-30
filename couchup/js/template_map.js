!(inc:js/erlangine.js)
function(doc){
    if(doc.type == 'entry')
        if(extract_field(doc, 'Тип') == 'Шаблон')
            emit(extract_field(doc, 'Название'), {'type': extract_values(doc, 'Тип')[1]})
}

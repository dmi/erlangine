function(doc){
    if(doc.type == 'entry')
        doc.fields.forEach(function(field){
            field.values.forEach(function(value){ emit([field.name, value.name, value.type], 1) })
        })
}

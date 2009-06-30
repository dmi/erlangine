function(doc){ if(doc.type == 'entry') emit({'author': doc.author, 'id': doc._id}, null) }

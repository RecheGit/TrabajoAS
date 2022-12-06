import couchdb

#Conexión con CouchDB

user = "root"
password = "1234"
couchserver = couchdb.Server("http://%s:%s@10.186.0.7:5984/" % (user, password))


#Creamos nueva BD si no existe
nombreDB=("Trabajo-AS")

if nombreDB in couchserver:
    db = couchserver[nombreDB]
else:
    db = couchdb.create(nombreDB)



#Insertamos documentos

db= couchserver['PRUEBA_AS']
nombre_alumno_1 = {
    'name': 'Kepa',
    'age': 21,
}

db.save(nombre_alumno_1)


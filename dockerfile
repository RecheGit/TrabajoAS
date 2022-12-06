FROM python:3.9


# Descargar e instalar dependencias
RUN apt-get update
RUN apt-get install python3.9
RUN pip3 install couchdb
# Comando de arranque
CMD ["python","./subida_de_datos.py]




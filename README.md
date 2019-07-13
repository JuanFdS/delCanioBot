# delCanio

## Desarollo

1. Clonar este repo
1. `$ stack setup` para instalar todas las dependencias
1. `$ stack build` para compilar

### Organizacion
Hay varios "paquetes" en el proyecto:

1. `canios`: Biblioteca compartida con la generacion de frases.
1. `canios-server`: Server web rest.
1. `canios-cli`: Para cuando necesitas una frase.
1. `canios-bot`: Postear en twitter una frase.

Los últimos 2 se pueden correr con `$ stack exec canios-server` y `stack exec canios-cli` respectivamente.

### Herramientas piolas
1. `hlint` linter. `hlint --refactor`
1. `brittany` para formatear. `$ brittany --write-mode=inplace *.hs` _(o [desde la web](https://hexagoxel.de/brittany/))_.

### Docker

Una imagen para todo! `docker-compose up` to your heart's contempt.


## Deploy
Estoy usando la imagen de docker para subir a Heroku

1. Asegurarse de estar logeado a Heroku `$ heroku login` y al hub `$ heroku container:login`
1. Buildear la imagen `docker build -t web .`
1. Empujar la imagen `heroku container:push web`
1. Hacer el deploy `heroku container:release web`


## TODO

- [x] Servir un endpoint con `delCanio` goodness via REST
- [ ] Deploy a Heroku
- [ ] Postear con una cuenta de Twitter
- [ ] Generar imagenes aplicando los pedacitos de frases
- [ ] Schedule de Heroku para :point_up: cada tanto
- [ ] Usar una base de datos para almacenar `delCanio`s
- [ ] Usar twitter para agregar a la base de datos de `delCanio`
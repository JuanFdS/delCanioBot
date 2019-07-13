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

Los últimos 2 se pueden correr con `$ stack exec canios-server` y `stack exec canios-cli` respectivamente.

### Herramientas piolas
1. `hlint` linter. `hlint --refactor`
1. `brittany` para formatear. `$ brittany --write-mode=inplace *.hs` _(o [desde la web](https://hexagoxel.de/brittany/))_.

### Docker

Una imagen para todo! `docker-compose up` to your heart's contempt.

## TODO

1. Servir un endpoint con `delCanio` goodness via REST
1. Deploy a Heroku
1. Postear con una cuenta de Twitter
1. Schedule de Heroku para :point_up: cada tanto
1. Usar una base de datos para almacenar `delCanio`s
1. Usar twitter para agregar a la base de datos de `delCanio`
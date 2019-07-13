# delCanio

## Desarollo

1. Clonar este repo
1. `$ stack setup` para instalar todas las dependencias
1. `$ stack build` para compilar
1. `$ stack exec delCanio-exe` para ejecutar el CLI

_Es particularmente comodo `stack build --file-watch --no-copy-bins --ghc-options -Wall --exec="stack exec delCanio-exe"`._

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
FROM haskell:8 as build

LABEL Author="Joaco <j@florius.com.ar>"

WORKDIR /ops

COPY package.yaml package.yaml
COPY stack.yaml stack.yaml
COPY stack.yaml.lock stack.yaml.lock

RUN stack setup
RUN stack build --only-dependencies

COPY . .

RUN stack build --test

RUN /ops/utils/cp-bin.sh

ENTRYPOINT [ "stack" ]

FROM base

COPY --from=build /ops/build/delCanio-exe /ops/delCanio-exec

CMD [ "/ops/delCanio-exec" ]
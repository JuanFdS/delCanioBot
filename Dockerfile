FROM haskell:8 as build

LABEL Author="Joaco <j@florius.com.ar>"

WORKDIR /ops

COPY . .

RUN stack setup
RUN stack build --test
RUN stack install

EXPOSE 8080
ENV PORT 8080

ENTRYPOINT [ "stack" ]
CMD [ "stack", "exec", "canios-server" ]

FROM base

COPY --from=build /ops/build/ /ops/

CMD [ "/ops/canios-server" ]
FROM docker.io/library/erlang:24.2.1.0 as builder

WORKDIR /BUILD/

COPY . .

RUN rebar3 escriptize

# ---

FROM docker.io/library/erlang:24.2.1.0-slim

WORKDIR /app/

COPY --from=builder /BUILD/_build/default/bin/oc /app/

WORKDIR /data/

ENTRYPOINT [ "/app/oc" ]

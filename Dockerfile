FROM docker.io/library/erlang:26.1.2.0-slim as base

# ---

FROM base as builder

WORKDIR /BUILD/

COPY . .

RUN rebar3 escriptize

# ---

FROM base

WORKDIR /app/

COPY --from=builder /BUILD/_build/default/bin/oc /app/

WORKDIR /data/

ENTRYPOINT [ "/app/oc" ]

FROM erlang:24-alpine

RUN apk update
# fetching deps
RUN apk add git
# c compiler
RUN apk add build-base

RUN mkdir /buildroot
WORKDIR /buildroot

COPY . /buildroot

RUN rebar3 release

FROM erlang:24-alpine

RUN apk add --no-cache libstdc++
COPY --from=0 /buildroot/_build/default/rel/divdash /divdash

RUN mkdir -p /divdash/mnesia
EXPOSE 8082

CMD ["/divdash/bin/divdash", "foreground"]

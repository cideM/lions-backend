ARG HASKELL_VERSION=8.10.7
ARG NODEJS_VERSION=19
ARG ALPINE_VERSION=3.16

FROM litestream/litestream AS litestream

FROM public.ecr.aws/docker/library/node:${NODEJS_VERSION}-alpine${ALPINE_VERSION} as client
WORKDIR /opt/app
COPY ./client/package.json ./client/package-lock.json .
RUN apk update && apk add git coreutils
RUN npm install
COPY ./client ./
RUN ./node_modules/.bin/sass --load-path=./node_modules/bootstrap/scss --style=compressed ./sass/styles.scss style.css
RUN mkdir build && ./node_modules/.bin/postcss ./style.css --use autoprefixer -d ./build/

FROM public.ecr.aws/docker/library/haskell:${HASKELL_VERSION} as base

WORKDIR /opt/app
RUN cabal update
# Add just the .cabal file to capture dependencies
COPY ./backend/lions-backend.cabal /opt/app/app.cabal
RUN apt-get update && apt-get -y install libscrypt-dev
# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4
# Add and Install Application Code
COPY ./cabal.project /opt/app
COPY ./backend /opt/app/backend
# https://github.com/haskell/cabal/issues/7236
# > Various combinations of configure/build/install and
# > --enable-executable-static / --enable-executable-stripping do not produce a
# > static + stripped executable.
RUN cabal install run-lions-backend --install-method=copy --installdir /opt/app

FROM public.ecr.aws/docker/library/haskell:${HASKELL_VERSION}
WORKDIR /opt/app
COPY --from=litestream /usr/local/bin/litestream /usr/local/bin/litestream
COPY ./entrypoint.sh ./
RUN chmod +x /opt/app/entrypoint.sh
COPY --from=base /opt/app/run-lions-backend ./run-lions-backend
COPY --from=client /opt/app/build/style.css ./public/style.css
COPY --from=client /opt/app/node_modules/bootstrap/dist/js/bootstrap.bundle.min.js ./public/bootstrap.bundle.min.js
COPY ./client/assets/ ./public/
ENTRYPOINT ["/opt/app/entrypoint.sh"]

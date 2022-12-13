FROM haskell:8

WORKDIR /opt/app

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./backend/lions-backend.cabal /opt/app/app.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/app
RUN cabal install

CMD ["server"]
ENTRYPOINT ["./entrypoint.sh"]

FROM fpco/stack-build:latest
RUN apt-get update && apt-get install -y postgresql-client netcat
RUN stack update

# hack to compile the major dependencies independent of the application code
# this increases turnaround time for Dockerfile development
RUN stack setup --resolver lts-16.9 && stack config set resolver lts-16.9
RUN stack build servant wai warp servant-server aeson postgresql-simple

RUN mkdir /app
COPY . /app
RUN cd /app && stack install

EXPOSE 8080
ENTRYPOINT [ "/root/.local/bin/motorola-exe" ]

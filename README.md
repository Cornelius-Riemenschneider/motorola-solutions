# motorola
Coding Task for Motorola Solutions.

Discussion points:
- everything implemented in Haskell, Dockerfile and docker-compose.yml are provided
 Build with `docker-compose build` and run with `docker-compose up`, then find the application at 
 `http://127.0.0.1:8080` (proxy/LB not provided).
 For persistency, mount a volume into the database server at `/var/lib/postgresql/data`.
- Due to the Haskell compiler being slow, building this initially might take a while
- It is my first time using both the libraries for the REST API, as well as the DB access
- Imo, the location API is a bit iffy - should be a separate API for adding/listing and deleting locations
- currently, modifying existing device profiles is not supported. We could easily allow the POST API to update existing records, BUT that would possibly invalidate the radio location, which would need additional logic
- Note that there is currently no separate test database - the tests (can be run with `stack test`)
  truncate the production database
- the script wait-for is from https://github.com/eficode/wait-for/

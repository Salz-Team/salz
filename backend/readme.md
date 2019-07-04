
## Start the postgresql container
docker run --name some-postgres -v /my/own/datadir:/var/lib/postgresql/data -d postgres:tag
docker run --rm   --name pg-docker -d -p 5432:5432 -v ~/Documents/salz/backend/dbdata:/var/lib/postgresql/data  postgres

# Salz

A bot based game.

## Database and API setup (non-dev)

If you need the database and API to work, use the provided `docker-compose.yml` file to spin up these services.

`salz-db` will run on `localhost:5432`

`salz-api` will run on `localhost:8080`

Make sure you have `docker` and `docker-compose` installed. To make sure `docker` is installed and running correctly (i.e., you have added yourself to the `Docker` group, and other weird linux stuff you need to get docker going) run `docker run hello-world`. Install `docker-compose`.

In the root directory, you can simply run 

```
docker-compose up
```

This will search for a `docker-compose.yml` file (which is in the root directory) and build the necessary images. Additionally, the logs from both services are tailed.

If you happen to make any changes to the database or API, you have to rebuild the images. The next time you need the services, run `docker-compose` with the `--build` flag. I.e.,

```
docker-compose up --build
```


For now, hit the `/frames/` endpoint. I.e., `localhost:8080/frames`

## Bot Requirements
Bots should be submitted as a .tar file containing a `run.sh` and a `build.sh`
in the root directory.

---

## Website

See `FrontEnd/README.md`.
